;;; json-in-place.el --- Enable modifying json files in place -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Robin Marchart
;;
;; Author: Robin Marchart <robin.marchart@tum.de>
;; Maintainer: Robin Marchart <robin.marchart@tum.de>
;; Created: März 26, 2022
;; Modified: März 26, 2022
;; Version: 0.0.1
;; Keywords: data files lisp
;; Homepage: https://github.com/RobinMarchart/json-in-place
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Enable modifying json files in place
;;
;;; Code:

(unless (json-available-p) (error "JSON support required"))

(require 'eieio)
(require 'generator)

(defclass json-in-place-observable ()
  ((change :initform () :type list)
   (removal :initform () :type list))
  "Value that supports registering change handlers" :abstract t)
(defclass json-in-place-parent () () "Potential parent of a value." :abstract t)
(defclass json-in-place-value (json-in-place-observable)
  ((len :initarg :len :type integer)
   (parent :initarg :parent :type json-in-place-parent))
  "Serializable JSON value." :abstract t)
(defclass json-in-place-container (json-in-place-value)
  () "Container for other values." :abstract t)
(defclass json-in-place-container-entry (json-in-place-parent)
  ((offset :initarg :offset :type integer)
   ;; allow null to break dependency circle at creation
   (value :initarg :value :type (or null json-in-place-value))
   (index :initarg :index :type cons))
  "Container entry." :abstract t)

(defclass json-in-place-string (json-in-place-value)
  ((value :initarg :value :type string))
  "JSON string.")
(defclass json-in-place-number (json-in-place-value)
  ((value :initarg :value :type number))
  "JSON number. Might be ether int or float.")
(defclass json-in-place-null (json-in-place-value) () "JSON null.")
(defclass json-in-place-bool (json-in-place-value)
  ((value :initarg :value :type boolean))
  "JSON boolean. Ether t or nil")

(defclass json-in-place-array (json-in-place-container)
  ((value :initarg :value :type list))
  "JSON array.")
(defclass json-in-place-array-entry (json-in-place-container-entry)
  ((parent :initarg :parent :type json-in-place-array))
  "JSON array entry")
(defclass json-in-place-object (json-in-place-container)
  ((value :initarg :value :type list))
  "JSON object")
(defclass json-in-place-object-entry (json-in-place-container-entry)
  ((parent :initarg :parent :type json-in-place-object))
  "JSON object entry")

(defclass json-in-place-root (json-in-place-parent)
  ((buffer :initarg :buffer :type buffer)
   (value :initarg :value :type (or null json-in-place-value)))
  "root of the document")

(defvar-local json-in-place-current-parsed () "Parsed value of the buffer.")

(defun json-in-place--from-point (parent)
  "Create a string object from point.
Move point to the end of the string.
Parent is set to PARENT."
  (unless (cl-typep parent 'json-in-place-parent)
    (error "Invalid parent object %s"parent))
  (let* (
         (begin (point))
         (value (json-parse-buffer :object-type 'alist :array-type 'array))
         (end (point))
         (endb (point-max))
         (object-length (- (if (equal end (- endb 1)) endb end) begin)))
    (pcase value
      ((pred stringp) (json-in-place-string :len object-length :parent parent :value value))
      ((pred numberp) (json-in-place-number :len object-length :parent parent :value value))
      (':null (json-in-place-null :len object-length :parent parent))
      (':false(json-in-place-bool :len object-length :parent parent :value nil))
      ('t(json-in-place-bool :len object-length :parent parent :value t))
      ((pred arrayp)
       (let* ( (value (append value nil)) (loop2 nil)
               (res (json-in-place-array :len object-length :parent parent :value value)))
         (goto-char begin)
         (search-forward "[" end)
         (while value
           (if loop2 (search-forward "," end) (setq loop2 t))
           (let ((rec (json-in-place-array-entry :parent res :index value :offset (- (point) begin) :value nil)))
             (oset rec value (json-in-place--from-point rec))
             (setcar value rec))
           (setq value (cdr value)))
         (goto-char end)
         res))
      ((pred listp)
       (let ((list-head value) (loop2 nil)
             (res (json-in-place-object :len object-length :parent parent :value value)))
         (goto-char  begin)
         (search-forward "{" end)
         (while list-head
           (if loop2 (search-forward "," end) (setq loop2 t))
           (json-parse-buffer)
           (search-forward ":" end)
           (let ((rec (json-in-place-object-entry :parent res :index list-head :offset (- (point) begin) :value nil)))
             (oset rec value (json-in-place--from-point rec))
             (setcdr (car list-head) rec))
           (setq list-head (cdr list-head)))
         (goto-char end)
         res))

      (_ (error "Unable to convert %s to json-in-place object" value)))))

(defun json-in-place-from-buffer (buffer)
  "Parse in place modifiable json structure from BUFFER."
  (let ((root (json-in-place-root :buffer buffer :value nil)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char 0)
        (oset root value (json-in-place--from-point root))
        (setq json-in-place-current-parsed root))
      root)))

(defun json-in-place-reparse (buffer-or-root print-status)
  "Re parse buffer and place the new value in root.
BUFFER-OR-ROOT can be ether nil for the current buffer, the buffer containing
the already parsed value or the root of the parsed value. If PRINT-STATUS is non
nil, a message will be written on successful completion."
  (interactive "bBuffer to parse\nd")
  (when (null buffer-or-root) (setq buffer-or-root (current-buffer)))
  (when (bufferp buffer-or-root) (setq buffer-or-root (buffer-local-value 'json-in-place-current-parsed buffer-or-root)))
  (unless (json-in-place-root-p buffer-or-root) (error "Expected root to be a root value, not %s" buffer-or-root))
  (with-current-buffer (oref buffer-or-root buffer)
    (save-excursion
      (goto-char 0)
      (oset buffer-or-root value (json-in-place--from-point buffer-or-root))))
  (when print-status (message "Parsing complete")))

(cl-defgeneric json-in-place--update-parent-offsets (parent difference)
  "Move all entries after PARENT by offsets adjusted by DIFFERENCE.
Recursively update parents.")

(cl-defmethod json-in-place--update-parent-offsets ((parent json-in-place-root) (difference integer))
  "No op version for PARENT root. DIFFERENCE is therefore unused."
  (let ((_ parent) (_ difference)) ()))
(cl-defmethod json-in-place--update-parent-offsets ((parent json-in-place-array-entry) (difference integer))
  "Move all entries after PARENT by DIFFERENCE. Array version."
  (let* ((index (cdr (oref parent index))))
    (while index
      (let ((sibling (car index)))
        (oset sibling offset (+ difference (oref sibling offset))))
      (setq index (cdr index))))
  (oset parent len (+ difference (oref parent len)))
  (json-in-place--update-parent-offsets (oref (oref parent parent) parent) difference))
(cl-defmethod json-in-place--update-parent-offsets ((parent json-in-place-object-entry) (difference integer))
  "Move all entries after PARENT by DIFFERENCE. Object version."
  (let ((index (cdr (oref parent index))))
    (while index
      (let ((sibling (cdr (car index))))
        (oset sibling offset (+ difference (oref sibling offset)))
        (setq index (cdr index))))
    (oset parent len (+ difference (oref parent len)))
    (json-in-place--update-parent-offsets (oref (oref parent parent) parent) difference)))

(cl-defmethod json-in-place-iter-values ((value json-in-place-value))
  "Generator over all values in this VALUE."
  (funcall (iter-lambda () (iter-yield value))))
(cl-defmethod json-in-place-iter-values ((value json-in-place-container))
  "Generator over all values in this VALUE."
  (funcall (iter-lambda () (let ((index (oref value value)))
                             (while index
                               (iter-yield-from (json-in-place-iter-values (oref (if (json-in-place-object-p value) (cdr (car index)) (car index)) value)))
                               (setq index (cdr index)))))))

(defun json-in-place-replace-value (old new)
  "Replace OLD with NEW in both data structure and buffer.
NEW is serialized and then parsed into a new json in place value.
keys of objects can only be replaced by strings."
  (unless (cl-typep old 'json-in-place-value) (error "Invalid value for old %s" old))
  (let* ((offset 1)
         (len (oref old len))
         (parent (oref old parent))
         (modify-hooks ())
         (buffer (let ((parent parent))
                   (while (not (json-in-place-root-p parent))
                     (setq offset (+ offset (oref parent offset)))
                     (setq modify-hooks (append (oref parent change) modify-hooks))
                     (setq parent (oref (oref parent parent) parent)))
                   (oref parent buffer)))
         (end (+ offset len)))
    (atomic-change-group
      (with-current-buffer buffer
        (save-excursion
          (goto-char end)
          (delete-region offset end)
          (json-insert new)
          (goto-char offset)
          (let* ((new (json-in-place--from-point parent))
                 (difference (- len (oref new len))))
            (oset parent value new)
            (json-in-place--update-parent-offsets parent difference)))))
    (dolist (f modify-hooks) (funcall f))
    (iter-do (val (json-in-place-iter-values old)) (dolist (f (oref val removal)) (funcall f)))))

(cl-defmethod json-in-place-remove-entry ((entry json-in-place-container-entry))
  "Remove ENTRY from parent container."
  (let ((index (oref entry index)) (prev (oref (oref entry parent) value)))
    (if (eq index prev)
        ;; first value
        (oset (oref entry parent) value (cdr prev))
      (progn
        (while (not (eq index (cdr prev)))
          (setq prev (cdr prev)))
        (setcdr prev (cdr index))))
    (let* ((offset 1)
           (modify-hooks ())
           (buffer (let ((parent (oref (oref entry parent) parent)))
                     (while (not (json-in-place-root-p entry))
                       (setq offset (+ offset (oref parent offset)))
                       (setq modify-hooks (append (oref parent change) modify-hooks))
                       (setq parent (oref (oref parent parent) parent)))
                     (oref parent buffer)))
           (start (+ offset (oref entry offset)))
           (end (if
                    (cdr index)
                    (+ offset (oref (car (cdr index)) offset))
                  (+ offset (oref (oref entry parent) len) -1)))
           (difference (- start end)))
      (with-current-buffer buffer
        (delete-region start end))
      (let ((index (cdr index)))
        (if (json-in-place-array-entry-p entry)
            (while index
              (aset (car index) offset (+ difference (aref (car index) offset)))
              (setq index (cdr index)))
          (while index
            (aset (cdr (car index)) offset (+ difference (aref (cdr (car index)) offset)))
            (setq index (cdr index)))))
      (json-in-place--update-parent-offsets (oref (oref entry parent) parent) difference)
      (dolist (f modify-hooks) (funcall f))
      (iter-do (val (json-in-place-iter-values (oref entry value))) (dolist (f (oref val removal)) (funcall f))))))

(cl-defgeneric json-in-place-insert ()
  "Function used to insert values into an container.
The exact arguments depend on the kind of container.")

(cl-defmethod json-in-place-insert ((key string) value (container json-in-place-object))
  "Insert the KEY VALUE pair at the end of CONTAINER.
KEY must be a string and VALUE must be serializable to JSON. A textual JSON
representation of the KEY VALUE pair will be inserted at the right position.
CONTAINER object. OBJECT must be a json-in-place-object value."
  (let* ((offset 1)
         (modify-hooks (append (oref container change) ()))
         (begin (+ offset (oref container len) -1))
         (buffer (let ((parent (oref container parent)))
                   (while (not (json-in-place-root-p parent))
                     (setq offset (+ offset (oref parent offset)))
                     (setq modify-hooks (append (oref parent change) modify-hooks))
                     (setq parent (oref (oref parent parent) parent)))
                   (oref parent buffer))))
    (with-current-buffer buffer
      (save-excursion
        (atomic-change-group
          (goto-char begin)
          (dlet ((print-escape-newlines t)
                 (print-escape-nonascii t)
                 (print-escape-newlines t))
            (insert "," (prin1-to-string value) " : "))
          (let ((off (point)))
            (json-insert value)
            (let ((diff (- (point) begin)))
              (goto-char off)
              (let ((entry (json-in-place-object-entry :parent container :index (cons () ()) :offset off :value ())))
                (oset entry value (json-in-place--from-point entry))
                (setcar (oref entry index) (cons (intern key) entry))
                (nconc (oref container value) (oref entry index)))
              (oset container len (+ diff (oref container len)))
              (json-in-place--update-parent-offsets (oref container parent) diff))))))
    (dolist (f modify-hooks) (funcall f))))

(cl-defmethod json-in-place-insert (value (container json-in-place-array))
  "Array Version.
Insert the VALUE at the end of the CONTAINER.
The textual JSON representation is inserted in the buffer in the correct spot.
VALUE must be serializable to JSON."
  (let* ((offset 1)
         (modify-hooks (append (oref container change) ()))
         (begin (+ offset (oref container len) -1))
         (buffer (let ((parent (oref container parent)))
                   (while (not (json-in-place-root-p parent))
                     (setq offset (+ offset (oref parent offset)))
                     (setq modify-hooks (append (oref parent change) modify-hooks))
                     (setq parent (oref (oref parent parent) parent)))
                   (oref parent buffer))))
    (with-current-buffer buffer
      (save-excursion
        (atomic-change-group
          (goto-char begin)
          (insert ",")
          (let ((off (point)))
            (json-insert value)
            (let ((diff (- (point) begin)))
              (goto-char off)
              (let ((entry (json-in-place-array-entry :parent container :index (cons () ()) :offset off :value ())))
                (oset entry value (json-in-place--from-point entry))
                (setcar (oref entry index) entry)
                (nconc (oref container value) (oref entry index)))
              (oset container len (+ diff (oref container len)))
              (json-in-place--update-parent-offsets (oref container parent) diff))))))
    (dolist (f modify-hooks) (funcall f))))

(cl-defgeneric json-in-place-mark (arg) "Mark the text associated with ARG.")

(cl-defmethod json-in-place-mark ((value json-in-place-value))
  "Mark the space occupied by the JSON VALUE."
  (let* ((offset 1)
         (buffer (let ((parent (oref value parent)))
                   (while (not (json-in-place-root-p parent))
                     (setq offset (+ offset (oref parent offset)))
                     (setq parent (oref (oref parent parent) parent)))
                   (oref parent buffer))))
    (with-current-buffer buffer
      (goto-char offset)
      (push-mark (+ offset (oref value len)) t t))))

(provide 'json-in-place)
;;; json-in-place.el ends here
