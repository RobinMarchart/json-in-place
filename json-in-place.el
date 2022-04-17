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
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Enable modifying json files in place
;;
;;; Code:

(require 'eieio)
(require 'json)

(eval-and-compile
  (if (and (> emacs-major-version 28) (> emacs-minor-version 1) (fboundp 'json-available-p) (funcall (symbol-function 'json-available-p)))
      ;; use native parsing
      (progn
        (defalias 'json-in-place--json-insert #'json-insert
          "Inserts json VALUE before point.
Special mappings:
':null->null
':false->false
nil->{}")
        (defun json-in-place--json-parse () "Parses json value from point. Point is moved to after the value.
json types are represented as follows:
null: ':null
false: ':false
true: t
array: array
object: alist"
               (json-parse-buffer :object-type 'alist :array-type 'array)))
    ;; elisp version for compatibility
    (progn
      (defun json-in-place--json-insert (value) "Inserts json VALUE before point.
Special mappings:
':null->null
':false->false
nil->{}"
             (let (_)
               (defvar json-false)
               (defvar json-null)
               (let ((json-false ':false) (json-null ':null))
                 (insert (json-encode value)))))
      (defun json-in-place--json-parse () "Parses json value from point. Point is moved to after the value.
json types are represented as follows:
null: ':null
false: ':false
true: t
array: array
object: alist"
             (let (_)
               (defvar json-false)
               (defvar json-null)
               (defvar json-object-type)
               (defvar json-array-type)
               (let ((json-false) ':false (json-null ':null) (json-object-type 'alist) (json-array-type 'vector))))))))

(defclass json-in-place-observable ()
  ((change :initform () :type list)
   (removal :initform () :type list))
  "Value that supports registering change handlers.
CHANGE is a function taking two arguments (new and old) executed when a non
array/object value is replaced or an array/object is replaced with another type.
REMOVAL is executed with the old entry if it is to be removed."
  :abstract t)
(defclass json-in-place-parent () () "Potential parent of a value." :abstract t)
(defclass json-in-place-value (json-in-place-observable)
  ((len :initarg :len :type integer)
   (parent :initarg :parent :type json-in-place-parent)
   (active :initform t :type boolean))
  "Serializable JSON value." :abstract t)
(defclass json-in-place-container (json-in-place-value)
  ((child-change :initform () :type list)
   (container-change :initform () :type list)) "Container for other values.
CHILD-CHANGE is executed with the current value if at least one child is modified.
CONTAINER-CHANGE is executed with a symbol (remove or add) describing the operation, the container and the entry." :abstract t)
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

(defun json-in-place--run-entry-remove-hooks (entry)
  "Run removal hooks for ENTRY recursively."
  (cond
   ((cl-typep entry 'json-in-place-container-entry)
    (let ((value (oref entry value)))
      (dolist (f (oref value removal)) (funcall f entry))
      (oset value active nil)
      (cond
       ((json-in-place-array-p value)
        (dolist (e (oref value value)) (json-in-place--run-entry-remove-hooks e)))
       ((json-in-place-object-p value)
        (dolist (e (oref value value)) (json-in-place--run-entry-remove-hooks (cdr e)))))))
   (t (error "expected container entry, got: %s" entry))))

(defun json-in-place--run-child-change-hooks (current)
  "execute all child-change hooks of CURRENT and all parents recursively"
  (unless (json-in-place-root-p current)
    (unless (cl-typep current 'json-in-place-container) (error "Invalid value for current: %s" current))
    (dolist (f (oref current child-change)) (funcall f current))
    (json-in-place--run-child-change-hooks (oref (oref current parent) parent))))

(defun json-in-place--run-change-delete-hooks (new old)
  "Run hooks to be executed when OLD is replaced by NEW.
All hooks are appended to NEW from OLD if appropriate."
  (unless (cl-typep new 'json-in-place-value) (error "Expected value, got: %s" new))
  (unless (cl-typep old 'json-in-place-value) (error "Expected value, got: %s" old))
  (oset new removal (nconc (oref new removal) (oref old removal)))
  (oset new change (nconc (oref new change) (oref old change)))
  (oset old active nil)
  (if (and (cl-typep new 'json-in-place-container) (eq (eieio-object-class new) (eieio-object-class old)))
      (progn
        (oset new child-change (nconc (oref new child-change) (oref old child-change)))
        (oset new container-change (nconc (oref new child-change) (oref old child-change)))
        (cond
         ((json-in-place-array-p new)
          (let ((new-i (oref new value))
                (old-i (oref old value)))
            (while (and new-i old-i)
              (json-in-place--run-change-delete-hooks (oref (car new-i) value) (oref (car old-i) value))
              (setq new-i (cdr new-i))
              (setq old-i (cdr old-i)))
            (cond
             (new-i (dolist (entry new-i)
                      (dolist (f (oref old container-change)) (funcall f 'add new entry))))
             (old-i (dolist (entry old-i)
                      (oset (oref entry value) active nil)
                      (dolist (f (oref old container-change)) (funcall f 'remove new entry))
                      (json-in-place--run-entry-remove-hooks entry)))
             (t (dolist (f (oref old child-change)) (funcall f new))))))
         ((json-in-place-object-p new)
          (let* ((new-l (cons nil (copy-sequence (oref new value))))
                 (old-l (cons nil (copy-sequence (oref old value))))
                 (new-i new-l))
            (while (cdr new-i)
              (let ((old-i old-l) (not-found t))
                (while (and (cdr old-i) not-found)
                  (if (eq (car (cdr new-i)) (car (cdr old-i)))
                      (progn
                        (setq not-found nil)
                        (json-in-place--run-change-delete-hooks (oref (cdr (car (cdr new-i))) value) (oref (cdr (car (cdr (old-i)))) value))
                        (setcdr new-i (cdr (cdr new-i)))
                        (setcdr old-i (cdr (cdr old-i))))
                    (setq old-i (cdr old-i))))
                (when not-found
                  (setq new-i (cdr new-i)))))
            (dolist (entry (cdr new-l))
              (dolist (f (oref old container-change)) (funcall f 'add new entry)))
            (dolist (entry (cdr old-l))
              (oset (oref entry value) active nil)
              (dolist (f (oref old container-change)) (funcall f 'remove new entry))
              (json-in-place--run-entry-remove-hooks entry))
            (unless (or (cdr new-l) (cdr old-l))
              (dolist (f (oref old child-change)) (funcall f new)))))
         (t (error "Unknown container type: %s" (eieio-object-class new)))))
    (progn
      (oset old active nil)
      (dolist (f (oref old change)) (funcall f new old))
      (when (json-in-place-array-p old)
        (dolist (entry (oref old value)) (json-in-place--run-entry-remove-hooks entry)))
      (when (json-in-place-object-p old)
        (dolist (entry (oref old value)) (json-in-place--run-entry-remove-hooks (cdr entry)))))))


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

(defun json-in-place-reparse (&optional buffer-or-root print-status)
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
      (let ((old (oref buffer-or-root value)))
        (oset buffer-or-root value (json-in-place--from-point buffer-or-root))
        (json-in-place--run-change-delete-hooks (oref buffer-or-root value) old))))
  (when print-status (message "Parsing complete")))


(defun json-in-place-replace-value (old new)
  "Replace OLD with NEW in both data structure and buffer.
NEW is serialized and then parsed into a new json in place value.
keys of objects can only be replaced by strings."
  (unless (cl-typep old 'json-in-place-value) (error "Invalid value for old %s" old))
  (let* ((offset 1)
         (len (oref old len))
         (parent (oref old parent))

         (buffer (let ((parent parent))
                   (while (not (json-in-place-root-p parent))
                     (setq offset (+ offset (oref parent offset)))
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
            (json-in-place--update-parent-offsets parent difference)
            (json-in-place--run-child-change-hooks (oref (oref old parent) parent))
            (json-in-place--run-change-delete-hooks new old)
            new))))))

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
           (buffer (let ((parent (oref (oref entry parent) parent)))
                     (while (not (json-in-place-root-p entry))
                       (setq offset (+ offset (oref parent offset)))
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
      (json-in-place--run-child-change-hooks (oref (oref entry parent) parent))
      (json-in-place--run-entry-remove-hooks entry)
      (dolist (f (oref (oref entry parent) change)) (funcall f))
      (iter-do (f (json-in-place--iter-parent-change-hooks (oref (oref (oref entry parent) parent) parent))) (funcall f))
      (iter-do (f (json-in-place--iter-entry-remove entry)) (funcall f)))))

(cl-defgeneric json-in-place-insert ()
  "Function used to insert values into an container.
The exact arguments depend on the kind of container.")

(cl-defmethod json-in-place-insert ((key string) value (container json-in-place-object))
  "Insert the KEY VALUE pair at the end of CONTAINER.
KEY must be a string and VALUE must be serializable to JSON. A textual JSON
representation of the KEY VALUE pair will be inserted at the right position.
CONTAINER object. OBJECT must be a json-in-place-object value."
  (json-in-place-insert (intern key) value container))


(cl-defmethod json-in-place-insert ((key symbol) value (container json-in-place-object))
  "Insert the KEY VALUE pair at the end of CONTAINER.
KEY must be a symbol and VALUE must be serializable to JSON. A textual JSON
representation of the KEY VALUE pair will be inserted at the right position.
CONTAINER object. OBJECT must be a json-in-place-object value."
  (let* ((offset 1)
         (modify-hooks (append (oref container change) ()))
         (begin (+ offset (oref container len) -1))
         (buffer (let ((parent (oref container parent)))
                   (while (not (json-in-place-root-p parent))
                     (setq offset (+ offset (oref parent offset)))
                     (setq modify-hooks (append (oref (oref parent parent) change) modify-hooks))
                     (setq parent (oref (oref parent parent) parent)))
                   (oref parent buffer))))
    (with-current-buffer buffer
      (save-excursion
        (atomic-change-group
          (goto-char begin)
          (dlet ((print-escape-newlines t)
                 (print-escape-nonascii t)
                 (print-escape-newlines t))
            (unless (eq () (oref container value)) (insert ","))
            (insert (prin1-to-string (symbol-name key)) " : "))
          (let ((off (point)))
            (json-insert value)
            (let ((diff (- (point) begin)))
              (goto-char off)
              (let ((entry (json-in-place-object-entry :parent container :index (cons () ()) :offset off :value ())))
                (oset entry value (json-in-place--from-point entry))
                (setcar (oref entry index) (cons key entry))
                (oset container value (nconc (oref container value) (oref entry index)))
                (oset container len (+ diff (oref container len)))
                (json-in-place--update-parent-offsets (oref container parent) diff)
                (dolist (f modify-hooks) (funcall f))
                entry))))))))

(cl-defmethod json-in-place-append (value (container json-in-place-array))
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
                     (setq modify-hooks (append (oref (oref parent parent) change) modify-hooks))
                     (setq parent (oref (oref parent parent) parent)))
                   (oref parent buffer))))
    (with-current-buffer buffer
      (save-excursion
        (atomic-change-group
          (goto-char begin)
          (unless (eq () (oref container value)) (insert ","))
          (let ((off (point)))
            (json-insert value)
            (let ((diff (- (point) begin)))
              (goto-char off)
              (let ((entry (json-in-place-array-entry :parent container :index (cons () ()) :offset off :value ())))
                (oset entry value (json-in-place--from-point entry))
                (setcar (oref entry index) entry)
                (oset container value (nconc (oref container value) (oref entry index)))
                (oset container len (+ diff (oref container len)))
                (json-in-place--update-parent-offsets (oref container parent) diff)
                (dolist (f modify-hooks) (funcall f))
                entry))))))))

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

(defun json-in-place-reformat (buffer) "Reformat json in BUFFER and reparse it"
       (interactive "bBuffer to parse")
       (unless (bufferp buffer) (error "Invalid argument, expected buffer, got %s" buffer))
       (with-current-buffer buffer
         (save-excursion
           (json-pretty-print-buffer)
           (goto-char 0)
           (if json-in-place-current-parsed
               (let ((original (oref json-in-place-current-parsed value)))
                 (oset json-in-place-current-parsed value (json-in-place--from-point json-in-place-current-parsed))
                 (iter-do (val (json-in-place-iter-values (oref entry value))) (dolist (f (oref val removal)) (funcall f))))
             (let ((root (json-in-place-root :buffer buffer :value nil)))
               (oset root value (json-in-place--from-point root))
               (setq json-in-place-current-parsed root))))))

(provide 'json-in-place)
;;; json-in-place.el ends here
