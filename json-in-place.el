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

(require 'eieio)
(require 'json)

(eval-and-compile
  (if (json-available-p)
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
             (dlet ((json-false :false) (json-null :null))
               (insert (json-encode value))))
      (defun json-in-place--json-parse () "Parses json value from point. Point is moved to after the value.
json types are represented as follows:
null: ':null
false: ':false
true: t
array: array
object: alist"
             (dlet ((json-false :false) (json-null :null) (json-object-type 'alist) (json-array-type 'vector))
               (json-read))))))

(defclass json-in-place-parent () () "Potential parent of a value." :abstract t)
(defclass json-in-place-value ()
  ((len :initarg :len :type integer)
   (parent :initarg :parent :type json-in-place-parent)
   (change :initform () :type list)
   (removal :initform () :type list)
   (verifier :type function :initform #'ignore)
   (active :initform t :type boolean))
  "Serializable JSON value.
LEN is the length of the item in buffer.
PARENT is the parent value
CHANGE is a function taking two arguments (new and old) executed when a non
array/object value is replaced or an array/object is replaced with another type.
REMOVAL is executed with the old entry if it is to be removed
VERIFIER is a function that takes the value and returns nil if the value is
valid, else a string describing the problem
ACTIVE is t, if this value is part of a json hierarchy." :abstract t)
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
  (oset (oref parent parent) len (+ difference (oref (oref parent parent) len)))
  (json-in-place--update-parent-offsets (oref (oref parent parent) parent) difference))
(cl-defmethod json-in-place--update-parent-offsets ((parent json-in-place-object-entry) (difference integer))
  "Move all entries after PARENT by DIFFERENCE. Object version."
  (let ((index (cdr (oref parent index))))
    (while index
      (let ((sibling (cdr (car index))))
        (oset sibling offset (+ difference (oref sibling offset)))
        (setq index (cdr index))))
    (oset (oref parent parent) len (+ difference (oref (oref parent parent) len)))
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
  "execute all child-change hooks of CURRENT and all parents recursively
CURRENT has to be ether root or an container entry"
  (unless (json-in-place-root-p current)
    (unless (cl-typep current 'json-in-place-container-entry) (error "Invalid value for current: %s" current))
    (dolist (f (oref (oref current parent) child-change)) (funcall f current))
    (json-in-place--run-child-change-hooks (oref (oref current parent) parent))))

(defun json-in-place--run-change-delete-hooks (new old)
  "Run hooks to be executed when OLD is replaced by NEW.
All hooks are appended to NEW from OLD if appropriate."
  (unless (cl-typep new 'json-in-place-value) (error "Expected value, got: %s" new))
  (unless (cl-typep old 'json-in-place-value) (error "Expected value, got: %s" old))
  (oset old active nil)
  (if (and (cl-typep new 'json-in-place-container) (eq (eieio-object-class new) (eieio-object-class old)))
      (progn
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
              (dolist (f (oref old container-change)) (funcall f 'add new (cdr entry))))
            (dolist (entry (cdr old-l))
              (oset (oref (cdr entry) value) active nil)
              (dolist (f (oref old container-change)) (funcall f 'remove new (cdr entry)))
              (json-in-place--run-entry-remove-hooks (cdr entry)))
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

(defun json-in-place--move-hooks (new old)
  "Run hooks to be executed when OLD is replaced by NEW.
All hooks are appended to NEW from OLD if appropriate."
  (unless (cl-typep new 'json-in-place-value) (error "Expected value, got: %s" new))
  (unless (cl-typep old 'json-in-place-value) (error "Expected value, got: %s" old))
  (oset new removal (nconc (oref new removal) (oref old removal)))
  (oset new change (nconc (oref new change) (oref old change)))
  (oset new verifier (oref old verifier))
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
              (setq old-i (cdr old-i)))))

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
                  (setq new-i (cdr new-i)))))))
         (t (error "Unknown container type: %s" (eieio-object-class new)))))))

(defun json-in-place--verify (root)
  "Verify ROOT structure by calling the installed verifier hooks.
Return an alist of path to offending item and the problem."
  (unless (json-in-place-root-p root)
    (error "Invalid root value %s" root))
  (let ((errors ())
        (to-verify (cons `(() . ,(oref root value)) ())))
    (while to-verify
      (let* ((path (car (car to-verify)))
             (value (cdr (car to-verify)))
             (verify-result (funcall (oref value verifier) value)))
        (when verify-result
          (unless (stringp verify-result) (error "verifier should return string value on error."))
          (setq errors (nconc errors (cons `(,path . , verify-result) ()))))
        (cond
         ((json-in-place-array-p value)
          (let* ((children (copy-sequence (oref value value)))
                 (index 0)
                 (current children))
            (while current
              (setcar current `(,(nconc path (cons index ())) . ,(oref (car current) value)))
              (setq index (1+ index))
              (setq current (cdr current)))
            (setq to-verify (nconc to-verify children))))
         ((json-in-place-object-p value)
          (setq to-verify
                (nconc to-verify
                       (mapcar
                        (lambda (v)
                          `(,(nconc path (cons (car v) ())) . , (oref (cdr v) value)))
                        (oref value value)))))))
      (setq to-verify (cdr to-verify)))
    errors))

(defun json-in-place--from-point (parent)
  "Create a string object from point.
Move point to the end of the string.
Parent is set to PARENT."
  (unless (cl-typep parent 'json-in-place-parent)
    (error "Invalid parent object %s"parent))
  (let* (
         (begin (point))
         (value (json-in-place--json-parse))
         (end (point))
         (object-length (- end begin)))
    (pcase value
      ((pred stringp) (json-in-place-string :len object-length :parent parent :value value))
      ((pred numberp) (json-in-place-number :len object-length :parent parent :value value))
      (:null (json-in-place-null :len object-length :parent parent))
      (:false(json-in-place-bool :len object-length :parent parent :value nil))
      ((guard (eq value t)) (json-in-place-bool :len object-length :parent parent :value t))
      ((pred arrayp)
       (let* ( (index (append value ())) (loop-start t)
               (array (json-in-place-array :len object-length :parent parent :value index)))
         (goto-char begin)
         (search-forward "[" end)
         (while index
           ;; skip , after each entry
           (if loop-start (setq loop-start nil) (search-forward "," end))
           (let ((entry (json-in-place-array-entry
                         :parent array :index index :offset (- (point) begin) :value nil)))
             (oset entry value (json-in-place--from-point entry))
             ;; set index
             (setcar index entry))
           ;; iterate through list
           (setq index (cdr index)))
         (goto-char end)
         array))
      ((pred listp)
       (let ((loop-start t)
             (object (json-in-place-object :len object-length :parent parent :value value)))
         (goto-char  begin)
         (search-forward "{" end)
         (while value
           (if loop-start (setq loop-start nil) (search-forward "," end))
           (json-parse-buffer)
           (search-forward ":" end)
           (let* ((entry (json-in-place-object-entry :parent object :index value :offset (- (point) begin) :value nil)))
             (oset entry value (json-in-place--from-point entry))
             (setcdr (car value) entry))
           (setq value (cdr value)))
         (goto-char end)
         object))
      (_ (error "Unable to convert %s to json-in-place object" value)))))

(defun json-in-place-from-buffer (buffer)
  "Parse in place modifiable json structure from BUFFER."
  (let ((root (json-in-place-root :buffer buffer :value nil)))
    (with-current-buffer buffer
      (save-excursion
        ;; ensure trailing newline
        (unless (equal "\n" (buffer-substring (- (point-max) 1) (point-max)))
          (goto-char (point-max))
          (insert "\n"))
        (goto-char (point-min))
        (oset root value (json-in-place--from-point root))
        (setq json-in-place-current-parsed root))
      root)))

(defun json-in-place-reparse (&optional buffer-or-root print-status)
  "Re parse buffer and place the new value in root.
BUFFER-OR-ROOT can be ether nil for the current buffer, the buffer containing
the already parsed value, its name or the root of the parsed value. If
PRINT-STATUS is non nil, a message will be written on successful completion."
  (interactive "bBuffer to parse\nd")
  (when (null buffer-or-root) (setq buffer-or-root (current-buffer)))
  (when (stringp buffer-or-root) (setq buffer-or-root (get-buffer buffer-or-root)))
  (when (bufferp buffer-or-root) (setq buffer-or-root (buffer-local-value 'json-in-place-current-parsed buffer-or-root)))
  (unless (json-in-place-root-p buffer-or-root) (error "Expected root to be a root value, not %s" buffer-or-root))
  (with-current-buffer (oref buffer-or-root buffer)
    (save-excursion
      (unless (equal "\n" (buffer-substring (- (point-max) 1) (point-max)))
        (goto-char (point-max))
        (insert "\n"))
      (goto-char (point-min))
      (let ((old (oref buffer-or-root value))
            (new (json-in-place--from-point buffer-or-root)))
        (json-in-place--move-hooks new old)
        (oset buffer-or-root value new)
        (let ((verify-result (json-in-place--verify buffer-or-root)))
          (when verify-result
            (oset buffer-or-root value old)
            (error "Verification of new buffer contents failed:\n%s" verify-result)))
        (json-in-place--run-change-delete-hooks new old))))
  (when print-status (message "Parsing complete")))

(defun json-in-place-replace-value (old new)
  "Replace OLD with NEW in both data structure and buffer.
NEW is serialized and then parsed into a new json in place value.
keys of objects can only be replaced by strings."
  (unless (cl-typep old 'json-in-place-value) (error "Invalid value for old %s" old))
  (let* ((offset 1)
         (len (oref old len))
         (parent (oref old parent))

         (root (let ((parent parent))
                 (while (not (json-in-place-root-p parent))
                   (setq offset (+ offset (oref parent offset)))
                   (setq parent (oref (oref parent parent) parent)))
                 parent))
         (buffer (oref root buffer))
         (end (+ offset len)))
    (atomic-change-group
      (with-current-buffer buffer
        (save-excursion
          (goto-char end)
          (delete-region offset end)
          (json-in-place--json-insert new)
          (goto-char offset)
          (let* ((new (json-in-place--from-point parent))
                 (difference (- len (oref new len))))
            (oset parent value new)
            (json-in-place--move-hooks new old)
            (let ((verify-result (json-in-place--verify root)))
              (when verify-result
                (oset parent value old)
                (error "Verification of replacement failed:\n%s" verify-result)))
            (json-in-place--update-parent-offsets parent difference)
            (json-in-place--run-child-change-hooks (oref new parent))
            (json-in-place--run-change-delete-hooks new old)
            new))))))

(defun json-in-place-remove-entry (entry)
  "Remove ENTRY from parent container."
  (unless (cl-typep entry 'json-in-place-container-entry)
    (error "Invalid container entry: %s" entry))
  (let ((index (oref entry index)) (prev (oref (oref entry parent) value)))
    (if (eq index prev)
        ;; first value
        (oset (oref entry parent) value (cdr prev))
      (progn
        (while (not (eq index (cdr prev)))
          (setq prev (cdr prev)))
        (setcdr prev (cdr index))))
    (let* ((offset 1)
           (root (let ((parent (oref (oref entry parent) parent)))
                   (while (not (json-in-place-root-p parent))
                     (setq offset (+ offset (oref parent offset)))
                     (setq parent (oref (oref parent parent) parent)))
                   parent))
           (buffer (oref root buffer))
           (start (+ offset (oref (cdr (car prev)) offset) (oref (oref (cdr (car prev)) value) len)))
           (end (+ offset (oref entry offset) (oref (oref entry value) len)))
           (difference (- start end)))
      (let ((verify-result (json-in-place--verify root)))
        (when verify-result
          (setcdr prev index)
          (error "Verification after remove failed:\n%s" verify-result)))
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
      (oset (oref entry parent) len (+ difference (oref (oref entry parent) len)))
      (json-in-place--update-parent-offsets (oref (oref entry parent) parent) difference)
      (json-in-place--run-child-change-hooks (oref (oref entry parent) parent))
      (json-in-place--run-entry-remove-hooks entry)
      (dolist (f (oref (oref entry parent) container-change )) (funcall f 'remove (oref entry parent) entry)))))

(defun json-in-place-insert (key value container)
  "Insert the KEY VALUE pair at the end of CONTAINER.
KEY must be a symbol or string and VALUE must be serializable to JSON. A textual
JSON representation of the KEY VALUE pair will be inserted at the right
position. CONTAINER object. OBJECT must be a json-in-place-object value."
  (unless (or (symbolp key) (stringp key)) (error "Invalid symbol or string: %s" key))
  (unless (json-in-place-object-p container) (error "Invalid json-in-place-object: %s" container))
  (when (stringp key) (setq key (intern key)))
  (let* ((offset 1)
         (root (let ((parent (oref container parent)))
                 (while (not (json-in-place-root-p parent))
                   (setq offset (+ offset (oref parent offset)))
                   (setq parent (oref (oref parent parent) parent)))
                 parent))
         (buffer (oref root buffer))
         (begin (+ offset (oref container len) -1)))
    (with-current-buffer buffer
      (save-excursion
        (atomic-change-group
          (goto-char begin)
          (dlet ((print-escape-newlines t)
                 (print-escape-nonascii t)
                 (print-escape-newlines t))
            (when (oref container value) (insert ","))
            (insert "\n" (prin1-to-string (symbol-name key)) ": "))
          (let ((off (point)))
            (json-in-place--json-insert value)
            (let ((diff (- (point) begin)))
              (goto-char off)
              (let ((entry (json-in-place-object-entry :parent container :index (cons () ()) :offset (- off offset) :value ())))
                (oset entry value (json-in-place--from-point entry))
                (setcar (oref entry index) (cons key entry))
                (oset container value (nconc (oref container value) (oref entry index)))
                (let ((verify-result (json-in-place--verify root)))
                  (when verify-result
                    ;; remove inserted value
                    (oset container value (seq-take (oref container value) (- (seq-length (oref container value)) 1)))
                    (error "Verification failed after insertion:\n%s" verify-result)))
                (oset container len (+ diff (oref container len)))
                (json-in-place--update-parent-offsets (oref container parent) diff)
                (dolist (f (oref container container-change)) (funcall f 'add container entry))
                (json-in-place--run-child-change-hooks (oref container parent))
                entry))))))))

(defun json-in-place-append (value container)
  "Array Version.
Insert the VALUE at the end of the CONTAINER.
The textual JSON representation is inserted in the buffer in the correct spot.
VALUE must be serializable to JSON."
  (unless (json-in-place-array-p container) (error "Invalid json-in-place-array: %s" container))
  (let* ((offset 1)
         (root (let ((parent (oref container parent)))
                 (while (not (json-in-place-root-p parent))
                   (setq offset (+ offset (oref parent offset)))
                   (setq parent (oref (oref parent parent) parent)))
                 parent))
         (buffer (oref root buffer))
         (begin (+ offset (oref container len) -1)))
    (with-current-buffer buffer
      (save-excursion
        (atomic-change-group
          (goto-char begin)
          (unless (eq () (oref container value)) (insert ","))
          (insert "\n")
          (let ((off (point)))
            (json-in-place--json-insert value)
            (let ((diff (- (point) begin)))
              (goto-char off)
              (let ((entry (json-in-place-array-entry :parent container :index (cons () ()) :offset (- off offset) :value ())))
                (oset entry value (json-in-place--from-point entry))
                (setcar (oref entry index) entry)
                (oset container value (nconc (oref container value) (oref entry index)))
                (let ((verify-result (json-in-place--verify root)))
                  (when verify-result
                    (oset container value (seq-take (oref container value) (- (seq-length (oref container value)) 1)))
                    (error "Verification failed after appending value:\n%s" verify-result)))
                (oset container len (+ diff (oref container len)))
                (json-in-place--update-parent-offsets (oref container parent) diff)
                (dolist (f (oref container container-change)) (funcall f 'add container entry))
                (json-in-place--run-child-change-hooks (oref container parent))
                entry))))))))

(defun json-in-place-reformat (&optional buffer) "Reformat json in BUFFER and reparse it"
       (interactive "bBuffer to parse")
       (unless buffer (setq buffer (current-buffer)))
       (when (stringp buffer) (setq buffer (get-buffer buffer)))
       (unless (bufferp buffer) (error "Invalid argument, expected buffer, got %s" buffer))
       (with-current-buffer buffer
         (save-excursion
           (json-pretty-print-buffer)
           (json-in-place-reparse))))

(provide 'json-in-place)
;;; json-in-place.el ends here
