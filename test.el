
(require 'json-in-place)

(dlet ((debug-on-error t))
  (let* (
         (buffer (get-buffer-create "json-test"))
         (root (progn
                 (set-buffer buffer)
                 (pop-to-buffer buffer)
                 (erase-buffer)
                 (insert "{}")
                 (json-in-place-from-buffer buffer)))
         (root-object (oref root value))
         (test-1 (json-in-place-insert 'test1 1337 root-object))
         (arr (json-in-place-insert 'array (vector) root-object))
         (arr-val (oref arr value))
         (a-1 (json-in-place-append 1 arr-val))
         (a-2 (json-in-place-append 2 arr-val))
         (test-2 (json-in-place-insert "test2" "lol" root-object))
         (object-entry (json-in-place-insert "obj" nil root-object))
         (object (oref object-entry value))
         (test-3 (progn
                   (json-in-place-remove-entry test-2)
                   (json-in-place-insert 'test3 "lol" root-object))))
    (json-in-place-reformat)))
