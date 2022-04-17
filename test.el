
(require 'json-in-place)

(let* (
       (buffer (get-buffer-create "json-test"))
       (root (progn
               (set-buffer buffer)
               (pop-to-buffer buffer)
               (erase-buffer)
               (insert "{}")
               (json-in-place-from-buffer buffer)))
       (root-object (oref root value))
       (test (json-in-place-insert 'test 1337 root-object))
       (arr (json-in-place-insert 'array (vector) root-object))
       (arr-val (oref arr value))
       (arr1 (json-in-place-append 1 arr-val))
       (arr2 (json-in-place-append 2 arr-val))))
