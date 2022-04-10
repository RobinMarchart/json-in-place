
(require 'json-in-place)
(let ((buffer (get-buffer-create "json-test")))
  (set-buffer buffer)
  (pop-to-buffer buffer)
  (delete-region 1 (buffer-end buffer))
  (insert "{}")
  (let ((root (json-in-place-from-buffer buffer)))))
