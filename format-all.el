(defun format-all-the-buffer-c ()
  (clang-format-buffer))

(defun format-all-the-buffer-c++ ()
  (clang-format-buffer))

(defun format-all-the-buffer-go ()
  (gofmt))

(defun format-all-the-buffer-python ()
  (py-autopep8-buffer))

(defun format-all-get-formatter ()
  (case major-mode
    (c-mode       #'format-all-the-buffer-c)
    (c++-mode     #'format-all-the-buffer-c++)
    (go-mode      #'format-all-the-buffer-go)
    (python-mode  #'format-all-the-buffer-python)
    (t            (error "Don't know how to format %S code" major-mode))))

(defun format-all-the-buffer ()
  (interactive)
  (let ((formatter (format-all-get-formatter)))
    (funcall formatter)))

(provide 'format-all)
