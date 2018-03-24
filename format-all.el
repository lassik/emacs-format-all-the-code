(defun format-all-the-buffer-c ()
  (clang-format-buffer))

(defun format-all-the-buffer-c++ ()
  (clang-format-buffer))

(defun format-all-the-buffer-elm ()
  (elm-mode-format-buffer t))

(defun format-all-the-buffer-emacs-lisp ()
  (save-excursion
    (save-restriction
      (widen)
      (indent-region (point-min) (point-max)))))

(defun format-all-the-buffer-go ()
  (gofmt))

(defun format-all-the-buffer-js ()
  (js-format-buffer))

(defun format-all-the-buffer-json ()
  (save-excursion
    (save-restriction
      (widen)
      (json-reformat-region (point-min) (point-max)))))

(defun format-all-the-buffer-python ()
  (py-autopep8-buffer))

(defun format-all-get-formatter ()
  (case major-mode
    (c-mode          #'format-all-the-buffer-c)
    (c++-mode        #'format-all-the-buffer-c++)
    (elm-mode        #'format-all-the-buffer-elm)
    (emacs-lisp-mode #'format-all-the-buffer-emacs-lisp)
    (go-mode         #'format-all-the-buffer-go)
    (js-mode         #'format-all-the-buffer-js)
    (js2-mode        #'format-all-the-buffer-js)
    (json-mode       #'format-all-the-buffer-json)
    (python-mode     #'format-all-the-buffer-python)
    (t               (error "Don't know how to format %S code" major-mode))))

(defun format-all-the-buffer ()
  (interactive)
  (let ((formatter (format-all-get-formatter)))
    (funcall formatter)))

(provide 'format-all)
