(defun format-all-fix-trailing-whitespace ()
  "Fix trailing whitespace since some formatters don't do that."
  (save-match-data
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match ""))
    (goto-char (point-max))
    (delete-region
     (if (re-search-backward "[^ \t\n]" nil t) (match-end 0) (point-min))
     (point-max))
    (unless (= (point-min) (point-max))
      (goto-char (point-max))
      (insert "\n"))))

(defun format-all-subprocess (executable &optional ok-statuses error-regexp
                                         &rest args)
  "Run a subprocess that reads messy code from stdin,
writes pretty code to stdout, and writes errors/warnings to stderr."
  (save-excursion
    (save-restriction
      (widen)
      (let ((inbuf (current-buffer))
            (input (buffer-substring-no-properties (point-min) (point-max))))
        (with-temp-buffer
          (let* ((ok-statuses (or ok-statuses '(0)))
                 (errfile (make-temp-file "format-all-"))
                 (status (apply #'call-process-region input nil
                                executable nil (list t errfile)
                                nil args))
                 (errput (with-temp-buffer
                           (insert-file-contents errfile)
                           (delete-file errfile)
                           (unless (= (point-min) (point-max))
                             (buffer-substring (point-min) (point-max)))))
                 (errorp (or (not (member status ok-statuses))
                             (and error-regexp errput
                                  (save-match-data
                                    (string-match error-regexp errput)))))
                 (no-chg (or errorp
                             (= 0 (compare-buffer-substrings inbuf nil nil
                                                             nil nil nil))))
                 (output (cond (errorp nil)
                               (no-chg t)
                               (t (buffer-substring (point-min) (point-max))))))
            (list output errput)))))))

(defun format-all-the-buffer-autopep8 (executable)
  (format-all-subprocess executable nil nil "-"))

(defun format-all-the-buffer-clang-format (executable)
  (format-all-subprocess executable))

(defun format-all-the-buffer-elm-format (executable)
  (format-all-subprocess executable nil nil  "--yes" "--stdin"))

(defun format-all-the-buffer-emacs-lisp (executable)
  (save-excursion
    (save-restriction
      (widen)
      (indent-region (point-min) (point-max))
      (format-all-fix-trailing-whitespace))))

(defun format-all-the-buffer-gofmt (executable)
  (format-all-subprocess executable))

(defun format-all-the-buffer-standard (executable)
  (format-all-subprocess executable '(0 1) "Parsing error:" "--fix" "--stdin"))

(defconst format-all-formatters
  '((autopep8
     (:executable "autopep8")
     (:install "pip install autopep8")
     (:function format-all-the-buffer-autopep8)
     (:modes python-mode))
    (clang-format
     (:executable "clang-format")
     (:install (darwin "brew install clang-format"))
     (:function format-all-the-buffer-clang-format)
     (:modes c-mode c++-mode))
    (elm-format
     (:executable "elm-format")
     (:install (darwin "brew install elm"))
     (:function format-all-the-buffer-elm-format)
     (:modes elm-mode))
    (emacs-lisp
     (:executable nil)
     (:install nil)
     (:function format-all-the-buffer-emacs-lisp)
     (:modes emacs-lisp-mode))
    (gofmt
     (:executable "gofmt")
     (:install (darwin "brew install go"))
     (:function format-all-the-buffer-gofmt)
     (:modes go-mode))
    (standard
     (:executable "standard")
     (:install "npm install standard")
     (:function format-all-the-buffer-standard)
     (:modes js-mode js2-mode))))

(defun format-all-property-list (property formatter)
  (cdr (or (assoc property formatter)
           (error "Property %S missing for formatter %S"
                  property formatter))))

(defun format-all-property (property formatter)
  (dolist (choice (format-all-property-list property formatter)
                  (error "Property %S missing for formatter %S system %S"
                         property formatter system-type))
    (cond ((atom choice) (return choice))
          ((eql system-type (car choice)) (return (cadr choice))))))

(defun format-all-please-install (executable formatter)
  (let ((installer (format-all-property :install formatter)))
    (concat (format "You need the %S command." executable)
            (if (not installer) ""
              (format " Please install it with the command %S."
                      installer)))))

(defun format-all-formatter-executable (formatter)
  (let ((executable (format-all-property :executable formatter)))
    (when executable
      (or (executable-find executable)
          (error (format-all-please-install executable formatter))))))

(defun format-all-formatter-for-mode (mode)
  (dolist (formatter format-all-formatters nil)
    (when (member mode (format-all-property-list :modes formatter))
      (return formatter))))

(defun format-all-the-buffer ()
  (interactive)
  (let* ((formatter (or (format-all-formatter-for-mode major-mode)
                        (error "Don't know how to format %S code" major-mode)))
         (f-function (format-all-property :function formatter))
         (executable (format-all-formatter-executable formatter)))
    (cl-destructuring-bind (output errput) (funcall f-function executable)
      (case output
        ((nil)
         (message "Syntax error"))
        ((t)
         (message "Already formatted"))
        (t
         (message "Reformatted!")
         (erase-buffer)
         (insert output)))
      (with-current-buffer (get-buffer-create "*format-all-errors*")
        (erase-buffer)
        (when errput
          (insert errput)
          (display-buffer (current-buffer)))))))

(provide 'format-all)
