(with-temp-buffer
  (insert-file-contents "../format-all.el")
  (let (forms)
    (condition-case () (while t (push (read (current-buffer)) forms))
      (end-of-file (setq forms (reverse forms))))
    (let* ((formatter-names
            (reverse
             (cl-reduce
              (lambda (names form)
                (if (and (consp form)
                         (eql 'define-format-all-formatter (cl-first form)))
                    (cons (cl-second form) names)
                  names))
              forms :initial-value '())))
           (sorted-names (sort (cl-copy-list formatter-names) 'string<)))
      (or (and (equal formatter-names sorted-names)
               (message "We have %d formatters" (length formatter-names)))
          (error "Formatter names are not in alphabetical order")))))
