;;; format-all.el --- Auto-format source code in many languages
;;
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-format-all-the-code
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: languages util
;; License: MIT
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Lets you auto-format source code in several languages using the
;; same command for all languages, instead of learning a different
;; elisp package and formatting command for each language.
;;
;; Just do M-x `format-all-the-buffer' and it will try its best to do
;; the right thing.
;;
;; For most languages, you will need to install an external program to
;; help with the formatting.  If you don't have the right program,
;; `format-all-the-buffer' will try to tell you how to install it.
;;
;;; Code:

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

(defun format-all-buffer-thunk (thunk)
  "Internal helper function to implement formatters.

THUNK is a function that implements a particular formatter.  It
takes INPUT (the unformatted source code as a string).  THUNK is
invoked such that the current buffer is an empty temp buffer.  It
should call the formatter on INPUT and write the formatted source
code output to the temp buffer.  It should return (ERRORP
ERRPUT).  ERRORP is a boolean indicating whether the formatter
caused an error and hence the contents of the temp buffer should
be discarded.  ERRPUT is a string containing all error/warning
output from the formatter (ERRPUT can also be nil if there were
no errors or warnings).

Note that in some cases we can use the output of the formatter
even if it produced warnings.  Not all warnings are errors."
  (save-excursion
    (save-restriction
      (widen)
      (let ((inbuf (current-buffer))
            (input (buffer-substring-no-properties (point-min) (point-max))))
        (with-temp-buffer
          (cl-destructuring-bind (errorp errput) (funcall thunk input)
            (let* ((no-chg (or errorp
                               (= 0 (compare-buffer-substrings inbuf nil nil
                                                               nil nil nil))))
                   (output (cond (errorp nil)
                                 (no-chg t)
                                 (t (buffer-substring (point-min) (point-max))))))
              (list output errput))))))))

(defun format-all-buffer-process
    (executable &optional ok-statuses error-regexp &rest args)
  "Internal helper function to implement formatters.

Runs the external program EXECUTABLE.  The program shall read
unformatted code from stdin, write its formatted equivalent to
stdout, and write errors/warnings to stderr.

The program should exit with status zero for the formatting to be
considered successful.  If a list of OK-STATUSES is given, all of
those are actually considered successful.  But if ERROR-REGEXP is
given, and the program's stderr contains that regexp, then the
formatting is considered failed even if the exit status is in
OK-STATUSES.  OK-STATUSES and ERROR-REGEXP are hacks to work
around formatter programs that don't make sensible use of their
exit status.

If ARGS are given, those are arguments to EXECUTABLE.  They don't
need to be shell-quoted."
  (let ((ok-statuses (or ok-statuses '(0))))
    (format-all-buffer-thunk
     (lambda (input)
       (let* ((errfile (make-temp-file "format-all-"))
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
                                 (string-match error-regexp errput))))))
         (list errorp errput))))))

(defun format-all-buffer-autopep8 (executable)
  "Format the current buffer as Python using autopep8.

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process executable nil nil "-"))

(defun format-all-buffer-clang-format (executable)
  "Format the current buffer as C/C++ using \"clang-format\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process executable))

(defun format-all-buffer-dfmt (executable)
  "Format the current buffer as D using \"dfmt\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process executable nil (regexp-quote "[error]")))

(defun format-all-buffer-elm-format (executable)
  "Format the current buffer as Elm using elm-format.

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process executable nil nil  "--yes" "--stdin"))

(defun format-all-buffer-emacs-lisp (executable)
  "Format the current buffer as Emacs Lisp using Emacs itself.

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-thunk
   (lambda (input)
     (emacs-lisp-mode)
     (insert input)
     (indent-region (point-min) (point-max))
     (format-all-fix-trailing-whitespace)
     (list nil nil))))

(defun format-all-buffer-gofmt (executable)
  "Format the current buffer as Go using gofmt.

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process executable))

(defun format-all-buffer-hindent (executable)
  "Format the current buffer as Haskell using \"hindent\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process executable))

(defun format-all-buffer-ocp-indent (executable)
  "Format the current buffer as OCaml using \"ocp-indent\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process executable))

(defun format-all-buffer-rufo (executable)
  "Format the current buffer as Ruby using \"rufo\".

EXECUTABLE is the full path to the formatter."
  (apply 'format-all-buffer-process executable nil nil
         (append (list "--simple-exit")
                 (when (buffer-file-name)
                   (list "--filename" (buffer-file-name))))))

(defun format-all-buffer-shfmt (executable)
  "Format the current buffer as Shell using \"shfmt\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process
   executable nil nil
   "-ln" (case sh-shell (bash "bash") (mksh "mksh") (t "posix"))))

(defun format-all-buffer-standard (executable)
  "Format the current buffer as JavaScript using standard.

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process
   executable '(0 1) "Parsing error:" "--fix" "--stdin"))

(defconst format-all-formatters
  '((autopep8
     (:executable "autopep8")
     (:install "pip install autopep8")
     (:function format-all-buffer-autopep8)
     (:modes python-mode))
    (clang-format
     (:executable "clang-format")
     (:install (darwin "brew install clang-format"))
     (:function format-all-buffer-clang-format)
     (:modes c-mode c++-mode))
    (dfmt
     (:executable "dfmt")
     (:install (darwin "brew install dfmt"))
     (:function format-all-buffer-dfmt)
     (:modes d-mode))
    (elm-format
     (:executable "elm-format")
     (:install (darwin "brew install elm"))
     (:function format-all-buffer-elm-format)
     (:modes elm-mode))
    (emacs-lisp
     (:executable nil)
     (:install nil)
     (:function format-all-buffer-emacs-lisp)
     (:modes emacs-lisp-mode lisp-interaction-mode))
    (gofmt
     (:executable "gofmt")
     (:install (darwin "brew install go"))
     (:function format-all-buffer-gofmt)
     (:modes go-mode))
    (hindent
     (:executable "hindent")
     (:install "stack install hindent")
     (:function format-all-buffer-hindent)
     (:modes haskell-mode))
    (ocp-indent
     (:executable "ocp-indent")
     (:install "opam install ocp-indent")
     (:function format-all-buffer-ocp-indent)
     (:modes caml-mode tuareg-mode))
    (rufo
     (:executable "rufo")
     (:install "gem install rufo")
     (:function format-all-buffer-rufo)
     (:modes ruby-mode enh-ruby-mode))
    (shfmt
     (:executable "shfmt")
     (:install (darwin "brew install shfmt"))
     (:function format-all-buffer-shfmt)
     (:modes sh-mode))
    (standard
     (:executable "standard")
     (:install "npm install standard")
     (:function format-all-buffer-standard)
     (:modes js-mode js2-mode)))
  "Table of source code formatters supported by format-all.")

(defun format-all-property-list (property formatter)
  "Internal helper function to get PROPERTY of FORMATTER."
  (cdr (or (assoc property formatter)
           (error "Property %S missing for formatter %S"
                  property formatter))))

(defun format-all-property (property formatter)
  "Internal helper function to get PROPERTY of FORMATTER."
  (dolist (choice (format-all-property-list property formatter)
                  (error "Property %S missing for formatter %S system %S"
                         property formatter system-type))
    (cond ((atom choice) (return choice))
          ((eql system-type (car choice)) (return (cadr choice))))))

(defun format-all-please-install (executable formatter)
  "Internal helper function for error about missing EXECUTABLE for FORMATTER."
  (let ((installer (format-all-property :install formatter)))
    (concat (format "You need the %S command." executable)
            (if (not installer) ""
              (format " Please install it with the command %S."
                      installer)))))

(defun format-all-formatter-executable (formatter)
  "Internal helper function to get the external program for FORMATTER."
  (let ((executable (format-all-property :executable formatter)))
    (when executable
      (or (executable-find executable)
          (error (format-all-please-install executable formatter))))))

(defun format-all-formatter-for-mode (mode)
  "Internal helper function to get the formatter corresponding to MODE."
  (dolist (formatter format-all-formatters nil)
    (when (member mode (format-all-property-list :modes formatter))
      (return formatter))))

(defun format-all-the-buffer ()
  "Auto-format the source code in the current buffer.

No disk files are touched - the buffer doesn't even need to be
saved.  If you don't like the results of the formatting, you can
use ordinary undo to get your code back to its previous state.

A suitable source code formatter is selected according to the
`major-mode' of the buffer.  Only a few programming languages are
supported currently.  Most of them use an external program to do
the formatting.  If the right program is not found, an error
message will in some cases try to tell you how you might be able
to install it on your operating system.

Any errors/warnings encountered during formatting are shown in a
buffer called *format-all-errors*."
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

;;; format-all.el ends here
