;;; format-all.el --- Auto-format C, C++, JS, Python, Ruby and 20 other languages
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
;; Lets you auto-format source code in many languages using the same
;; command for all languages, instead of learning a different elisp
;; package and formatting command for each language.  Just do M-x
;; `format-all-buffer' and it will try its best to do the right thing.
;;
;; Supported languages:
;;
;; - C/C++/Objective-C (clang-format)
;; - CSS/Less/SCSS (prettier)
;; - D (dfmt)
;; - Elm (elm-format)
;; - Emacs Lisp (native)
;; - Go (gofmt)
;; - GraphQL (prettier)
;; - Haskell (hindent)
;; - JavaScript/JSON/JSX/TypeScript/Vue (prettier)
;; - Markdown (prettier)
;; - OCaml (ocp-indent)
;; - Perl (perltidy)
;; - Python (autopep8)
;; - Ruby (rufo)
;; - Rust (rustfmt)
;; - Shell script (shfmt)
;; - Swift (swiftformat)
;;
;; You will need to install external programs to do the formatting.
;; If `format-all-buffer` can't find the right program, it will try to
;; tell you how to install it.
;;
;; There is currently no before-save hook and no customize variables
;; either, since it's not clear what approach should be taken.  Please
;; see https://github.com/lassik/emacs-format-all-the-code/issues for
;; discussion.
;;
;; Many of the external formatters support configuration files in the
;; source code directory to control their formatting. Please see the
;; documentation for each formatter.
;;
;; New external formatters can be added easily if they can read code
;; from standard input and format it to standard output. Feel free to
;; submit a pull request or ask for help in GitHub issues.
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

(defun format-all-remove-ansi-color (string)
  "Internal helper function to remove terminal color codes from a string."
  (save-match-data (replace-regexp-in-string "\x1b\\[[0-9]+m" "" string t)))

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
            (let* ((first-diff (abs (compare-buffer-substrings inbuf nil nil
                                                               nil nil nil)))
                   (no-chg (or errorp (= 0 first-diff)))
                   (output (cond (errorp nil)
                                 (no-chg t)
                                 (t (buffer-substring (point-min) (point-max))))))
              (list output errput first-diff))))))))

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
  "Format the current buffer as Python using \"autopep8\".

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
  "Format the current buffer as Elm using \"elm-format\".

EXECUTABLE is the full path to the formatter."
  (cl-destructuring-bind (output errput first-diff)
      (format-all-buffer-process executable nil nil  "--yes" "--stdin")
    (let ((errput (format-all-remove-ansi-color (or errput ""))))
      (list output errput first-diff))))

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
  "Format the current buffer as Go using \"gofmt\".

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

(defun format-all-buffer-perltidy (executable)
  "Format the current buffer as Perl using \"perl-tidy\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process executable))

(defun format-all-buffer-prettier (executable)
  "Format the current buffer using \"prettier\".

EXECUTABLE is the full path to the formatter."
  (let ((parser (ecase major-mode
                  ;; The prettier folks seem to be currently pondering
                  ;; whether to use flow, babylon or some other parser
                  ;; for all JS-like code. Hopefully they will settle
                  ;; on one parser so this can become less convoluted.
                  ((js-mode js2-mode)
                   (if (and (boundp 'flow-minor-mode)
                            (not (null (symbol-value 'flow-minor-mode))))
                       "flow"
                     "babylon"))
                  ((jsx-mode rjsx-mode) "babylon")
                  (typescript-mode "typescript")
                  (json-mode "json")
                  (vue-mode "vue")
                  (css-mode "css")
                  (scss-mode "scss")
                  (less-css-mode "less")
                  (graphql-mode "graphql")
                  (markdown-mode "markdown"))))
    (apply 'format-all-buffer-process executable nil nil
           (append (list "--parser" parser)
                   (when (buffer-file-name)
                     (list "--stdin-filepath" (buffer-file-name)))))))

(defun format-all-buffer-rufo (executable)
  "Format the current buffer as Ruby using \"rufo\".

EXECUTABLE is the full path to the formatter."
  (apply 'format-all-buffer-process executable nil nil
         (append (list "--simple-exit")
                 (when (buffer-file-name)
                   (list "--filename" (buffer-file-name))))))

(defun format-all-buffer-rustfmt (executable)
  "Format the current buffer as Rust using \"rustfmt\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process executable))

(defun format-all-buffer-shfmt (executable)
  "Format the current buffer as Shell using \"shfmt\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process
   executable nil nil
   "-ln" (case (and (boundp 'sh-shell) (symbol-value 'sh-shell))
           (bash "bash") (mksh "mksh") (t "posix"))))

(defun format-all-buffer-standard (executable)
  "Format the current buffer as JavaScript using \"standard\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process
   executable '(0 1) "Parsing error:" "--fix" "--stdin"))

(defun format-all-buffer-swiftformat (executable)
  "Format the current buffer as Swift using \"swiftformat\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-process executable nil "error:"))

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
     (:modes c-mode c++-mode objc-mode))
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
    (perltidy
     (:executable "perltidy")
     (:install "cpan install Perl::Tidy")
     (:function format-all-buffer-perltidy)
     (:modes perl-mode))
    (prettier
     (:executable "prettier")
     (:install "npm install prettier")
     (:function format-all-buffer-prettier)
     (:modes
      css-mode graphql-mode js-mode js2-mode json-mode jsx-mode less-css-mode
      markdown-mode rjsx-mode scss-mode typescript-mode vue-mode))
    (rufo
     (:executable "rufo")
     (:install "gem install rufo")
     (:function format-all-buffer-rufo)
     (:modes ruby-mode enh-ruby-mode))
    (rustfmt
     (:executable "rustfmt")
     (:install "cargo install rustfmt")
     (:function format-all-buffer-rustfmt)
     (:modes rust-mode))
    (shfmt
     (:executable "shfmt")
     (:install (darwin "brew install shfmt"))
     (:function format-all-buffer-shfmt)
     (:modes sh-mode))
    (standard
     (:executable "standard")
     (:install "npm install standard")
     (:function format-all-buffer-standard)
     (:modes js-mode js2-mode))
    (swiftformat
     (:executable "swiftformat")
     (:install (darwin "brew install swiftformat"))
     (:function format-all-buffer-swiftformat)
     (:modes swift-mode swift3-mode)))
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
              (format " You may be able to install it via %S."
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

;;;###autoload
(defun format-all-buffer ()
  "Auto-format the source code in the current buffer.

No disk files are touched - the buffer doesn't even need to be
saved.  If you don't like the results of the formatting, you can
use ordinary undo to get your code back to its previous state.

You will need to install external programs to do the formatting.
If the command can't find the program that it needs, it will try
to tell you how you might be able to install it on your operating
system.  Only Emacs Lisp is formatted without an external program.

A suitable formatter is selected according to the `major-mode' of
the buffer.  Many popular programming languages are supported,
but not all of them by any means, so unfortunately it's still
likely that your favorite language is missing.  It is fairly easy
to add new languages that have an external formatter.

Any errors/warnings encountered during formatting are shown in a
buffer called *format-all-errors*.  If the formatter made any
changes to the code, point is placed at the first change."
  (interactive)
  (let* ((formatter (or (format-all-formatter-for-mode major-mode)
                        (error "Don't know how to format %S code" major-mode)))
         (f-function (format-all-property :function formatter))
         (executable (format-all-formatter-executable formatter)))
    (cl-destructuring-bind (output errput first-diff)
        (funcall f-function executable)
      (case output
        ((nil)
         (message "Syntax error"))
        ((t)
         (message "Already formatted"))
        (t
         (message "Reformatted!")
         (erase-buffer)
         (insert output)
         (goto-char first-diff)))
      (with-current-buffer (get-buffer-create "*format-all-errors*")
        (erase-buffer)
        (when errput
          (insert errput)
          (display-buffer (current-buffer)))))))

(provide 'format-all)

;;; format-all.el ends here
