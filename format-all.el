;;; format-all.el --- Auto-format C, C++, JS, Python, Ruby and 25 other languages -*- lexical-binding: t -*-
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
;; - Crystal (crystal tool format)
;; - CSS/Less/SCSS (prettier)
;; - D (dfmt)
;; - Elixir (mix format)
;; - Elm (elm-format)
;; - Emacs Lisp (native)
;; - Go (gofmt)
;; - GraphQL (prettier)
;; - Haskell (hindent)
;; - HTML/XHTML/XML (tidy)
;; - Java (clang-format)
;; - JavaScript/JSON/JSX/TypeScript/Vue (prettier)
;; - Kotlin (ktlint)
;; - Markdown (prettier)
;; - OCaml (ocp-indent)
;; - Perl (perltidy)
;; - Protocol Buffers (clang-format)
;; - Python (autopep8)
;; - Ruby (rufo)
;; - Rust (rustfmt)
;; - Shell script (shfmt)
;; - SQL (sqlformat)
;; - Swift (swiftformat)
;; - YAML (yq)
;;
;; You will need to install external programs to do the formatting.
;; If `format-all-buffer` can't find the right program, it will try to
;; tell you how to install it.
;;
;; A local minor mode called `format-all-mode` is available to format
;; code on save.  Please see the documentation for that function for
;; instructions.
;;
;; There are currently no customize variables, since it's not clear
;; what approach should be taken.  Please see
;; https://github.com/lassik/emacs-format-all-the-code/issues for
;; discussion.
;;
;; Many of the external formatters support configuration files in the
;; source code directory to control their formatting.  Please see the
;; documentation for each formatter.
;;
;; New external formatters can be added easily if they can read code
;; from standard input and format it to standard output.  Feel free to
;; submit a pull request or ask for help in GitHub issues.
;;
;;; Code:

(eval-when-compile
  (defconst format-all-system-type
    (cl-case system-type
      (windows-nt 'windows)
      (cygwin     'windows)
      (darwin     'macos)
      (gnu/linux  'linux)
      (berkeley-unix
       (save-match-data
	 (let ((case-fold-search t))
           (cond ((string-match "freebsd" system-configuration) 'freebsd)
		 ((string-match "openbsd" system-configuration) 'openbsd)
		 ((string-match "netbsd"  system-configuration) 'netbsd))))))
    "Current operating system according to the format-all package."))

(eval-when-compile
  (defun format-all-resolve-system (choices)
    "Get first choice matching `format-all-system-type' from CHOICES."
    (cl-dolist (choice choices)
      (cond ((atom choice)
             (cl-return choice))
            ((eql format-all-system-type (car choice))
             (cl-return (cadr choice)))))))

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
  "Internal helper function to remove terminal color codes from STRING."
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
output from the formatter.

Note that in some cases we can use the output of the formatter
even if it produced warnings.  Not all warnings are errors."
  (save-excursion
    (save-restriction
      (widen)
      (let ((inbuf (current-buffer))
            (input (buffer-string)))
        (with-temp-buffer
          (cl-destructuring-bind (errorp errput) (funcall thunk input)
            (let* ((first-diff (abs (compare-buffer-substrings inbuf nil nil
                                                               nil nil nil)))
                   (no-chg (or errorp (= 0 first-diff)))
                   (output (cond (errorp nil)
                                 (no-chg t)
                                 (t (buffer-string)))))
              (list output errput first-diff))))))))

(defun format-all-buffer-hard (ok-statuses error-regexp executable &rest args)
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
  (let ((ok-statuses (or ok-statuses '(0)))
        (args (cl-mapcan (lambda (arg) (if (listp arg) arg (list arg)))
                         args)))
    (format-all-buffer-thunk
     (lambda (input)
       (let* ((errfile (make-temp-file "format-all-"))
              (status (apply #'call-process-region input nil
                             executable nil (list t errfile)
                             nil args))
              (errput (with-temp-buffer
                        (insert-file-contents errfile)
                        (delete-file errfile)
                        (buffer-string)))
              (errorp (or (not (member status ok-statuses))
                          (and error-regexp
                               (save-match-data
                                 (string-match error-regexp errput))))))
         (list errorp errput))))))

(defun format-all-buffer-easy (executable &rest args)
  "Internal helper function to implement formatters.

Runs the external program EXECUTABLE.  The program shall read
unformatted code from stdin, write its formatted equivalent to
stdout, write errors/warnings to stderr, and exit zero/non-zero
on success/failure.

If ARGS are given, those are arguments to EXECUTABLE.  They don't
need to be shell-quoted."
  (apply 'format-all-buffer-hard nil nil executable args))

(defvar format-all-executable-table (make-hash-table)
  "Internal table of formatter executable names for format-all.")

(defvar format-all-install-table (make-hash-table)
  "Internal table of formatter install commands for format-all.")

(defvar format-all-mode-table (make-hash-table)
  "Internal table of major mode formatter lists for format-all.")

(defvar format-all-format-table (make-hash-table)
  "Internal table of formatter formatting functions for format-all.")

(defun format-all-pushhash (key value table)
  "Push VALUE onto the list under KEY in hash table TABLE."
  (puthash key (cons value (remove value (gethash key table))) table))

(defmacro define-format-all-formatter (formatter &rest body)
  "Define a new source code formatter for use with format-all."
  (let (executable install modes format)
    (cl-assert (equal (mapcar 'car body) '(:executable :install :modes :format)))
    (cl-dolist (part body)
      (cl-ecase (car part)
        (:executable
         (setq executable
               (unless (null (cdr part))
                 (or (format-all-resolve-system (cdr part))
                     (error "Executable not specified for %S system %S"
                            formatter format-all-system-type)))))
        (:install
         (setq install (format-all-resolve-system (cdr part))))
        (:modes
         (setq modes
               (cl-mapcan
                (lambda (modex)
                  (let ((modex (if (listp modex) modex (list modex))))
                    (cl-destructuring-bind (mmodes &optional probex) modex
                      (let* ((mmodes (if (listp mmodes) mmodes (list mmodes)))
                             (probe (when probex `(lambda () ,probex))))
                        (mapcar
                         (lambda (mmode)
                           `(format-all-pushhash ',mmode
                                                 (cons ',formatter ,probe)
                                                 format-all-mode-table))
                         mmodes)))))
                (cdr part))))
        (:format
         (setq format `(lambda (executable mode-result)
                         (ignore mode-result
                                 ,@(unless executable '(executable)))
                         ,(cadr part))))))
    `(progn (puthash ',formatter ,executable format-all-executable-table)
            (puthash ',formatter ,install format-all-install-table)
            ,@modes
            (puthash ',formatter ,format format-all-format-table)
            ',formatter)))

(define-format-all-formatter autopep8
  (:executable "autopep8")
  (:install "pip install autopep8")
  (:modes python-mode)
  (:format (format-all-buffer-easy executable "-")))

(define-format-all-formatter clang-format
  (:executable "clang-format")
  (:install (macos "brew install clang-format"))
  (:modes
   (c-mode ".c")
   (c++-mode ".cpp")
   (java-mode ".java")
   (objc-mode ".m")
   (protobuf-mode ".proto"))
  (:format
   (format-all-buffer-easy
    executable
    (let ((assume-filename (or (buffer-file-name) mode-result "")))
      (when assume-filename (concat "-assume-filename=" assume-filename))))))

(define-format-all-formatter crystal
  (:executable "crystal")
  (:install (macos "brew install crystal"))
  (:modes crystal-mode)
  (:format (format-all-buffer-easy executable "tool" "format" "-")))

(define-format-all-formatter dfmt
  (:executable "dfmt")
  (:install (macos "brew install dfmt"))
  (:modes d-mode)
  (:format (format-all-buffer-hard nil (regexp-quote "[error]") executable)))

(define-format-all-formatter elm-format
  (:executable "elm-format")
  (:install (macos "brew install elm"))
  (:modes elm-mode)
  (:format
   (cl-destructuring-bind (output errput first-diff)
       (format-all-buffer-easy executable "--yes" "--stdin")
     (let ((errput (format-all-remove-ansi-color errput)))
       (list output errput first-diff)))))

(define-format-all-formatter emacs-lisp
  (:executable)
  (:install)
  (:modes emacs-lisp-mode lisp-interaction-mode)
  (:format
   (format-all-buffer-thunk
    (lambda (input)
      (emacs-lisp-mode)
      (insert input)
      (indent-region (point-min) (point-max))
      (format-all-fix-trailing-whitespace)
      (list nil "")))))

(define-format-all-formatter gofmt
  (:executable "gofmt")
  (:install (macos "brew install go"))
  (:modes go-mode)
  (:format (format-all-buffer-easy executable)))

(define-format-all-formatter hindent
  (:executable "hindent")
  (:install "stack install hindent")
  (:modes haskell-mode)
  (:format (format-all-buffer-easy executable)))

(define-format-all-formatter html-tidy
  (:executable "tidy")
  (:install (macos "brew install tidy-html5"))
  (:modes
   html-helper-mode html-mode mhtml-mode nxhtml-mode web-mode
   nxml-mode xml-mode)
  (:format
   (format-all-buffer-hard
    '(0 1) nil
    executable
    "-q" "-indent"
    (when (member major-mode '(nxml-mode xml-mode))
      "-xml"))))

(define-format-all-formatter ktlint
  (:executable "ktlint")
  (:install (macos "brew install ktlint"))
  (:modes kotlin-mode)
  (:format (format-all-buffer-easy executable "--format" "--stdin")))

(defun format-all-buffer-mix-format (executable)
  "Format the current buffer as Elixir using \"mix format\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-easy executable "format" "-"))

(defun format-all-buffer-ocp-indent (executable)
  "Format the current buffer as OCaml using \"ocp-indent\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-easy executable))

(defun format-all-buffer-perltidy (executable)
  "Format the current buffer as Perl using \"perl-tidy\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-easy executable))

(defun format-all-buffer-prettier (executable)
  "Format the current buffer using \"prettier\".

EXECUTABLE is the full path to the formatter."
  (let ((parser (cl-ecase major-mode
                  ;; The prettier folks seem to be currently pondering
                  ;; whether to use flow, babylon or some other parser
                  ;; for all JS-like code. Hopefully they will settle
                  ;; on one parser so this can become less convoluted.
                  ((js-mode js2-mode js3-mode)
                   (if (and (boundp 'flow-minor-mode)
                            (not (null (symbol-value 'flow-minor-mode))))
                       "flow"
                     "babylon"))
                  ((js2-jsx-mode jsx-mode rjsx-mode) "babylon")
                  ((typescript-mode typescript-tsx-mode) "typescript")
                  (json-mode "json")
                  (vue-mode "vue")
                  (css-mode "css")
                  (scss-mode "scss")
                  (less-css-mode "less")
                  (graphql-mode "graphql")
                  ((gfm-mode markdown-mode) "markdown"))))
    (format-all-buffer-easy
     executable
     "--parser" parser
     (when (buffer-file-name)
       (list "--stdin-filepath" (buffer-file-name))))))

(defun format-all-buffer-rufo (executable)
  "Format the current buffer as Ruby using \"rufo\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-easy
   executable
   "--simple-exit"
   (when (buffer-file-name)
     (list "--filename" (buffer-file-name)))))

(defun format-all-buffer-rustfmt (executable)
  "Format the current buffer as Rust using \"rustfmt\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-easy executable))

(defun format-all-buffer-shfmt (executable)
  "Format the current buffer as Shell using \"shfmt\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-easy
   executable
   "-ln" (cl-case (and (boundp 'sh-shell) (symbol-value 'sh-shell))
           (bash "bash") (mksh "mksh") (t "posix"))))

(defun format-all-buffer-sqlformat (executable)
  "Format the current buffer as SQL using \"sqlformat\".

EXECUTABLE is the full path to the formatter."
  (let* ((ic (car default-process-coding-system))
         (oc (cdr default-process-coding-system))
         (ienc (symbol-name (or (coding-system-get ic :mime-charset) 'utf-8)))
         (oenc (symbol-name (or (coding-system-get oc :mime-charset) 'utf-8)))
         (process-environment (cons (concat "PYTHONIOENCODING=" oenc)
                                    process-environment)))
    (format-all-buffer-easy
     executable
     "--keywords" "upper"
     "--reindent_aligned"
     "--encoding" ienc
     "-")))

(defun format-all-buffer-standard (executable)
  "Format the current buffer as JavaScript using \"standard\".

EXECUTABLE is the full path to the formatter."
  ;; `standard --stdin` properly uses zero vs non-zero exit codes to
  ;; indicate success vs error.  However, it checks for quite a broad
  ;; range of errors, all the way up to undeclared identifiers and
  ;; such.  To catch only syntax errors, we need to look specifically
  ;; for the text "Parsing error:".
  (format-all-buffer-hard
   '(0 1) ".*?:.*?:[0-9]+:[0-9]+: Parsing error:"
   executable "--fix" "--stdin"))

(defun format-all-buffer-swiftformat (executable)
  "Format the current buffer as Swift using \"swiftformat\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-easy executable))

(defun format-all-buffer-yq (executable)
  "Format the current buffer as YAML using \"yq\".

EXECUTABLE is the full path to the formatter."
  (format-all-buffer-easy executable "read" "-"))

(defconst format-all-formatters
  '((mix-format
     (:executable "mix")
     (:install (macos "brew install elixir"))
     (:function format-all-buffer-mix-format)
     (:modes elixir-mode))
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
      css-mode gfm-mode graphql-mode js-mode js2-mode js2-jsx-mode js3-mode
      json-mode jsx-mode less-css-mode markdown-mode rjsx-mode scss-mode
      typescript-mode typescript-tsx-mode vue-mode))
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
     (:install (macos "brew install shfmt"))
     (:function format-all-buffer-shfmt)
     (:modes sh-mode))
    (sqlformat
     (:executable "sqlformat")
     (:install "pip install sqlparse")
     (:function format-all-buffer-sqlformat)
     (:modes sql-mode))
    (standard
     (:executable "standard")
     (:install "npm install standard")
     (:function format-all-buffer-standard)
     (:modes js-mode js2-mode))
    (swiftformat
     (:executable "swiftformat")
     (:install (macos "brew install swiftformat"))
     (:function format-all-buffer-swiftformat)
     (:modes swift-mode swift3-mode))
    (yq
     (:executable "yq")
     (:install (macos "brew install yq"))
     (:function format-all-buffer-yq)
     (:modes yaml-mode)))
  "Table of source code formatters supported by format-all.")

(defun format-all-property-list (property formatter)
  "Internal helper function to get PROPERTY of FORMATTER."
  (cdr (or (assoc property formatter)
           (error "Property %S missing for formatter %S"
                  property formatter))))

(defun format-all-property-system (property formatter)
  "Internal helper function to get PROPERTY of FORMATTER."
  (cl-dolist (choice (format-all-property-list property formatter))
    (cond ((atom choice)
           (cl-return choice))
          ((eql format-all-system-type (car choice))
           (cl-return (cadr choice))))))

(defun format-all-please-install (executable installer)
  "Internal helper function for error about missing EXECUTABLE and INSTALLER."
  (concat (format "You need the %S command." executable)
          (if (not installer) ""
            (format " You may be able to install it via %S." installer))))

(defun format-all-formatter-executable (formatter)
  "Internal helper function to get the external program for FORMATTER."
  (let ((executable (format-all-property-system :executable formatter)))
    (cond ((not executable)
           (error "Executable not specified for formatter %S system %S"
                  formatter format-all-system-type))
          ((not (eql t executable))
           (or (executable-find executable)
               (error (format-all-please-install
                       executable
                       (format-all-property-system :install formatter))))))))

(defun format-all-formatter-for-mode (mode)
  "Internal helper function to get the formatter corresponding to MODE."
  (cl-dolist (formatter format-all-formatters nil)
    (when (member mode (format-all-property-list :modes formatter))
      (cl-return formatter))))

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
      (cl-case output
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
        (unless (= 0 (length errput))
          (insert errput)
          (display-buffer (current-buffer)))))))

(defun format-all-probe ()
  "Internal helper function to get the formatter for the current buffer."
  (cl-dolist (pair (gethash major-mode format-all-mode-table) (list nil nil))
    (cl-destructuring-bind (formatter . probe) pair
      (let ((mode-result (if probe (funcall probe) t)))
        (when mode-result (cl-return (list formatter mode-result)))))))

(defun format-all-formatter-executable-new-framework (formatter)
  "Internal helper function to get the external program for FORMATTER."
  (let ((executable (gethash formatter format-all-executable-table)))
    (when executable
      (or (executable-find executable)
          (error (format-all-please-install
                  executable (gethash formatter format-all-install-table)))))))

(defun format-all-buffer-new-framework ()
  (interactive)
  (cl-destructuring-bind (formatter mode-result) (format-all-probe)
    (unless formatter (error "Don't know how to format %S code" major-mode))
    (let ((f-function (gethash formatter format-all-format-table))
          (executable (format-all-formatter-executable-new-framework formatter)))
      (cl-destructuring-bind (output errput first-diff)
          (funcall f-function executable mode-result)
        (cl-case output
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
          (unless (= 0 (length errput))
            (insert errput)
            (display-buffer (current-buffer))))))))

;;;###autoload
(define-minor-mode format-all-mode
  "Toggle automatic source code formatting before save.

When the Format-All minor mode is enabled, `format-all-buffer' is
automatically called each time before you save the buffer.

When called from Lisp, the mode is toggled if ARG is ‘toggle’,
disabled if ARG is a negative integer or zero, and enabled
otherwise.

The mode is buffer-local and needs to be enabled separately each
time a file is visited or a temporary buffer is created.

You may want to use `add-hook' to add a function to your personal
`after-change-major-mode-hook' in your `user-init-file' to enable
the mode based on the buffer's `major-mode' and some
`buffer-file-name' patterns. For example:

    (defun my-after-change-major-mode ()
      (format-all-mode
       (if (and (buffer-file-name)
                (save-match-data
                  (let ((dir (file-name-directory (buffer-file-name))))
                    (or (string-match \"foo\" dir)
                        (string-match \"bar\" dir))))
                (member major-mode '(js-mode python-mode)))
           1 0)))

    (add-hook 'after-change-major-mode-hook 'my-after-change-major-mode)"
  :lighter " Format-All"
  :global nil
  (if format-all-mode
      (add-hook 'before-save-hook 'format-all-buffer nil 'local)
    (remove-hook 'before-save-hook 'format-all-buffer 'local)))

(provide 'format-all)

;;; format-all.el ends here
