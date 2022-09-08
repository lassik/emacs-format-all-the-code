;;; format-all.el --- Auto-format C, C++, JS, Python, Ruby and 50 other languages -*- lexical-binding: t -*-

;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-format-all-the-code
;; Version: 0.5.0
;; Package-Requires: ((emacs "24.4") (inheritenv "0.1") (language-id "0.19"))
;; Keywords: languages util
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Lets you auto-format source code in many languages using the same
;; command for all languages, instead of learning a different Emacs
;; package and formatting command for each language.

;; Just do M-x format-all-buffer and it will try its best to do the
;; right thing.  To auto-format code on save, use the minor mode
;; format-all-mode.  Please see the documentation for that function
;; for instructions.

;; Supported languages:

;; - Angular (prettier)
;; - Assembly (asmfmt)
;; - ATS (atsfmt)
;; - Awk (gawk)
;; - Bazel Starlark (buildifier)
;; - BibTeX (Emacs)
;; - C/C++/Objective-C (clang-format, astyle)
;; - C# (clang-format, astyle)
;; - Cabal (cabal-fmt)
;; - Clojure/ClojureScript (zprint, node-cljfmt)
;; - CMake (cmake-format)
;; - Crystal (crystal tool format)
;; - CSS/Less/SCSS (prettier, prettierd)
;; - Cuda (clang-format)
;; - D (dfmt)
;; - Dart (dartfmt, dart-format)
;; - Dhall (dhall format)
;; - Dockerfile (dockfmt)
;; - Elixir (mix format)
;; - Elm (elm-format)
;; - Emacs Lisp (Emacs)
;; - Erb (erb-format)
;; - Erlang (efmt)
;; - F# (fantomas)
;; - Fish Shell (fish_indent)
;; - Fortran Free Form (fprettify)
;; - Gleam (gleam format)
;; - GLSL (clang-format)
;; - Go (gofmt, goimports)
;; - GraphQL (prettier, prettierd)
;; - Haskell (brittany, fourmolu, hindent, ormolu, stylish-haskell)
;; - HTML/XHTML/XML (tidy)
;; - Java (clang-format, astyle)
;; - JavaScript/JSON/JSX (prettier, standard, prettierd)
;; - Jsonnet (jsonnetfmt)
;; - Kotlin (ktlint)
;; - LaTeX (latexindent, auctex)
;; - Ledger (ledger-mode)
;; - Lua (lua-fmt, stylua, prettier plugin)
;; - Markdown (prettier, prettierd)
;; - Nginx (nginxfmt)
;; - Nix (nixpkgs-fmt, nixfmt, alejandra)
;; - OCaml (ocp-indent)
;; - Perl (perltidy)
;; - PHP (prettier plugin)
;; - Protocol Buffers (clang-format)
;; - PureScript (purty, purs-tidy)
;; - Python (black, yapf, isort)
;; - R (styler)
;; - Racket (raco-fmt)
;; - Reason (bsrefmt)
;; - ReScript (rescript)
;; - Ruby (rubocop, rufo, standardrb)
;; - Rust (rustfmt)
;; - Scala (scalafmt)
;; - Shell script (beautysh, shfmt)
;; - Snakemake (snakefmt)
;; - Solidity (prettier plugin)
;; - SQL (pgformatter, sqlformat)
;; - Svelte (prettier plugin)
;; - Swift (swiftformat)
;; - Terraform (terraform fmt)
;; - TOML (prettier plugin, taplo fmt)
;; - TypeScript/TSX (prettier, ts-standard, prettierd)
;; - V (v fmt)
;; - Vue (prettier, prettierd)
;; - Verilog (iStyle)
;; - YAML (prettier, prettierd)
;; - Zig (zig)

;; You will need to install external programs to do the formatting.
;; If `format-all-buffer` can't find the right program, it will try to
;; tell you how to install it.

;; Many of the external formatters support configuration files in the
;; source code directory to control their formatting.  Please see the
;; documentation for each formatter.

;; New external formatters can be added easily if they can read code
;; from standard input and format it to standard output.  Feel free to
;; submit a pull request or ask for help in GitHub issues.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'inheritenv)
(require 'language-id)

(defgroup format-all nil
  "Lets you auto-format source code."
  :group 'format-all)

(defcustom format-all-debug nil
  "When non-nil, troubleshooting info is written into the *Messages* buffer."
  :type 'boolean
  :group 'format-all)

(defcustom format-all-default-formatters
  '(("Assembly" asmfmt)
    ("ATS" atsfmt)
    ("Bazel" buildifier)
    ("BibTeX" emacs-bibtex)
    ("C" clang-format)
    ("C#" clang-format)
    ("C++" clang-format)
    ("Cabal Config" cabal-fmt)
    ("Clojure" zprint)
    ("CMake" cmake-format)
    ("Crystal" crystal)
    ("CSS" prettier)
    ("Cuda" clang-format)
    ("D" dfmt)
    ("Dart" dart-format)
    ("Dhall" dhall)
    ("Dockerfile" dockfmt)
    ("Elixir" mix-format)
    ("Elm" elm-format)
    ("Emacs Lisp" emacs-lisp)
    ("Erlang" efmt)
    ("F#" fantomas)
    ("Fish" fish-indent)
    ("Fortran Free Form" fprettify)
    ("GLSL" clang-format)
    ("Go" gofmt)
    ("GraphQL" prettier)
    ("Haskell" brittany)
    ("HTML" html-tidy)
    ("HTML+ERB" erb-format)
    ("Java" clang-format)
    ("JavaScript" prettier)
    ("JSON" prettier)
    ("JSON5" prettier)
    ("Jsonnet" jsonnetfmt)
    ("JSX" prettier)
    ("Kotlin" ktlint)
    ("LaTeX" latexindent)
    ("Less" prettier)
    ("Literate Haskell" brittany)
    ("Lua" lua-fmt)
    ("Markdown" prettier)
    ("Nix" nixpkgs-fmt)
    ("Objective-C" clang-format)
    ("OCaml" ocp-indent)
    ("Perl" perltidy)
    ("PHP" prettier)
    ("Protocol Buffer" clang-format)
    ("PureScript" purty)
    ("Python" black)
    ("R" styler)
    ("Reason" bsrefmt)
    ("ReScript" rescript)
    ("Ruby" rufo)
    ("Rust" rustfmt)
    ("Scala" scalafmt)
    ("SCSS" prettier)
    ("Shell" shfmt)
    ("Solidity" prettier)
    ("SQL" sqlformat)
    ("Svelte" prettier)
    ("Swift" swiftformat)
    ("Terraform" terraform-fmt)
    ("TOML" prettier)
    ("TSX" prettier)
    ("TypeScript" prettier)
    ("V" v-fmt)
    ("Verilog" istyle-verilog)
    ("Vue" prettier)
    ("XML" html-tidy)
    ("YAML" prettier)
    ("Zig" zig)

    ("_Angular" prettier)
    ("_Flow" prettier)
    ("_Gleam" gleam)
    ("_Ledger" ledger-mode)
    ("_Nginx" nginxfmt)
    ("_Snakemake" snakefmt))
  "Default formatter to use for each language."
  :type '(repeat (list string symbol))
  :group 'format-all)

(defcustom format-all-show-errors 'errors
  "When to show formatting errors or warnings."
  :type '(choice (const :tag "Always" always)
                 (const :tag "Errors" errors)
                 (const :tag "Warnings" warnings)
                 (const :tag "Never" never))
  :group 'format-all)

(defcustom format-all-mode-lighter " FmtAll"
  "Lighter for command `format-all-mode'."
  :type '(string :tag "Lighter")
  :risky t
  :group 'format-all)

(defvar format-all-after-format-functions nil
  "Hook run after each time `format-all-buffer' has formatted a buffer.

The value is a list of hook functions.  Use `add-hook' to add a
function.  The function is called with two arguments: (FORMATTER
STATUS).  FORMATTER is a symbol naming the formatter, as given to
`define-format-all-formatter'.  STATUS is one of the following
keywords:

* :reformatted -- The formatter made changes to the buffer.

* :already-formatted -- The buffer was already formatted
  correctly so the formatter didn't make any changes to it.

* :error -- The formatter encountered an error (usually a syntax
  error).  The buffer contents are the same as before formatting.

The current buffer is the buffer that was just formatted.  Point
is not guaranteed to be in any particular place, so `goto-char'
before editing the buffer.  Narrowing may be in effect unless
STATUS is :reformatted.")

(defvar format-all--user-args nil
  "Internal variable to temporarily store arguments for formatters.")

(defvar-local format-all-formatters nil
  "Rules to select which formatter format-all uses.

The value is an association list.

The first item of each association is the name of a programming
language. (GitHub Linguist names are used.)

The remaining items are one or more formatters to use for that
language. Each formatter is either:

* a symbol (e.g. black, clang-format, rufo)

* a list whose first item is that symbol, and any remaining items
  are extra command line arguments to pass to the formatter

If more than one formatter is given for the same language, all of
them are run as a chain, with the code from each formatter passed
to the next. The final code is from the last formatter. In case
any formatter in the chain is missing or fails to format the
code, the entire chain fails and the old code before formatting
is preserved.

You'll probably want to set this in a \".dir-locals.el\" file or
in a hook function. Any number of buffers can share the same
association list. Using \".dir-locals.el\" is convenient since
the rules for an entire source tree can be given in one file.")

(define-error 'format-all-executable-not-found
  "Formatter not found")

(defun format-all--proper-list-p (object)
  "Return t if OBJECT is a proper list, nil otherwise."
  ;; If we could depend on Emacs 27.1 this function would be built in.
  (condition-case _ (not (null (cl-list-length object)))
    (wrong-type-argument nil)))

(defun format-all--normalize-formatter (formatter)
  "Internal function to convert FORMATTER spec into normal form."
  (let ((formatter (if (listp formatter) formatter (list formatter))))
    (when (cdr (last formatter))
      (error "Formatter is not a proper list: %S" formatter))
    (when (null formatter)
      (error "Formatter name missing"))
    (unless (symbolp (car formatter))
      (error "Formatter name is not a symbol: %S" (car formatter)))
    (unless (cl-every #'stringp (cdr formatter))
      (error "Formatter command line arguments are not all strings: %S"
             formatter))
    formatter))

(defun format-all--normalize-chain (chain)
  "Internal function to convert CHAIN spec into normal form."
  (when (or (not (listp chain)) (cdr (last chain)))
    (error "Formatter chain is not a proper list: %S" chain))
  (mapcar #'format-all--normalize-formatter chain))

(defun format-all-valid-formatters-p (formatters)
  "Return t if FORMATTERS is a valid value for `format-all-formatters'."
  (and (format-all--proper-list-p formatters)
       (cl-every
        (lambda (chain)
          (and (not (null chain))
               (format-all--proper-list-p chain)
               (stringp (car chain))
               (cl-every
                (lambda (formatter)
                  (and (not (null formatter))
                       (or (symbolp formatter)
                           (and (format-all--proper-list-p formatter)
                                (and (symbolp (car formatter))
                                     (not (null (car formatter))))
                                (cl-every #'stringp (cdr formatter))))))
                (cdr chain))))
        formatters)))

(put 'format-all-formatters 'safe-local-variable
     'format-all-valid-formatters-p)

(eval-and-compile
  (defconst format-all--system-type
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

(eval-and-compile
  (defun format-all--resolve-system (choices)
    "Get first choice matching `format-all--system-type' from CHOICES."
    (cl-dolist (choice choices)
      (cond ((atom choice)
             (cl-return choice))
            ((eql format-all--system-type (car choice))
             (cl-return (cadr choice)))))))

(defun format-all--fix-trailing-whitespace ()
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

(defun format-all--remove-ansi-color (string)
  "Internal helper function to remove terminal color codes from STRING."
  (save-match-data (replace-regexp-in-string "\x1b\\[[0-9]+m" "" string t)))

(defun format-all--flatten-once (list)
  "Internal helper function to remove nested lists in LIST."
  (cl-mapcan (lambda (x) (if (listp x) x (list x)))
             list))

(defun format-all--buffer-extension-p (&rest extensions)
  "Internal helper function to test file name EXTENSIONS."
  (and (buffer-file-name)
       (save-match-data
         (let ((case-fold-search t))
           (cl-some (lambda (ext)
                      (string-match (concat "\\." (regexp-quote ext) "\\'")
                                    (buffer-file-name)))
                    extensions)))))

(defun format-all--buffer-thunk (thunk)
  "Internal helper function to implement formatters.

THUNK is a function that implements a particular formatter.  It
takes INPUT (the unformatted source code as a string).  THUNK is
invoked such that the current buffer is an empty temp buffer.  It
should call the formatter on INPUT and write the formatted source
code output to the temp buffer.  It should return (ERRORP
ERROR-OUTPUT).  ERRORP is a boolean indicating whether the formatter
caused an error and hence the contents of the temp buffer should
be discarded.  ERROR-OUTPUT is a string containing all error/warning
output from the formatter.

Note that in some cases we can use the output of the formatter
even if it produced warnings.  Not all warnings are errors."
  (save-excursion
    (save-restriction
      (widen)
      (let ((inbuf (current-buffer))
            (input (buffer-string)))
        (inheritenv
         (with-temp-buffer
           (cl-destructuring-bind (errorp error-output) (funcall thunk input)
             (let* ((no-chg (or errorp
                                (= 0 (let ((case-fold-search nil))
                                       (compare-buffer-substrings
                                        inbuf nil nil nil nil nil)))))
                    (output (cond (errorp nil)
                                  (no-chg t)
                                  (t (buffer-string)))))
               (list output error-output)))))))))

(defun format-all--buffer-native (mode &rest funcs)
  "Internal helper function to implement formatters.

In a new temp buffer, switches to MODE then calls FUNCS in order
to format the code. MODE and FUNCS should be symbols instead of
functions to avoid warnings from the Emacs byte compiler."
  (format-all--buffer-thunk
   (lambda (input)
     (funcall mode)
     (insert input)
     (mapc #'funcall funcs)
     (format-all--fix-trailing-whitespace)
     (list nil ""))))

(defun format-all--locate-file (filename)
  "Internal helper to locate dominating copy of FILENAME for current buffer."
  (let* ((dir (and (buffer-file-name)
                   (locate-dominating-file (buffer-file-name) filename))))
    (when dir (expand-file-name (concat dir filename)))))

(defun format-all--locate-default-directory (root-files)
  "Internal helper function to find working directory for formatter.

ROOT-FILES is a list of strings which are the filenames to look
for using `locate-dominating-file'.  Details in documentation for
`format-all--buffer-hard'."
  (let ((found-dirs
         (when (and root-files (buffer-file-name))
           (mapcan (lambda (root-file)
                     (let ((found-file (locate-dominating-file
                                        (buffer-file-name) root-file)))
                       (when found-file
                         (list (file-name-directory found-file)))))
                   root-files))))
    (or (car (sort found-dirs (lambda (a b) (> (length a) (length b)))))
        (and (buffer-file-name) (file-name-directory (buffer-file-name)))
        default-directory)))

(defun format-all--buffer-hard
    (ok-statuses error-regexp root-files executable &rest args)
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

If ARGS are given, those are arguments to EXECUTABLE. They should
not be shell-quoted.

If ROOT-FILES are given, the working directory of the formatter
will be the deepest directory (starting from the file being
formatted) containing one of these files.  If ROOT-FILES is nil,
or none of ROOT-FILES are found in any parent directories, the
working directory will be the one where the formatted file is.
ROOT-FILES is ignored for buffers that are not visiting a file."
  (let ((ok-statuses (or ok-statuses '(0)))
        (args (append format-all--user-args (format-all--flatten-once args)))
        (default-directory (format-all--locate-default-directory root-files)))
    (when format-all-debug
      (message "Format-All: Running: %s"
               (mapconcat #'shell-quote-argument (cons executable args) " "))
      (message "Format-All: Directory: %s" default-directory))
    (format-all--buffer-thunk
     (lambda (input)
       (let* ((errfile (make-temp-file "format-all-"))
              (status (apply #'call-process-region input nil
                             executable nil (list t errfile)
                             nil args))
              (error-output (with-temp-buffer
                              (insert-file-contents errfile)
                              (delete-file errfile)
                              (buffer-string)))
              (errorp (or (not (member status ok-statuses))
                          (and error-regexp
                               (save-match-data
                                 (string-match error-regexp error-output))))))
         (list errorp error-output))))))

(defun format-all--buffer-easy (executable &rest args)
  "Internal helper function to implement formatters.

Runs the external program EXECUTABLE.  The program shall read
unformatted code from stdin, write its formatted equivalent to
stdout, write errors/warnings to stderr, and exit zero/non-zero
on success/failure.

If ARGS are given, those are arguments to EXECUTABLE.  They don't
need to be shell-quoted."
  (apply 'format-all--buffer-hard nil nil nil executable args))

(defun format-all--ruby-gem-bundled-p (gem-name)
  "Internal helper function to check for a Ruby gem.

Returns t if GEM-NAME is listed in the current project's
Gemfile.lock, nil otherwise."
  (let* ((lockfile "Gemfile.lock")
         (dir (locate-dominating-file (buffer-file-name) lockfile)))
    (and dir
         (with-temp-buffer
           (insert-file-contents (expand-file-name lockfile dir))
           (re-search-forward (format "^    %s " (regexp-quote gem-name))
                              nil t))
         t)))

(defun format-all--buffer-hard-ruby
    (gem-name ok-statuses error-regexp root-files executable &rest args)
  "Internal helper function to implement ruby based formatters.

GEM-NAME is the name of a Ruby gem required to run EXECUTABLE.

For OK-STATUSES, ERROR-REGEXP, ROOT-FILES, EXECUTABLE and ARGS,
see `format-all--buffer-hard'."
  (let* ((command (file-name-nondirectory executable))
         (error-regexp
          (regexp-opt
           (append
            (if error-regexp (list error-regexp))
            (list
             "Bundler::GemNotFound"
             (concat "bundler: failed to load command: "
                     (regexp-quote command))
             (concat (regexp-opt (list "bundle" (regexp-quote command)))
                     ": command not found")))))
         (command-args
          (append
           (if (format-all--ruby-gem-bundled-p gem-name)
               (list "bundle" "exec" command)
             (list executable))
           (format-all--flatten-once args))))
    (format-all--buffer-hard
     ok-statuses error-regexp root-files
     (car command-args)
     (cdr command-args))))

(defvar format-all--executable-table (make-hash-table)
  "Internal table of formatter executable names for format-all.")

(defvar format-all--install-table (make-hash-table)
  "Internal table of formatter install commands for format-all.")

(defvar format-all--language-table (make-hash-table :test 'equal)
  "Internal table of major mode formatter lists for format-all.")

(defvar format-all--features-table (make-hash-table)
  "Internal table of formatter feature lists for format-all.")

(defvar format-all--format-table (make-hash-table)
  "Internal table of formatter formatting functions for format-all.")

(defun format-all--pushhash (key value table)
  "Push VALUE onto the list under KEY in hash table TABLE."
  (puthash key (cons value (remove value (gethash key table))) table))

(defmacro define-format-all-formatter (formatter &rest body)
  "Define a new source code formatter for use with format-all.

FORMATTER is a symbol naming the formatter.  The name of the
command used to run the formatter is usually a good choice.

Consult the existing formatters for examples of BODY."
  (let (executable install languages features format)
    (cl-assert
     (equal (mapcar 'car body)
            '(:executable :install :languages :features :format)))
    (cl-dolist (part body)
      (cl-ecase (car part)
        (:executable
         (setq executable
               (unless (null (cdr part))
                 (or (format-all--resolve-system (cdr part))
                     (error "Executable not specified for %S system %S"
                            formatter format-all--system-type)))))
        (:install
         (setq install (format-all--resolve-system (cdr part))))
        (:languages
         (setq languages
               (mapcar (lambda (language)
                         `(format-all--pushhash
                           ',language ',formatter format-all--language-table))
                       (cdr part))))
        (:features
         (setq features (cdr part)))
        (:format
         (setq format `(lambda (executable language region)
                         (ignore language
                                 ,@(unless executable '(executable))
                                 ,@(unless (memq 'region features) '(region)))
                         ,(cadr part))))))
    `(progn (puthash ',formatter ,executable format-all--executable-table)
            (puthash ',formatter ,install format-all--install-table)
            ,@languages
            (puthash ',formatter ',features format-all--features-table)
            (puthash ',formatter ,format format-all--format-table)
            ',formatter)))

(define-format-all-formatter alejandra
  (:executable "alejandra")
  (:install "nix-env -if https://github.com/kamadorueda/alejandra/tarball/master")
  (:languages "Nix")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter asmfmt
  (:executable "asmfmt")
  (:install)
  (:languages "Assembly")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter astyle
  (:executable "astyle")
  (:install (macos "brew install astyle"))
  (:languages "C" "C++" "C#" "Java")
  (:features)
  (:format (format-all--buffer-easy
            executable
            (let ((astylerc (format-all--locate-file ".astylerc")))
              (when astylerc (concat "--options=" astylerc))))))

(define-format-all-formatter atsfmt
  (:executable "atsfmt")
  (:install "cabal new-install ats-format --happy-options='-gcsa' -O2")
  (:languages "ATS")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter auctex
  (:executable)
  (:install)
  (:languages "LaTeX")
  (:features)
  (:format (format-all--buffer-native
            'latex-mode
            (lambda ()
              (let ((f (symbol-function 'LaTeX-fill-buffer)))
                (when f (funcall f nil)))))))

(define-format-all-formatter beautysh
  (:executable "beautysh")
  (:install "pip install beautysh")
  (:languages "Shell")
  (:features)
  (:format (format-all--buffer-easy executable "-")))

(define-format-all-formatter black
  (:executable "black")
  (:install "pip install black")
  (:languages "Python")
  (:features)
  (:format (format-all--buffer-easy
            executable "-q"
            (when (format-all--buffer-extension-p "pyi") "--pyi")
            "-")))

(define-format-all-formatter brittany
  (:executable "brittany")
  (:install "stack install brittany")
  (:languages "Haskell" "Literate Haskell")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter bsrefmt
  (:executable "bsrefmt")
  (:install "npm install --global bs-platform")
  (:languages "Reason")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter buildifier
  (:executable "buildifier")
  (:install
   (macos "brew install buildifier")
   "go get github.com/bazelbuild/buildtools/buildifier")
  (:languages "Bazel")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter cabal-fmt
  (:executable "cabal-fmt")
  (:install "cabal install cabal-fmt")
  (:languages "Cabal Config")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter clang-format
  (:executable "clang-format")
  (:install
   (macos "brew install clang-format")
   (windows "scoop install llvm"))
  (:languages "C" "C#" "C++" "Cuda" "GLSL" "Java" "Objective-C" "Protocol Buffer")
  (:features region)
  (:format
   (format-all--buffer-easy
    executable
    "-assume-filename"
    (or (buffer-file-name)
        (cdr (assoc language
                    '(("C"               . ".c")
                      ("C#"              . ".cs")
                      ("C++"             . ".cpp")
                      ("Cuda"            . ".cu")
                      ("GLSL"            . ".glsl")
                      ("Java"            . ".java")
                      ("Objective-C"     . ".m")
                      ("Protocol Buffer" . ".proto")))))
    (when region
      (list "--offset" (number-to-string (1- (car region)))
            "--length" (number-to-string (- (cdr region) (car region))))))))

(define-format-all-formatter cljfmt
  (:executable "cljfmt")
  (:install "npm install --global node-cljfmt")
  (:languages "Clojure")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter cmake-format
  (:executable "cmake-format")
  (:install "pip install cmake-format")
  (:languages "CMake")
  (:features)
  (:format (format-all--buffer-easy executable "-")))

(define-format-all-formatter crystal
  (:executable "crystal")
  (:install (macos "brew install crystal"))
  (:languages "Crystal")
  (:features)
  (:format (format-all--buffer-easy executable "tool" "format" "-")))

(define-format-all-formatter dart-format
  (:executable "dart")
  (:install (macos "brew tap dart-lang/dart && brew install dart"))
  (:languages "Dart")
  (:features)
  (:format
   (format-all--buffer-easy executable "format" "--output" "show")))

(define-format-all-formatter dartfmt
  (:executable "dartfmt")
  (:install (macos "brew tap dart-lang/dart && brew install dart"))
  (:languages "Dart")
  (:features)
  (:format
   (format-all--buffer-easy
    executable
    (when (buffer-file-name)
      (list "--stdin-name" (buffer-file-name))))))

(define-format-all-formatter dfmt
  (:executable "dfmt")
  (:install (macos "brew install dfmt"))
  (:languages "D")
  (:features)
  (:format
   (format-all--buffer-hard nil (regexp-quote "[error]") nil executable)))

(define-format-all-formatter dhall
  (:executable "dhall")
  (:install (macos "brew install dhall"))
  (:languages "Dhall")
  (:features)
  (:format (format-all--buffer-easy executable "format")))

(define-format-all-formatter dockfmt
  (:executable "dockfmt")
  (:install "go get github.com/jessfraz/dockfmt")
  (:languages "Dockerfile")
  (:features)
  (:format (format-all--buffer-easy executable "fmt")))

(define-format-all-formatter efmt
  (:executable "efmt")
  (:install "cargo install efmt")
  (:languages "Erlang")
  (:features)
  (:format (format-all--buffer-easy executable "-")))

(define-format-all-formatter elm-format
  (:executable "elm-format")
  (:install (macos "brew install elm"))
  (:languages "Elm")
  (:features)
  (:format
   (cl-destructuring-bind (output error-output)
       (format-all--buffer-hard nil nil '("elm.json" "elm-package.json")
                                executable "--yes" "--stdin")
     (let ((error-output (format-all--remove-ansi-color error-output)))
       (list output error-output)))))

(define-format-all-formatter emacs-bibtex
  (:executable)
  (:install)
  (:languages "BibTeX")
  (:features)
  (:format (format-all--buffer-native 'bibtex-mode 'bibtex-reformat)))

(define-format-all-formatter emacs-bibtex-sort
  (:executable)
  (:install)
  (:languages "BibTeX")
  (:features)
  (:format (format-all--buffer-native 'bibtex-mode 'bibtex-sort-buffer)))

(define-format-all-formatter emacs-lisp
  (:executable)
  (:install)
  (:languages "Emacs Lisp")
  (:features region)
  (:format
   (format-all--buffer-native
    'emacs-lisp-mode
    (if region
        (lambda () (indent-region (car region) (cdr region)))
        (lambda () (indent-region (point-min) (point-max)))))))

(define-format-all-formatter erb-format
  (:executable "erb-format")
  (:install "gem install erb-formatter")
  (:languages "HTML+ERB")
  (:features)
  (:format (format-all--buffer-easy executable "--stdin")))

(define-format-all-formatter fantomas
  (:executable "fantomas")
  (:install "dotnet tool install -g fantomas-tool")
  (:languages "F#")
  (:features)
  (:format (format-all--buffer-easy executable "--stdin" "--stdout")))

(define-format-all-formatter fish-indent
  (:executable "fish_indent")
  (:install (macos "brew install fish OR port install fish"))
  (:languages "Fish")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter fourmolu
  (:executable "fourmolu")
  (:install "stack install fourmolu")
  (:languages "Haskell" "Literate Haskell")
  (:features)
  (:format (format-all--buffer-easy executable "--stdin-input-file" (buffer-file-name))))

(define-format-all-formatter fprettify
  (:executable "fprettify")
  (:install "pip install fprettify")
  (:languages "Fortran Free Form")
  (:features)
  (:format (format-all--buffer-easy executable "--silent")))

(define-format-all-formatter gawk
  (:executable "gawk")
  (:install (macos "brew install gawk"))
  (:languages "Awk")
  (:features)
  (:format (format-all--buffer-easy executable "-f" "-" "--pretty-print=-")))

(define-format-all-formatter gleam
  (:executable "gleam")
  (:install (macos "brew install gleam"))
  (:languages "_Gleam")
  (:features)
  (:format (format-all--buffer-easy executable "format" "--stdin")))

(define-format-all-formatter gofmt
  (:executable "gofmt")
  (:install
   (macos "brew install go")
   (windows "scoop install go"))
  (:languages "Go")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter goimports
  (:executable "goimports")
  (:install "go get golang.org/x/tools/cmd/goimports")
  (:languages "Go")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter hindent
  (:executable "hindent")
  (:install "stack install hindent")
  (:languages "Haskell" "Literate Haskell")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter html-tidy
  (:executable "tidy")
  (:install
   (macos "brew install tidy-html5")
   (windows "scoop install tidy"))
  (:languages "HTML" "XML")
  (:features)
  (:format
   (format-all--buffer-hard
    '(0 1) nil nil
    executable
    "-q"
    "--tidy-mark" "no"
    "-indent"
    (when (equal language "XML") "-xml"))))

(define-format-all-formatter isort
  (:executable "isort")
  (:install "pip install isort")
  (:languages "Python")
  (:features)
  (:format (format-all--buffer-easy executable "-q" "-")))

(define-format-all-formatter istyle-verilog
  (:executable "iStyle")
  (:install)
  (:languages "Verilog")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter jsonnetfmt
  (:executable "jsonnetfmt")
  (:install (macos "brew install jsonnet"))
  (:languages "Jsonnet")
  (:features)
  (:format (format-all--buffer-easy executable "-")))

(define-format-all-formatter ktlint
  (:executable "ktlint")
  (:install (macos "brew install ktlint"))
  (:languages "Kotlin")
  (:features)
  (:format (format-all--buffer-easy executable "--format" "--stdin")))

(define-format-all-formatter latexindent
  (:executable "latexindent")
  (:install)
  (:languages "LaTeX")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter ledger-mode
  (:executable)
  (:install)
  (:languages "_Ledger")
  (:features)
  (:format
   (format-all--buffer-native 'ledger-mode 'ledger-mode-clean-buffer)))

(define-format-all-formatter lua-fmt
  (:executable "luafmt")
  (:install "npm install --global lua-fmt")
  (:languages "Lua")
  (:features)
  (:format (format-all--buffer-easy executable "--stdin")))

(define-format-all-formatter mix-format
  (:executable "mix")
  (:install (macos "brew install elixir"))
  (:languages "Elixir")
  (:features)
  (:format
   (format-all--buffer-hard
    nil nil '("mix.exs")
    executable
    "format"
    (let ((config-file (format-all--locate-file ".formatter.exs")))
      (when config-file (list "--dot-formatter" config-file)))
    "-")))

(define-format-all-formatter nginxfmt
  (:executable "nginxfmt")
  (:install  "pip install nginxfmt")
  (:languages "_Nginx")
  (:features)
  (:format (format-all--buffer-easy executable "-")))

(define-format-all-formatter nixfmt
  (:executable "nixfmt")
  (:install
   "nix-env -f https://github.com/serokell/nixfmt/archive/master.tar.gz -i")
  (:languages "Nix")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter nixpkgs-fmt
  (:executable "nixpkgs-fmt")
  (:install "nix-env -f https://github.com/nix-community/nixpkgs-fmt/archive/master.tar.gz -i")
  (:languages "Nix")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter ocp-indent
  (:executable "ocp-indent")
  (:install "opam install ocp-indent")
  (:languages "OCaml")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter ormolu
  (:executable "ormolu")
  (:install "stack install ormolu")
  (:languages "Haskell" "Literate Haskell")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter perltidy
  (:executable "perltidy")
  (:install "cpan install Perl::Tidy")
  (:languages "Perl")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter pgformatter
  (:executable "pg_format")
  (:install)
  (:languages "SQL")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter prettier
  (:executable "prettier")
  (:install "npm install --global prettier @prettier/plugin-lua @prettier/plugin-php prettier-plugin-solidity prettier-plugin-svelte prettier-plugin-toml")
  (:languages
   "CSS" "GraphQL" "HTML" "JavaScript" "JSON" "JSON5" "JSX" "Less" "Lua"
   "Markdown" "PHP" "SCSS" "Solidity" "Svelte" "TOML" "TSX" "TypeScript"
   "Vue" "YAML"
   "_Angular" "_Flow")
  (:features region)
  (:format
   (format-all--buffer-easy
    executable
    (when (let* ((file (buffer-file-name))
                 (info (and file
                            (with-temp-buffer
                              (call-process executable nil t nil
                                            "--file-info" file)
                              (buffer-string)))))
            (when (and format-all-debug info)
              (message "Format-All: --file-info: %s" info))
            (or (not info)
                (save-match-data
                  (string-match
                   (regexp-quote "\"inferredParser\": null")
                   info))))
      (list "--parser"
            (let ((pair (assoc language
                               '(("_Angular"   . "angular")
                                 ("_Flow"      . "flow")
                                 ("JavaScript" . "babel")
                                 ("JSX"        . "babel")
                                 ("Solidity"   . "solidity-parse")
                                 ("TSX"        . "typescript")))))
              (if pair (cdr pair) (downcase language)))))
    (when (buffer-file-name)
      (list "--stdin-filepath" (buffer-file-name)))
    (let ((ignore-file (format-all--locate-file ".prettierignore")))
      (when ignore-file
        (list "--ignore-path" ignore-file)))
    (when region
      (list "--range-start" (number-to-string (1- (car region)))
            "--range-end"   (number-to-string (1- (cdr region))))))))

(define-format-all-formatter prettierd
  (:executable "prettierd")
  (:install "npm install --global @fsouza/prettierd")
  (:languages
   "CSS" "GraphQL" "HTML" "JavaScript" "JSON" "JSON5" "JSX"
   "Less" "Markdown" "SCSS" "TSX" "TypeScript" "Vue" "YAML")
  (:features)
  (:format
   (format-all--buffer-easy
    executable
    (or (buffer-file-name)
        (buffer-name)))))

(define-format-all-formatter purs-tidy
  (:executable "purs-tidy")
  (:install "npm install --global purs-tidy")
  (:languages "PureScript")
  (:features)
  (:format (format-all--buffer-easy executable "format")))

(define-format-all-formatter purty
  (:executable "purty")
  (:install "npm install --global purty")
  (:languages "PureScript")
  (:features)
  (:format (format-all--buffer-easy executable "-")))

(define-format-all-formatter raco-fmt
  (:executable "raco")
  (:install "raco pkg install fmt")
  (:languages "Racket")
  (:features)
  (:format (format-all--buffer-easy executable "fmt")))

(define-format-all-formatter rescript
  (:executable "rescript")
  (:install "npm install --global rescript")
  (:languages "ReScript")
  (:features)
  (:format
   (format-all--buffer-easy
    executable "format" "-stdin"
    (let ((ext (if (not (buffer-file-name)) ""
                   (file-name-extension (buffer-file-name)))))
      (concat "." (if (equal ext "") "res" ext))))))

(define-format-all-formatter rubocop
  (:executable "rubocop")
  (:install "gem install rubocop:'>=1.4.0'")
  (:languages "Ruby")
  (:features)
  (:format
   (format-all--buffer-hard-ruby
    "rubocop" '(0 1) nil nil
    executable
    "--auto-correct"
    "--format" "quiet"
    "--stderr"
    "--stdin" (or (buffer-file-name) (buffer-name)))))

(define-format-all-formatter rufo
  (:executable "rufo")
  (:install "gem install rufo")
  (:languages "Ruby")
  (:features)
  (:format
   (format-all--buffer-hard-ruby
    "rufo" nil nil nil
    executable
    "--simple-exit"
    (when (buffer-file-name)
      (list "--filename" (buffer-file-name))))))

(define-format-all-formatter rustfmt
  (:executable "rustfmt")
  (:install "rustup component add rustfmt")
  (:languages "Rust")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter scalafmt
  (:executable "scalafmt")
  (:install "coursier bootstrap org.scalameta:scalafmt-cli_2.12:2.4.0-RC1 -r sonatype:snapshots -o /usr/local/bin/scalafmt --standalone --main org.scalafmt.cli.Cli")
  (:languages "Scala")
  (:features)
  (:format
   (format-all--buffer-easy
    executable "--stdin" "--non-interactive" "--quiet")))

(define-format-all-formatter shfmt
  (:executable "shfmt")
  (:install
   (macos "brew install shfmt")
   (windows "scoop install shfmt"))
  (:languages "Shell")
  (:features)
  (:format
   (format-all--buffer-easy
    executable
    (if (buffer-file-name)
        (list "-filename" (buffer-file-name))
      (list "-ln" (cl-case (and (eql major-mode 'sh-mode)
                                (boundp 'sh-shell)
                                (symbol-value 'sh-shell))
                    (bash "bash")
                    (mksh "mksh")
                    (t "posix")))))))

(define-format-all-formatter snakefmt
  (:executable "snakefmt")
  (:install)
  (:languages "_Snakemake")
  (:features)
  (:format (format-all--buffer-easy executable "-")))

(define-format-all-formatter sqlformat
  (:executable "sqlformat")
  (:install "pip install sqlparse")
  (:languages "SQL")
  (:features)
  (:format
   (let* ((ic (car default-process-coding-system))
          (oc (cdr default-process-coding-system))
          (ienc (symbol-name (or (coding-system-get ic :mime-charset)
                                 'utf-8)))
          (oenc (symbol-name (or (coding-system-get oc :mime-charset)
                                 'utf-8)))
          (process-environment (cons (concat "PYTHONIOENCODING=" oenc)
                                     process-environment)))
     (format-all--buffer-easy executable "--encoding" ienc "-"))))

(define-format-all-formatter standard
  (:executable "standard")
  (:install "npm install --global standard")
  (:languages "JavaScript" "JSX")
  (:features)
  (:format
   ;; `standard --stdin` properly uses zero vs non-zero exit codes to
   ;; indicate success vs error. However, it checks for quite a broad
   ;; range of errors, all the way up to undeclared identifiers and
   ;; such. To catch only syntax errors, we need to look specifically
   ;; for the text "Parsing error:".
   (format-all--buffer-hard
    '(0 1) ".*?:.*?:[0-9]+:[0-9]+: Parsing error:" nil
    executable "--fix" "--stdin")))

(define-format-all-formatter standardrb
  (:executable "standardrb")
  (:install "gem install standard:'>=0.13.0'")
  (:languages "Ruby")
  (:features)
  (:format
   (format-all--buffer-hard-ruby
    "standard" '(0 1) nil nil
    executable
    "--stderr"
    "--fix"
    "--stdin" (or (buffer-file-name) (buffer-name)))))

(define-format-all-formatter styler
  (:executable "Rscript")
  (:install "Rscript -e \"install.packages('styler')\"")
  (:languages "R")
  (:features)
  (:format
   (format-all--buffer-easy
    executable
    "-e" (concat
          "options(styler.colored_print.vertical=FALSE);"
          " con <- file('stdin');"
          " out <- styler::style_text(readLines(con));"
          " close(con);"
          " out"))))

(define-format-all-formatter stylish-haskell
  (:executable "stylish-haskell")
  (:install "stack install stylish-haskell")
  (:languages "Haskell")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter stylua
  (:executable "stylua")
  (:install "cargo install stylua")
  (:languages "Lua")
  (:features)
  (:format (format-all--buffer-easy executable "-")))

(define-format-all-formatter swiftformat
  (:executable "swiftformat")
  (:install (macos "brew install swiftformat"))
  (:languages "Swift")
  (:features region)
  (:format
   (format-all--buffer-easy
    executable
    "--quiet"
    (let ((config (format-all--locate-file ".swiftformat")))
      (when config (list "--config" config)))
    (when region
      (list "--linerange" (format "%d,%d"
                                  (line-number-at-pos (car region))
                                  (line-number-at-pos (cdr region))))))))

(define-format-all-formatter taplo-fmt
  (:executable "taplo")
  (:install "npm install --global @taplo/cli")
  (:languages "TOML")
  (:features)
  (:format (format-all--buffer-easy executable "fmt" "-")))

(define-format-all-formatter terraform-fmt
  (:executable "terraform")
  (:install (macos "brew install terraform"))
  (:languages "Terraform")
  (:features)
  (:format (format-all--buffer-easy executable "fmt" "-no-color" "-")))

(define-format-all-formatter ts-standard
  (:executable "ts-standard")
  (:install "npm install --global ts-standard")
  (:languages "TypeScript" "TSX")
  (:features)
  (:format
   ;; `ts-standard --stdin` properly uses zero vs non-zero exit codes to
   ;; indicate success vs error. However, it checks for quite a broad
   ;; range of errors, all the way up to undeclared identifiers and
   ;; such. To catch only syntax errors, we need to look specifically
   ;; for the text "Parsing error:".
   (format-all--buffer-hard
    '(0 1) ".*?:.*?:[0-9]+:[0-9]+: Parsing error:" '("tsconfig.json")
    executable "--fix" "--stdin"
    (when (buffer-file-name)
      (list "--stdin-filename" (buffer-file-name))))))

(define-format-all-formatter v-fmt
  (:executable "v")
  (:install)
  (:languages "V")
  (:features)
  (:format (format-all--buffer-easy executable "fmt")))

(define-format-all-formatter yapf
  (:executable "yapf")
  (:install "pip install yapf")
  (:languages "Python")
  (:features)
  (:format (format-all--buffer-easy executable)))

(define-format-all-formatter zig
  (:executable "zig")
  (:install)
  (:languages "Zig")
  (:features)
  (:format (format-all--buffer-easy executable "fmt" "--stdin")))

(define-format-all-formatter zprint
  (:executable "zprint")
  (:install)
  (:languages "Clojure")
  (:features)
  (:format (format-all--buffer-easy executable)))

(defun format-all--language-id-buffer ()
  "Return the language used in the current buffer, or NIL.

Prefer getting the ID from the language-id library. Some
languages do not yet have official GitHub Linguist identifiers,
yet format-all needs to know about them anyway. That's why we
have this custom language-id function in format-all. The
unofficial languages IDs are prefixed with \"_\"."
  (or (and (or (equal major-mode 'angular-html-mode)
               (and (equal major-mode 'web-mode)
                    (equal (symbol-value 'web-mode-content-type) "html")
                    (equal (symbol-value 'web-mode-engine) "angular")))
           "_Angular")
      (and (member major-mode '(js-mode js2-mode js3-mode))
           (boundp 'flow-minor-mode)
           (not (null (symbol-value 'flow-minor-mode)))
           "_Flow")
      (and (equal major-mode 'gleam-mode) "_Gleam")
      (and (equal major-mode 'ledger-mode) "_Ledger")
      (and (equal major-mode 'nginx-mode) "_Nginx")
      (and (equal major-mode 'snakemake-mode) "_Snakemake")
      (language-id-buffer)))

(defun format-all--please-install (executable installer)
  "Internal helper function for error about missing EXECUTABLE and INSTALLER."
  (concat (format "You need the %s command." executable)
          (if (not installer) ""
            (format " You may be able to install it via: %s" installer))))

(defun format-all--formatter-executable (formatter)
  "Internal helper function to get the external program for FORMATTER."
  (let ((executable (gethash formatter format-all--executable-table)))
    (when executable
      (or (executable-find executable)
          (signal 'format-all-executable-not-found
                  (list (format-all--please-install
                         executable
                         (gethash formatter format-all--install-table))))))))

(defun format-all--show-errors-buffer (error-output show-errors-p)
  "Internal shorthand function to update and show error output.

ERROR-OUTPUT come from the formatter.  SHOW-ERRORS-P determines
whether or not to display the errors buffer."
  (save-selected-window
    (with-current-buffer (get-buffer-create "*format-all-errors*")
      (erase-buffer)
      (insert error-output)
      (if show-errors-p
          (display-buffer (current-buffer))
        (let ((error-window (get-buffer-window (current-buffer))))
          (when error-window (quit-window nil error-window)))))))

(defun format-all--update-errors-buffer (status error-output)
  "Internal helper function to update *format-all-errors*.

STATUS and ERROR-OUTPUT come from the formatter."
  (let* ((has-warnings-p (not (= 0 (length error-output))))
         (has-errors-p (eq status :error))
         (show-errors-p (cl-case format-all-show-errors
                          ((never) nil)
                          ((always) t)
                          ((warnings) (or has-errors-p has-warnings-p))
                          ((errors) has-errors-p))))
    (format-all--show-errors-buffer error-output show-errors-p)))

(defun format-all--save-line-number (thunk)
  "Internal helper function to run THUNK and go back to the same line."
  (let ((old-line-number (line-number-at-pos))
        (old-column (current-column)))
    (funcall thunk)
    (goto-char (point-min))
    (forward-line (1- old-line-number))
    (let ((line-length (- (point-at-eol) (point-at-bol))))
      (goto-char (+ (point) (min old-column line-length))))))

(defun format-all--run-chain (language chain region)
  "Internal function to run a formatter CHAIN on the current buffer.

LANGUAGE is the language ID of the current buffer, from
`format-all--language-id-buffer`.

REGION is either a (START . END) pair, or nil to format the
entire buffer."
  (let* ((chain (format-all--normalize-chain chain))
         (chain-tail chain)
         (error-output "")
         (reformatted-by '()))
    (let ((unsupported
           (when region
             (cl-remove-if
              (lambda (f-name)
                (memq 'region (gethash f-name format-all--features-table)))
              (mapcar #'car chain)))))
      (when unsupported
        (error "The format-all-region command is not supported for %s"
               (string-join (mapcar #'symbol-name unsupported) ", "))))
    (format-all--save-line-number
     (lambda ()
       (cl-loop
        (unless (and chain-tail (= 0 (length error-output)))
          (cl-return))
        (let* ((formatter (car chain-tail))
               (f-name (car formatter))
               (f-args (cdr formatter))
               (f-function (gethash f-name format-all--format-table))
               (f-executable (format-all--formatter-executable f-name)))
          (when format-all-debug
            (message
             "Format-All: Formatting %s as %s using %S%s"
             (buffer-name) language f-name
             (with-temp-buffer
               (dolist (arg f-args) (insert " " (shell-quote-argument arg)))
               (buffer-string))))
          (cl-destructuring-bind (f-output f-error-output)
              (let ((format-all--user-args f-args))
                (funcall f-function f-executable language region))
            (let ((f-status :already-formatted))
              (cond ((null f-output)
                     (setq error-output f-error-output)
                     (setq f-status :error))
                    ((not (equal f-output t))
                     (setq reformatted-by
                           (append reformatted-by (list f-name)))
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       (insert f-output))
                     (setq f-status :reformatted)))
              (run-hook-with-args 'format-all-after-format-functions
                                  f-name f-status)
              (format-all--update-errors-buffer f-status f-error-output))))
        (setq chain-tail (cdr chain-tail)))
       (message "%s"
                (cond ((not (= 0 (length error-output))) "Formatting error")
                      ((not reformatted-by) "Already formatted")
                      (t "Reformatted!")))))))

(defun format-all--get-default-chain (language)
  "Internal function to get the default formatter chain for LANGUAGE."
  (when language (cdr (assoc language format-all-default-formatters))))

(defun format-all--get-chain (language)
  "Internal function to get LANGUAGE formatter chain for current buffer."
  (when language (cdr (assoc language format-all-formatters))))

(defun format-all--set-chain (language chain)
  "Internal function to set LANGUAGE formatter CHAIN for current buffer."
  (cl-assert (stringp language))
  (cl-assert (listp chain))
  (setq format-all-formatters
        (append (cl-remove-if (lambda (pair) (equal language (car pair)))
                              format-all-formatters)
                (when chain (list (cons language chain))))))

(defun format-all--prompt-for-formatter (language)
  "Internal function to choose a formatter for LANGUAGE."
  (let ((f-names (gethash language format-all--language-table)))
    (cond ((null f-names)
           (error "No supported formatters for %s"
                  (or language "this language")))
          ((null (cdr f-names))
           (car f-names))
          (t
           (let ((f-string (completing-read
                            (format "Formatter for %s: " language)
                            (mapcar #'list f-names) nil t)))
             (and (not (= 0 (length f-string)))
                  (intern f-string)))))))

(defun format-all--buffer-from-hook ()
  "Internal helper function to auto-format current buffer from a hook.

Format-All installs this function into `before-save-hook' to
format buffers on save. This is a lenient version of
`format-all-buffer' that silently succeeds instead of signaling
an error if the current buffer has no formatter."
  (let ((language (format-all--language-id-buffer)))
    (format-all--run-chain language
                           (format-all--get-chain language)
                           nil)))

(defun format-all--buffer-or-region (prompt region)
  "Internal helper function to auto-format current buffer from command.

If PROMPT is non-nil, prompt interactively for formatter.
If REGION is non-nil, it is a (START . END) pair passed to the formatter."
  (let* ((language (format-all--language-id-buffer))
         (chain (format-all--get-chain language)))
    (when (or (equal 'always prompt) (and prompt (not chain)))
      (let ((f-name (format-all--prompt-for-formatter language)))
        (when f-name
          (message "Setting formatter to %S" f-name)
          (setq chain (list f-name))
          (format-all--set-chain language chain))))
    (unless chain (error "No formatter"))
    (format-all--run-chain language chain region)))

;;;###autoload
(defun format-all-buffer (&optional prompt)
  "Auto-format the source code in the current buffer.

No disk files are touched - the buffer doesn't even need to be
saved.  If you don't like the results of the formatting, you can
use ordinary undo to get your code back to its previous state.

You will need to install external programs to do the formatting.
If the command can't find the program that it needs, it will try
to tell you how you might be able to install it on your operating
system. Only BibTeX, Emacs Lisp and Ledger are formatted without an
external program.

A suitable formatter is selected according to the `major-mode' of
the buffer.  Many popular programming languages are supported.
It is fairly easy to add new languages that have an external
formatter.  When called interactively or PROMPT-P is non-nil, a
missing formatter is prompted in the minibuffer.

If PROMPT is non-nil (or the function is called as an interactive
command), a missing formatter is prompted in the minibuffer.  If
PROMPT is the symbol `always' (or a prefix argument is given),
the formatter is prompted for even if one has already been set.

If any errors or warnings were encountered during formatting,
they are shown in a buffer called *format-all-errors*."
  (interactive (list (if current-prefix-arg 'always t)))
  (format-all--buffer-or-region prompt nil))

;;;###autoload
(defun format-all-region (start end &optional prompt)
  "Auto-format the source code in the current region.

Like `format-all-buffer' but format only the active region
instead of the entire buffer.  This requires support from the
formatter.

Called non-interactively, START and END delimit the region.
The PROMPT argument works as for `format-all-buffer'."
  (interactive
   (let ((prompt (if current-prefix-arg 'always t)))
     (if (use-region-p)
         (list (region-beginning) (region-end) prompt)
         (error "The region is not active now"))))
  (format-all--buffer-or-region prompt (cons start end)))

(defun format-all-ensure-formatter ()
  "Ensure current buffer has a formatter, using default if not."
  (interactive)
  (let ((language (format-all--language-id-buffer)))
    (unless (format-all--get-chain language)
      (cond ((not language)
             (message "No formatter for this language"))
            ((not (gethash language format-all--language-table))
             (message "No formatter for %s" language))
            (t
             (let ((default (format-all--get-default-chain language)))
               (cond ((not default)
                      (message "No default formatter for %s" language))
                     (t
                      (message "Using default formatter%s"
                               (with-temp-buffer
                                 (dolist (formatter default (buffer-string))
                                   (insert (format " %S" formatter)))))
                      (format-all--set-chain language default)))))))))

;;;###autoload
(define-minor-mode format-all-mode
  "Toggle automatic source code formatting before save.

When this minor mode (FmtAll) is enabled, `format-all-buffer' is
automatically called to format your code each time before you
save the buffer.

The mode is buffer-local and needs to be enabled separately each
time a file is visited. You may want to use `add-hook' in your
`user-init-file' to enable the mode based on buffer modes. E.g.:

    (add-hook 'prog-mode-hook 'format-all-mode)

To use a default formatter for projects that don't have one, add
this too:

    (add-hook 'prog-mode-hook 'format-all-ensure-formatter)

When `format-all-mode' is called as a Lisp function, the mode is
toggled if ARG is ‘toggle’, disabled if ARG is a negative integer
or zero, and enabled otherwise."
  :lighter format-all-mode-lighter
  :global nil
  (if format-all-mode
      (add-hook 'before-save-hook
                'format-all--buffer-from-hook
                nil 'local)
    (remove-hook 'before-save-hook
                 'format-all--buffer-from-hook
                 'local)))

(provide 'format-all)

;;; format-all.el ends here
