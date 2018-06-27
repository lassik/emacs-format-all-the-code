*format-all* for Emacs
======================

What does it do
---------------

Lets you auto-format source code in many languages using the same
command for all languages, instead of learning a different elisp
package and formatting command for each language. Just do **M-x**
`format-all-buffer` and it will try its best to do the right thing.

Supported languages
-------------------

* **C/C++/Objective-C** (*clang-format*)
* **CSS/Less/SCSS** (*prettier*)
* **D** (*dfmt*)
* **Elixir** (*mix format*)
* **Elm** (*elm-format*)
* **Emacs Lisp** (native)
* **Go** (*gofmt*)
* **GraphQL** (*prettier*)
* **Haskell** (*hindent*)
* **JavaScript/JSON/JSX/TypeScript/Vue** (*prettier*)
* **Kotlin** (*ktlint*)
* **Markdown** (*prettier*)
* **OCaml** (*ocp-indent*)
* **Perl** (*perltidy*)
* **Python** (*autopep8*)
* **Ruby** (*rufo*)
* **Rust** (*rustfmt*)
* **Shell script** (*shfmt*)
* **Swift** (*swiftformat*)

How to install
--------------

From [MELPA](https://melpa.org/#/?q=format-all)

You will need to install external programs to do the formatting. If
`format-all-buffer` can't find the right program, it will try to tell
you how to install it.

How to customize
----------------

A minor mode called `format-all-mode` is available.

You can hook it with other modes like this:

```emacs-lisp
(add-hook 'js2-mode #'format-all-mode)
```

There is currently no customize variables either, since it's not
clear what approach should be taken. Please see
[GitHub issues][github-issues] for discussion.

Many of the external formatters support configuration files in the
source code directory to control their formatting. Please see the
documentation for each formatter.

How to add new languages
------------------------

New external formatters can be added easily if they can read code from
standard input and format it to standard output. Feel free to submit a
pull request or ask for help in [GitHub issues][github-issues].

How to report bugs
------------------

[GitHub issues][github-issues] are preferred. Email is also ok.

Feature requests are welcome. If you are interested in doing anything
beyong just adding new formatters in the current framework, please
discuss in issues before writing code, since there are big unresolved
questions about where the project should go from here.

Roadmap
-------

**[atom-beautify](https://atom.io/packages/atom-beautify#beautifiers)**
sports a very impressive set of formatters. We should aspire to that
level of coverage for Emacs.

**[Unibeautify](https://github.com/Unibeautify/unibeautify)** is a
project to provide one shell command to run all beautifiers.
*atom-beautify* will be rewritten to be based on it. Perhaps we should
be too, once it stabilizes.

[github-issues]: https://github.com/lassik/emacs-format-all-the-code/issues
