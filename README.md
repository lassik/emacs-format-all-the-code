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

* **C/C++** (*clang-format*)
* **D** (*dfmt*)
* **Elm** (*elm-format*)
* **Emacs Lisp** (native)
* **Go** (*gofmt*)
* **Haskell** (*hindent*)
* **JavaScript** (*standard*)
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

There is currently no before-save hook and no customize variables
either, since it's not clear what approach should be taken. Please see
https://github.com/lassik/emacs-format-all-the-code/issues for
discussion.

Many formatters support configuration files in the source code
directory to control their formatting. Please see the documentation
for each external formatter.

Adding new languages
--------------------

New formatters can be added easily if they are external programs that
can read code from stdin and format it to stdout.

How to report bugs
------------------

GitHub issues are preferred. Email is also ok. Feature requests are
welcome. PRs are very welcome, but for non-trivial changes please open
an issue to coordinate with me first.

Roadmap
-------

**[atom-beautify](https://atom.io/packages/atom-beautify#beautifiers)**
sports a very impressive set of formatters. We should aspire to that
level of coverage for Emacs.

**[Unibeautify](https://github.com/Unibeautify/unibeautify)** is a
project to provide one shell command to run all beautifiers.
*atom-beautify* will be rewritten to be based on it. Perhaps we should
be too, once it stabilizes.
