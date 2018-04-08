*format-all* for Emacs
======================

What does it do
---------------

Lets you auto-format source code in several languages using the same
command for all languages, instead of learning a different elisp
package and formatting command for each language.

Just do **M-x** `format-all-buffer` and it will try its best to do the
right thing.

For most languages, you will need to install an external program to
help with the formatting.  If you don't have the right program,
`format-all-buffer` will try to tell you how to install it.

How to install
--------------

From [MELPA](https://melpa.org/#/?q=format-all)

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
