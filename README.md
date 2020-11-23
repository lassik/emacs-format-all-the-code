*format-all* for Emacs
======================

What does it do
---------------

Lets you auto-format source code in many languages using the same
command for all languages, instead of learning a different Emacs
package and formatting command for each language.

Just do **M-x** `format-all-buffer` and it will try its best to do the
right thing. To auto-format code on save, use the minor mode
`format-all-mode`. Please see the documentation for that function for
instructions.

Supported languages
-------------------

* **Angular/Vue** ([*prettier*](https://prettier.io/))
* **Assembly** ([*asmfmt*](https://github.com/klauspost/asmfmt))
* **Bazel Starlark** ([*buildifier*](https://github.com/bazelbuild/buildtools/tree/master/buildifier))
* **BibTeX** (emacs)
* **C/C++/Objective-C** ([*clang-format*](https://clang.llvm.org/docs/ClangFormat.html))
* **Cabal** ([*cabal-fmt*](https://github.com/phadej/cabal-fmt))
* **Clojure/ClojureScript** ([*node-cljfmt*](https://github.com/snoe/node-cljfmt))
* **CMake** ([*cmake-format*](https://github.com/cheshirekow/cmake_format))
* **Crystal** ([*crystal tool format*](http://www.motion-express.com/blog/crystal-code-formatter))
* **CSS/Less/SCSS** ([*prettier*](https://prettier.io/))
* **D** ([*dfmt*](https://github.com/dlang-community/dfmt))
* **Dart** ([*dartfmt*](https://github.com/dart-lang/dart_style))
* **Dhall** ([*dhall format*](https://github.com/dhall-lang/dhall-lang))
* **Dockerfile** ([*dockfmt*](https://github.com/jessfraz/dockfmt))
* **Elixir** ([*mix format*](https://hexdocs.pm/mix/master/Mix.Tasks.Format.html))
* **Elm** ([*elm-format*](https://github.com/avh4/elm-format))
* **Emacs Lisp** (emacs)
* **Fish Shell** ([*fish_indent*](https://fishshell.com/docs/current/commands.html#fish_indent))
* **Fortran 90** ([*fprettify*](https://github.com/pseewald/fprettify))
* **Gleam** ([*gleam format*](https://gleam.run/))
* **Go** ([*gofmt*](https://golang.org/cmd/gofmt/))
* **GraphQL** ([*prettier*](https://prettier.io/))
* **Haskell** ([*brittany*](https://github.com/lspitzner/brittany), [*hindent*](https://github.com/commercialhaskell/hindent), [stylish-haskell](https://github.com/jaspervdj/stylish-haskell))
* **HTML/XHTML/XML** ([*tidy*](http://www.html-tidy.org/))
* **Java** ([*clang-format*](https://clang.llvm.org/docs/ClangFormat.html))
* **JavaScript/JSON/JSX** ([*prettier*](https://prettier.io/), [*standard*](https://standardjs.com/))
* **Jsonnet** ([jsonnetfmt](https://jsonnet.org/))
* **Kotlin** ([*ktlint*](https://github.com/shyiko/ktlint))
* **LaTeX** ([*latexindent*](https://github.com/cmhughes/latexindent.pl))
* **Ledger** ([*ledger-mode*](https://github.com/ledger/ledger-mode))
* **Lua** ([*lua-fmt*](https://github.com/trixnz/lua-fmt), [*prettier plugin-lua*](https://github.com/prettier/plugin-lua))
* **Markdown** ([*prettier*](https://prettier.io/))
* **Nix** ([*nixfmt*](https://github.com/serokell/nixfmt))
* **OCaml** ([*ocp-indent*](https://opam.ocaml.org/packages/ocp-indent/))
* **Perl** ([*perltidy*](http://perltidy.sourceforge.net/))
* **PHP** ([*prettier plugin-php*](https://github.com/prettier/plugin-php))
* **Protocol Buffers** ([*clang-format*](https://clang.llvm.org/docs/ClangFormat.html))
* **PureScript** ([*purty*](https://gitlab.com/joneshf/purty))
* **Python** ([*black*](https://github.com/ambv/black), [*yapf*](https://github.com/google/yapf))
* **R** ([*styler*](https://github.com/r-lib/styler))
* **Reason** ([*bsrefmt*](https://github.com/glennsl/bs-refmt))
* **Ruby** ([*rufo*](https://github.com/ruby-formatter/rufo))
* **Rust** ([*rustfmt*](https://github.com/rust-lang-nursery/rustfmt))
* **Scala** ([*scalafmt*](https://github.com/scalameta/scalafmt))
* **Shell script** ([*beautysh*](https://github.com/lovesegfault/beautysh), [*shfmt*](https://github.com/mvdan/sh))
* **Snakemake** ([*snakefmt*](https://github.com/snakemake/snakefmt))
* **Solidity** ([*prettier-plugin-solidity*](https://github.com/prettier-solidity/prettier-plugin-solidity))
* **SQL** ([pgformatter](https://github.com/darold/pgFormatter), [*sqlformat*](https://pypi.org/project/sqlparse/))
* **Swift** ([*swiftformat*](https://github.com/nicklockwood/SwiftFormat))
* **Terraform** ([*terraform fmt*](https://www.terraform.io/docs/commands/fmt.html))
* **TOML** ([*prettier-plugin-toml*](https://github.com/bd82/toml-tools/tree/master/packages/prettier-plugin-toml))
* **TypeScript/TSX** ([*prettier*](https://prettier.io/))
* **Verilog** ([*iStyle*](https://github.com/thomasrussellmurphy/istyle-verilog-formatter))
* **YAML** ([*prettier*](https://prettier.io/))

How to install
--------------

From [MELPA](https://melpa.org/#/format-all)

You will need to install external programs to do the formatting. If
`format-all-buffer` can't find the right program, it will try to tell
you how to install it.

If you have installed a formatter but Emacs cannot find it, Emacs may
be using a `PATH` different from your shell. The path searched by
Emacs is in the `exec-path` variable. You can easily make it match
your shell's `PATH` using the
[exec-path-from-shell](http://melpa.org/#/exec-path-from-shell)
package from MELPA.

How to customize
----------------

There are currently no customize variables, since it's not clear what
approach should be taken. Please see [GitHub issues][github-issues]
for discussion.

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
