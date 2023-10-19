*format-all* for Emacs
======================

> *NOTE:* The package is actively maintained but due to lack of time,
> complex tasks are done at a slow pace. Simple tasks like adding or
> fixing formatter definitions are often done immediately. For faster
> progress, additional maintainers are welcome.

What does it do
---------------

Lets you auto-format source code in many languages using the same
command for all languages, instead of learning a different Emacs
package and formatting command for each language.

Just do **M-x** `format-all-region-or-buffer` and it will try its best
to do the right thing. To auto-format code on save, use the minor mode
`format-all-mode`. Please see the documentation for that function for
instructions.

Supported languages
-------------------

* **Angular** ([*prettier*](https://prettier.io/))
* **Assembly** ([*asmfmt*](https://github.com/klauspost/asmfmt))
* **ATS** ([*atsfmt*](https://hackage.haskell.org/package/ats-format))
* **Awk** ([*gawk*](https://www.gnu.org/software/gawk/))
* **Bazel Starlark** ([*buildifier*](https://github.com/bazelbuild/buildtools/tree/master/buildifier))
* **Beancount** ([*bean-format*](https://beancount.github.io/))
* **BibTeX** (Emacs)
* **C/C++/Objective-C** ([*clang-format*](https://clang.llvm.org/docs/ClangFormat.html), [*astyle*](http://astyle.sourceforge.net/))
* **C#** ([*clang-format*](https://clang.llvm.org/docs/ClangFormat.html), [*astyle*](http://astyle.sourceforge.net/), [*csharpier*](https://github.com/belav/csharpier))
* **Cabal** ([*cabal-fmt*](https://github.com/phadej/cabal-fmt))
* **Caddyfile** ([*caddy-fmt*](https://caddyserver.com/docs/command-line#caddy-fmt))
* **Clojure/ClojureScript** ([*zprint*](https://github.com/kkinnear/zprint), [*node-cljfmt*](https://github.com/snoe/node-cljfmt))
* **CMake** ([*cmake-format*](https://github.com/cheshirekow/cmake_format))
* **Crystal** ([*crystal tool format*](http://www.motion-express.com/blog/crystal-code-formatter))
* **CSS/Less/SCSS** ([*prettier*](https://prettier.io/), [*prettierd*](https://github.com/fsouza/prettierd))
* **Cuda** ([*clang-format*](https://clang.llvm.org/docs/ClangFormat.html))
* **D** ([*dfmt*](https://github.com/dlang-community/dfmt))
* **Dart** ([*dartfmt*](https://github.com/dart-lang/dart_style), [*dart-format*](https://dart.dev/tools/dart-format))
* **Dhall** ([*dhall format*](https://github.com/dhall-lang/dhall-lang))
* **Dockerfile** ([*dockfmt*](https://github.com/jessfraz/dockfmt))
* **Elixir** ([*mix format*](https://hexdocs.pm/mix/master/Mix.Tasks.Format.html))
* **Elm** ([*elm-format*](https://github.com/avh4/elm-format))
* **Emacs Lisp** (Emacs)
* **Erb** ([*erb-format*](https://github.com/nebulab/erb-formatter))
* **Erlang** ([*efmt*](https://github.com/sile/efmt))
* **F#** ([*fantomas*](https://github.com/fsprojects/fantomas))
* **Fish Shell** ([*fish_indent*](https://fishshell.com/docs/current/commands.html#fish_indent))
* **Fortran Free Form** ([*fprettify*](https://github.com/pseewald/fprettify))
* **Gleam** ([*gleam format*](https://gleam.run/))
* **GLSL** ([*clang-format*](https://clang.llvm.org/docs/ClangFormat.html))
* **Go** ([*gofmt*](https://golang.org/cmd/gofmt/), [*goimports*](https://godoc.org/golang.org/x/tools/cmd/goimports))
* **GraphQL** ([*prettier*](https://prettier.io/), [*prettierd*](https://github.com/fsouza/prettierd))
* **Haskell** ([*brittany*](https://github.com/lspitzner/brittany), [*fourmolu*](https://github.com/fourmolu/fourmolu), [*hindent*](https://github.com/commercialhaskell/hindent), [*ormolu*](https://github.com/tweag/ormolu), [*stylish-haskell*](https://github.com/jaspervdj/stylish-haskell))
* **HTML/XHTML/XML** ([*tidy*](http://www.html-tidy.org/))
* **Hy** (Emacs)
* **Java** ([*clang-format*](https://clang.llvm.org/docs/ClangFormat.html), [*astyle*](http://astyle.sourceforge.net/))
* **JavaScript/JSON/JSX** ([*prettier*](https://prettier.io/), [*standard*](https://standardjs.com/), [*prettierd*](https://github.com/fsouza/prettierd), [*deno*](https://deno.land/manual/tools/formatter))
* **Jsonnet** ([*jsonnetfmt*](https://jsonnet.org/))
* **Kotlin** ([*ktlint*](https://github.com/shyiko/ktlint))
* **LaTeX** ([*latexindent*](https://github.com/cmhughes/latexindent.pl), [*auctex*](https://www.gnu.org/software/auctex/))
* **Ledger** ([*ledger-mode*](https://github.com/ledger/ledger-mode))
* **Lua** ([*lua-fmt*](https://github.com/trixnz/lua-fmt), [stylua](https://github.com/JohnnyMorganz/StyLua),  [*prettier plugin*](https://github.com/prettier/plugin-lua))
* **Markdown** ([*prettier*](https://prettier.io/), [*prettierd*](https://github.com/fsouza/prettierd), [*deno*](https://deno.land/manual/tools/formatter))
* **Meson** ([*muon fmt*](https://sr.ht/~lattis/muon/))
* **Nginx** ([*nginxfmt*](https://github.com/slomkowski/nginx-config-formatter))
* **Nix** ([*nixpkgs-fmt*](https://github.com/nix-community/nixpkgs-fmt), [*nixfmt*](https://github.com/serokell/nixfmt),
[*alejandra*](https://github.com/kamadorueda/alejandra))
* **OCaml** ([*ocp-indent*](https://opam.ocaml.org/packages/ocp-indent/), [*ocamlformat*](https://github.com/ocaml-ppx/ocamlformat))
* **Perl** ([*perltidy*](http://perltidy.sourceforge.net/))
* **PHP** ([*prettier plugin*](https://github.com/prettier/plugin-php))
* **Protocol Buffers** ([*clang-format*](https://clang.llvm.org/docs/ClangFormat.html))
* **PureScript** ([*purty*](https://gitlab.com/joneshf/purty), [*purescript-tidy*](https://github.com/natefaubion/purescript-tidy))
* **Python** ([*black*](https://github.com/ambv/black), [*yapf*](https://github.com/google/yapf), [*isort*](https://github.com/PyCQA/isort))
* **R** ([*styler*](https://github.com/r-lib/styler))
* **Racket** ([*raco fmt*](https://docs.racket-lang.org/fmt/))
* **Reason** ([*bsrefmt*](https://github.com/glennsl/bs-refmt))
* **ReScript** ([*rescript format*](https://www.npmjs.com/package/rescript))
* **Ruby** ([*rubocop*](https://github.com/rubocop/rubocop), [*rufo*](https://github.com/ruby-formatter/rufo), [*standardrb*](https://github.com/testdouble/standard), [*stree (syntax_tree)*](https://github.com/ruby-syntax-tree/syntax_tree))
* **Rust** ([*rustfmt*](https://github.com/rust-lang-nursery/rustfmt))
* **Scala** ([*scalafmt*](https://github.com/scalameta/scalafmt))
* **Shell script** ([*beautysh*](https://github.com/lovesegfault/beautysh), [*shfmt*](https://github.com/mvdan/sh))
* **Snakemake** ([*snakefmt*](https://github.com/snakemake/snakefmt))
* **Solidity** ([*prettier plugin*](https://github.com/prettier-solidity/prettier-plugin-solidity))
* **SQL** ([*pgformatter*](https://github.com/darold/pgFormatter), [*sqlformat*](https://pypi.org/project/sqlparse/))
* **Svelte** ([*prettier plugin*](https://github.com/sveltejs/prettier-plugin-svelte))
* **Swift** ([*swiftformat*](https://github.com/nicklockwood/SwiftFormat))
* **Terraform** ([*terraform fmt*](https://www.terraform.io/docs/commands/fmt.html))
* **TOML** ([*prettier plugin*](https://github.com/bd82/toml-tools/tree/master/packages/prettier-plugin-toml), [*taplo fmt*](https://taplo.tamasfe.dev/cli/usage/formatting.html))
* **TypeScript/TSX** ([*prettier*](https://prettier.io/), [*ts-standard*](https://standardjs.com/), [*prettierd*](https://github.com/fsouza/prettierd), [*deno*](https://deno.land/manual/tools/formatter))
* **V** ([*v fmt*](https://github.com/vlang/v))
* **Vue** ([*prettier*](https://prettier.io/), [*prettierd*](https://github.com/fsouza/prettierd))
* **Verilog** ([*iStyle*](https://github.com/thomasrussellmurphy/istyle-verilog-formatter), [*Verible*](https://github.com/chipsalliance/verible/tree/master/verilog/tools/formatter))
* **YAML** ([*prettier*](https://prettier.io/), [*prettierd*](https://github.com/fsouza/prettierd))
* **Zig** ([*zig*](https://ziglang.org/))

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

`M-x customize-group format-all` has a few basic settings.

However, the main thing you probably want to set is
`format-all-formatters`. That variable is buffer-local, and can be
made project-local by setting it in a `.dir-locals.el` file in a
project's directory. That file can be committed to version control to
share it with the whole project.

To enable format on save for most programming language buffers:
`(add-hook 'prog-mode-hook 'format-all-mode)`.

To control displaying the formatting errors buffer when
formatting fails or has warnings, customize the variable
`format-all-show-errors`. Set it to one of these - `'always` (shows
errors buffer regardless),`'warnings` (shows errors buffer for
both errors and warnings), `'errors` (only show errors buffer
when there are errors) or `'never` (never show errors buffer).

The command `format-all-ensure-formatter` will ensure that a default
formatter is selected in case you don't have one set; you can
customize the default formatter for each language. To ensure a
formatter is set whenever you enable `format-all-mode`, you can use:
`(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)`.

Additionally, many of the external formatters support configuration
files in the source code directory to control their formatting. Please
see the documentation for each formatter.

How to add new languages
------------------------

New external formatters can be added easily if they can read code from
standard input and format it to standard output. Feel free to submit a
pull request or ask for help in [GitHub issues][github-issues].

How to report bugs
------------------

[GitHub issues][github-issues] are preferred. Email is also ok.

Feature requests are welcome. If you are interested in doing anything
beyond adding new formatters in the current framework, please discuss
in issues before writing code.

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
