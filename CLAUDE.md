# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## What this is

quartodown is an R package that builds pkgdown-style (Bootstrap 5) package
websites with Quarto. It generates a Quarto website project in `_quarto/` from
the package sources, renders it into `docs/` with `quarto::quarto_render()`,
then post-processes the HTML. Configuration is the optional `_quartodown.yml` at
the package root. The exported API is `use_quartodown()` (one-time setup, writes
`_quartodown.yml` and gitignore entries), `build_site()` and `preview_site()`.

## Commands

```r
devtools::load_all() # load package for interactive use
devtools::document() # regenerate man/ and NAMESPACE from roxygen
devtools::test() # run all tests
testthat::test_file("tests/testthat/test-tooltips.R") # run one test file
devtools::test(filter = "build-site") # run tests matching a file name
devtools::check() # R CMD check
```

Build this package's own site (needs Quarto installed; CI uses these args):

```r
install.packages(".", repos = NULL, type = "source")
quartodown::build_site()
quartodown::preview_site()
```

`README.md` is generated from `README.qmd` — edit the `.qmd` and re-render.

## Architecture

`build_site()` in [R/build_site.R](R/build_site.R) runs three stages:

1. **Before rendering** — empties `_quarto/` (keeping `_freeze/` with
   `freeze = TRUE`) and writes the whole project into it
   ([R/project.R](R/project.R)): `vignettes/`, `man/figures/`, `NEWS.md` (with
   `#123`/`@user` links), `LICENSE.md`, `LICENSE` as `LICENSE-text.md`, and
   CODE_OF_CONDUCT/CONTRIBUTING/SUPPORT `.md` from the root or `.github/`
   (pkgdown's layout), plus:
   - `reference/<topic>.qmd` ([R/rd2qmd.R](R/rd2qmd.R)): each Rd parsed with the
     package's Rd macros ([R/rd-macros.R](R/rd-macros.R)), rendered with
     `tools::Rd2HTML()` running build/install/render `\Sexpr` stages, then cut
     into sections (YAML `title:`, `##` headings). Usage → fenced
     ```` ```r ````; prose stays HTML except inline `<code>` → Markdown code
     spans, `reqn` → `$`/`$$` math, `$` escaped, language-tagged `<pre>` →
     fenced blocks. Examples are rebuilt from `tools::Rd2ex()` markers into run
     / `\dontrun` (plain block) / `\dontshow` (hidden chunk) blocks, with
     `@examplesIf` evaluated at build time.
   - `_quarto.yml`: fully resolved Quarto config (navbar, footer, theme). User
     overrides from `_quartodown.yml`'s `quarto:` key are applied by
     `merge_quarto_config()` (unnamed lists append, other values replace, `~`
     removes).
   - `index.md`: README + pkgdown-style sidebar ([R/sidebar.R](R/sidebar.R));
     badges are pulled out of the `<!-- badges: start/end -->` block, and
     `<picture>` light/dark images are rewritten to
     `.light-content`/`.dark-content` `<img>`s.
   - `reference/index.qmd` ([R/reference.R](R/reference.R)): pkgdown-compatible
     `reference:` grouping with selectors (`starts_with()`, `has_concept()`,
     `-`, …) evaluated against Rd topics.
   - `authors.qmd` ([R/authors.R](R/authors.R)).
   - Theme files copied verbatim from [inst/quarto/](inst/quarto/)
     (`quartodown.scss`, header/after-body HTML).
2. **Rendering** — clears `docs/` (keeping `CNAME` and hidden files), makes the
   package available to the pages' R processes (installed into a temp library
   prepended via `R_LIBS`, or with `devel = TRUE` a generated `R_PROFILE_USER`
   profile that runs `pkgload::load_all()`), renders `_quarto/` into it
   (`output-dir: ../docs/`) and writes `docs/pkgdown.yml` (`urls` + `articles`,
   all downlit reads). The name is fixed: downlit fetches
   `<site URL>/pkgdown.yml` to link other packages' code to this site, so it
   must not be renamed.
3. **After rendering** — `add_tooltips()` ([R/tooltips.R](R/tooltips.R))
   regex-rewrites every `docs/**/*.html`, linking references to the package's
   own topics (`<span class="fu|va">` in code blocks and `<code>name()</code>`
   inline) to their reference page with a hover tooltip. Pages are marked with
   `<!-- quartodown-tooltips -->` so reruns skip them.

### Navbar / articles

[R/articles.R](R/articles.R): "Get started" is the vignette named after the
package, or the only top-level vignette. Remaining vignettes (top-level, then
`vignettes/articles/`) go under "Articles", ordered by `_quartodown.yml`'s
`articles:` if given.

## Repo layout notes

- `_quartodown.yml` at the repo root configures this package's *own* site
  (dogfooding); `_quarto/` and `docs/` are build outputs.
- Tests create throwaway packages in temp dirs (see `new_pkg()` helper in
  [tests/testthat/helper-pkg.R](tests/testthat/helper-pkg.R)) and test the
  generation functions directly rather than running a full Quarto render.
- [vignettes/articles/technical.qmd](vignettes/articles/technical.qmd) documents
  the build internals in more depth; keep it in sync when changing build
  behavior.
- Roxygen with markdown; `@section Configuration:` in `build_site()` is the
  user-facing reference for `quartodown.yml` — update it when adding config
  keys.
