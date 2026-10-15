# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## What this is

altdown is an R package that layers a pkgdown-style (Bootstrap 5) theme on top
of [altdoc](https://altdoc.etiennebacher.com/) Quarto websites. altdown renders
nothing itself: it generates source files into `altdoc/`, calls
`altdoc::render_docs()`, then post-processes the HTML in `docs/`. The exported
API is just `use_altdown()` (one-time setup, writes `altdoc/altdown.yml` and
gitignore entries) and `build_site()`.

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
altdown::build_site(parallel = FALSE, freeze = FALSE)
altdoc::preview_docs()
```

`README.md` is generated from `README.qmd` — edit the `.qmd` and re-render.

## Architecture

`build_site()` in [R/build_site.R](R/build_site.R) runs three stages:

1. **Before rendering** — writes generated files into `altdoc/` (listed in
   `generated_files`; all gitignored, rewritten each build; only
   `altdoc/altdown.yml` is hand-edited). altdoc copies `altdoc/` into its Quarto
   project (`_quarto/`) *after* importing README etc., which is how
   `altdoc/index.md` overrides altdoc's home page without touching `README.md`.
   - `quarto_website.yml`: fully resolved Quarto config (navbar, footer, theme).
     It contains no `$ALTDOC_*` variables, so altdoc has nothing to substitute.
     User overrides from `altdown.yml`'s `quarto:` key are applied by
     `merge_quarto_config()` (unnamed lists append, other values replace, `~`
     removes).
   - `index.md`: README + pkgdown-style sidebar ([R/sidebar.R](R/sidebar.R));
     badges are pulled out of the `<!-- badges: start/end -->` block, and
     `<picture>` light/dark images are rewritten to
     `.light-content`/`.dark-content` `<img>`s.
   - `reference.qmd` ([R/reference.R](R/reference.R)): pkgdown-compatible
     `reference:` grouping with selectors (`starts_with()`, `has_concept()`,
     `-`, …) evaluated against Rd topics.
   - `authors.qmd` ([R/authors.R](R/authors.R)).
   - Theme files copied verbatim from [inst/quarto/](inst/quarto/)
     (`altdown.scss`, header/after-body HTML).
   - `altdown-pre-render.R`: Quarto pre-render hook that fixes altdoc's
     `man/*.qmd` (Usage highlighting, inline code, hiding `library(pkg)` in
     examples, etc.).
2. **Rendering** — `altdoc::render_docs()` converts Rd → qmd and renders into
   `docs/`; altdown's own source files are then deleted from `docs/`.
3. **After rendering** — `add_tooltips()` ([R/tooltips.R](R/tooltips.R))
   regex-rewrites every `docs/**/*.html`, linking references to the package's
   own topics (`<span class="fu|va">` in code blocks and `<code>name()</code>`
   inline) to their man page with a hover tooltip. Pages are marked with
   `<!-- altdown-tooltips -->` so reruns skip them.

### Pre-render script is generated from package code

The pre-render script runs in Quarto's standalone R process where altdown may
not be installed, so `write_pre_render_script()` ([R/usage.R](R/usage.R))
**deparses** the functions/objects named in `pre_render_objects` into the
script. Any function or constant used by `fix_man_qmd()`/`pre_render_main()`
must be base-R only (no altdown internals, no `cli`/`fs` etc.) and must be added
to `pre_render_objects`.

### Navbar / articles

[R/articles.R](R/articles.R): "Get started" is the vignette named after the
package, or the only top-level vignette. Remaining vignettes (top-level, then
`vignettes/articles/`) go under "Articles", ordered by `altdown.yml`'s
`articles:` if given.

## Repo layout notes

- `altdoc/` and `_quarto/` at the repo root are this package's *own* site build
  (dogfooding); `_quarto/` and `docs/` are build outputs.
- Tests create throwaway packages in temp dirs (see `new_pkg()` helper in
  [tests/testthat/test-build-site.R](tests/testthat/test-build-site.R)) and test
  the generation functions directly rather than running a full Quarto render.
- [vignettes/articles/technical.qmd](vignettes/articles/technical.qmd) documents
  the build internals in more depth; keep it in sync when changing build
  behavior.
- Roxygen with markdown; `@section Configuration:` in `build_site()` is the
  user-facing reference for `altdown.yml` — update it when adding config keys.
