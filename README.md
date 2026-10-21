

<!-- badges: start -->

[![](https://github.com/bjarkehautop/quartodown/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bjarkehautop/quartodown/actions/workflows/R-CMD-check.yaml)
[![](https://codecov.io/gh/bjarkehautop/quartodown/graph/badge.svg)](https://app.codecov.io/gh/bjarkehautop/quartodown)
<!-- badges: end -->

# quartodown <picture><source media="(prefers-color-scheme: dark)" srcset="man/figures/logo.png" /><img src="man/figures/logo-light.png" align="right" height="139" alt="quartodown logo" /></picture>

quartodown is an R package that turns your package’s README, help pages
and vignettes into a website, rendered entirely by Quarto.

## Why quartodown?

Every page, help pages included, is a Quarto page, so you get:

- every Quarto feature, from callouts and tabsets to code annotations
  and cross-references, on every page;
- Quarto’s themes, extensions and settings;
- the package website layout R users know from pkgdown: a home page
  sidebar, a grouped function reference, articles and a changelog;
- function links with hover tooltips.

## Installation

Install from GitHub:

``` r
pak::pak("https://github.com/BjarkeHautop/quartodown")
```

## Usage

Browse around to see how it looks! [Quarto
features](https://bjarkehautop.github.io/quartodown/vignettes/articles/showcase.html)
shows callouts, tabsets, footnotes and more on a quartodown site. If you
like what you see, and want to use it for your own package, run:

``` r
quartodown::use_quartodown()
```

This writes `_quartodown.yml`, with a `reference:` field that lists your
package’s current functions in one section; edit it to group them
however you’d like. It also gitignores the files the build generates.

Then build the site:

``` r
quartodown::build_site()
```

A preview can then be viewed using `quartodown::preview_site()`.

See `?quartodown::build_site` for everything `_quartodown.yml` can
configure.

## Packages using quartodown

- [jlbind](https://bjarkehautop.github.io/jlbind/)

Using quartodown for your package? Feel free to open a pull request
adding it to this list.
