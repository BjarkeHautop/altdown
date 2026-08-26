

# altdown

altdown is a toy R package built to demonstrate
[altdoc](https://altdoc.etiennebacher.com/) using Quarto, but mirroring
the output style of pkgdown (Bootstrap 5).

Browse around to see how it looks!

## Installation

``` r
# install.packages("pak")
pak::pak("bjarkehautop/altdown")
```

## Example

``` r
library(altdown)

greet("world")
```

    [1] "Hello, world!"

``` r
add(1, 2)
```

    [1] 3

## Why?

pkgdown support Quarto (see
[`vignette("quarto")`](https://pkgdown.r-lib.org/dev/articles/quarto.html)),
but the way it’s implemented is hard to maintain and lags behind in
features, as they say themselves. What I actually wanted was [callout
blocks](https://quarto.org/docs/authoring/callouts.html), since I’ve
grown fond of them. pkgdown hardcodes `theme: "none"` and
`minimal: TRUE` so it can supply its own CSS/Bootstrap instead of
Quarto’s. Without a real theme, Quarto’s callout filter just drops
`::: {.callout-note}` blocks down to a
[Blockquote](https://quarto.org/docs/authoring/markdown-basics.html#other-blocks).

The same underlying issue means TOC for `.qmd` files disappear. The only
fix would be patching pkgdown itself, but it is more work than it
sounds. A properly themed Quarto page is a full standalone document, and
hence you’d end up merging two competing asset systems. Hence I use
altdoc, and manually construct my layout for something that looks like
pkgdown.

If you are used to the pkgdown layout, but want some of these features
this is for you! Simply just copy the contents of `/altdoc` to your
package. Then you build docs using

``` r
source('altdoc/build-site.R')
build_site(verbose = FALSE)
```

and view them using `altdoc::preview_docs()`.
