test_that("reset_project() empties _quarto/, keeping _freeze/ only when freezing", {
  dir <- new_pkg()
  project <- file.path(dir, "_quarto")
  dir.create(file.path(project, "_freeze"))
  dir.create(file.path(project, ".quarto"))
  writeLines("x", file.path(project, "stale.qmd"))

  reset_project(dir, freeze = TRUE)
  expect_identical(
    list.files(project, all.files = TRUE, no.. = TRUE),
    "_freeze"
  )

  reset_project(dir)
  expect_identical(
    list.files(project, all.files = TRUE, no.. = TRUE),
    character()
  )
})

test_that("import_markdown_files() imports NEWS, the license and community files like pkgdown", {
  dir <- new_pkg()
  writeLines("# examplepkg 1.0.0", file.path(dir, "NEWS.md"))
  writeLines("# MIT License", file.path(dir, "LICENSE.md"))
  writeLines("YEAR: 2026", file.path(dir, "LICENSE"))
  dir.create(file.path(dir, ".github"))
  writeLines("Be nice.", file.path(dir, ".github", "CODE_OF_CONDUCT.md"))

  import_markdown_files(dir)

  project <- file.path(dir, "_quarto")
  expect_setequal(
    list.files(project),
    c("NEWS.md", "LICENSE.md", "LICENSE-text.md", "CODE_OF_CONDUCT.md")
  )
  expect_identical(
    readLines(file.path(project, "CODE_OF_CONDUCT.md")),
    "Be nice."
  )
  expect_identical(
    readLines(file.path(project, "LICENSE-text.md")),
    c("---", "title: License", "---", "", "```", "YEAR: 2026", "```")
  )
})

test_that("link_news() links issues and users, but not in code or existing links", {
  lines <- c(
    "* Fixed a bug (#12, @jane-doe).",
    "* See [#3](https://example.com) and https://x.org/#4.",
    "* Mail me at me@example.com.",
    "```",
    "#5 @code",
    "```"
  )
  expect_identical(
    link_news(lines, "https://github.com/o/r"),
    c(
      "* Fixed a bug ([#12](https://github.com/o/r/issues/12), [@jane-doe](https://github.com/jane-doe)).",
      "* See [#3](https://example.com) and https://x.org/#4.",
      "* Mail me at me@example.com.",
      "```",
      "#5 @code",
      "```"
    )
  )
  expect_identical(link_news(lines, NA_character_), lines)
})

test_that("copy_figures() copies man/figures/ and README_files/", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  dir.create(file.path(dir, "man", "figures"))
  dir.create(file.path(dir, "README_files", "figure-gfm"), recursive = TRUE)
  writeLines("x", file.path(dir, "man", "figures", "logo.svg"))
  writeLines("x", file.path(dir, "README_files", "figure-gfm", "plot.png"))

  copy_figures(dir)
  expect_setequal(
    list.files(file.path(dir, "_quarto"), recursive = TRUE),
    c("man/figures/logo.svg", "README_files/figure-gfm/plot.png")
  )
})

test_that("write_pkgdown_yml() points downlit at the site's reference pages and vignettes", {
  dir <- new_pkg(articles = "technical")
  write_vignette(dir, "examplepkg.qmd")
  dir.create(file.path(dir, "docs"))
  write_pkgdown_yml(dir)
  expect_false(file.exists(file.path(dir, "docs", "pkgdown.yml")))

  desc::desc_set_urls(
    c("https://github.com/o/examplepkg", "https://o.github.io/examplepkg/"),
    file = dir
  )
  write_pkgdown_yml(dir)
  meta <- yaml::read_yaml(file.path(dir, "docs", "pkgdown.yml"))
  expect_identical(
    meta$urls,
    list(
      reference = "https://o.github.io/examplepkg/reference",
      article = "https://o.github.io/examplepkg/vignettes"
    )
  )
  expect_identical(
    meta$articles,
    list(examplepkg = "examplepkg.html", technical = "articles/technical.html")
  )
})

test_that("preview_site() errors when the site hasn't been built", {
  expect_error(preview_site(new_pkg()), "build_site")
})
