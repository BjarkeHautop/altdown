# altdoc/build-site.R isn't part of the package - it's a script sourced by
# `Rscript altdoc/build-site.R` / use_altdown()'s copy in inst/altdoc-template/.
# Source it into its own environment so its top-level `build_site()` guard
# (`identical(environment(), globalenv())`) stays false and nothing actually
# builds.
.build_site_pkg_root <- testthat::test_path("..", "..")
.build_site_r <- file.path(.build_site_pkg_root, "altdoc", "build-site.R")

build_site_env <- NULL
if (file.exists(.build_site_r)) {
  build_site_env <- new.env(parent = globalenv())
  sys.source(.build_site_r, envir = build_site_env)
}

new_pkg <- function(articles = character()) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines(
    c(
      "Package: examplepkg",
      "Authors@R: person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\"))"
    ),
    file.path(dir, "DESCRIPTION")
  )
  dir.create(file.path(dir, "vignettes"))
  if (length(articles) > 0) {
    dir.create(file.path(dir, "vignettes", "articles"))
    for (f in articles) {
      writeLines(
        c("---", sprintf("title: \"%s\"", f), "---", "content"),
        file.path(dir, "vignettes", "articles", paste0(f, ".qmd"))
      )
    }
  }
  dir
}

test_that("vignette_title() reads the YAML title, falling back to the filename", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env

  dir <- withr::local_tempdir()
  titled <- file.path(dir, "titled.qmd")
  writeLines(c("---", "title: \"Getting Started\"", "---", "content"), titled)
  expect_identical(fns$vignette_title(titled), "Getting Started")

  untitled <- file.path(dir, "untitled.qmd")
  writeLines(c("---", "format: html", "---", "content"), untitled)
  expect_identical(fns$vignette_title(untitled), "untitled")

  no_frontmatter <- file.path(dir, "plain.qmd")
  writeLines("just content", no_frontmatter)
  expect_identical(fns$vignette_title(no_frontmatter), "plain")
})

test_that("find_getting_started_vignette() picks the single top-level vignette", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env
  dir <- new_pkg()
  writeLines("content", file.path(dir, "vignettes", "getting-started.qmd"))

  expect_identical(
    basename(fns$find_getting_started_vignette(dir)),
    "getting-started.qmd"
  )
})

test_that("find_getting_started_vignette() disambiguates by package name", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env
  dir <- new_pkg()
  writeLines("content", file.path(dir, "vignettes", "examplepkg.qmd"))
  writeLines("content", file.path(dir, "vignettes", "other.qmd"))

  expect_identical(
    basename(fns$find_getting_started_vignette(dir)),
    "examplepkg.qmd"
  )
})

test_that("find_getting_started_vignette() errors on ambiguous or missing vignettes", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env

  dir <- new_pkg()
  writeLines("content", file.path(dir, "vignettes", "a.qmd"))
  writeLines("content", file.path(dir, "vignettes", "b.qmd"))
  expect_error(fns$find_getting_started_vignette(dir), "multiple")

  dir_empty <- new_pkg()
  expect_error(fns$find_getting_started_vignette(dir_empty), "No top-level")

  dir_no_vignettes <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir_no_vignettes, "DESCRIPTION"))
  expect_error(fns$find_getting_started_vignette(dir_no_vignettes), "vignettes/")
})

test_that("find_articles() lists vignettes/articles/*.qmd sorted, empty when absent", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env

  dir <- new_pkg(articles = c("b-article", "a-article"))
  articles <- fns$find_articles(dir)
  expect_identical(basename(articles), c("a-article.qmd", "b-article.qmd"))

  dir_no_articles <- new_pkg()
  expect_identical(fns$find_articles(dir_no_articles), character())
})

test_that("build_articles_nav() is empty with no articles and lists titles otherwise", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env

  dir_no_articles <- new_pkg()
  expect_identical(fns$build_articles_nav(dir_no_articles), character())

  dir <- new_pkg(articles = c("technical"))
  nav <- fns$build_articles_nav(dir, indent = "  ")
  expect_identical(
    nav,
    c(
      "  - text: Articles",
      "    menu:",
      "      - text: technical",
      "        file: vignettes/articles/technical.qmd"
    )
  )
})

test_that("replace_placeholder_line() substitutes, removes, or no-ops", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env

  expect_identical(
    fns$replace_placeholder_line(c("a", "$PLACEHOLDER", "b"), "$PLACEHOLDER", c("x", "y")),
    c("a", "x", "y", "b")
  )
  expect_identical(
    fns$replace_placeholder_line(c("a", "$PLACEHOLDER", "b"), "$PLACEHOLDER", character()),
    c("a", "b")
  )
  expect_identical(
    fns$replace_placeholder_line(c("a", "b"), "$PLACEHOLDER", "x"),
    c("a", "b")
  )
})

test_that("data_sidebar_authors() links to authors.html only when a non-default-role author exists", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env

  dir <- new_pkg()
  bullets <- fns$data_sidebar_authors(dir)
  expect_false(any(grepl("More about authors", bullets, fixed = TRUE)))

  dir_ctb <- withr::local_tempdir()
  writeLines(
    c(
      "Package: examplepkg",
      "Authors@R: c(",
      "    person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\")),",
      "    person(\"John\", \"Smith\", role = \"ctb\"))"
    ),
    file.path(dir_ctb, "DESCRIPTION")
  )
  bullets_ctb <- fns$data_sidebar_authors(dir_ctb)
  expect_true(any(grepl("More about authors...\\]\\(authors.html\\)", bullets_ctb)))
  expect_false(any(grepl("John Smith", bullets_ctb)))
})

test_that("data_sidebar_citation() links to the authors.html #citation anchor", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env
  dir <- new_pkg()

  bullets <- fns$data_sidebar_citation(dir)
  expect_true(any(grepl("authors.html#citation", bullets, fixed = TRUE)))
})

test_that("build_authors_qmd() writes an Authors and Citation page with all authors and a fenced BibTeX code block", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env

  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "altdoc"))
  writeLines(
    c(
      "Package: examplepkg",
      "Version: 1.0.0",
      "Title: Example Package",
      "Authors@R: c(",
      "    person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\")),",
      "    person(\"John\", \"Smith\", role = \"ctb\"))"
    ),
    file.path(dir, "DESCRIPTION")
  )

  fns$build_authors_qmd(dir)

  out_path <- file.path(dir, "altdoc", "authors.qmd")
  expect_true(file.exists(out_path))
  lines <- readLines(out_path)

  expect_identical(lines[1:3], c("---", 'title: "Authors and Citation"', "---"))
  expect_true(any(grepl("Jane Doe", lines, fixed = TRUE)))
  expect_true(any(grepl("John Smith", lines, fixed = TRUE)))
  expect_true(any(grepl("^## Citation \\{#citation\\}$", lines)))
  expect_true(any(grepl("Source: <a href=\"DESCRIPTION\"><code>DESCRIPTION</code></a>", lines, fixed = TRUE)))
  expect_true(any(lines == "```bibtex"))
  open_idx <- which(lines == "```bibtex")
  close_idx <- which(lines == "```")
  bibtex_block <- lines[seq(open_idx + 1, close_idx[close_idx > open_idx][1] - 1)]
  expect_true(any(grepl("@Manual\\{", bibtex_block)))
})

test_that("build_authors_qmd() prefers an inst/CITATION file and links the Source note to it", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env

  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "altdoc"))
  writeLines(
    c(
      "Package: examplepkg",
      "Version: 1.0.0",
      "Title: Example Package",
      "URL: https://github.com/someuser/examplepkg",
      "Authors@R: person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\"))"
    ),
    file.path(dir, "DESCRIPTION")
  )
  dir.create(file.path(dir, "inst"), recursive = TRUE)
  writeLines(
    'bibentry("Manual", title = "Custom Citation Title", author = "Someone Else")',
    file.path(dir, "inst", "CITATION")
  )

  fns$build_authors_qmd(dir)

  lines <- readLines(file.path(dir, "altdoc", "authors.qmd"))
  expect_true(any(grepl("Custom Citation Title", lines, fixed = TRUE)))
  expect_true(any(grepl(
    "Source: <a href=\"https://github.com/someuser/examplepkg/blob/HEAD/inst/CITATION\"><code>inst/CITATION</code></a>",
    lines,
    fixed = TRUE
  )))
})

test_that("update_quarto_settings() fills in the getting-started and articles placeholders", {
  skip_if(is.null(build_site_env), "altdoc/build-site.R not present (not a dev checkout)")
  fns <- build_site_env
  pkg_root <- testthat::test_path("..", "..")
  static_template <- file.path(pkg_root, "inst", "altdoc-template", "quarto_website_static.yml")
  skip_if_not(file.exists(static_template), "inst/altdoc-template/quarto_website_static.yml missing")

  dir <- new_pkg(articles = "technical")
  writeLines("content", file.path(dir, "vignettes", "getting-started.qmd"))
  dir.create(file.path(dir, "altdoc"))
  file.copy(static_template, file.path(dir, "altdoc", "quarto_website_static.yml"))

  fns$update_quarto_settings(dir)

  out <- readLines(file.path(dir, "altdoc", "quarto_website.yml"))
  expect_true(any(grepl("file: vignettes/getting-started.qmd", out, fixed = TRUE)))
  expect_true(any(grepl("text: technical", out, fixed = TRUE)))
  expect_false(any(grepl("$ALTDOC_ARTICLES_NAV", out, fixed = TRUE)))
  expect_false(any(grepl("$ALTDOC_GETTING_STARTED", out, fixed = TRUE)))
})
