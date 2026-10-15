new_pkg <- function(articles = character(), .local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  writeLines(
    c(
      "Package: examplepkg",
      "Version: 1.0.0",
      "Authors@R: person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\"))"
    ),
    file.path(dir, "DESCRIPTION")
  )
  dir.create(file.path(dir, "vignettes"))
  dir.create(file.path(dir, "altdoc"))
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

write_vignette <- function(dir, name, title = name) {
  writeLines(
    c("---", sprintf("title: \"%s\"", title), "---", "content"),
    file.path(dir, "vignettes", name)
  )
}

nav_files <- function(left) {
  unlist(lapply(left, function(item) {
    c(item$file, vapply(item$menu, function(m) m$file, character(1)))
  }))
}

test_that("vignette_title() reads the YAML title, falling back to the filename", {
  dir <- withr::local_tempdir()
  titled <- file.path(dir, "titled.qmd")
  writeLines(c("---", "title: \"Getting Started\"", "---", "content"), titled)
  expect_identical(vignette_title(titled), "Getting Started")

  untitled <- file.path(dir, "untitled.qmd")
  writeLines(c("---", "format: html", "---", "content"), untitled)
  expect_identical(vignette_title(untitled), "untitled")

  no_frontmatter <- file.path(dir, "plain.qmd")
  writeLines("just content", no_frontmatter)
  expect_identical(vignette_title(no_frontmatter), "plain")
})

test_that("find_vignettes() lists top-level vignettes, then vignettes/articles/", {
  dir <- new_pkg(articles = c("b-article", "a-article"))
  write_vignette(dir, "z.qmd")
  write_vignette(dir, "examplepkg.Rmd")

  expect_identical(
    find_vignettes(dir),
    c("examplepkg.Rmd", "z.qmd", "articles/a-article.qmd", "articles/b-article.qmd")
  )

  dir_none <- withr::local_tempdir()
  expect_identical(find_vignettes(dir_none), character())
})

test_that("find_getting_started_vignette() picks the single top-level vignette", {
  dir <- new_pkg(articles = "technical")
  write_vignette(dir, "getting-started.qmd")

  expect_identical(find_getting_started_vignette(dir), "getting-started.qmd")
})

test_that("find_getting_started_vignette() disambiguates by package name", {
  dir <- new_pkg()
  write_vignette(dir, "examplepkg.qmd")
  write_vignette(dir, "other.qmd")

  expect_identical(find_getting_started_vignette(dir), "examplepkg.qmd")
})

test_that("find_getting_started_vignette() is NULL when ambiguous or missing", {
  dir <- new_pkg()
  write_vignette(dir, "a.qmd")
  write_vignette(dir, "b.qmd")
  expect_message(expect_null(find_getting_started_vignette(dir)), "several")

  expect_null(find_getting_started_vignette(new_pkg(articles = "technical")))
})

test_that("select_articles() keeps the default order, or follows `articles:`", {
  vignettes <- c("other.qmd", "articles/a.qmd", "articles/b.qmd")
  expect_identical(select_articles(vignettes), vignettes)
  expect_identical(
    select_articles(vignettes, list("articles/b", "other", "articles/a")),
    c("articles/b.qmd", "other.qmd", "articles/a.qmd")
  )
  expect_warning(
    expect_identical(select_articles(vignettes, list("other")), "other.qmd"),
    "articles/a"
  )
  expect_error(select_articles(vignettes, list("nope")), "nope")
})

test_that("navbar_left() links Get started, Reference, Articles and Changelog", {
  dir <- new_pkg(articles = "technical")
  write_vignette(dir, "examplepkg.qmd", title = "Get started with examplepkg")
  write_vignette(dir, "usage.qmd", title = "Using examplepkg")
  writeLines("# examplepkg 1.0.0", file.path(dir, "NEWS.md"))

  left <- navbar_left(dir)
  expect_identical(
    vapply(left, function(x) x$text, character(1)),
    c("Get started", "Reference", "Articles", "Changelog")
  )
  expect_identical(
    nav_files(left),
    c(
      "vignettes/examplepkg.qmd",
      "reference.qmd",
      "vignettes/usage.qmd",
      "vignettes/articles/technical.qmd",
      "NEWS.md"
    )
  )
  expect_identical(left[[3]]$menu[[1]]$text, "Using examplepkg")
})

test_that("navbar_left() leaves out Articles and Changelog when there are none", {
  dir <- new_pkg()
  write_vignette(dir, "examplepkg.qmd")

  left <- navbar_left(dir)
  expect_identical(
    vapply(left, function(x) x$text, character(1)),
    c("Get started", "Reference")
  )
})

test_that("news_file() finds NEWS and CHANGELOG in the places altdoc looks", {
  dir <- new_pkg()
  expect_null(news_file(dir))

  dir.create(file.path(dir, "inst"))
  writeLines("news", file.path(dir, "inst", "NEWS.Rd"))
  expect_identical(news_file(dir), "NEWS.md")

  dir2 <- new_pkg()
  writeLines("changes", file.path(dir2, "CHANGELOG.md"))
  expect_identical(news_file(dir2), "CHANGELOG.md")
})

test_that("write_quarto_config() writes a complete config with no altdoc variables left", {
  dir <- new_pkg()
  write_vignette(dir, "examplepkg.qmd")

  write_quarto_config(dir)
  out_path <- file.path(dir, "altdoc", "quarto_website.yml")
  expect_false(any(grepl("$ALTDOC", readLines(out_path), fixed = TRUE)))

  settings <- yaml::read_yaml(out_path)
  expect_identical(settings$website$title, "examplepkg 1.0.0")
  expect_identical(settings$project$`output-dir`, "../docs/")
  expect_true(settings$format$html$`code-link`)
  expect_null(settings$website$navbar$right)
  expect_match(settings$website$`page-footer`$left, "Jane Doe")
})

test_that("quarto_config() links GitHub and merges `quarto:` overrides", {
  dir <- new_pkg()
  write("URL: https://github.com/someuser/examplepkg", file.path(dir, "DESCRIPTION"), append = TRUE)

  config <- list(quarto = list(
    format = list(html = list(theme = list(light = c("default", "altdown.scss", "custom.scss")))),
    website = list(`site-url` = "https://example.org")
  ))
  settings <- quarto_config(dir, config)

  expect_identical(settings$website$navbar$right[[1]]$href, "https://github.com/someuser/examplepkg")
  expect_identical(settings$website$`site-url`, "https://example.org")
  expect_identical(settings$format$html$theme$light, c("default", "altdown.scss", "custom.scss"))
  expect_identical(settings$format$html$theme$dark, c("darkly", "altdown.scss"))
  expect_true(settings$format$html$toc)
})

test_that("build_website_readme() moves badges into the sidebar and keeps the body", {
  dir <- new_pkg()
  readme <- c(
    "<!-- badges: start -->",
    "[![badge](b.svg)](https://example.org)",
    "<!-- badges: end -->",
    "",
    "# examplepkg",
    "",
    "Some text."
  )

  home <- build_website_readme(readme, dir)
  expect_identical(home[1], "# examplepkg")
  expect_identical(home[3], "::: {.column-margin}")
  expect_identical(home[length(home)], "Some text.")
  expect_false(any(grepl("badges:", home, fixed = TRUE)))
  dev_status <- which(home == "#### Dev status")
  expect_identical(home[dev_status + 2], "[![badge](b.svg)](https://example.org)")
})

test_that("theme_pictures_to_images() turns a light/dark <picture> into themed <img>s", {
  readme <- c(
    "# pkg <picture><source media=\"(prefers-color-scheme: dark)\" srcset=\"dark.png\" /><img src=\"light.png\" align=\"right\" alt=\"logo\" /></picture>",
    ""
  )
  expect_identical(
    theme_pictures_to_images(readme),
    c(
      paste0(
        "# pkg ",
        "<img src=\"light.png\" align=\"right\" alt=\"logo\" class=\"light-content\" />",
        "<img src=\"dark.png\" align=\"right\" alt=\"logo\" class=\"dark-content\" />"
      ),
      ""
    )
  )
})

test_that("theme_pictures_to_images() handles multi-line pictures, both sources and existing classes", {
  readme <- c(
    "<picture>",
    "  <source media=\"(prefers-color-scheme: light)\" srcset=\"l.png 1x, l@2x.png 2x\">",
    "  <source media='(prefers-color-scheme: dark)' srcset='d.png'>",
    "  <img src=\"fallback.png\" class=\"logo\">",
    "</picture>",
    "Text."
  )
  expect_identical(
    theme_pictures_to_images(readme),
    c(
      "<img src=\"l.png\" class=\"logo light-content\"><img src=\"d.png\" class=\"logo dark-content\">",
      "Text."
    )
  )
})

test_that("theme_pictures_to_images() leaves other pictures and plain READMEs alone", {
  other <- "<picture><source media=\"(min-width: 600px)\" srcset=\"w.png\"><img src=\"n.png\"></picture>"
  expect_identical(theme_pictures_to_images(other), other)
  plain <- c("# pkg", "", "Text.", "")
  expect_identical(theme_pictures_to_images(plain), plain)
})

test_that("write_home() writes altdoc/index.md and leaves README.md alone", {
  dir <- new_pkg()
  readme_path <- file.path(dir, "README.md")
  writeBin(charToRaw("# examplepkg\n\nSome text.\n"), readme_path)
  before <- readBin(readme_path, "raw", file.size(readme_path))

  write_home(dir)

  expect_identical(readBin(readme_path, "raw", file.size(readme_path)), before)
  home <- readLines(file.path(dir, "altdoc", "index.md"))
  expect_identical(home[1], "# examplepkg")
  expect_true("Some text." %in% home)
})

test_that("read_config() errors without altdown.yml and accepts an empty one", {
  dir <- new_pkg()
  expect_error(read_config(dir), "use_altdown")

  file.create(file.path(dir, "altdoc", "altdown.yml"))
  expect_identical(read_config(dir), list())
})

test_that("warn_legacy_files() names files left from the copied-template setup", {
  dir <- new_pkg()
  expect_no_warning(warn_legacy_files(dir))

  writeLines("x", file.path(dir, "altdoc", "build-site.R"))
  expect_warning(warn_legacy_files(dir), "build-site.R")
})

test_that("copy_theme_files() copies the SCSS and scripts into altdoc/", {
  dir <- new_pkg()
  copy_theme_files(dir)
  expect_true(all(file.exists(file.path(dir, "altdoc", theme_files))))
})

test_that("data_sidebar_authors() links to authors.html only when a non-default-role author exists", {
  dir <- new_pkg()
  bullets <- data_sidebar_authors(dir)
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
  bullets_ctb <- data_sidebar_authors(dir_ctb)
  expect_true(any(grepl("More about authors...\\]\\(authors.html\\)", bullets_ctb)))
  expect_false(any(grepl("John Smith", bullets_ctb)))
})

test_that("data_sidebar_citation() links to the authors.html #citation anchor", {
  dir <- new_pkg()
  bullets <- data_sidebar_citation(dir)
  expect_true(any(grepl("authors.html#citation", bullets, fixed = TRUE)))
})

test_that("build_authors_qmd() writes an Authors and Citation page with all authors and a fenced BibTeX code block", {
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

  build_authors_qmd(dir)

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

  build_authors_qmd(dir)

  lines <- readLines(file.path(dir, "altdoc", "authors.qmd"))
  expect_true(any(grepl("Custom Citation Title", lines, fixed = TRUE)))
  expect_true(any(grepl(
    "Source: <a href=\"https://github.com/someuser/examplepkg/blob/HEAD/inst/CITATION\"><code>inst/CITATION</code></a>",
    lines,
    fixed = TRUE
  )))
})

write_rd <- function(dir, name, alias = name, title = name, concept = NULL, keyword = NULL) {
  lines <- c(
    sprintf("\\name{%s}", name),
    sprintf("\\alias{%s}", alias),
    sprintf("\\title{%s}", title),
    if (!is.null(concept)) sprintf("\\concept{%s}", concept),
    if (!is.null(keyword)) sprintf("\\keyword{%s}", keyword),
    "\\description{d}"
  )
  writeLines(lines, file.path(dir, "man", paste0(name, ".Rd")))
}

new_pkg_with_man <- function(.local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  writeLines(
    c(
      "Package: examplepkg",
      "Authors@R: person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\"))"
    ),
    file.path(dir, "DESCRIPTION")
  )
  dir.create(file.path(dir, "man"))
  dir.create(file.path(dir, "altdoc"))
  dir
}

reference_config <- function(text) {
  yaml::yaml.load(paste(text, collapse = "\n"))
}

test_that("rd_topics() reads alias/title/concept/keyword tags from every man/*.Rd", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo_bar", title = "Foo bar", concept = "strings", keyword = "internal")

  topics <- rd_topics(dir)
  expect_length(topics, 1)
  expect_identical(topics[[1]]$alias, "foo_bar")
  expect_identical(topics[[1]]$title, "Foo bar")
  expect_identical(topics[[1]]$concepts, "strings")
  expect_identical(topics[[1]]$keywords, "internal")
})

test_that("build_reference_qmd() supports starts_with()/-exclude/has_concept()/has_keyword() selectors", {
  dir <- new_pkg_with_man()
  write_rd(dir, "str_foo", concept = "strings")
  write_rd(dir, "str_bar", concept = "strings")
  write_rd(dir, "num_add", keyword = "internal")

  config <- reference_config(c(
    "reference:",
    "  - title: Strings",
    "    contents:",
    '      - starts_with("str_")',
    '      - -has_keyword("internal")',
    "  - title: Internal",
    "    contents:",
    '      - has_keyword("internal")'
  ))

  build_reference_qmd(dir, config)
  lines <- readLines(file.path(dir, "altdoc", "reference.qmd"))

  expect_true(any(grepl("str_foo()", lines, fixed = TRUE)))
  expect_true(any(grepl("str_bar()", lines, fixed = TRUE)))
  expect_true(any(grepl("num_add()", lines, fixed = TRUE)))

  strings_start <- which(grepl("^## Strings$", lines))
  internal_start <- which(grepl("^## Internal$", lines))
  strings_block <- lines[strings_start:(internal_start - 1)]
  expect_false(any(grepl("num_add()", strings_block, fixed = TRUE)))
})

test_that("build_reference_qmd() lists every topic in one section without `reference:`", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  write_rd(dir, "bar")

  expect_no_warning(build_reference_qmd(dir))
  lines <- readLines(file.path(dir, "altdoc", "reference.qmd"))
  expect_true("## All functions" %in% lines)
  expect_true(any(grepl("foo()", lines, fixed = TRUE)))
  expect_true(any(grepl("bar()", lines, fixed = TRUE)))
})

test_that("build_reference_qmd() warns about topics missing from `reference:`", {
  dir <- new_pkg_with_man()
  write_rd(dir, "listed_fun")
  write_rd(dir, "orphan_fun")

  config <- reference_config(c("reference:", "  - title: All", "    contents:", "      - listed_fun"))
  expect_warning(build_reference_qmd(dir, config), "orphan_fun")
})

test_that("build_reference_qmd() accepts a non-syntactic alias like a *-package alias verbatim", {
  dir <- new_pkg_with_man()
  write_rd(dir, "examplepkg-package", alias = "examplepkg-package", title = "examplepkg")

  config <- reference_config(c("reference:", "  - title: All", "    contents:", "      - examplepkg-package"))
  build_reference_qmd(dir, config)
  lines <- readLines(file.path(dir, "altdoc", "reference.qmd"))
  expect_true(any(grepl("examplepkg-package()", lines, fixed = TRUE)))
})

test_that("build_reference_qmd() errors on a selector matching no topic", {
  dir <- new_pkg_with_man()
  write_rd(dir, "listed_fun")

  config <- reference_config(c("reference:", "  - title: All", "    contents:", "      - not_a_real_function"))
  expect_error(build_reference_qmd(dir, config), "known")
})

test_that("convert_inline_code() turns raw <code> into Markdown code spans", {
  expect_identical(
    convert_inline_code("stored in <code>tools::R_user_dir(&quot;pkg&quot;, &quot;data&quot;)</code>."),
    "stored in `tools::R_user_dir(\"pkg\", \"data\")`."
  )
  expect_identical(
    convert_inline_code("<code>a</code> and <code>x &lt;- `y`</code>"),
    "`a` and `` x <- `y` ``"
  )
  # A <code> with a link inside stays HTML.
  linked <- "<code><a href=\"foo.html\">foo()</a></code>"
  expect_identical(convert_inline_code(linked), linked)
})

test_that("convert_inline_code() leaves fenced code blocks alone", {
  lines <- c("<code>a</code>", "```r", "x <- \"<code>b</code>\"", "```", "<code>c</code>")
  expect_identical(
    convert_inline_code(lines),
    c("`a`", "```r", "x <- \"<code>b</code>\"", "```", "`c`")
  )
})

test_that("unevaluated_chunks_to_code() turns only eval=FALSE chunks into plain code blocks", {
  lines <- c(
    "```{r, warning=FALSE, message=FALSE, eval=FALSE}",
    "f()",
    "```",
    "```{r, warning=FALSE, message=FALSE}",
    "g()",
    "```"
  )
  expect_identical(
    unevaluated_chunks_to_code(lines),
    c("```r", "f()", "```", "```{r, warning=FALSE, message=FALSE}", "g()", "```")
  )
})

test_that("write_pre_render_script() writes a standalone script that fixes up man/*.qmd", {
  dir <- new_pkg()
  write_pre_render_script(dir)
  script <- normalizePath(file.path(dir, "altdoc", "altdown-pre-render.R"))

  project <- withr::local_tempdir()
  dir.create(file.path(project, "man"))
  qmd <- file.path(project, "man", "foo.qmd")
  writeLines(
    c(
      "Uses <code>f(&quot;a&quot;)</code>.",
      "<pre><code class='language-R'>foo(x = &quot;a&quot;)</code></pre>",
      "```{r, eval=FALSE}",
      "foo()",
      "```"
    ),
    qmd
  )

  withr::with_dir(project, {
    status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(script))
  })
  expect_identical(status, 0L)
  expect_identical(
    readLines(qmd),
    c("Uses `f(\"a\")`.", "```r", "foo(x = \"a\")", "```", "```r", "foo()", "```")
  )
})

test_that("convert_inline_code() handles <code> with attributes but leaves <pre><code> alone", {
  expect_identical(
    convert_inline_code("a `/` (or <code style=\"white-space: pre;\">\\</code> on Windows), or"),
    "a `/` (or `\\` on Windows), or"
  )
  block <- "<pre><code class='language-R'>x</code></pre>"
  expect_identical(convert_inline_code(block), block)
})

test_that("trim_code_blocks() drops leading and trailing blank lines inside code blocks only", {
  lines <- c("", "```r", "", "f()", "", "g()", "", "", "```", "", "```{r}", "", "h()", "```")
  expect_identical(
    trim_code_blocks(lines),
    c("", "```r", "f()", "", "g()", "```", "", "```{r}", "h()", "```")
  )
})

test_that("collapse_example_output() makes run examples show output pkgdown-style", {
  lines <- c("```{r, warning=FALSE, message=FALSE, eval=TRUE}", "f()", "```", "```{r}", "```r", "```{r, collapse=FALSE}")
  expect_identical(
    collapse_example_output(lines),
    c(
      "```{r, warning=FALSE, message=FALSE, eval=TRUE, collapse=TRUE, comment=\"#>\"}",
      "f()",
      "```",
      "```{r, collapse=TRUE, comment=\"#>\"}",
      "```r",
      "```{r, collapse=FALSE}"
    )
  )
})

test_that("hide_library_calls() drops the package's own library() from plain blocks and hides it before run chunks", {
  lines <- c(
    "```r", "library(\"my.pkg\")", "", "f()", "```",
    "```{r, eval=TRUE}", "library(my.pkg)", "", "g()", "```",
    "```{r}", "library(\"other\")", "h()", "```",
    "```r", "library(\"myXpkg\")", "```"
  )
  expect_identical(
    hide_library_calls(lines, "my.pkg"),
    c(
      "```r", "", "f()", "```",
      "```{r, include=FALSE}", "library(my.pkg)", "```",
      "```{r, eval=TRUE}", "", "g()", "```",
      "```{r}", "library(\"other\")", "h()", "```",
      "```r", "library(\"myXpkg\")", "```"
    )
  )
})
