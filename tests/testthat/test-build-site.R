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
    c(
      "examplepkg.Rmd",
      "z.qmd",
      "articles/a-article.qmd",
      "articles/b-article.qmd"
    )
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
      "reference/index.qmd",
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

test_that("write_quarto_config() writes a complete _quarto/_quarto.yml", {
  dir <- new_pkg()
  write_vignette(dir, "examplepkg.qmd")

  write_quarto_config(dir)
  out_path <- file.path(dir, "_quarto", "_quarto.yml")
  settings <- yaml::read_yaml(out_path)
  expect_identical(settings$website$title, "examplepkg 1.0.0")
  expect_identical(settings$project$`output-dir`, "../docs/")
  expect_true(settings$format$html$`code-link`)
  expect_null(settings$website$navbar$right)
  expect_match(settings$website$`page-footer`$left, "Jane Doe")
})

test_that("quarto_config() turns on Quarto's freeze when asked", {
  dir <- new_pkg()
  expect_null(quarto_config(dir)$execute)
  expect_identical(quarto_config(dir, freeze = TRUE)$execute$freeze, "auto")
})

test_that("quarto_config() links GitHub and merges `quarto:` overrides", {
  dir <- new_pkg()
  write(
    "URL: https://github.com/someuser/examplepkg",
    file.path(dir, "DESCRIPTION"),
    append = TRUE
  )

  config <- list(
    quarto = list(
      format = list(
        html = list(
          theme = list(light = c("default", "quartodown.scss", "custom.scss"))
        )
      ),
      website = list(`site-url` = "https://example.org")
    )
  )
  settings <- quarto_config(dir, config)

  expect_identical(
    settings$website$navbar$right[[1]]$href,
    "https://github.com/someuser/examplepkg"
  )
  expect_identical(settings$website$`site-url`, "https://example.org")
  expect_identical(
    settings$format$html$theme$light,
    c("default", "quartodown.scss", "custom.scss")
  )
  expect_identical(
    settings$format$html$theme$dark,
    c("darkly", "quartodown.scss")
  )
  expect_true(settings$format$html$toc)
})

test_that("`quarto:` adds navbar entries and `~` removes a setting", {
  dir <- new_pkg()
  write(
    "URL: https://github.com/someuser/examplepkg",
    file.path(dir, "DESCRIPTION"),
    append = TRUE
  )

  config <- yaml::yaml.load(
    "
quarto:
  website:
    navbar:
      right:
        - text: Blog
          href: https://example.com
  format:
    html:
      toc: ~
"
  )
  settings <- quarto_config(dir, config)

  right <- settings$website$navbar$right
  expect_length(right, 2)
  expect_identical(right[[1]]$icon, "github")
  expect_identical(
    right[[2]],
    list(text = "Blog", href = "https://example.com")
  )
  expect_false("toc" %in% names(settings$format$html))
  expect_true(settings$format$html$`code-link`)
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
  expect_identical(
    home[1:5],
    c("::::: {.quartodown-home}", "# examplepkg", "", "Some text.", ":::::")
  )
  expect_identical(home[7], "::: {.column-margin}")
  expect_identical(home[length(home)], ":::")
  expect_false(any(grepl("badges:", home, fixed = TRUE)))
  dev_status <- which(home == "#### Dev status")
  expect_identical(
    home[dev_status + 2],
    "[![badge](b.svg)](https://example.org)"
  )
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

test_that("write_home() writes _quarto/index.md and leaves README.md alone", {
  dir <- new_pkg()
  readme_path <- file.path(dir, "README.md")
  writeBin(charToRaw("# examplepkg\n\nSome text.\n"), readme_path)
  before <- readBin(readme_path, "raw", file.size(readme_path))

  write_home(dir)

  expect_identical(readBin(readme_path, "raw", file.size(readme_path)), before)
  home <- readLines(file.path(dir, "_quarto", "index.md"))
  expect_true("# examplepkg" %in% home)
  expect_true("Some text." %in% home)
})

test_that("read_config() treats a missing or empty _quartodown.yml as no settings", {
  dir <- new_pkg()
  expect_identical(read_config(dir), list())

  file.create(file.path(dir, "_quartodown.yml"))
  expect_identical(read_config(dir), list())

  writeLines("articles: [a]", file.path(dir, "_quartodown.yml"))
  expect_identical(read_config(dir), list(articles = "a"))
})

test_that("copy_theme_files() copies the SCSS and scripts into _quarto/", {
  dir <- new_pkg()
  copy_theme_files(dir)
  expect_true(all(file.exists(file.path(dir, "_quarto", theme_files))))
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
  expect_true(any(grepl(
    "More about authors...\\]\\(authors.html\\)",
    bullets_ctb
  )))
  expect_false(any(grepl("John Smith", bullets_ctb)))
})

test_that("data_sidebar_citation() links to the authors.html #citation anchor", {
  dir <- new_pkg()
  bullets <- data_sidebar_citation(dir)
  expect_true(any(grepl("authors.html#citation", bullets, fixed = TRUE)))
})

test_that("data_sidebar_community() links the community files, from the root or .github/", {
  dir <- new_pkg()
  expect_identical(data_sidebar_community(dir), character())

  dir.create(file.path(dir, ".github"))
  writeLines("x", file.path(dir, ".github", "CODE_OF_CONDUCT.md"))
  writeLines("x", file.path(dir, "CONTRIBUTING.md"))

  bullets <- data_sidebar_community(dir)
  expect_true(any(grepl(
    "[Code of conduct](CODE_OF_CONDUCT.html)",
    bullets,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "[Contributing guide](CONTRIBUTING.html)",
    bullets,
    fixed = TRUE
  )))
  expect_false(any(grepl("SUPPORT", bullets)))
})

test_that("data_sidebar_license() links LICENSE.md and the LICENSE file like pkgdown", {
  dir <- new_pkg()
  desc::desc_set(License = "MIT + file LICENSE", file = dir)
  writeLines("YEAR: 2026", file.path(dir, "LICENSE"))
  bullets <- data_sidebar_license(dir)
  expect_true(any(grepl("[LICENSE](LICENSE-text.html)", bullets, fixed = TRUE)))
  expect_false(any(grepl("Full license", bullets)))

  writeLines("# MIT License", file.path(dir, "LICENSE.md"))
  bullets <- data_sidebar_license(dir)
  expect_true(any(grepl("[Full license](LICENSE.html)", bullets, fixed = TRUE)))
})

test_that("build_authors_qmd() writes an Authors and Citation page with all authors and a fenced BibTeX code block", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "_quarto"))
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

  out_path <- file.path(dir, "_quarto", "authors.qmd")
  expect_true(file.exists(out_path))
  lines <- readLines(out_path)

  expect_identical(lines[1:3], c("---", 'title: "Authors and Citation"', "---"))
  expect_true(any(grepl("Jane Doe", lines, fixed = TRUE)))
  expect_true(any(grepl("John Smith", lines, fixed = TRUE)))
  expect_true(any(grepl("^## Citation \\{#citation\\}$", lines)))
  expect_true(any(grepl(
    "Source: <a href=\"DESCRIPTION\"><code>DESCRIPTION</code></a>",
    lines,
    fixed = TRUE
  )))
  expect_true(any(lines == "```bibtex"))
  open_idx <- which(lines == "```bibtex")
  close_idx <- which(lines == "```")
  bibtex_block <- lines[seq(
    open_idx + 1,
    close_idx[close_idx > open_idx][1] - 1
  )]
  expect_true(any(grepl("@Manual\\{", bibtex_block)))
})

test_that("build_authors_qmd() prefers an inst/CITATION file and links the Source note to it", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "_quarto"))
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

  lines <- readLines(file.path(dir, "_quarto", "authors.qmd"))
  expect_true(any(grepl("Custom Citation Title", lines, fixed = TRUE)))
  expect_true(any(grepl(
    "Source: <a href=\"https://github.com/someuser/examplepkg/blob/HEAD/inst/CITATION\"><code>inst/CITATION</code></a>",
    lines,
    fixed = TRUE
  )))
})

test_that("rd_topics() reads alias/title/concept/keyword tags from every man/*.Rd", {
  dir <- new_pkg_with_man()
  write_rd(
    dir,
    "foo_bar",
    title = "Foo bar",
    concept = "strings",
    keyword = "internal"
  )

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
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))

  expect_true(any(grepl("str_foo()", lines, fixed = TRUE)))
  expect_true(any(grepl("str_bar()", lines, fixed = TRUE)))
})

test_that("build_reference_qmd() leaves out internal topics, which get no page", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  write_rd(dir, "examplepkg-package", usage = NULL, keyword = "internal")

  expect_no_warning(build_reference_qmd(dir))
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  expect_true(any(grepl("foo()", lines, fixed = TRUE)))
  expect_false(any(grepl("examplepkg-package", lines, fixed = TRUE)))

  config <- reference_config(c(
    "reference:",
    "  - title: Main",
    "    contents:",
    "      - foo",
    "  - title: Internal",
    "    contents:",
    "      - examplepkg-package"
  ))
  expect_no_warning(build_reference_qmd(dir, config))
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  expect_false("## Internal" %in% lines)
  expect_false(any(grepl("examplepkg-package", lines, fixed = TRUE)))
})

test_that("build_reference_qmd() renders pkgdown's title-only and subtitle blocks", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")

  config <- reference_config(c(
    "reference:",
    "  - title: Verbs",
    "  - subtitle: Rows",
    "    desc: Row verbs.",
    "    contents:",
    "      - foo"
  ))

  build_reference_qmd(dir, config)
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  expect_true("## Verbs" %in% lines)
  expect_true("### Rows" %in% lines)
  expect_false("## " %in% lines)
  # The title-only block gets no empty <dl>.
  expect_length(grep('<dl class="ref-index">', lines, fixed = TRUE), 1)
})

test_that("build_reference_qmd() lists every topic in one section without `reference:`", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  write_rd(dir, "bar")

  expect_no_warning(build_reference_qmd(dir))
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  expect_true("## All functions" %in% lines)
  expect_true(any(grepl("foo()", lines, fixed = TRUE)))
  expect_true(any(grepl("bar()", lines, fixed = TRUE)))
})

test_that("build_reference_qmd() warns about topics missing from `reference:`", {
  dir <- new_pkg_with_man()
  write_rd(dir, "listed_fun")
  write_rd(dir, "orphan_fun")

  config <- reference_config(c(
    "reference:",
    "  - title: All",
    "    contents:",
    "      - listed_fun"
  ))
  expect_warning(build_reference_qmd(dir, config), "orphan_fun")
})

test_that("build_reference_qmd() accepts a non-syntactic alias like a *-package alias verbatim", {
  dir <- new_pkg_with_man()
  write_rd(
    dir,
    "examplepkg-package",
    alias = "examplepkg-package",
    title = "examplepkg",
    usage = NULL
  )

  config <- reference_config(c(
    "reference:",
    "  - title: All",
    "    contents:",
    "      - examplepkg-package"
  ))
  build_reference_qmd(dir, config)
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  expect_true(any(grepl(">examplepkg-package</a>", lines, fixed = TRUE)))
})

test_that("build_reference_qmd() lists every function in a topic's usage, like pkgdown", {
  dir <- new_pkg_with_man()
  write_rd(
    dir,
    "is_bound",
    alias = c("is_bound", "check_bound"),
    usage = c("is_bound(pkg)", "", "check_bound(pkg)")
  )

  build_reference_qmd(dir)
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  expect_true(any(grepl(
    '<dt><code><a href="is_bound.qmd">is_bound()</a></code> <code><a href="is_bound.qmd">check_bound()</a></code></dt>',
    lines,
    fixed = TRUE
  )))
})

test_that("usage_names() formats data, infix, replacement and method usages", {
  rd_usage <- function(usage) {
    rd <- tools::parse_Rd(textConnection(c(
      "\\name{x}",
      "\\usage{",
      usage,
      "}"
    )))
    rd[vapply(rd, attr, character(1), "Rd_tag") == "\\usage"]
  }

  expect_identical(usage_names(rd_usage("mtcars")), "mtcars")
  expect_identical(usage_names(rd_usage("x \\%in\\% table")), "`%in%`")
  expect_identical(usage_names(rd_usage("names(x) <- value")), "`names&lt;-`()")
  expect_identical(
    usage_names(rd_usage("\\method{print}{foo}(x, \\dots)")),
    "print(<i>&lt;foo&gt;</i>)"
  )
  # A method of a generic documented in the same topic is dropped.
  expect_identical(
    usage_names(rd_usage(c(
      "summary(object, ...)",
      "\\method{summary}{foo}(object, ...)"
    ))),
    "summary()"
  )
  expect_identical(usage_names(list()), character())
})

test_that("build_reference_qmd() errors on a selector matching no topic", {
  dir <- new_pkg_with_man()
  write_rd(dir, "listed_fun")

  config <- reference_config(c(
    "reference:",
    "  - title: All",
    "    contents:",
    "      - not_a_real_function"
  ))
  expect_error(build_reference_qmd(dir, config), "known")
})
