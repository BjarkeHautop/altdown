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

test_that("import_markdown_files() imports NEWS, the license and community files", {
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
    link_news(
      lines,
      list(
        issue = "https://github.com/o/r/issues/",
        user = "https://github.com/"
      )
    ),
    c(
      "* Fixed a bug ([#12](https://github.com/o/r/issues/12), [@jane-doe](https://github.com/jane-doe)).",
      "* See [#3](https://example.com) and https://x.org/#4.",
      "* Mail me at me@example.com.",
      "```",
      "#5 @code",
      "```"
    )
  )
  expect_identical(
    link_news(lines, list(issue = NA_character_, user = NA_character_)),
    lines
  )
})

test_that("pkg_repo() reads the repository from DESCRIPTION's URL", {
  dir <- new_pkg()
  expect_true(all(is.na(unlist(pkg_repo(dir)))))

  desc::desc_set(
    URL = "https://example.com, https://github.com/o/r/",
    file = dir
  )
  expect_identical(
    pkg_repo(dir),
    list(
      home = "https://github.com/o/r",
      source = "https://github.com/o/r/blob/HEAD/",
      issue = "https://github.com/o/r/issues/",
      user = "https://github.com/"
    )
  )

  # A package in a folder of the repository, on another branch.
  desc::desc_set(URL = "https://github.com/o/r/tree/main/pkg", file = dir)
  repo <- pkg_repo(dir)
  expect_identical(repo$home, "https://github.com/o/r/tree/main/pkg")
  expect_identical(repo$source, "https://github.com/o/r/blob/main/pkg/")
  expect_identical(repo$issue, "https://github.com/o/r/issues/")

  desc::desc_set(URL = "https://gitlab.com/o/r", file = dir)
  repo <- pkg_repo(dir)
  expect_identical(repo$source, "https://gitlab.com/o/r/-/blob/HEAD/")
  expect_identical(repo$issue, "https://gitlab.com/o/r/-/issues/")
  expect_identical(repo$user, "https://gitlab.com/")

  desc::desc_set(
    URL = "https://gitlab.example.org/o/r/-/tree/main/pkg",
    file = dir
  )
  repo <- pkg_repo(dir)
  expect_identical(
    repo$source,
    "https://gitlab.example.org/o/r/-/blob/main/pkg/"
  )
  expect_identical(repo$issue, "https://gitlab.example.org/o/r/-/issues/")

  desc::desc_set(URL = "https://codeberg.org/o/r", file = dir)
  repo <- pkg_repo(dir)
  expect_identical(repo$source, "https://codeberg.org/o/r/src/branch/HEAD/")
  expect_identical(repo$issue, "https://codeberg.org/o/r/issues/")
  expect_identical(repo$user, "https://codeberg.org/")

  desc::desc_set(
    URL = "https://codeberg.org/o/r/src/branch/main/pkg",
    file = dir
  )
  repo <- pkg_repo(dir)
  expect_identical(repo$source, "https://codeberg.org/o/r/src/branch/main/pkg/")
  expect_identical(repo$issue, "https://codeberg.org/o/r/issues/")
})

test_that("repo_host() recognizes GitHub, GitLab and Codeberg", {
  expect_identical(
    repo_host(c(
      "https://github.com/o/r",
      "https://www.github.com/o/r",
      "https://gitlab.com/o/r",
      "https://gitlab.example.org/o/r",
      "https://codeberg.org/o/r",
      "https://o.github.io/r/",
      "https://example.com",
      NA
    )),
    c("github", "github", "gitlab", "gitlab", "codeberg", NA, NA, NA)
  )
})

test_that("`repo:` overrides the repository's URLs and branch", {
  dir <- new_pkg()
  desc::desc_set(URL = "https://github.com/o/r", file = dir)
  repo <- pkg_repo(dir, list(repo = list(branch = "devel")))
  expect_identical(repo$source, "https://github.com/o/r/blob/devel/")

  config <- yaml::yaml.load(
    "
repo:
  url:
    home: https://code.example.com/r/
    source: https://code.example.com/r/src/
    issue: https://bugs.example.com/
"
  )
  repo <- pkg_repo(dir, config)
  expect_identical(repo$home, "https://code.example.com/r")
  expect_identical(repo$source, "https://code.example.com/r/src/")
  expect_identical(repo$issue, "https://bugs.example.com/")
  expect_identical(repo$user, "https://github.com/")
})

test_that("github_markdown_to_quarto() turns alerts into callouts and mermaid blocks into diagrams", {
  lines <- c(
    "Intro.",
    "> [!Warning]",
    "> Be careful.",
    ">",
    "> Really.",
    "After.",
    "> A plain quote.",
    "```mermaid",
    "graph TD; A-->B;",
    "```",
    "````md",
    "> [!NOTE]",
    "```mermaid",
    "````"
  )
  converted <- github_markdown_to_quarto(lines, mermaid = TRUE)
  expect_identical(
    converted,
    c(
      "Intro.",
      "",
      "::: {.callout-warning}",
      "Be careful.",
      "",
      "Really.",
      ":::",
      "",
      "After.",
      "> A plain quote.",
      "```{mermaid}",
      "graph TD; A-->B;",
      "```",
      "````md",
      "> [!NOTE]",
      "```mermaid",
      "````"
    )
  )
  # Only the mermaid block differs without `mermaid = TRUE`.
  expect_identical(
    github_markdown_to_quarto(lines),
    replace(converted, converted == "```{mermaid}", "```mermaid")
  )
})

test_that("copy_vignettes() puts the source file under each vignette's title", {
  dir <- new_pkg(articles = "technical")
  write_vignette(dir, "examplepkg.Rmd")
  writeLines("x", file.path(dir, "vignettes", "data.csv"))
  desc::desc_set(
    URL = "https://github.com/someuser/examplepkg",
    file = dir,
    normalize = FALSE
  )

  copy_vignettes(dir)
  out <- file.path(dir, "_quarto", "vignettes")
  expect_identical(
    readLines(file.path(out, "articles", "technical.qmd")),
    c(
      "---",
      "title: \"technical\"",
      "---",
      "",
      "<p class=\"source-note\"><small class=\"dont-index\">Source: <a href=\"https://github.com/someuser/examplepkg/blob/HEAD/vignettes/articles/technical.qmd\"><code>vignettes/articles/technical.qmd</code></a></small></p>",
      "",
      "content"
    )
  )
  expect_match(
    readLines(file.path(out, "examplepkg.Rmd"))[[5]],
    "Source: .*<code>vignettes/examplepkg.Rmd</code>"
  )
  expect_identical(readLines(file.path(out, "data.csv")), "x")
})

test_that("insert_after_front_matter() inserts after the front matter, or at the top", {
  expect_identical(
    insert_after_front_matter(c("", "---", "a: 1", "...", "text"), "x"),
    c("", "---", "a: 1", "...", "", "x", "text")
  )
  expect_identical(insert_after_front_matter("text", "x"), c("x", "text"))
  expect_identical(
    insert_after_front_matter(c("---", "unclosed"), "x"),
    c("x", "---", "unclosed")
  )
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

test_that("write_pkgdown_yml() lists no articles without vignettes", {
  dir <- new_pkg()
  dir.create(file.path(dir, "docs"))
  desc::desc_set_urls("https://o.github.io/examplepkg/", file = dir)
  write_pkgdown_yml(dir)
  meta <- yaml::read_yaml(file.path(dir, "docs", "pkgdown.yml"))
  expect_length(meta$articles, 0)
})

test_that("with_libpaths() puts the temporary library first while it runs", {
  lib <- withr::local_tempdir()
  old <- .libPaths()
  envvars <- c(R_LIBS = paste(c(lib, old), collapse = .Platform$path.sep))
  expect_identical(
    with_libpaths(envvars, .libPaths()[1]),
    normalizePath(lib, "/")
  )
  expect_identical(.libPaths(), old)
  # devel = TRUE has no library.
  expect_identical(with_libpaths(c(R_PROFILE_USER = "p"), .libPaths()), old)
})

test_that("preview_site() errors when the site hasn't been built", {
  expect_error(preview_site(new_pkg()), "build_site")
})

test_that("install_envvars() installs the package into a library put first on R_LIBS", {
  dir <- new_pkg()
  desc::desc_set(
    Title = "Example",
    Description = "An example.",
    License = "MIT",
    `Authors@R` = 'person("Jane", "Doe", email = "jane@example.com", role = c("aut", "cre"))',
    file = dir
  )
  dir.create(file.path(dir, "R"))
  writeLines("hello <- function() 'hi'", file.path(dir, "R", "hello.R"))
  writeLines("export(hello)", file.path(dir, "NAMESPACE"))
  scratch <- withr::local_tempdir()

  expect_message(envvars <- install_envvars(dir, scratch), "temporary library")
  lib <- strsplit(envvars[["R_LIBS"]], .Platform$path.sep, fixed = TRUE)[[1]][1]
  expect_true(file.exists(file.path(lib, "examplepkg", "DESCRIPTION")))
})

test_that("install_envvars() errors when the package doesn't install", {
  dir <- new_pkg()
  dir.create(file.path(dir, "R"))
  writeLines("this is not R code(", file.path(dir, "R", "broken.R"))
  expect_error(
    suppressMessages(install_envvars(dir, withr::local_tempdir())),
    "devel = TRUE"
  )
})

test_that("load_all_envvars() writes a profile that loads the package from source", {
  dir <- new_pkg()
  envvars <- load_all_envvars(dir, withr::local_tempdir())
  profile <- readLines(envvars[["R_PROFILE_USER"]])
  expect_match(profile, "pkgload::load_all(", fixed = TRUE, all = FALSE)
  expect_match(profile, deparse(normalizePath(dir)), fixed = TRUE, all = FALSE)
})

test_that("local_envvars() restores set and unset variables on exit", {
  withr::local_envvar(QUARTODOWN_A = "old", QUARTODOWN_B = NA)
  f <- function() {
    local_envvars(c(QUARTODOWN_A = "new", QUARTODOWN_B = "set"))
    Sys.getenv(c("QUARTODOWN_A", "QUARTODOWN_B"))
  }
  expect_identical(unname(f()), c("new", "set"))
  expect_identical(Sys.getenv("QUARTODOWN_A"), "old")
  expect_identical(Sys.getenv("QUARTODOWN_B", unset = NA), NA_character_)
})
