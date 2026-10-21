# A built site: an empty page for each of `pages` in docs/.
local_docs <- function(dir, pages) {
  for (page in pages) {
    file <- file.path(dir, "docs", page)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines("<html></html>", file)
  }
}

redirect_target <- function(dir, from) {
  html <- readLines(file.path(dir, "docs", from))
  sub(".*url=([^\"]+)\".*", "\\1", grep("http-equiv", html, value = TRUE))
}

test_that("`redirects:` writes a page sending visitors to the new one", {
  dir <- new_pkg()
  local_docs(dir, c("reference/set_scale.html", "index.html"))
  config <- yaml::yaml.load(
    '
redirects:
  - ["reference/scale.html", "reference/set_scale.html"]
  - ["old.html", "https://example.com/new.html"]
  - ["/deep/old/page.html", "/index.html"]
'
  )

  write_redirects(dir, config)
  expect_identical(
    redirect_target(dir, "reference/scale.html"),
    "../reference/set_scale.html"
  )
  expect_identical(
    redirect_target(dir, "old.html"),
    "https://example.com/new.html"
  )
  expect_identical(
    redirect_target(dir, "deep/old/page.html"),
    "../../index.html"
  )
})

test_that("`redirects:` doesn't overwrite pages and warns about missing targets", {
  dir <- new_pkg()
  local_docs(dir, c("index.html", "reference/a.html"))

  config <- list(redirects = list(c("index.html", "reference/a.html")))
  expect_warning(write_redirects(dir, config), "is a page of the site")
  expect_identical(
    readLines(file.path(dir, "docs", "index.html")),
    "<html></html>"
  )

  config <- list(redirects = list(c("old.html", "reference/nope.html#x")))
  expect_warning(write_redirects(dir, config), "reference/nope.html")
})

test_that("`redirects:` must be pairs of paths", {
  expect_error(
    config_redirects(list(redirects = list("a.html"))),
    "pairs of paths"
  )
  expect_identical(config_redirects(list()), character())
})

test_that("redirect pages keep the address's anchor", {
  html <- redirect_page("../a.html")
  expect_match(html, 'var target = "../a.html";', fixed = TRUE, all = FALSE)
  expect_match(html, "window.location.hash", fixed = TRUE, all = FALSE)
})
