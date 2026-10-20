write_rd <- function(dir, name, content) {
  dir.create(file.path(dir, "man"), showWarnings = FALSE)
  writeLines(content, file.path(dir, "man", paste0(name, ".Rd")))
}

sample_html <- function(body) {
  paste0("<html><head><title>t</title></head><body>", body, "</body></html>")
}

foo_index <- list(
  foo = list(usage = "foo(a, b)", brief = "Adds a and b.", topic = "foo")
)

# ---- build_topic_index() ------------------------------------------------------

test_that("build_topic_index() extracts usage and first-sentence brief", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "foo",
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Add two numbers}",
      "\\usage{",
      "foo(a, b)",
      "}",
      "\\description{",
      "Adds \\code{a} and \\code{b} together and returns the sum. See details.",
      "}"
    )
  )

  index <- build_topic_index(dir)

  expect_named(index, "foo")
  expect_equal(index$foo$usage, "foo(a, b)")
  expect_equal(
    index$foo$brief,
    "Adds `a` and `b` together and returns the sum."
  )
  expect_equal(index$foo$topic, "foo")
})

test_that("build_topic_index() narrows a multi-alias \\usage{} block by name", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "print.foo",
    c(
      "\\name{print.foo}",
      "\\alias{print.foo}",
      "\\alias{foo}",
      "\\title{Foo methods}",
      "\\usage{",
      "foo(x)",
      "",
      "\\method{print}{foo}(x, ...)",
      "}",
      "\\description{",
      "Prints a foo object.",
      "}"
    )
  )

  index <- build_topic_index(dir)

  expect_setequal(names(index), c("print.foo", "foo"))
  expect_equal(index[["print.foo"]]$brief, index[["foo"]]$brief)
  expect_equal(index[["foo"]]$usage, "foo(x)")
  expect_equal(index[["print.foo"]]$usage, "print.foo(x, ...)")
  expect_equal(index[["foo"]]$topic, "print.foo")
})

test_that("build_topic_index() matches a \\usage{} block wrapped over several lines", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "glue",
    c(
      "\\name{glue}",
      "\\alias{glue}",
      "\\alias{glue_data}",
      "\\title{Format a string}",
      "\\usage{",
      "glue_data(",
      "  .x,",
      "  ...",
      ")",
      "",
      "glue(...)",
      "}",
      "\\description{",
      "Formats a string.",
      "}"
    )
  )

  expect_no_warning(index <- build_topic_index(dir))
  expect_equal(index$glue_data$usage, "glue_data(\n  .x,\n  ...\n)")
  expect_equal(index$glue$usage, "glue(...)")
})

test_that("build_topic_index() skips internal topics, which altdoc doesn't render", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "helper",
    c(
      "\\name{helper}",
      "\\alias{helper}",
      "\\title{Helper}",
      "\\usage{",
      "helper()",
      "}",
      "\\description{",
      "Helps.",
      "}",
      "\\keyword{internal}"
    )
  )

  expect_length(build_topic_index(dir), 0)
})

test_that("an alias with no matching \\usage{} block falls back to the whole block and warns", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "print.foo",
    c(
      "\\name{print.foo}",
      "\\alias{print.foo}",
      "\\alias{foo}",
      "\\title{Foo methods}",
      "\\usage{",
      "\\method{print}{foo}(x, ...)",
      "}",
      "\\description{",
      "Prints a foo object.",
      "}"
    )
  )

  expect_warning(index <- build_topic_index(dir), "foo")
  expect_equal(index[["foo"]]$usage, "print.foo(x, ...)")
  expect_no_warning(build_topic_index(dir, quiet = TRUE))
})

test_that("a shared topic's own name doesn't need a \\usage{} block", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "foo-queries",
    c(
      "\\name{foo-queries}",
      "\\alias{foo-queries}",
      "\\alias{foo}",
      "\\alias{bar}",
      "\\title{Foo queries}",
      "\\usage{",
      "foo(x)",
      "",
      "bar(x)",
      "}",
      "\\description{",
      "Queries a foo.",
      "}"
    )
  )

  expect_no_warning(index <- build_topic_index(dir))
  expect_equal(index[["bar"]]$usage, "bar(x)")
  expect_equal(index[["foo-queries"]]$usage, "foo(x)\n\nbar(x)")
})

test_that("an infix operator alias matches its \\usage{} block", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "ops",
    c(
      "\\name{ops}",
      "\\alias{ops}",
      "\\alias{\\%+-\\%}",
      "\\alias{\\%-+\\%}",
      "\\title{Operators}",
      "\\usage{",
      "lhs \\%+-\\% rhs",
      "",
      "lhs \\%-+\\% rhs",
      "}",
      "\\description{",
      "Two operators.",
      "}"
    )
  )

  expect_no_warning(index <- build_topic_index(dir))
  expect_equal(index[["%+-%"]]$usage, "lhs %+-% rhs")
  expect_equal(index[["%-+%"]]$usage, "lhs %-+% rhs")
})

test_that("build_topic_index() errors without a man/ directory", {
  expect_error(build_topic_index(withr::local_tempdir()), "man/")
})

test_that("a description longer than 200 characters warns and clips with '...'", {
  dir <- withr::local_tempdir()
  # no sentence boundary at all
  write_rd(
    dir,
    "foo",
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Foo}",
      "\\usage{",
      "foo()",
      "}",
      "\\description{",
      paste(rep("word", 60), collapse = " "),
      "}"
    )
  )
  expect_warning(index <- build_topic_index(dir), "truncated mid-word")
  expect_true(endsWith(index$foo$brief, "..."))
  expect_no_warning(build_topic_index(dir, quiet = TRUE))

  # one well-formed sentence that simply runs past 200 characters
  write_rd(
    dir,
    "foo",
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Foo}",
      "\\usage{",
      "foo()",
      "}",
      "\\description{",
      paste0(paste(rep("word", 45), collapse = " "), "."),
      "}"
    )
  )
  expect_warning(index <- build_topic_index(dir), "truncated mid-word")
  expect_true(endsWith(index$foo$brief, "..."))
})

test_that("a topic with no description prose has a NULL brief", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "bar",
    c(
      "\\name{bar}",
      "\\alias{bar}",
      "\\title{Bar}",
      "\\usage{",
      "bar()",
      "}",
      "\\description{",
      "}"
    )
  )
  expect_null(build_topic_index(dir)$bar$brief)
})

# ---- inject_tooltips_html() ---------------------------------------------------

test_that("a downlit-linked code block reference is relinked to the local man page and gets a tooltip", {
  html <- sample_html(paste0(
    "<code class=\"sourceCode R\"><span class=\"fu\">",
    "<a href=\"https://rdrr.io/pkg/p/man/foo.html\">foo</a></span>",
    "<span class=\"op\">(</span></code>"
  ))

  result <- inject_tooltips_html(html, foo_index, "man/")

  expect_equal(result$n, 1L)
  expect_length(result$tips, 1L)
  expect_match(
    result$html,
    '<span class="fu"><a class="altdown-tip-ref" data-altdown-tip="t1" href="man/foo.html">foo</a></span>',
    fixed = TRUE
  )
  expect_equal(result$tips[["t1"]]$usage, "foo(a, b)")
})

test_that("an unlinked fu span is linked and tagged too", {
  html <- sample_html(
    "<span class=\"fu\">foo</span><span class=\"op\">(</span>"
  )
  result <- inject_tooltips_html(html, foo_index, "man/")
  expect_equal(result$n, 1L)
  expect_match(
    result$html,
    'data-altdown-tip="t1" href="man/foo.html">foo</a></span>',
    fixed = TRUE
  )
})

test_that("a va-class span (e.g. an R6 class name used as a value) is tagged too", {
  index <- list(
    Counter = list(usage = NULL, brief = "An R6 counter.", topic = "Counter")
  )
  html <- sample_html(paste0(
    "<span class=\"va\"><a href=\"https://rdrr.io/pkg/p/man/Counter.html\">Counter</a></span>",
    "<span class=\"op\">$</span><span class=\"fu\">new</span>"
  ))

  result <- inject_tooltips_html(html, index, "man/")

  expect_equal(result$n, 1L)
  expect_match(
    result$html,
    '<span class="va"><a class="altdown-tip-ref"',
    fixed = TRUE
  )
  expect_match(result$html, 'href="man/Counter.html"', fixed = TRUE)
  expect_equal(result$tips[["t1"]]$name, "Counter")
})

test_that("inline code references are linked and tagged, with or without a downlit link", {
  html <- sample_html(paste0(
    "<p>Add numbers with <code><a href=\"https://rdrr.io/pkg/p/man/foo.html\">foo()</a></code>:</p>"
  ))
  result <- inject_tooltips_html(html, foo_index, "man/")
  expect_equal(result$n, 1L)
  expect_match(
    result$html,
    '<code><a class="altdown-tip-ref" data-altdown-tip="t1" href="man/foo.html">foo()</a></code>',
    fixed = TRUE
  )

  # e.g. an S3 method written out by name, which downlit leaves unlinked
  index <- list(
    print.animal = list(
      usage = "print.animal(x, ...)",
      brief = NULL,
      topic = "animal"
    )
  )
  html <- sample_html("<p>See <code>print.animal()</code> for details.</p>")
  result <- inject_tooltips_html(html, index, "man/")
  expect_match(
    result$html,
    'href="man/animal.html">print.animal()</a></code>',
    fixed = TRUE
  )
})

test_that("an inline reference to a backtick-quoted name resolves through its backticks", {
  index <- list(
    `name<-` = list(
      usage = "name(x) <- value",
      brief = "Set the name.",
      topic = "animal"
    )
  )
  html <- sample_html("<p>See <code>`name&lt;-`()</code>.</p>")
  result <- inject_tooltips_html(html, index, "man/")
  expect_equal(result$n, 1L)
  expect_equal(result$tips[["t1"]]$usage, "name(x) <- value")
})

test_that("references qualified with the package's own namespace resolve, others don't", {
  html <- sample_html(
    "<p><code>p::foo()</code> and <code>p:::foo(1, 2)</code></p>"
  )
  result <- inject_tooltips_html(html, foo_index, "man/", pkg = "p")
  expect_equal(result$n, 2L)
  expect_length(result$tips, 1L)
  expect_match(
    result$html,
    'href="man/foo.html">p::foo()</a></code>',
    fixed = TRUE
  )

  html <- sample_html("<p><code>other::foo()</code></p>")
  result <- inject_tooltips_html(html, foo_index, "man/", pkg = "p")
  expect_equal(result$n, 0L)
  expect_equal(result$html, html)
})

test_that("references to names not in the index are left untouched", {
  html <- sample_html(paste0(
    "<span class=\"fu\"><a href=\"https://rdrr.io/r/base/library.html\">library</a></span>",
    "<span class=\"fu\">bar</span>"
  ))
  result <- inject_tooltips_html(html, foo_index, "man/")
  expect_equal(result$n, 0L)
  expect_equal(result$html, html)
})

test_that("repeated references to the same name share one tip id", {
  link <- "<span class=\"fu\"><a href=\"foo.html\">foo</a></span>"
  result <- inject_tooltips_html(
    sample_html(paste(link, link)),
    foo_index,
    "man/"
  )
  expect_equal(result$n, 2L)
  expect_length(result$tips, 1L)
  expect_length(
    gregexpr('data-altdown-tip="t1"', result$html, fixed = TRUE)[[1]],
    2L
  )
})

test_that("reference_href_prefix() points at docs/man/ relative to the page's depth", {
  expect_equal(reference_href_prefix("/x/docs/index.html", "/x/docs"), "man/")
  expect_equal(
    reference_href_prefix("/x/docs/vignettes/using.html", "/x/docs"),
    "../man/"
  )
  expect_equal(reference_href_prefix("/x/docs/man/foo.html", "/x/docs"), "./")
})

# ---- tooltip payload ----------------------------------------------------------

test_that("a topic with no \\usage{} shows its name as the signature", {
  tips <- list(
    t1 = list(usage = NULL, brief = "An R6 counter.", name = "Counter")
  )
  expect_match(
    tooltip_payload_html(tips),
    '<code class="altdown-tip-sig">Counter</code>',
    fixed = TRUE
  )
})

test_that("code in the brief is rendered as <code>, with its contents escaped", {
  tips <- list(
    t1 = list(usage = "f()", brief = "Stored in `dir(\"a\") < b`.", name = "f")
  )
  expect_match(
    tooltip_payload_html(tips),
    '<span class="altdown-tip-brief">Stored in <code>dir(&quot;a&quot;) &lt; b</code>.</span>',
    fixed = TRUE
  )
})

# ---- add_tooltips() -----------------------------------------------------------

test_that("add_tooltips() rewrites pages on disk and is idempotent on rerun", {
  dir <- withr::local_tempdir()
  writeLines("Package: p", file.path(dir, "DESCRIPTION"))
  write_rd(
    dir,
    "foo",
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Foo}",
      "\\usage{",
      "foo(a, b)",
      "}",
      "\\description{",
      "Adds a and b.",
      "}"
    )
  )
  dir.create(file.path(dir, "docs", "man"), recursive = TRUE)
  page <- file.path(dir, "docs", "index.html")
  writeLines(
    sample_html(
      "<span class=\"fu\"><a href=\"https://rdrr.io/pkg/p/man/foo.html\">foo</a></span>"
    ),
    page
  )

  res1 <- add_tooltips(dir, quiet = TRUE)
  expect_equal(res1, list(files = 1L, links = 1L))
  html <- paste(readLines(page), collapse = "\n")
  expect_match(html, 'href="man/foo.html"', fixed = TRUE)
  expect_match(html, paste0(tooltip_marker, "\n<style>"), fixed = TRUE)
  expect_match(html, "</script>\n</body></html>", fixed = TRUE)

  expect_equal(add_tooltips(dir, quiet = TRUE), list(files = 0L, links = 0L))
})

test_that("add_tooltips() errors when the site hasn't been built", {
  expect_error(add_tooltips(withr::local_tempdir()), "man/")
})
