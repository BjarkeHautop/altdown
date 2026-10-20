annotated_page <- function(code) {
  file <- withr::local_tempfile(
    fileext = ".html",
    .local_envir = parent.frame()
  )
  writeLines(
    paste0(
      "<html><body><div class=\"sourceCode\">",
      "<pre class=\"sourceCode r code-annotation-code code-annotated\">",
      "<code class=\"sourceCode r\">",
      "<span id=\"cell-1\" class=\"code-annotation-target\">",
      code,
      "</span></code></pre></div></body></html>"
    ),
    file
  )
  file
}

test_that("annotated blocks keep their markup and get calls linked", {
  file <- annotated_page(paste0(
    "x <span class=\"ot\">&lt;-</span> stats<span class=\"sc\">::</span>",
    "<span class=\"fu\">median</span>(<span class=\"dv\">1</span>) ",
    "<span class=\"fu\">sd</span>(x) ",
    "<span class=\"fu\">not_a_function_anywhere</span>()"
  ))
  link_code_file(file)
  html <- paste(readLines(file), collapse = "\n")

  expect_match(html, "code-annotation-target", fixed = TRUE)
  expect_match(html, "<span class=\"ot\">", fixed = TRUE)
  expect_match(
    html,
    "<span class=\"fu\"><a href=\"https://rdrr.io/r/stats/median.html\">median</a></span>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<span class=\"fu\"><a href=\"https://rdrr.io/r/stats/sd.html\">sd</a></span>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<span class=\"fu\">not_a_function_anywhere</span>",
    fixed = TRUE
  )
})

test_that("library() calls in an annotated block attach packages", {
  skip_if_not_installed("pkgload")
  file <- annotated_page(paste0(
    "<span class=\"fu\">library</span>(pkgload)\n",
    "<span class=\"fu\">load_all</span>()"
  ))
  link_code_file(file)
  html <- paste(readLines(file), collapse = "\n")

  expect_match(html, "load_all.html\">load_all</a>", fixed = TRUE)
})

test_that("call_prefix() reads the package before ::", {
  html <- xml2::read_html(paste0(
    "<code>a <span class=\"sc\">+</span><span class=\"fu\">f</span>",
    " stats<span class=\"sc\">::</span><span class=\"fu\">g</span>",
    "<span class=\"fu\">h</span></code>"
  ))
  calls <- xml2::xml_find_all(html, ".//span[@class = 'fu']")
  expect_equal(call_prefix(calls[[1]]), "")
  expect_equal(call_prefix(calls[[2]]), "stats::")
  expect_equal(call_prefix(calls[[3]]), "")
})
