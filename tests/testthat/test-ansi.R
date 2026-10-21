test_that("colored output survives link_code() and becomes styled spans", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "docs"))
  esc <- "\u001b"
  writeLines(
    c(
      "<html><body>",
      "<div class=\"sourceCode\"><pre class=\"sourceCode r\"><code class=\"sourceCode r\">",
      "<span><span class=\"fu\">f</span>()</span>",
      paste0(
        "<span><span class=\"co\">#&gt; ",
        esc,
        "[31mred &amp; ",
        esc,
        "[1mbold",
        esc,
        "[22m",
        esc,
        "[39m</span></span>"
      ),
      "</code></pre></div>",
      paste0("<pre><code>#&gt; ", esc, "[32mgreen", esc, "[39m</code></pre>"),
      "</body></html>"
    ),
    file.path(dir, "docs", "page.html")
  )
  writeLines(
    '[{"text": "f()\\n#> \\u001b[31mred\\u001b[39m"}]',
    file.path(dir, "docs", "search.json")
  )

  protect_ansi(dir)
  link_code(dir)
  ansi_to_html(dir)

  html <- readLines(file.path(dir, "docs", "page.html"), encoding = "UTF-8")
  expect_false(any(grepl(esc, html, fixed = TRUE)))
  expect_false(any(grepl(ansi_placeholder, html, fixed = TRUE)))
  expect_true(any(grepl(
    "<span class='fansi-color-001'>red &amp; </span><span class='fansi-color-001' style='font-weight: bold;'>bold</span>",
    html,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "<pre><code>#&gt; <span class='fansi-color-002'>green</span></code></pre>",
    html,
    fixed = TRUE
  )))
  expect_identical(
    readLines(file.path(dir, "docs", "search.json")),
    '[{"text": "f()\\n#> red"}]'
  )
})

test_that("ansi_scss_rules() picks the basic colors by the theme's background", {
  rules <- ansi_scss_rules()
  start <- grep("^@if", rules)
  else_at <- grep("^\\} @else", rules)
  expect_identical(rules[start], "@if lightness($body-bg) > 50% {")
  expect_true(".fansi-color-208 { color: #ff6600 }" %in% rules[seq_len(start)])
  expect_true(
    "  .fansi-color-001 { color: #aa0000 }" %in% rules[start:else_at]
  )
  expect_true(
    "  .fansi-bgcol-004 { background-color: #0000aa }" %in% rules[start:else_at]
  )
  expect_true(
    "  .fansi-color-001 { color: #cd3131 }" %in% rules[-seq_len(else_at)]
  )
})
