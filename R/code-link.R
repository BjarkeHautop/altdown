# ---- code linking -------------------------------------------------------------
#
# Links function calls in R code blocks, and inline code like `mean()`, to
# their documentation with downlit, on every page in docs/. This replaces
# Quarto's own `code-link`, which runs downlit on knitr pages only and
# re-highlights every R code block, dropping the markers of annotated ones.
# Annotated blocks keep Quarto's highlighting; their calls and infix operators
# are linked token by token instead (link_annotated()).

# Links code on every docs/**/*.html file.
link_code <- function(path = ".") {
  html_files <- fs::dir_ls(
    file.path(path, "docs"),
    regexp = "\\.html$",
    recurse = TRUE
  )
  for (f in html_files) {
    link_code_file(f)
  }
  invisible(html_files)
}

link_code_file <- function(file) {
  # downlit collects the packages a page attaches with `library()` in this
  # option; reset it so they don't carry over to the next page.
  old <- options(downlit.attached = NULL)
  on.exit(options(old))

  html <- xml2::read_html(file, encoding = "UTF-8")
  # downlit rewrites every `pre.sourceCode.r`; hide annotated blocks from it.
  annotated <- xml2::xml_find_all(
    html,
    ".//pre[contains(@class, 'code-annotated')]"
  )
  classes <- xml2::xml_attr(annotated, "class")
  xml2::xml_set_attr(annotated, "class", "quartodown-annotated")
  downlit::downlit_html_node(html)
  for (i in seq_along(annotated)) {
    xml2::xml_set_attr(annotated[[i]], "class", classes[[i]])
    link_annotated(annotated[[i]])
  }
  xml2::write_html(html, file, format = FALSE)
}

# Links the calls and infix operators in an annotated block without
# re-highlighting it: each Pandoc `<span class="fu">` (with a preceding
# `pkg::` if there is one), and each `<span class="sc">` holding a `%op%`, is
# resolved with downlit and its text wrapped in a link, as downlit would.
link_annotated <- function(pre) {
  # Packages attached in the block itself count too.
  code <- xml2::xml_text(pre)
  attached <- regmatches(
    code,
    gregexpr(
      "\\b(?:library|require)\\(\\s*[\"']?\\K[A-Za-z][A-Za-z0-9.]*",
      code,
      perl = TRUE
    )
  )[[1]]
  options(downlit.attached = union(getOption("downlit.attached"), attached))

  calls <- xml2::xml_find_all(pre, ".//span[@class = 'fu'][not(a)]")
  for (call in calls) {
    name <- xml2::xml_text(call)
    url <- downlit::autolink_url(paste0(call_prefix(call), name, "()"))
    if (is.na(url)) {
      next
    }
    link_span(call, url)
  }

  ops <- xml2::xml_find_all(pre, ".//span[@class = 'sc'][not(a)]")
  for (op in ops) {
    name <- xml2::xml_text(op)
    if (!grepl("^%[^%]+%$", name)) {
      next
    }
    # downlit resolves an operator only written as a call.
    url <- downlit::autolink_url(paste0("`", name, "`()"))
    if (!is.na(url)) {
      link_span(op, url)
    }
  }
}

link_span <- function(span, url) {
  name <- xml2::xml_text(span)
  xml2::xml_remove(xml2::xml_contents(span))
  xml2::xml_add_child(span, "a", name, href = url)
}

# "pkg::" for a call highlighted as `pkg<span class="sc">::</span><span
# class="fu">fun</span>`, otherwise "".
call_prefix <- function(call) {
  op <- xml2::xml_find_first(call, "preceding-sibling::node()[1]")
  if (
    !identical(xml2::xml_attr(op, "class"), "sc") ||
      !xml2::xml_text(op) %in% c("::", ":::")
  ) {
    return("")
  }
  pkg <- xml2::xml_find_first(op, "preceding-sibling::node()[1]")
  if (inherits(pkg, "xml_missing") || xml2::xml_type(pkg) != "text") {
    return("")
  }
  pkg <- regmatches(
    xml2::xml_text(pkg),
    regexpr("[A-Za-z][A-Za-z0-9.]*$", xml2::xml_text(pkg))
  )
  if (length(pkg) == 0) "" else paste0(pkg, xml2::xml_text(op))
}
