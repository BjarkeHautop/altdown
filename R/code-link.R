# ---- after rendering ----------------------------------------------------------
#
# Each docs/**/*.html page is read and written once, and in between: its ANSI
# codes protected from xml2 (see protect_ansi()), its code linked
# (link_code_html()), references to the package's own topics given tooltips
# (add_tooltips_html()) and the ANSI codes made `<span>`s (ansi_to_html()).

postprocess_pages <- function(
  path,
  config = list(),
  topics = rd_topics(path),
  tooltips = TRUE
) {
  docs <- site_dir(path, config)
  index <- if (isTRUE(tooltips)) build_topic_index(path, config, topics)
  pkg <- if (length(index) > 0) pkg_name(path)
  n_files <- 0L
  n_links <- 0L
  pages <- fs::dir_ls(docs, regexp = "\\.html$", recurse = TRUE)
  # A release build leaves the development site in dev/ as it was.
  if (docs == docs_dir(path)) {
    pages <- pages[!fs::path_has_parent(pages, file.path(docs, "dev"))]
  }
  for (f in pages) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    html <- link_code_html(protect_ansi(paste(lines, collapse = "\n")))
    if (length(index) > 0) {
      prefix <- reference_href_prefix(f, docs)
      result <- add_tooltips_html(html, index, prefix, pkg = pkg)
      html <- result$html
      n_files <- n_files + (result$n > 0)
      n_links <- n_links + result$n
    }
    html <- paste(ansi_to_html(split_lines(html)), collapse = "\n")
    writeLines(enc2utf8(html), f, sep = "", useBytes = TRUE)
  }
  strip_search_ansi(docs)
  if (length(index) > 0) {
    cli::cli_inform(
      "Added tooltips to {n_links} reference link{?s} across {n_files} file{?s}."
    )
  }
  invisible()
}

# ---- code linking -------------------------------------------------------------
#
# Replaces Quarto's `code-link`, which only covers knitr pages and drops the
# markers of annotated code blocks. Those are linked token by token instead
# (link_annotated()).

# The page `html` (a string) with its code linked, as xml2 writes it.
link_code_html <- function(html) {
  # downlit collects the packages a page attaches with `library()` in this
  # option; reset it so they don't carry over to the next page.
  old <- options(downlit.attached = NULL)
  on.exit(options(old))

  doc <- xml2::read_html(html, encoding = "UTF-8")
  # downlit rewrites every `pre.sourceCode.r`; hide annotated blocks from it.
  annotated <- xml2::xml_find_all(
    doc,
    ".//pre[contains(@class, 'code-annotated')]"
  )
  classes <- xml2::xml_attr(annotated, "class")
  xml2::xml_set_attr(annotated, "class", "quartodown-annotated")
  downlit::downlit_html_node(doc)
  for (i in seq_along(annotated)) {
    xml2::xml_set_attr(annotated[[i]], "class", classes[[i]])
    link_annotated(annotated[[i]])
  }
  con <- rawConnection(raw(0), "wb")
  on.exit(close(con), add = TRUE)
  xml2::write_html(doc, con)
  html <- rawToChar(rawConnectionValue(con))
  Encoding(html) <- "UTF-8"
  html
}

# Links an annotated block's `fu` spans (with any `pkg::`) and `%op%` `sc`
# spans without re-highlighting it.
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
    ns <- call_namespace(call)
    if (!is.null(ns)) {
      highlight_namespace(ns)
    }
    url <- downlit::autolink_url(paste0(ns$prefix, name, "()"))
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

# For a call highlighted as `pkg<span class="sc">::</span><span
# class="fu">fun</span>`: its `prefix` ("pkg::"), the text node ending in
# `pkg` and the `op` span. NULL for a call without one.
call_namespace <- function(call) {
  op <- xml2::xml_find_first(call, "preceding-sibling::node()[1]")
  if (
    !identical(xml2::xml_attr(op, "class"), "sc") ||
      !xml2::xml_text(op) %in% c("::", ":::")
  ) {
    return(NULL)
  }
  text <- xml2::xml_find_first(op, "preceding-sibling::node()[1]")
  if (inherits(text, "xml_missing") || xml2::xml_type(text) != "text") {
    return(NULL)
  }
  pkg <- regmatches(
    xml2::xml_text(text),
    regexpr("[A-Za-z][A-Za-z0-9.]*$", xml2::xml_text(text))
  )
  if (length(pkg) == 0) {
    return(NULL)
  }
  list(prefix = paste0(pkg, xml2::xml_text(op)), text = text, op = op)
}

# Highlights `pkg::` as downlit does in the other blocks: `pkg` and `::` each
# in a `fu` span.
highlight_namespace <- function(ns) {
  pkg <- sub("::+$", "", ns$prefix)
  text <- xml2::xml_text(ns$text)
  xml2::xml_add_sibling(ns$text, "span", pkg, class = "fu", .where = "after")
  xml2::xml_text(ns$text) <- substr(text, 1, nchar(text) - nchar(pkg))
  xml2::xml_set_attr(ns$op, "class", "fu")
}
