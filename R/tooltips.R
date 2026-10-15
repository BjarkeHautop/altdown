# ---- hover tooltips -----------------------------------------------------------
#
# Rewrites the rendered HTML in docs/ so references to the package's own
# documented topics link to its man page and get a hover tooltip with the
# topic's signature and a one-line summary.
#
# downlit can't resolve the package's own (local, unpublished) functions on
# an altdoc site, nor a name it can't statically dispatch (e.g. an S3 method
# called by name), so any existing href on a matched reference is replaced.
#
# Two reference shapes are matched, each optionally already wrapped in a
# downlit link:
#  1. A called function inside a fenced code block: Pandoc tags it
#     `<span class="fu">name</span>` (or "va" for a referenced-but-not-
#     called value, e.g. an R6 class name).
#  2. Inline code in prose (`` `foo()` ``): `<code>name()</code>`.

tooltip_link_re <- paste0(
  "<span class=\"(fu|va)\">(?:<a href=\"[^\"]*\">)?([^<]*?)(?:</a>)?</span>",
  "|",
  "<code>(?:<a href=\"[^\"]*\">)?([^<]*?)(?:</a>)?</code>"
)

# Marks a processed page so a rerun of add_tooltips() skips it.
tooltip_marker <- "<!-- altdown-tooltips -->"

# Adds tooltips to every docs/**/*.html file. Returns, invisibly, how many
# files and reference links were touched.
add_tooltips <- function(path = ".", quiet = FALSE) {
  docs_dir <- file.path(path, "docs")
  if (!dir.exists(file.path(docs_dir, "man"))) {
    cli::cli_abort("No built site with a {.path man/} directory found at {.path {docs_dir}}.")
  }

  index <- build_topic_index(path, quiet = quiet)
  if (length(index) == 0) {
    return(invisible(list(files = 0L, links = 0L)))
  }
  pkg <- desc::desc_get_field("Package", file = path)

  html_files <- fs::dir_ls(docs_dir, regexp = "\\.html$", recurse = TRUE)
  n_files <- 0L
  n_links <- 0L

  for (f in html_files) {
    html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (grepl(tooltip_marker, html, fixed = TRUE)) {
      next
    }

    href_prefix <- reference_href_prefix(f, docs_dir)
    result <- inject_tooltips_html(html, index, href_prefix, pkg = pkg)
    if (result$n == 0) {
      next
    }

    new_html <- sub("</head>", paste0(tooltip_css_block(), "</head>"), result$html, fixed = TRUE)
    body_addition <- paste0(tooltip_payload_html(result$tips), tooltip_script_block())
    # A plain `sub("</body>", ...)` would match the first literal
    # occurrence anywhere in the raw HTML text, including one inside a
    # <script> block's own source (e.g. a JS comment mentioning "</body>").
    # The real closing tag is always the last one, right before </html>.
    if (grepl("</body>\\s*</html>\\s*$", new_html, perl = TRUE)) {
      new_html <- sub(
        "</body>(\\s*</html>\\s*)$",
        paste0(body_addition, "</body>\\1"),
        new_html,
        perl = TRUE
      )
    } else {
      new_html <- paste0(new_html, body_addition)
    }

    writeLines(new_html, f, useBytes = TRUE)
    n_files <- n_files + 1L
    n_links <- n_links + result$n
  }

  if (!quiet) {
    cli::cli_inform(
      "Added tooltips to {n_links} reference link{?s} across {n_files} file{?s}."
    )
  }
  invisible(list(files = n_files, links = n_links))
}

# Relative path from a rendered page to docs/man/, e.g. "man/" from
# docs/index.html or "../man/" one directory down.
reference_href_prefix <- function(file, docs_dir) {
  rel <- fs::path_rel(fs::path(docs_dir, "man"), start = fs::path_dir(file))
  paste0(as.character(rel), "/")
}

# One pass over an HTML document: every `tooltip_link_re` match that
# resolves in `index` is linked to `href_prefix`/<topic>.html and tagged
# with `data-altdown-tip="<id>"`; everything else passes through unchanged.
# `tips` collects one entry per unique resolved name (id -> {usage, brief,
# topic, name}), the page's hidden tooltip payload. `pkg` is the package's
# own name, so an inline `pkg::foo()` resolves like `foo()`.
inject_tooltips_html <- function(html, index, href_prefix = "man/", pkg = NULL) {
  m <- gregexpr(tooltip_link_re, html, perl = TRUE)[[1]]
  if (m[1] == -1) {
    return(list(html = html, n = 0L, tips = list()))
  }

  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  cap_starts <- attr(m, "capture.start")
  cap_lens <- attr(m, "capture.length")
  capture <- function(i, col) {
    substr(html, cap_starts[i, col], cap_starts[i, col] + cap_lens[i, col] - 1L)
  }

  pieces <- vector("list", length(starts) * 2L + 1L)
  pos <- 1L
  pi <- 1L
  n_linked <- 0L
  tip_ids <- character(0)
  tips <- list()

  for (i in seq_along(starts)) {
    s <- starts[i]
    l <- lens[i]
    is_block <- cap_starts[i, 1] != 0L # span (fu/va) vs. inline <code>
    name_html <- capture(i, if (is_block) 2L else 3L)
    name <- html_unescape(name_html)
    # strip a trailing call, e.g. "foo(1, 2)" -> "foo", the package's own
    # namespace, e.g. "pkg::foo" -> "foo", and a wrapping backtick pair
    # from a literal `` `name<-`() `` in prose
    lookup <- sub("\\(.*$", "", name, perl = TRUE)
    if (!is.null(pkg)) {
      lookup <- sub(paste0("^", pkg, ":::?"), "", lookup)
    }
    lookup <- sub("^`(.*)`$", "\\1", lookup)

    entry <- index[[lookup]]

    pieces[[pi]] <- substr(html, pos, s - 1L)
    pi <- pi + 1L
    if (is.null(entry)) {
      pieces[[pi]] <- substr(html, s, s + l - 1L)
    } else {
      id <- tip_ids[lookup]
      if (is.na(id)) {
        id <- paste0("t", length(tips) + 1L)
        tip_ids[lookup] <- id
        entry$name <- lookup
        tips[[id]] <- entry
      }
      link <- sprintf(
        '<a class="altdown-tip-ref" data-altdown-tip="%s" href="%s%s.html">%s</a>',
        id, href_prefix, entry$topic, name_html
      )
      pieces[[pi]] <- if (is_block) {
        sprintf('<span class="%s">%s</span>', capture(i, 1L), link)
      } else {
        paste0("<code>", link, "</code>")
      }
      n_linked <- n_linked + 1L
    }
    pi <- pi + 1L
    pos <- s + l
  }
  pieces[[pi]] <- substr(html, pos, nchar(html))

  list(html = paste(unlist(pieces), collapse = ""), n = n_linked, tips = tips)
}

# ---- topic index --------------------------------------------------------------
#
# For each documented alias in man/*.Rd: its usage, a one-line summary and
# the topic it's documented on. When a page documents several aliases (e.g.
# an S3 generic and its methods), its \usage{} block is split on blank lines
# and each alias is matched to the block whose call starts with its name; an
# alias with no matching block falls back to the whole \usage{} text.

build_topic_index <- function(path = ".", quiet = FALSE) {
  man_dir <- file.path(path, "man")
  if (!dir.exists(man_dir)) {
    cli::cli_abort("No {.path man/} directory found at {.path {path}}.")
  }

  index <- list()
  for (rd_file in fs::dir_ls(man_dir, regexp = "\\.Rd$")) {
    rd <- tryCatch(tools::parse_Rd(rd_file), error = function(e) NULL)
    if (is.null(rd)) next
    topic <- fs::path_ext_remove(fs::path_file(rd_file))
    rd_name <- basename(rd_file)

    tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
    aliases <- vapply(rd[tags == "\\alias"], function(x) as.character(x[[1]]), character(1))
    if (length(aliases) == 0) next

    usage_node <- rd[tags == "\\usage"]
    usage_blocks <- if (length(usage_node) == 0) character(0) else rd_usage_blocks(usage_node[[1]])
    usage_whole <- if (length(usage_blocks) == 0) NULL else paste(usage_blocks, collapse = "\n\n")
    usage_by_name <- usage_blocks
    names(usage_by_name) <- vapply(usage_blocks, rd_usage_leading_name, character(1))

    sentence <- first_sentence(rd_section_text(rd, tags, "\\description"))

    unmatched <- setdiff(aliases, names(usage_by_name))
    if (!quiet && length(aliases) > 1 && length(unmatched) > 0) {
      cli::cli_warn(c(
        "{.file {rd_name}} documents {length(aliases)} aliases but no \\usage{{}} line matches {.val {unmatched}}.",
        "i" = "Their tooltips show the full shared \\usage{{}} block instead."
      ))
    }
    if (!quiet && sentence$clipped) {
      cli::cli_warn(c(
        "The tooltip summary for {.file {rd_name}} is longer than 200 characters and got truncated mid-word.",
        "i" = "Shorten the first sentence of \\description{{}} to fit within 200 characters."
      ))
    }

    for (alias in aliases) {
      match_idx <- match(alias, names(usage_by_name))
      usage <- if (is.na(match_idx)) usage_whole else usage_by_name[[match_idx]]
      index[[alias]] <- list(usage = usage, brief = sentence$text, topic = topic)
    }
  }
  index
}

# Split a \usage{} node into one block per signature, on blank-line
# separators. A \method{generic}{class} macro is reassembled as
# "generic.class", matching the S3 method's alias spelling (e.g. "print.foo").
rd_usage_blocks <- function(usage_node) {
  blocks <- character(0)
  current <- character(0)

  flush <- function() {
    if (length(current) > 0) {
      text <- trimws(paste(current, collapse = ""))
      if (nchar(text) > 0) blocks[[length(blocks) + 1L]] <<- text
    }
    current <<- character(0)
  }

  for (child in usage_node) {
    if (identical(attr(child, "Rd_tag"), "\\method")) {
      current[[length(current) + 1L]] <- paste0(
        paste(unlist(child[[1]]), collapse = ""), ".",
        paste(unlist(child[[2]]), collapse = "")
      )
    } else {
      piece <- paste(unlist(child), collapse = "")
      if (nchar(trimws(piece)) == 0) {
        flush() # a blank-only run separates one signature from the next
      } else {
        current[[length(current) + 1L]] <- piece
      }
    }
  }
  flush()

  blocks
}

# The alias a \usage{} block documents: the identifier before its first "(".
rd_usage_leading_name <- function(block) {
  name <- trimws(sub("\\(.*$", "", block, perl = TRUE))
  sub("^`(.*)`$", "\\1", name)
}

# Plain-text content of a top-level Rd section (e.g. "\\description"),
# stripped of markup except code, which is kept as `backticks` so the
# tooltip can render it as <code>. NULL when the section is absent or empty.
rd_section_text <- function(rd, tags, section) {
  hit <- rd[tags == section]
  if (length(hit) == 0) {
    return(NULL)
  }
  text <- trimws(paste(vapply(hit, rd_flatten, character(1)), collapse = ""))
  if (nchar(text) == 0) NULL else text
}

rd_flatten <- function(x) {
  tag <- attr(x, "Rd_tag")
  if (identical(tag, "COMMENT")) {
    return("")
  }
  if (is.character(x)) {
    return(paste(x, collapse = ""))
  }
  inner <- paste(vapply(x, rd_flatten, character(1)), collapse = "")
  if (!is.null(tag) && tag %in% c("\\code", "\\verb", "\\samp")) {
    paste0("`", inner, "`")
  } else {
    inner
  }
}

# First sentence of a flattened Rd description, capped so a runaway
# paragraph can't blow up a tooltip. `clipped` is TRUE whenever the result
# had to be cut at `max_chars`, whether or not a sentence boundary was found.
first_sentence <- function(text, max_chars = 200) {
  if (is.null(text)) {
    return(list(text = NULL, clipped = FALSE))
  }
  text <- gsub("\\s+", " ", trimws(text))
  if (nchar(text) == 0) {
    return(list(text = NULL, clipped = FALSE))
  }

  m <- regexpr("[.!?](\\s|$)", text, perl = TRUE)
  sentence <- trimws(if (m != -1) substr(text, 1, m) else text)

  clipped <- nchar(sentence) > max_chars
  if (clipped) {
    sentence <- paste0(trimws(substr(sentence, 1, max_chars)), "...")
  }
  list(text = sentence, clipped = clipped)
}

html_escape <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  gsub("\"", "&quot;", text, fixed = TRUE)
}

html_unescape <- function(text) {
  text <- gsub("&lt;", "<", text, fixed = TRUE)
  text <- gsub("&gt;", ">", text, fixed = TRUE)
  text <- gsub("&quot;", "\"", text, fixed = TRUE)
  text <- gsub("&#39;", "'", text, fixed = TRUE)
  gsub("&amp;", "&", text, fixed = TRUE)
}

# ---- tooltip assets -----------------------------------------------------------
#
# One `position: fixed` tooltip div shared by the whole page, positioned by
# a few lines of vanilla JS. `position: fixed` (rather than a pure-CSS
# `:hover` sibling) avoids Quarto's code-block wrapper clipping it via
# `overflow: auto`.

tooltip_css_block <- function() {
  paste0(
    tooltip_marker, "\n",
    "<style>\n",
    ".altdown-tip-float{display:none;position:fixed;z-index:10000;",
    "min-width:220px;max-width:420px;",
    "background:var(--bs-body-bg,#1e1e1e);color:var(--bs-body-color,#eee);",
    "border:1px solid var(--bs-border-color,#444);",
    "border-radius:6px;padding:.5em .75em;font-size:.85em;",
    "line-height:1.4;box-shadow:0 4px 14px rgba(0,0,0,.35);",
    "white-space:normal;text-align:left;font-weight:normal;",
    "font-style:normal;}\n",
    ".altdown-tip-float .altdown-tip-sig{display:block;white-space:pre-wrap;",
    "font-family:ui-monospace,SFMono-Regular,Consolas,monospace;",
    "font-weight:600;color:var(--bs-link-color,#9cdcfe);margin-bottom:.35em;",
    # undo Bootstrap's code{} background so this doesn't look boxed in dark mode
    "background-color:transparent;padding:0;border-radius:0;}\n",
    ".altdown-tip-float .altdown-tip-brief{margin:0;color:var(--bs-secondary-color,#ccc);}\n",
    "a.altdown-tip-ref{cursor:help;}\n",
    "</style>\n"
  )
}

# `tips`: id -> list(usage, brief, name), rendered as a hidden payload the
# script below reads via `data-for`/`data-altdown-tip`.
tooltip_payload_html <- function(tips) {
  entries <- vapply(names(tips), function(id) {
    tip <- tips[[id]]
    sig <- if (!is.null(tip$usage)) tip$usage else tip$name
    brief_html <- if (!is.null(tip$brief)) {
      # `code` in the brief (kept from Rd \code{}) becomes <code>
      brief <- gsub("`([^`]+)`", "<code>\\1</code>", html_escape(tip$brief))
      paste0("<span class=\"altdown-tip-brief\">", brief, "</span>")
    } else {
      ""
    }
    sprintf(
      '<div data-for="%s"><code class="altdown-tip-sig">%s</code>%s</div>',
      id, html_escape(sig), brief_html
    )
  }, character(1))

  paste0('<div class="altdown-tip-payload" hidden>', paste(entries, collapse = ""), "</div>\n")
}

tooltip_script_block <- function() {
  paste0(
    "<script>\n",
    "(function(){\n",
    "  var payload = document.currentScript.previousElementSibling;\n",
    "  var float = document.createElement('div');\n",
    "  float.className = 'altdown-tip-float';\n",
    "  float.setAttribute('role', 'tooltip');\n",
    "  document.body.appendChild(float);\n",
    "  var hideTimer;\n",
    "  function place(el){\n",
    "    var r = el.getBoundingClientRect();\n",
    "    var top = r.bottom + 6;\n",
    "    var left = r.left;\n",
    "    var maxLeft = window.innerWidth - float.offsetWidth - 8;\n",
    "    if (left > maxLeft) left = Math.max(8, maxLeft);\n",
    "    var maxTop = window.innerHeight - float.offsetHeight - 8;\n",
    "    if (top > maxTop) top = Math.max(8, r.top - float.offsetHeight - 6);\n",
    "    float.style.top = top + 'px';\n",
    "    float.style.left = left + 'px';\n",
    "  }\n",
    "  function show(el){\n",
    "    var key = el.getAttribute('data-altdown-tip');\n",
    "    var tip = payload.querySelector('[data-for=\"' + key + '\"]');\n",
    "    if (!tip) return;\n",
    "    float.innerHTML = tip.innerHTML;\n",
    "    float.style.display = 'block';\n",
    "    place(el);\n",
    "  }\n",
    "  function hide(){ float.style.display = 'none'; }\n",
    "  function cancelHide(){ clearTimeout(hideTimer); }\n",
    "  function scheduleHide(){ hideTimer = setTimeout(hide, 150); }\n",
    "  var links = document.querySelectorAll('a.altdown-tip-ref');\n",
    "  for (var i = 0; i < links.length; i++) {\n",
    "    (function(el){\n",
    "      el.addEventListener('mouseenter', function(){ cancelHide(); show(el); });\n",
    "      el.addEventListener('mouseleave', scheduleHide);\n",
    "      el.addEventListener('focus', function(){ show(el); });\n",
    "      el.addEventListener('blur', hide);\n",
    "    })(links[i]);\n",
    "  }\n",
    "  float.addEventListener('mouseenter', cancelHide);\n",
    "  float.addEventListener('mouseleave', scheduleHide);\n",
    "})();\n",
    "</script>\n"
  )
}
