# ---- hover tooltips -----------------------------------------------------------
#
# Links references to the package's own topics in docs/ to their reference
# page, with a tooltip showing the signature and a one-line summary. Any
# existing downlit href is replaced, as downlit can't resolve the package's
# own, unpublished topics. Matched are `fu`/`va` spans and `%op%` `op`/`sc`
# spans in code blocks, and inline `<code>`, linked or not.

tooltip_link_re <- paste0(
  # One group number each for the class and the name in either kind of span.
  "(?|<span class=\"(fu|va)\">(?:<a href=\"[^\"]*\">)?([^<]*?)(?:</a>)?</span>",
  "|<span class=\"(op|sc)\">(?:<a href=\"[^\"]*\">)?(%[^%<]+%)(?:</a>)?</span>)",
  "|",
  # One group number for the name in either shape of inline code.
  "(?|<code>(?:<a href=\"[^\"]*\">)?([^<]*?)(?:</a>)?</code>",
  "|<a href=\"[^\"]*\"><code>([^<]*?)</code></a>)"
)

# Marks a processed page so a rerun of add_tooltips() skips it.
tooltip_marker <- "<!-- quartodown-tooltips -->"

# Adds tooltips to every docs/**/*.html file. Returns, invisibly, how many
# files and reference links were touched.
add_tooltips <- function(path = ".", quiet = FALSE) {
  docs_dir <- file.path(path, "docs")
  if (!dir.exists(file.path(docs_dir, "reference"))) {
    cli::cli_abort(
      "No built site with a {.path reference/} directory found at {.path {docs_dir}}."
    )
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
    html <- paste(
      readLines(f, warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
    if (grepl(tooltip_marker, html, fixed = TRUE)) {
      next
    }

    href_prefix <- reference_href_prefix(f, docs_dir)
    result <- inject_tooltips_html(html, index, href_prefix, pkg = pkg)
    if (result$n == 0) {
      next
    }

    new_html <- sub(
      "</head>",
      paste0(tooltip_css_block(), "</head>"),
      result$html,
      fixed = TRUE
    )
    body_addition <- paste0(
      tooltip_payload_html(result$tips),
      tooltip_script_block()
    )
    # The last </body>, as a script may contain the string too.
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

# Relative path from a rendered page to docs/reference/, e.g. "reference/"
# from docs/index.html or "../reference/" one directory down.
reference_href_prefix <- function(file, docs_dir) {
  rel <- fs::path_rel(
    fs::path(docs_dir, "reference"),
    start = fs::path_dir(file)
  )
  paste0(as.character(rel), "/")
}

# Links every `tooltip_link_re` match found in `index` and tags it with a tip
# id. `tips` has one entry per linked name, for tooltip_payload_html(). `pkg`
# is the package's name, so `pkg::foo()` resolves like `foo()`.
inject_tooltips_html <- function(
  html,
  index,
  href_prefix = "reference/",
  pkg = NULL
) {
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
    is_block <- cap_starts[i, 1] != 0L # span vs. inline <code>
    name_html <- capture(i, if (is_block) 2L else 3L)
    name <- html_unescape(name_html)
    # "foo(1, 2)", "?foo", "pkg::foo" and "`foo`" all look up "foo".
    lookup <- sub("\\(.*$", "", name, perl = TRUE)
    lookup <- sub("^\\?", "", lookup)
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
        '<a class="quartodown-tip-ref" data-quartodown-tip="%s" href="%s%s.html">%s</a>',
        id,
        href_prefix,
        entry$topic,
        name_html
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
# For each documented alias: its usage, a one-line summary and its topic.
# Each alias gets the \usage{} block that calls it; the topic's \name gets the
# whole \usage{}, and any other alias none.

build_topic_index <- function(path = ".", quiet = FALSE) {
  man_dir <- file.path(path, "man")
  if (!dir.exists(man_dir)) {
    cli::cli_abort("No {.path man/} directory found at {.path {path}}.")
  }

  macros <- rd_macros(path)
  index <- list()
  for (rd_file in fs::dir_ls(man_dir, regexp = "\\.Rd$")) {
    rd <- tryCatch(
      tools::parse_Rd(rd_file, macros = macros),
      error = function(e) NULL
    )
    if (is.null(rd)) {
      next
    }
    topic <- fs::path_ext_remove(fs::path_file(rd_file))
    # Relative, so the {.file} link in the warning below resolves.
    rd_name <- fs::path_rel(rd_file)

    tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
    aliases <- vapply(
      rd[tags == "\\alias"],
      function(x) as.character(x[[1]]),
      character(1)
    )
    # \keyword{internal} topics get no page, so there's nothing to link to.
    keywords <- vapply(
      rd[tags == "\\keyword"],
      function(x) trimws(as.character(x[[1]])),
      character(1)
    )
    if (length(aliases) == 0 || "internal" %in% keywords) {
      next
    }

    usage_node <- rd[tags == "\\usage"]
    usage_blocks <- if (length(usage_node) == 0) {
      character(0)
    } else {
      rd_usage_blocks(usage_node[[1]])
    }
    usage_whole <- if (length(usage_blocks) == 0) {
      NULL
    } else {
      paste(usage_blocks, collapse = "\n\n")
    }

    sentence <- first_sentence(rd_section_text(rd, tags, "\\description"))

    rd_name_field <- rd[tags == "\\name"]
    page_name <- if (length(rd_name_field) == 0) {
      character(0)
    } else {
      as.character(rd_name_field[[1]][[1]])
    }
    if (!quiet && sentence$clipped) {
      cli::cli_warn(c(
        "The tooltip summary for {.file {rd_name}} is longer than 200 characters and got truncated mid-word.",
        "i" = "Shorten the first sentence of \\description{{}} to fit within 200 characters."
      ))
    }

    for (alias in aliases) {
      match_idx <- match(alias, names(usage_blocks))
      usage <- if (!is.na(match_idx)) {
        usage_blocks[[match_idx]]
      } else if (alias %in% page_name) {
        usage_whole
      }
      index[[alias]] <- list(
        usage = usage,
        brief = sentence$text,
        topic = topic
      )
    }
  }
  index
}

# A \usage{} node split on blank lines into one block per signature, named by
# the alias it documents (see rd_usage_alias()).
rd_usage_blocks <- function(usage_node) {
  blocks <- character(0)
  current <- character(0)
  method <- NULL

  flush <- function() {
    if (length(current) > 0) {
      text <- trimws(paste(current, collapse = ""))
      if (nchar(text) > 0) {
        alias <- rd_usage_alias(text, method)
        blocks <<- c(blocks, structure(text, names = alias))
      }
    }
    current <<- character(0)
    method <<- NULL
  }

  for (child in usage_node) {
    tag <- attr(child, "Rd_tag")
    if (!is.null(tag) && tag %in% c("\\method", "\\S3method", "\\S4method")) {
      method <- list(
        s4 = tag == "\\S4method",
        generic = paste(unlist(child[[1]]), collapse = ""),
        signature = gsub("\\s", "", paste(unlist(child[[2]]), collapse = ""))
      )
      current[[length(current) + 1L]] <- if (method$s4) {
        method$generic
      } else {
        paste0(method$generic, ".", method$signature)
      }
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

# The alias the \usage{} block `block` documents. For a method, `method` is
# its macro's generic and class or signature, and the alias is spelled as
# roxygen does: "generic.class" or "generic,signature-method", with "<-"
# after the generic for a replacement method.
rd_usage_alias <- function(block, method = NULL) {
  replacement <- grepl("(?s)\\)\\s*<-\\s*\\S+$", block, perl = TRUE)
  if (is.null(method)) {
    name <- rd_usage_leading_name(block)
    return(if (replacement) paste0(name, "<-") else name)
  }
  generic <- if (replacement) paste0(method$generic, "<-") else method$generic
  if (method$s4) {
    paste0(generic, ",", method$signature, "-method")
  } else {
    paste0(generic, ".", method$signature)
  }
}

# The identifier before the first "(" of a \usage{} block, or the operator of
# an infix call such as "lhs %op% rhs".
rd_usage_leading_name <- function(block) {
  infix <- regmatches(block, regexec("^\\S+\\s+(%[^%]+%)\\s+\\S+$", block))[[1]]
  if (length(infix) == 2) {
    return(infix[[2]])
  }
  name <- trimws(sub("(?s)\\(.*$", "", block, perl = TRUE))
  sub("^`(.*)`$", "\\1", name)
}

# The text of a top-level Rd section, with code kept in `backticks`, or NULL.
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
  # No text from images (such as lifecycle badges), raw output or \Sexpr.
  if (!is.null(tag) && tag %in% c("COMMENT", "\\figure", "\\out", "\\Sexpr")) {
    return("")
  }
  if (identical(tag, "\\href")) {
    return(rd_flatten(x[[2]]))
  }
  # Format-conditional text: the HTML version, as the tooltip is HTML.
  if (!is.null(tag) && tag %in% c("\\if", "\\ifelse")) {
    html <- grepl("html", paste(unlist(x[[1]]), collapse = ""), fixed = TRUE)
    branch <- if (html) {
      x[[2]]
    } else if (tag == "\\ifelse") {
      x[[3]]
    }
    return(if (is.null(branch)) "" else rd_flatten(branch))
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

# The first sentence of `text`, cut at `max_chars` (then `clipped` is TRUE).
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
# One `position: fixed` tooltip shared by the page, so code blocks'
# `overflow: auto` can't clip it.

tooltip_css_block <- function() {
  paste0(
    tooltip_marker,
    "\n",
    "<style>\n",
    ".quartodown-tip-float{display:none;position:fixed;z-index:10000;",
    "min-width:220px;max-width:420px;",
    "background:var(--bs-body-bg,#1e1e1e);color:var(--bs-body-color,#eee);",
    "border:1px solid var(--bs-border-color,#444);",
    "border-radius:6px;padding:.5em .75em;font-size:.85em;",
    "line-height:1.4;box-shadow:0 4px 14px rgba(0,0,0,.35);",
    "white-space:normal;text-align:left;font-weight:normal;",
    "font-style:normal;}\n",
    ".quartodown-tip-float .quartodown-tip-sig{display:block;white-space:pre-wrap;",
    "font-family:ui-monospace,SFMono-Regular,Consolas,monospace;",
    "font-weight:600;color:var(--bs-link-color,#9cdcfe);margin-bottom:.35em;",
    "background-color:transparent;padding:0;border-radius:0;}\n",
    ".quartodown-tip-float .quartodown-tip-brief{margin:0;color:var(--bs-secondary-color,#ccc);}\n",
    "a.quartodown-tip-ref{cursor:help;}\n",
    "</style>\n"
  )
}

# `tips`: id -> list(usage, brief, name), rendered as a hidden payload the
# script below reads via `data-for`/`data-quartodown-tip`.
tooltip_payload_html <- function(tips) {
  entries <- vapply(
    names(tips),
    function(id) {
      tip <- tips[[id]]
      sig <- if (!is.null(tip$usage)) tip$usage else tip$name
      brief_html <- if (!is.null(tip$brief)) {
        brief <- gsub("`([^`]+)`", "<code>\\1</code>", html_escape(tip$brief))
        paste0("<span class=\"quartodown-tip-brief\">", brief, "</span>")
      } else {
        ""
      }
      sprintf(
        '<div data-for="%s"><code class="quartodown-tip-sig">%s</code>%s</div>',
        id,
        html_escape(sig),
        brief_html
      )
    },
    character(1)
  )

  paste0(
    '<div class="quartodown-tip-payload" hidden>',
    paste(entries, collapse = ""),
    "</div>\n"
  )
}

tooltip_script_block <- function() {
  paste0(
    "<script>\n",
    "(function(){\n",
    "  var payload = document.currentScript.previousElementSibling;\n",
    "  var float = document.createElement('div');\n",
    "  float.className = 'quartodown-tip-float';\n",
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
    "    var key = el.getAttribute('data-quartodown-tip');\n",
    "    var tip = payload.querySelector('[data-for=\"' + key + '\"]');\n",
    "    if (!tip) return;\n",
    "    float.innerHTML = tip.innerHTML;\n",
    "    float.style.display = 'block';\n",
    "    place(el);\n",
    "  }\n",
    "  function hide(){ float.style.display = 'none'; }\n",
    "  function cancelHide(){ clearTimeout(hideTimer); }\n",
    "  function scheduleHide(){ hideTimer = setTimeout(hide, 150); }\n",
    "  var links = document.querySelectorAll('a.quartodown-tip-ref');\n",
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
