# ---- man/*.Rd -> reference/*.qmd ----------------------------------------------
#
# Each Rd file is rendered with `tools::Rd2HTML()` and cut into its `<h3>`
# sections, each a `##` heading. Usage becomes a fenced R block, examples are
# rebuilt from `tools::Rd2ex()` (see example_blocks()) and the rest stays HTML
# (see rd_html_to_markdown()).

write_man_pages <- function(path, config = list()) {
  out_dir <- file.path(project_dir(path), "reference")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  topics <- rd_topics(path)
  topics <- topics[has_page(topics, config)]
  # Not paste0(<basenames>, ".Rd"), which is ".Rd" without topics.
  rd_files <- vapply(
    topics,
    function(x) file.path(path, "man", paste0(x$basename, ".Rd")),
    character(1)
  )
  macros <- rd_macros(path)
  pkg <- desc::desc_get_field("Package", file = path)
  pages <- topic_pages(topics)
  n <- 0L
  for (rd_file in rd_files) {
    rd <- tools::parse_Rd(rd_file, macros = macros)
    lines <- tryCatch(
      rd_to_qmd(rd, pkg, pages),
      error = function(e) {
        cli::cli_abort("Failed to convert {.file {rd_file}}.", parent = e)
      }
    )
    topic <- sub("\\.Rd$", "", basename(rd_file))
    writeLines(
      enc2utf8(lines),
      file.path(out_dir, paste0(topic, ".qmd")),
      useBytes = TRUE
    )
    n <- n + 1L
  }
  cli::cli_alert_success("Converted {n} man page{?s}.")
  invisible()
}

# The lines of the reference/*.qmd page for the parsed Rd `rd`. `pages` maps
# the package's topics to their pages, see topic_pages().
rd_to_qmd <- function(rd, pkg, pages = character()) {
  html <- rd_html(link_rd(rd, pkg, pages))
  html <- link_help_hrefs(html, pkg, pages)

  title_end <- grep("</h2>", html, fixed = TRUE)[1]
  title <- paste(html[seq_len(title_end)], collapse = " ")
  title <- trimws(sub("^<h2[^>]*>(.*)</h2>$", "\\1", title))
  html <- html[-seq_len(title_end)]

  heads <- grep("^<h3>.*</h3>$", html)
  names <- sub("^<h3>(.*)</h3>$", "\\1", html[heads])
  ends <- c(heads[-1] - 1L, length(html))
  # Rd2HTML() puts \examples{} last, after any custom \section{Examples}{}.
  has_examples <- any(
    vapply(rd, function(x) attr(x, "Rd_tag"), character(1)) == "\\examples"
  )
  examples_at <- if (has_examples) utils::tail(which(names == "Examples"), 1)

  sections <- lapply(seq_along(heads), function(i) {
    body <- if (ends[i] > heads[i]) {
      html[seq(heads[i] + 1L, ends[i])]
    } else {
      character()
    }
    body <- if (identical(i, examples_at)) {
      example_blocks(rd, pkg)
    } else if (names[i] == "Usage") {
      usage_block(body)
    } else {
      rd_html_to_markdown(body)
    }
    c(paste("##", rd_html_to_markdown(names[i])), "", body, "")
  })

  title <- paste(rd_html_to_markdown(title), collapse = " ")
  front_matter <- yaml::as.yaml(list(title = title), line.sep = "\n")
  c("---", sub("\n$", "", front_matter), "---", "", unlist(sections))
}

# The HTML `tools::Rd2HTML()` makes of `rd`, from the title's `<h2>` up to the
# end of the last section.
rd_html <- function(rd) {
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))
  with_sexpr_stages(rd, function(stages) {
    # `toc`'s default changed in R 4.6.0.
    tools::Rd2HTML(rd, out = out, toc = FALSE, stages = stages)
  })
  lines <- readLines(out, warn = FALSE, encoding = "UTF-8")
  start <- grep("^<h2", lines)[1]
  end <- grep("^</main>", lines)[1]
  end <- if (is.na(end)) length(lines) else end - 1L
  lines[seq(start, end)]
}

# Calls `render(stages)` running every `\Sexpr` stage, as `R CMD build` and
# `INSTALL` would, or, if one of them fails, only the render-stage ones.
with_sexpr_stages <- function(rd, render) {
  tryCatch(
    render(c("build", "install", "render")),
    error = function(e) {
      cli::cli_warn(
        "Running the {.code \\Sexpr} macros of {.file {rd_file_name(rd)}} failed; rendering it without them.",
        parent = e
      )
      render("render")
    }
  )
}

# ---- links ------------------------------------------------------------------------
#
# Rd2HTML() drops `\link{}` targets, so each is first made an `\href{}` to
# the package's own reference page, or to wherever downlit finds the topic.

# The page of each of `topics` (those with one, see has_page()), named by
# alias and by Rd file name, which `\link[pkg:file]{}` can give: "foo" for
# reference/foo.html.
topic_pages <- function(topics) {
  pages <- lapply(topics, function(t) {
    keys <- unique(c(t$alias, t$basename))
    structure(rep(t$basename, length(keys)), names = keys)
  })
  unlist(unname(pages))
}

# The URL of `topic` in `package` (NULL for any package) from a reference
# page of the package `pkg`, or NA.
topic_url <- function(topic, package, pkg, pages) {
  if (
    (is.null(package) || identical(package, pkg)) && topic %in% names(pages)
  ) {
    return(paste0(pages[[topic]], ".html"))
  }
  if (identical(package, pkg)) {
    return(NA_character_)
  }
  tryCatch(downlit::href_topic(topic, package), error = function(e) NA)
}

# `rd` with every `\link{}` it can resolve made an `\href{}`.
link_rd <- function(rd, pkg, pages) {
  tag <- attr(rd, "Rd_tag")
  if (identical(tag, "\\link")) {
    url <- rd_link_url(rd, pkg, pages)
    if (is.na(url)) {
      return(rd)
    }
    text <- rd
    attributes(text) <- NULL
    return(structure(
      list(list(structure(url, Rd_tag = "VERB")), text),
      Rd_tag = "\\href"
    ))
  }
  if (is.list(rd)) {
    rd[] <- lapply(rd, link_rd, pkg = pkg, pages = pages)
  }
  rd
}

# `\link{topic}`, `\link[=topic]{text}`, `\link[package]{topic}` or
# `\link[package:topic]{text}`.
rd_link_url <- function(link, pkg, pages) {
  text <- paste(unlist(link), collapse = "")
  option <- attr(link, "Rd_option")
  option <- if (is.null(option)) "" else paste(unlist(option), collapse = "")
  if (!nzchar(option) || startsWith(option, "=")) {
    package <- NULL
    topic <- if (nzchar(option)) substring(option, 2) else text
  } else {
    parts <- strsplit(option, ":", fixed = TRUE)[[1]]
    package <- parts[[1]]
    topic <- if (length(parts) > 1) parts[[2]] else text
  }
  topic_url(topic, package, pkg, pages)
}

# roxygen links an R6 class's inherited methods with raw HTML, to R's help
# system: `href='../../<package>/html/<topic>.html#<method>'`.
link_help_hrefs <- function(html, pkg, pages) {
  pattern <- "href=(['\"])\\.\\./\\.\\./([^/'\"]+)/html/([^/'\"#]+)\\.html(#[^'\"]*)?\\1"
  m <- gregexpr(pattern, html, perl = TRUE)
  regmatches(html, m) <- lapply(regmatches(html, m), function(hrefs) {
    vapply(
      hrefs,
      function(href) {
        parts <- regmatches(href, regexec(pattern, href, perl = TRUE))[[1]]
        url <- topic_url(parts[[4]], parts[[3]], pkg, pages)
        if (is.na(url)) href else sprintf("href=\"%s%s\"", url, parts[[5]])
      },
      character(1),
      USE.NAMES = FALSE
    )
  })
  html
}

# The file `rd` was parsed from, e.g. "man/foo.Rd".
rd_file_name <- function(rd) {
  file <- attr(attr(rd, "srcref"), "srcfile")$filename
  if (is.null(file)) "" else file.path("man", basename(file))
}

# Undoes the HTML-escaping `tools::Rd2HTML()` applies.
unescape_rd2html <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE) # must come last
}

# ---- Usage ----------------------------------------------------------------------

usage_block <- function(body) {
  text <- paste(body, collapse = "\n")
  m <- regexec(
    "(?s)<pre><code class=['\"]language-[Rr]['\"]>(.*?)</code></pre>",
    text,
    perl = TRUE
  )[[1]]
  if (m[1] == -1) {
    return(rd_html_to_markdown(body))
  }
  code <- substr(text, m[2], m[2] + attr(m, "match.length")[2] - 1L)
  code <- unescape_rd2html(gsub("&#8288;", "", code, fixed = TRUE))
  c("```r", trim_blank_lines(strsplit(code, "\n", fixed = TRUE)[[1]]), "```")
}

trim_blank_lines <- function(lines) {
  keep <- which(nzchar(trimws(lines)))
  if (length(keep) == 0) {
    return(character())
  }
  lines[seq(min(keep), max(keep))]
}

# ---- prose sections ---------------------------------------------------------------
#
# Section bodies stay HTML, which Pandoc reads with Markdown in between, except:
#
# * Inline `<code>` becomes a Markdown code span, as Pandoc would otherwise
#   smart-quote its text, and a trailing backslash would escape `</code>`.
# * `\eqn{}`/`\deqn{}` become `$...$`/`$$...$$`; any other `$` is escaped.
# * `<p>` tags are dropped, as paragraphs are separated by blank lines anyway.
# * Language-tagged `<pre>` blocks become fenced code blocks.

rd_html_token_pattern <- paste0(
  # One group number for the language and code of either kind of block.
  "(?s)(?|<div class=\"sourceCode (\\w+)\"><pre>(.*?)</pre></div>",
  # An R6 method's usage or example, as roxygen writes it.
  "|<div class=\"(r)(?: [^\"]*)?\">\\s*<pre>(.*?)</pre>\\s*</div>)",
  "|<pre\\b.*?</pre>",
  "|<p style=\"text-align: center;\"><code class=\"reqn\">(.*?)</code>\\s*</p>",
  "|<code(\\s[^>]*)?>([^<]*)</code>",
  "|<code><a href=\"([^\"]*)\">([^<]*)</a></code>"
)

rd_html_to_markdown <- function(lines) {
  if (length(lines) == 0) {
    return(character())
  }
  text <- paste(lines, collapse = "\n")
  text <- gsub("&#8288;", "", text, fixed = TRUE)
  text <- gsub(
    "src=\"../help/figures/",
    "src=\"../man/figures/",
    text,
    fixed = TRUE
  )

  m <- gregexpr(rd_html_token_pattern, text, perl = TRUE)[[1]]
  if (m[1] == -1) {
    return(split_lines(rd_html_text(text)))
  }
  starts <- as.integer(m)
  ends <- starts + attr(m, "match.length") - 1L
  cap_start <- attr(m, "capture.start")
  cap_len <- attr(m, "capture.length")
  capture <- function(i, col) {
    if (cap_start[i, col] < 1) {
      return(NA_character_)
    }
    substr(text, cap_start[i, col], cap_start[i, col] + cap_len[i, col] - 1L)
  }

  pieces <- character()
  pos <- 1L
  for (i in seq_along(starts)) {
    token <- substr(text, starts[i], ends[i])
    lang <- capture(i, 1)
    pieces <- c(
      pieces,
      rd_html_text(substr(text, pos, starts[i] - 1L)),
      if (!is.na(lang) && nzchar(lang)) {
        code <- split_lines(unescape_rd2html(capture(i, 2)))
        fenced <- c(paste0("```", tolower(lang)), trim_blank_lines(code), "```")
        paste(c("", fenced, ""), collapse = "\n")
      } else if (startsWith(token, "<pre")) {
        token
      } else if (!is.na(capture(i, 3))) {
        paste0("\n\n$$", unescape_rd2html(capture(i, 3)), "$$\n\n")
      } else if (!is.na(capture(i, 6))) {
        sprintf(
          "[%s](%s)",
          code_span(unescape_rd2html(capture(i, 7))),
          capture(i, 6)
        )
      } else if (grepl("class=\"reqn\"", token, fixed = TRUE)) {
        paste0("$", unescape_rd2html(capture(i, 5)), "$")
      } else {
        code_span(unescape_rd2html(capture(i, 5)))
      }
    )
    pos <- ends[i] + 1L
  }
  pieces <- c(pieces, rd_html_text(substr(text, pos, nchar(text))))
  split_lines(paste(pieces, collapse = ""))
}

# HTML outside code and math.
rd_html_text <- function(x) {
  x <- gsub("$", "\\$", x, fixed = TRUE)
  x <- gsub("<p>", "", x, fixed = TRUE)
  gsub("</p>", "", x, fixed = TRUE)
}

split_lines <- function(text) {
  # The sentinel keeps trailing empty lines, which strsplit() would drop.
  utils::head(strsplit(paste0(text, "\n."), "\n", fixed = TRUE)[[1]], -1)
}

code_span <- function(code) {
  # The fence must be longer than any backtick run inside, and padded with a
  # space if the code starts or ends with a backtick.
  runs <- gregexpr("`+", code)[[1]]
  longest <- if (runs[1] == -1) 0L else max(attr(runs, "match.length"))
  fence <- strrep("`", longest + 1L)
  pad <- if (grepl("^`|`$", code)) " " else ""
  paste0(fence, pad, code, pad, fence)
}

# ---- Examples ---------------------------------------------------------------------
#
# Like pkgdown, examples run with `#>` output in the same block, except that
# `\dontrun{}` code is shown but not run, `\dontshow{}` code is run but not
# shown, and `@examplesIf` examples run only if the condition is TRUE.

example_chunk_header <- "```{r, collapse=TRUE, comment=\"#>\"}"

example_blocks <- function(rd, pkg) {
  segments <- example_segments(rd)
  types <- vapply(segments, `[[`, character(1), "type")
  blocks <- lapply(segments, function(s) {
    code <- trim_blank_lines(s$code)
    if (length(code) == 0) {
      return(character())
    }
    header <- switch(
      s$type,
      run = example_chunk_header,
      dontrun = "```r",
      hidden = "```{r, include=FALSE}"
    )
    c(header, code, "```", "")
  })
  if (any(types %in% c("run", "hidden"))) {
    attach <- c("```{r, include=FALSE}", sprintf("library(%s)", pkg), "```", "")
    blocks <- c(list(attach), blocks)
  }
  unlist(blocks)
}

# The examples of `rd` as a list of runs of code, each with a type: "run",
# "dontrun" (shown, not run) or "hidden" (`\dontshow{}`: run, not shown).
# Adjacent runs never have the same type.
example_segments <- function(rd) {
  out <- tempfile(fileext = ".R")
  on.exit(unlink(out))
  # Rd2ex() marks each \dontrun{}, \donttest{} and \dontshow{} block, and
  # comments out the \dontrun{} code with "##D ".
  with_sexpr_stages(rd, function(stages) {
    tools::Rd2ex(
      rd,
      out = out,
      commentDontrun = TRUE,
      commentDonttest = FALSE,
      stages = stages
    )
  })
  if (!file.exists(out)) {
    return(list())
  }
  lines <- readLines(out, warn = FALSE, encoding = "UTF-8")
  start <- grep("^### \\*\\* Examples$", lines)[1]
  lines <- if (is.na(start)) character() else lines[-seq_len(start)]

  markers <- c(
    "## Not run:" = "dontrun",
    "## Don't show:" = "hidden",
    "## No test:" = "run"
  )
  end_markers <- c("## End(Not run)", "## End(Don't show)", "## End(No test)")

  segments <- list()
  stack <- "run"
  add <- function(type, line) {
    n <- length(segments)
    if (n > 0 && segments[[n]]$type == type && !segments[[n]]$closed) {
      segments[[n]]$code <<- c(segments[[n]]$code, line)
    } else {
      segments[[n + 1L]] <<- list(type = type, code = line, closed = FALSE)
    }
  }
  # Ends the current run, so code after a marker never joins the code before
  # it, even of the same type: an @examplesIf wrapper must stay on its own.
  close <- function() {
    n <- length(segments)
    if (n > 0) segments[[n]]$closed <<- TRUE
  }

  marker_pattern <- sprintf(
    "^(%s) ?(.*)$",
    paste(names(markers), collapse = "|")
  )
  for (line in lines) {
    marker <- trimws(line)
    inline <- sub(marker_pattern, "\\2", marker)
    if (grepl(marker_pattern, marker) && nzchar(inline)) {
      # A one-line block, e.g. `\dontrun{f()}`, has neither an end marker
      # nor "##D ".
      close()
      add(markers[[sub(marker_pattern, "\\1", marker)]], inline)
      close()
    } else if (marker %in% names(markers)) {
      close()
      stack <- c(stack, markers[[marker]])
    } else if (marker %in% end_markers) {
      close()
      stack <- utils::head(stack, -1)
    } else {
      type <- if ("dontrun" %in% stack) "dontrun" else utils::tail(stack, 1)
      if (type == "dontrun") {
        line <- sub("^##D ?", "", line)
      }
      add(type, line)
    }
  }
  segments <- resolve_examples_if(segments)
  # Blank lines between two \dontrun{} blocks would otherwise keep them
  # apart, as a run of code to run.
  blank <- vapply(
    segments,
    function(s) !any(nzchar(trimws(s$code))),
    logical(1)
  )
  merge_segments(segments[!blank])
}

# roxygen writes `@examplesIf cond` as a \dontshow{} `if (cond) ...({` before
# the examples and a \dontshow{} `})` after them. The wrappers are dropped,
# and the examples between them aren't run unless `cond` is TRUE.
examples_if_open <- paste0(
  "^if \\((.*)\\) ",
  # The second is what older roxygen versions write.
  "(withAutoprint|\\(if \\(getRversion\\(\\) >= \"3\\.4\"\\) withAutoprint else force\\))",
  "\\(\\{ # examplesIf$"
)
examples_if_close <- "^\\}\\) # examplesIf$"

resolve_examples_if <- function(segments) {
  out <- list()
  run <- TRUE
  for (s in segments) {
    code <- trim_blank_lines(s$code)
    if (s$type == "hidden" && length(code) == 1) {
      if (grepl(examples_if_open, code)) {
        cond <- sub(examples_if_open, "\\1", code)
        run <- isTRUE(tryCatch(
          eval(str2lang(cond), new.env(parent = globalenv())),
          error = function(e) FALSE
        ))
        next
      }
      if (grepl(examples_if_close, code)) {
        run <- TRUE
        next
      }
    }
    if (!run) {
      if (s$type == "hidden") {
        next
      }
      s$type <- "dontrun"
    }
    out[[length(out) + 1L]] <- s
  }
  out
}

merge_segments <- function(segments) {
  out <- list()
  for (s in segments) {
    n <- length(out)
    if (n > 0 && out[[n]]$type == s$type) {
      out[[n]]$code <- c(out[[n]]$code, s$code)
    } else {
      out[[n + 1L]] <- list(type = s$type, code = s$code)
    }
  }
  out
}
