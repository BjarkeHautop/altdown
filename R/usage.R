# ---- man-page Usage syntax highlighting -------------------------------------
#
# `altdoc:::.rd2qmd()` re-wraps each Rd's Examples section into an
# executable ```` ```{r} ```` fenced chunk, so Quarto syntax-highlights it and
# gives it a copy button - but it leaves the Usage section alone. We make it into
# a ``` r ``` block so it gets syntax highlight+copy.
usage_block_pattern <- "^<pre><code class=['\"]language-[Rr]['\"]>"

# Undoes the HTML-escaping `tools::Rd2HTML()` applies
unescape_rd2html <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x <- gsub("\\$", "$", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE) # must come last
  x
}

# Replaces the first raw Usage `<pre><code>` block in `lines` (a man/*.qmd's
# content) with a fenced one. Returns NULL if there's no such block, so
# callers can tell "nothing to do" apart from "already fenced".
fence_usage_block <- function(lines) {
  start <- grep(usage_block_pattern, lines)[1]
  if (is.na(start)) {
    return(NULL)
  }

  first <- sub(usage_block_pattern, "", lines[start])
  if (grepl("</code></pre>$", first)) {
    end <- start
    code <- sub("</code></pre>$", "", first)
  } else {
    rest <- lines[seq(start + 1, length(lines))]
    close_at <- which(trimws(rest) == "</code></pre>")[1]
    if (is.na(close_at)) {
      return(NULL)
    }
    end <- start + close_at
    middle <- if (close_at > 1) rest[seq_len(close_at - 1)] else character()
    code <- c(first, middle)
  }

  before <- if (start > 1) lines[seq_len(start - 1)] else character()
  after <- if (end < length(lines)) {
    lines[seq(end + 1, length(lines))]
  } else {
    character()
  }
  c(before, "```r", unescape_rd2html(code), "```", after)
}

# ---- inline code in man pages ------------------------------------------------
#
# altdoc embeds `tools::Rd2HTML()` output in each man/*.qmd, so `\code{}`
# arrives as raw `<code>...</code>`. Pandoc still parses the text inside it
# as Markdown, so its smart-quotes extension turns `"` into curly quotes
# there: `\code{f("a")}` renders as f(“a”). Worse, a backslash right before
# `</code>`, as `\verb{\\}` gives, escapes the closing tag, so the code runs
# on to the end of the paragraph. A Markdown code span is left alone, and
# still renders as `<code>...</code>`.
#
# `<code>` can carry attributes (`\verb{}` adds a `style`); one directly
# inside `<pre>` is a code block and is left as it is.

inline_code_pattern <- "(?<!<pre>)<code(?:\\s[^>]*)?>([^<]*)</code>"

# Rewrites every `<code>...</code>` in `x` (with no tags inside) as a
# Markdown code span.
inline_code_to_markdown <- function(x) {
  m <- gregexpr(inline_code_pattern, x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(matches) {
    vapply(matches, code_span, character(1))
  })
  x
}

code_span <- function(html) {
  code <- unescape_rd2html(sub(inline_code_pattern, "\\1", html, perl = TRUE))
  # The fence must be longer than any backtick run inside, and padded with a
  # space if the code starts or ends with a backtick.
  runs <- gregexpr("`+", code)[[1]]
  longest <- if (runs[1] == -1) 0L else max(attr(runs, "match.length"))
  fence <- strrep("`", longest + 1L)
  pad <- if (grepl("^`|`$", code)) " " else ""
  paste0(fence, pad, code, pad, fence)
}

# Applies inline_code_to_markdown() to every line outside fenced code blocks,
# where `<code>` is literal text.
convert_inline_code <- function(lines) {
  is_fence <- grepl("^\\s*```", lines)
  in_block <- cumsum(is_fence) %% 2 == 1 | is_fence
  lines[!in_block] <- inline_code_to_markdown(lines[!in_block])
  lines
}

# ---- examples that aren't run -------------------------------------------------
#
# altdoc turns an Rd's Examples into a ```{r} chunk, with `eval=FALSE` when
# they're in \dontrun{}. Such a chunk is never run, but it still makes
# Quarto render the page with knitr, which starts an R session per page. A
# plain ```r block renders the same code without one.

noeval_chunk_pattern <- "^```\\{r\\b.*\\beval\\s*=\\s*FALSE\\b.*\\}\\s*$"

unevaluated_chunks_to_code <- function(lines) {
  lines[grepl(noeval_chunk_pattern, lines, perl = TRUE)] <- "```r"
  lines
}

# ---- examples that are run ------------------------------------------------------
#
# By default knitr shows a chunk's output in a separate block below the code.
# pkgdown shows it in the same block, each line prefixed with `#>`; knitr's
# `collapse` and `comment` options do the same.

eval_chunk_pattern <- "^(```\\{r\\b.*?)\\}\\s*$"

collapse_example_output <- function(lines) {
  chunk <- grepl(eval_chunk_pattern, lines, perl = TRUE) &
    !grepl("\\bcollapse\\s*=", lines, perl = TRUE)
  lines[chunk] <- sub(
    eval_chunk_pattern,
    "\\1, collapse=TRUE, comment=\"#>\"}",
    lines[chunk],
    perl = TRUE
  )
  lines
}

# ---- library() call in examples -------------------------------------------------
#
# altdoc starts every example with `library("<pkg>")`, which pkgdown doesn't
# show. A call attaching the package itself is dropped from examples that
# aren't run; ones that are run still need the package attached, so the call
# moves into a hidden chunk before them. Calls attaching other packages are
# part of the example and stay.

library_call_pattern <- function(pkg) {
  sprintf(
    "^library\\(([\"']?)%s\\1\\)\\s*$",
    gsub(".", "\\.", pkg, fixed = TRUE)
  )
}

hide_library_calls <- function(lines, pkg) {
  starts <- which(
    grepl("^```(r|\\{r\\b.*\\})\\s*$", lines, perl = TRUE) &
      c(grepl(library_call_pattern(pkg), lines[-1], perl = TRUE), FALSE)
  )
  for (i in rev(starts)) {
    library_call <- lines[i + 1]
    lines <- lines[-(i + 1)]
    if (grepl("^```\\{", lines[i])) {
      lines <- append(
        lines,
        c("```{r, include=FALSE}", library_call, "```"),
        i - 1
      )
    }
  }
  lines
}

# Drops blank lines at the start and end of every fenced code block. altdoc
# leaves some where it removed the `\dontrun{` and `}` around the examples;
# knitr would strip them from a chunk, but a plain code block shows them.
trim_code_blocks <- function(lines) {
  is_fence <- grepl("^\\s*```", lines)
  block_id <- cumsum(is_fence)
  in_block <- block_id %% 2 == 1 & !is_fence
  blank <- !nzchar(trimws(lines))

  drop <- logical(length(lines))
  for (idx in split(which(in_block), block_id[in_block])) {
    content <- idx[!blank[idx]]
    keep <- length(content) > 0 &
      idx >= min(content, Inf) &
      idx <= max(content, -Inf)
    drop[idx[!keep]] <- TRUE
  }
  lines[!drop]
}

# ---- pre-render script ----------------------------------------------------------
#
# The man/*.qmd files only exist once altdoc::render_docs() has converted the
# Rd files, and it renders the Quarto project straight after. So they're
# fixed up by a Quarto pre-render script, which runs inside that render.
# The script carries its own copy of the functions below, so it always
# matches the altdown version that wrote it.

fix_man_qmd <- function(lines, pkg) {
  fixed <- fence_usage_block(lines)
  if (is.null(fixed)) {
    fixed <- lines
  }
  fixed <- unevaluated_chunks_to_code(convert_inline_code(fixed))
  fixed <- hide_library_calls(fixed, pkg)
  trim_code_blocks(collapse_example_output(fixed))
}

pre_render_main <- function(pkg) {
  for (f in list.files("man", pattern = "\\.qmd$", full.names = TRUE)) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    fixed <- fix_man_qmd(lines, pkg)
    if (!identical(fixed, lines)) {
      writeLines(enc2utf8(fixed), f, useBytes = TRUE)
    }
  }
}

pre_render_objects <- c(
  "usage_block_pattern",
  "unescape_rd2html",
  "fence_usage_block",
  "inline_code_pattern",
  "inline_code_to_markdown",
  "code_span",
  "convert_inline_code",
  "noeval_chunk_pattern",
  "unevaluated_chunks_to_code",
  "eval_chunk_pattern",
  "collapse_example_output",
  "library_call_pattern",
  "hide_library_calls",
  "trim_code_blocks",
  "fix_man_qmd",
  "pre_render_main"
)

write_pre_render_script <- function(path) {
  ns <- environment(write_pre_render_script)
  definitions <- vapply(
    pre_render_objects,
    function(name) {
      paste(name, "<-", paste(deparse(get(name, envir = ns)), collapse = "\n"))
    },
    character(1)
  )
  writeLines(
    c(
      "# Generated by altdown::build_site(): fixes up altdoc's man/*.qmd files",
      "# before Quarto renders them. Don't edit by hand.",
      "",
      definitions,
      "",
      sprintf(
        "pre_render_main(%s)",
        deparse(desc::desc_get_field("Package", file = path))
      )
    ),
    file.path(path, "altdoc", "altdown-pre-render.R"),
    useBytes = TRUE
  )
}
