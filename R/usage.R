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
  after <- if (end < length(lines)) lines[seq(end + 1, length(lines))] else character()
  c(before, "```r", unescape_rd2html(code), "```", after)
}

# ---- inline code in man pages ------------------------------------------------
#
# altdoc embeds `tools::Rd2HTML()` output in each man/*.qmd, so `\code{}`
# arrives as raw `<code>...</code>`. Pandoc still parses the text inside it
# as Markdown, so its smart-quotes extension turns `"` into curly quotes
# there: `\code{f("a")}` renders as f(“a”). A Markdown code span is left
# alone, and still renders as `<code>...</code>`.

inline_code_pattern <- "<code>([^<]*)</code>"

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

# Fences the Usage block and converts inline code in every rendered
# man/*.qmd, then re-renders just the files that changed so the changes
# actually reach `docs/`.
fix_man_qmds <- function(path) {
  man_dir <- file.path(path, "_quarto", "man")
  if (!dir.exists(man_dir)) {
    return(invisible())
  }

  for (f in fs::dir_ls(man_dir, regexp = "\\.qmd$")) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    fixed <- fence_usage_block(lines)
    if (is.null(fixed)) {
      fixed <- lines
    }
    fixed <- convert_inline_code(fixed)
    if (identical(fixed, lines)) {
      next
    }
    writeLines(enc2utf8(fixed), f, useBytes = TRUE)
    quarto::quarto_render(input = f, quiet = TRUE)
  }
  invisible()
}
