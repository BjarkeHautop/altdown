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

# Fences the Usage block in every rendered man/*.qmd, then re-renders just
# the files that changed so the swap actually reaches `docs/`.
fix_man_usage_blocks <- function(path) {
  man_dir <- file.path(path, "_quarto", "man")
  if (!dir.exists(man_dir)) {
    return(invisible())
  }

  for (f in fs::dir_ls(man_dir, regexp = "\\.qmd$")) {
    lines <- readLines(f, warn = FALSE)
    fenced <- fence_usage_block(lines)
    if (is.null(fenced)) {
      next
    }
    writeLines(fenced, f)
    quarto::quarto_render(input = f, quiet = TRUE)
  }
  invisible()
}
