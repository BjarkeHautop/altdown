# ---- shared helpers -------------------------------------------------------------

html_escape <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  gsub("\"", "&quot;", text, fixed = TRUE)
}

# Undoes html_escape(), and the escaping of `tools::Rd2HTML()` and
# short_name().
html_unescape <- function(text) {
  text <- gsub("&lt;", "<", text, fixed = TRUE)
  text <- gsub("&gt;", ">", text, fixed = TRUE)
  text <- gsub("&quot;", "\"", text, fixed = TRUE)
  text <- gsub("&#39;", "'", text, fixed = TRUE)
  text <- gsub("&#96;", "`", text, fixed = TRUE)
  gsub("&amp;", "&", text, fixed = TRUE) # must come last
}

split_lines <- function(text) {
  # The sentinel keeps trailing empty lines, which strsplit() would drop.
  utils::head(strsplit(paste0(text, "\n."), "\n", fixed = TRUE)[[1]], -1)
}

# For each of the Markdown `lines`, "open" or "close" for a code block's
# fences, "code" inside one, and "" outside. A block is closed by a fence of
# the same character, at least as long as the one that opened it.
code_block_lines <- function(lines) {
  kinds <- character(length(lines))
  fence <- NULL # The open code block's fence, if any.
  for (i in seq_along(lines)) {
    if (!is.null(fence)) {
      closing <- grepl(paste0("^\\s*", fence, "+\\s*$"), lines[[i]])
      kinds[[i]] <- if (closing) "close" else "code"
      if (closing) fence <- NULL
    } else if (grepl("^\\s*(```+|~~~+)", lines[[i]])) {
      fence <- sub("^\\s*(```+|~~~+).*$", "\\1", lines[[i]])
      kinds[[i]] <- "open"
    }
  }
  kinds
}

# `lines` as its YAML `front_matter` (`---` to `---` or `...`, after any
# blank lines, empty if there's none) and the `body` after it.
split_front_matter <- function(lines) {
  first <- which(nzchar(trimws(lines)))[1]
  end <- 0L
  if (!is.na(first) && trimws(lines[first]) == "---") {
    close <- grep("^(---|\\.\\.\\.)\\s*$", lines[-seq_len(first)])[1]
    if (!is.na(close)) end <- first + close
  }
  list(
    front_matter = lines[seq_len(end)],
    body = lines[seq_len(length(lines) - end) + end]
  )
}
