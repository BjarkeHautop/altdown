# ---- Rd macros ------------------------------------------------------------------
#
# altdoc converts each man/*.Rd with `tools::parse_Rd()` and `tools::Rd2HTML()`
# without the package's own Rd macros (`RdMacros:` in DESCRIPTION, such as
# Rdpack's `\insertRef{}`, or man/macros/*.Rd), and without running the
# build-stage `\Sexpr`s those macros expand to, so `\insertRef{key}{pkg}` comes
# out as the bare text "\insertRefkeypkg". So build_site()
# renders each Rd again with them, and writes every section that comes out
# differently to altdoc/altdown-rd-sections.rds, which the pre-render script
# splices into man/*.qmd in place of altdoc's.

rd_sections_file <- "altdown-rd-sections.rds"

# Sections altdoc rewrites into code blocks: splicing in the plain HTML would
# undo that.
rd_sections_keep <- c("Usage", "Examples")

write_rd_sections <- function(path) {
  saveRDS(rd_macro_sections(path), file.path(path, "altdoc", rd_sections_file))
  invisible()
}

has_rd_macros <- function(path) {
  macros_dir <- file.path(path, "man", "macros")
  description <- file.path(path, "DESCRIPTION")
  (file.exists(description) &&
    !is.na(desc::desc_get_field(
      "RdMacros",
      default = NA,
      file = description
    ))) ||
    length(list.files(macros_dir, pattern = "\\.Rd$")) > 0
}

# A list with an entry per man/*.Rd file (named after it, without extension)
# that comes out differently with the package's macros, holding the changed
# sections' lines (named after their headings), ready for a man/*.qmd.
rd_macro_sections <- function(path) {
  if (!has_rd_macros(path)) {
    return(list())
  }
  macros <- tools::loadPkgRdMacros(path)
  rd_files <- list.files(
    file.path(path, "man"),
    pattern = "\\.Rd$",
    full.names = TRUE
  )

  out <- list()
  for (rd_file in rd_files) {
    as_altdoc <- rd_html_sections(rd_file)
    with_macros <- rd_html_sections(rd_file, macros)
    changed <- names(with_macros)[
      !names(with_macros) %in% rd_sections_keep &
        !vapply(
          names(with_macros),
          function(s) identical(with_macros[[s]], as_altdoc[[s]]),
          logical(1)
        )
    ]
    if (length(changed) > 0) {
      topic <- sub("\\.Rd$", "", basename(rd_file))
      out[[topic]] <- with_macros[changed]
    }
  }
  out
}

# Renders `rd_file` to HTML as `altdoc:::.rd2qmd()` does (or, given `macros`,
# with them and with build-stage `\Sexpr`s run) and splits it into its
# `<h3>` sections, each with altdoc's clean-up applied.
rd_html_sections <- function(rd_file, macros = NULL) {
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out))
  suppressWarnings(
    if (is.null(macros)) {
      tools::Rd2HTML(tools::parse_Rd(rd_file), out = out, toc = FALSE)
    } else {
      tools::Rd2HTML(
        tools::parse_Rd(rd_file, macros = macros),
        out = out,
        toc = FALSE,
        stages = c("build", "install", "render")
      )
    }
  )
  lines <- readLines(out, warn = FALSE, encoding = "UTF-8")

  heads <- grep("^<h3>.*</h3>$", lines)
  main_end <- grep("^</main>", lines)[1]
  ends <- c(heads[-1] - 1, if (is.na(main_end)) length(lines) else main_end - 1)
  sections <- Map(
    function(from, to) {
      if (to > from) altdoc_html_cleanup(lines[(from + 1):to]) else character()
    },
    heads,
    ends
  )
  names(sections) <- sub("^<h3>(.*)</h3>$", "\\1", lines[heads])
  sections
}

# The substitutions `altdoc:::.rd2qmd()` applies to every line of the HTML.
altdoc_html_cleanup <- function(x) {
  x <- gsub("\\$", "\\\\$", x)
  x <- gsub(
    "<code class=\"reqn\">(.*?)&gt;(.*?)</code>",
    "<code class=\"reqn\">\\1>\\2</code>",
    x
  )
  x <- gsub(
    "<code class=\"reqn\">(.*?)&lt;(.*?)</code>",
    "<code class=\"reqn\">\\1<\\2</code>",
    x
  )
  x <- gsub("<code class=\"reqn\">(.*?)</code>", "\\$\\1\\$", x)
  x <- gsub("&#8288;", "", x, fixed = TRUE)
  x <- gsub("<p>", "", x, fixed = TRUE)
  gsub("</p>", "", x, fixed = TRUE)
}

# The macros for `tools::parse_Rd()`: R's system macros plus the package's own,
# so it doesn't warn about the package's.
rd_macros <- function(path) {
  system_macros <- file.path(R.home("share"), "Rd", "macros", "system.Rd")
  if (has_rd_macros(path)) {
    tools::loadPkgRdMacros(path)
  } else {
    tools::loadRdMacros(system_macros)
  }
}

# The package's own Rd macros: those loaded for it beyond R's system macros.
package_rd_macros <- function(path) {
  if (!has_rd_macros(path)) {
    return(character())
  }
  system_macros <- tools::loadRdMacros(
    file.path(R.home("share"), "Rd", "macros", "system.Rd")
  )
  setdiff(
    ls(tools::loadPkgRdMacros(path), all.names = TRUE),
    ls(system_macros, all.names = TRUE)
  )
}

# altdoc's `tools::parse_Rd()` warns "unknown macro" at every use of one of the
# package's macros. Those sections are rendered with them anyway, so the
# warnings are muffled.
muffle_package_macro_warnings <- function(expr, macros) {
  # Evaluated now: evaluating it in the handler would load the macros, calling
  # tools::parse_Rd() in the middle of altdoc's, which breaks that one.
  force(macros)
  pattern <- ".*unknown macro '([^']+)'.*"
  withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl(pattern, msg) && sub(pattern, "\\1", msg) %in% macros) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# Replaces the body of each `### <heading>` section of `lines` (a man/*.qmd's
# content) named in `sections` with its lines. Runs in the pre-render script.
splice_rd_sections <- function(lines, sections) {
  for (heading in names(sections)) {
    start <- which(lines == paste("###", heading))[1]
    if (is.na(start)) {
      next
    }
    next_head <- grep("^### ", lines)
    next_head <- next_head[next_head > start][1]
    end <- if (is.na(next_head)) length(lines) else next_head - 1
    after <- if (end < length(lines)) {
      lines[seq(end + 1, length(lines))]
    } else {
      character()
    }
    lines <- c(lines[seq_len(start)], "", sections[[heading]], "", after)
  }
  lines
}
