# ---- the Quarto project ---------------------------------------------------------
#
# build_site() assembles a Quarto website project in _quarto/ from the package
# sources, then renders it into docs/ (`output-dir: ../docs/`). _quarto/ is
# emptied first on every build, except Quarto's _freeze/ when `freeze = TRUE`.

project_dir <- function(path) file.path(path, "_quarto")

reset_project <- function(path, freeze = FALSE) {
  dir <- project_dir(path)
  files <- list.files(dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (freeze) {
    files <- files[basename(files) != "_freeze"]
  }
  unlink(files, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  invisible()
}

# Copies everything in `from` to the same paths under `to`.
copy_dir <- function(from, to) {
  files <- list.files(from, recursive = TRUE, all.files = TRUE)
  for (dir in unique(file.path(to, dirname(files)))) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.copy(file.path(from, files), file.path(to, files), overwrite = TRUE)
  invisible()
}

# ---- package files ----------------------------------------------------------------

# Vignettes, including vignettes/articles/, are rendered by Quarto itself,
# alongside any images or data files they use. Each gets a source_note() under
# its title, ruled off from the text below like a heading (see
# quartodown.scss), as a reference page's is by its first `##` heading.
copy_vignettes <- function(path) {
  out <- file.path(project_dir(path), "vignettes")
  copy_dir(file.path(path, "vignettes"), out)
  gh_url <- pkg_github_url(path)
  for (vignette in find_vignettes(path)) {
    file <- file.path(out, vignette)
    lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
    note <- source_note(
      file.path("vignettes", vignette),
      gh_url,
      class = "source-note"
    )
    writeLines(
      enc2utf8(insert_after_front_matter(lines, c(note, ""))),
      file,
      useBytes = TRUE
    )
  }
}

# `lines` with `insert` after the YAML front matter (`---` to `---` or `...`),
# or at the top if there's none.
insert_after_front_matter <- function(lines, insert) {
  first <- which(nzchar(trimws(lines)))[1]
  end <- 0L
  if (!is.na(first) && trimws(lines[first]) == "---") {
    close <- which(grepl("^(---|\\.\\.\\.)\\s*$", lines[-seq_len(first)]))[1]
    if (!is.na(close)) end <- first + close
  }
  append(lines, c(if (end > 0) "", insert), after = end)
}

# man/figures/ holds the images README.md and `\figure{}` use. README.Rmd's
# knitr figures can land in README_files/ instead.
copy_figures <- function(path) {
  for (dir in c("man/figures", "README_files")) {
    copy_dir(file.path(path, dir), file.path(project_dir(path), dir))
  }
}

# Markdown files rendered as a page of the same name, from the package root or
# .github/.
community_files <- c("CODE_OF_CONDUCT.md", "CONTRIBUTING.md", "SUPPORT.md")

# The path, relative to `path`, of the community file `name`, or NULL.
community_file_source <- function(path, name) {
  candidates <- c(name, file.path(".github", name))
  found <- candidates[file.exists(file.path(path, candidates))]
  if (length(found) == 0) NULL else found[[1]]
}

# NEWS.md and the community files keep their name; LICENSE.md is the full
# license, and a plain LICENSE file (e.g. the "YEAR:/COPYRIGHT HOLDER:" stub
# of "MIT + file LICENSE") becomes LICENSE-text.md.
import_markdown_files <- function(path) {
  out <- project_dir(path)
  read <- function(file) readLines(file, warn = FALSE, encoding = "UTF-8")
  write <- function(lines, name) {
    writeLines(enc2utf8(lines), file.path(out, name), useBytes = TRUE)
  }

  news <- file.path(path, "NEWS.md")
  if (file.exists(news)) {
    write(link_news(read(news), pkg_github_url(path)), "NEWS.md")
  }
  for (name in community_files) {
    src <- community_file_source(path, name)
    if (!is.null(src)) write(read(file.path(path, src)), name)
  }
  if (file.exists(file.path(path, "LICENSE.md"))) {
    write(read(file.path(path, "LICENSE.md")), "LICENSE.md")
  }
  if (file.exists(file.path(path, "LICENSE"))) {
    write(
      c(
        "---",
        "title: License",
        "---",
        "",
        "```",
        read(file.path(path, "LICENSE")),
        "```"
      ),
      "LICENSE-text.md"
    )
  }
  invisible()
}

# Links issues and pull requests (`#123`) and GitHub users (`@user`). Code
# blocks are left alone.
link_news <- function(lines, gh_url) {
  if (is.na(gh_url)) {
    return(lines)
  }
  is_fence <- grepl("^\\s*```", lines)
  text <- !(cumsum(is_fence) %% 2 == 1 | is_fence)
  lines[text] <- gsub(
    "(?<![\\w\\[/&#])#(\\d+)\\b",
    sprintf("[#\\1](%s/issues/\\1)", gh_url),
    lines[text],
    perl = TRUE
  )
  lines[text] <- gsub(
    "(^|\\s|\\()@([-\\w]+)",
    "\\1[@\\2](https://github.com/\\2)",
    lines[text],
    perl = TRUE
  )
  lines
}

# ---- rendering --------------------------------------------------------------------

check_quarto <- function() {
  if (is.null(quarto::quarto_path())) {
    cli::cli_abort(c(
      "Quarto (the command-line tool) isn't installed.",
      "i" = "Install it from {.url https://quarto.org/docs/get-started/}."
    ))
  }
}

# `envvars` makes the package available to the pages' R processes, see
# install_envvars() and load_all_envvars().
render_project <- function(path, envvars, quiet = TRUE) {
  docs <- file.path(path, "docs")
  # A stale page would otherwise outlive its source. CNAME (GitHub Pages'
  # custom domain) and hidden files such as .nojekyll stay.
  files <- list.files(docs, full.names = TRUE)
  unlink(files[basename(files) != "CNAME"], recursive = TRUE)

  local_envvars(envvars)
  quarto::quarto_render(
    input = project_dir(path),
    quiet = quiet,
    as_job = FALSE
  )
  invisible()
}

# ---- the package for the pages' code ------------------------------------------
#
# Quarto runs each page's code in a new R process. The package is either
# installed into a temporary library first on its library path, or, with
# `devel = TRUE`, loaded with pkgload::load_all() by an R profile.

install_envvars <- function(path, scratch) {
  lib <- file.path(scratch, "library")
  dir.create(lib)
  pkg <- desc::desc_get_field("Package", file = path)
  cli::cli_inform("Installing {.pkg {pkg}} into a temporary library.")
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD",
      "INSTALL",
      "--no-multiarch",
      "--with-keep.source",
      "-l",
      shQuote(lib),
      shQuote(normalizePath(path))
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    # The log's lines, with braces escaped so cli doesn't interpolate them.
    log <- gsub("([{}])", "\\1\\1", utils::tail(output, 20))
    cli::cli_abort(c(
      "Installing {.pkg {pkg}} failed.",
      "i" = "Fix the installation, or build with {.code devel = TRUE}.",
      structure(log, names = rep(" ", length(log)))
    ))
  }
  c(R_LIBS = paste(c(lib, .libPaths()), collapse = .Platform$path.sep))
}

load_all_envvars <- function(path, scratch) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    cli::cli_abort(c(
      "Building with {.code devel = TRUE} needs the {.pkg pkgload} package.",
      "i" = "Install it with {.code install.packages(\"pkgload\")}."
    ))
  }
  profile <- file.path(scratch, "Rprofile.R")
  writeLines(
    c(
      # R_PROFILE_USER replaces the user's own profile, so it's run first.
      "if (file.exists('.Rprofile')) {",
      "  source('.Rprofile')",
      "} else if (file.exists('~/.Rprofile')) {",
      "  source('~/.Rprofile')",
      "}",
      sprintf(
        "suppressMessages(pkgload::load_all(%s, export_all = FALSE, helpers = FALSE, quiet = TRUE))",
        deparse(normalizePath(path))
      )
    ),
    profile
  )
  c(R_PROFILE_USER = profile)
}

# Evaluates `code` with the temporary library of `envvars` (from
# install_envvars()), if any, first on this R session's library path.
with_libpaths <- function(envvars, code) {
  if (is.na(envvars["R_LIBS"])) {
    return(code)
  }
  old <- .libPaths()
  on.exit(.libPaths(old))
  lib <- strsplit(envvars[["R_LIBS"]], .Platform$path.sep, fixed = TRUE)[[1]]
  .libPaths(c(lib[[1]], old))
  code
}

# Sets `envvars` until the calling function exits.
local_envvars <- function(envvars, envir = parent.frame()) {
  old <- Sys.getenv(names(envvars), unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(envvars))
  defer <- function() {
    unset <- is.na(old)
    Sys.unsetenv(names(old)[unset])
    if (!all(unset)) do.call(Sys.setenv, as.list(old[!unset]))
  }
  do.call(on.exit, list(substitute(defer()), add = TRUE), envir = envir)
  invisible()
}

# The site's own URL: the first DESCRIPTION URL that isn't a source
# repository. NA if there's none.
site_url <- function(path) {
  urls <- tryCatch(desc::desc_get_urls(path), error = function(e) character())
  urls <- urls[!grepl("github\\.com|gitlab\\.com|codeberg\\.org", urls)]
  if (length(urls) == 0) NA_character_ else sub("/+$", "", urls[[1]])
}

# downlit finds a package's reference pages and vignettes through the
# pkgdown.yml at its site's root.
write_pkgdown_yml <- function(path) {
  url <- site_url(path)
  if (is.na(url)) {
    return(invisible())
  }
  vignettes <- find_vignettes(path)
  articles <- as.list(sprintf("%s.html", strip_ext(vignettes)))
  names(articles) <- basename(strip_ext(vignettes))
  yaml::write_yaml(
    list(
      urls = list(
        reference = paste0(url, "/reference"),
        article = paste0(url, "/vignettes")
      ),
      articles = articles
    ),
    file.path(path, "docs", "pkgdown.yml")
  )
}

#' Preview the built site
#'
#' Serves `docs/`, as built by [build_site()], in a web browser, and reloads
#' the page when the site is rebuilt.
#'
#' @inheritParams build_site
#' @return The server, invisibly; see [servr::httw()].
#' @export
#'
#' @examples
#' \dontrun{
#' build_site()
#' preview_site()
#' }
preview_site <- function(path = ".") {
  docs <- file.path(path, "docs")
  if (!file.exists(file.path(docs, "index.html"))) {
    cli::cli_abort(c(
      "No built site found in {.path {docs}}.",
      "i" = "Build it with {.run quartodown::build_site()}."
    ))
  }
  if (!requireNamespace("servr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Previewing the site needs the {.pkg servr} package.",
      "i" = "Install it with {.code install.packages(\"servr\")}."
    ))
  }
  invisible(servr::httw(docs))
}
