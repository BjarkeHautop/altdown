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
# alongside any images or data files they use.
copy_vignettes <- function(path) {
  copy_dir(
    file.path(path, "vignettes"),
    file.path(project_dir(path), "vignettes")
  )
}

# man/figures/ holds the images README.md and `\figure{}` use. README.Rmd's
# knitr figures can land in README_files/ instead.
copy_figures <- function(path) {
  for (dir in c("man/figures", "README_files")) {
    copy_dir(file.path(path, dir), file.path(project_dir(path), dir))
  }
}

# Markdown files rendered as a page of the same name, from the package root or
# .github/, as pkgdown does.
community_files <- c("CODE_OF_CONDUCT.md", "CONTRIBUTING.md", "SUPPORT.md")

# The path, relative to `path`, of the community file `name`, or NULL.
community_file_source <- function(path, name) {
  candidates <- c(name, file.path(".github", name))
  found <- candidates[file.exists(file.path(path, candidates))]
  if (length(found) == 0) NULL else found[[1]]
}

# NEWS.md and the community files keep their name; LICENSE.md is the full
# license, and a plain LICENSE file (e.g. the "YEAR:/COPYRIGHT HOLDER:" stub
# of "MIT + file LICENSE") becomes LICENSE-text.md, as in pkgdown.
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

# Links issues and pull requests (`#123`) and GitHub users (`@user`), as
# pkgdown does. Code blocks are left alone.
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

render_project <- function(path, quiet = TRUE) {
  docs <- file.path(path, "docs")
  # A stale page would otherwise outlive its source. CNAME (GitHub Pages'
  # custom domain) and hidden files such as .nojekyll stay.
  files <- list.files(docs, full.names = TRUE)
  unlink(files[basename(files) != "CNAME"], recursive = TRUE)

  quarto::quarto_render(
    input = project_dir(path),
    quiet = quiet,
    as_job = FALSE
  )
  invisible()
}

# The site's own URL: the first DESCRIPTION URL that isn't a source
# repository. NA if there's none.
site_url <- function(path) {
  urls <- tryCatch(desc::desc_get_urls(path), error = function(e) character())
  urls <- urls[!grepl("github\\.com|gitlab\\.com|codeberg\\.org", urls)]
  if (length(urls) == 0) NA_character_ else sub("/+$", "", urls[[1]])
}

# downlit, which pkgdown, quartodown and Quarto (`code-link`) use to link
# function calls and `vignette()` calls to their docs, finds another
# package's reference pages and vignettes through the pkgdown.yml at its
# site's root: `urls` and, for each vignette, its path under `urls$article`.
write_pkgdown_yml <- function(path) {
  url <- site_url(path)
  if (is.na(url)) {
    return(invisible())
  }
  vignettes <- find_vignettes(path)
  articles <- as.list(paste0(strip_ext(vignettes), ".html"))
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
