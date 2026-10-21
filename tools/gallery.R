# Regenerates the screenshots in vignettes/articles/gallery/, which
# vignettes/articles/gallery.qmd shows. Builds this package's site once per
# entry there, with the entry's settings, and screenshots one page of it in
# light and dark mode with headless Chrome.
#
# Run from the package root, with Quarto and Chrome installed:
#
#   Rscript tools/gallery.R           # every entry
#   Rscript tools/gallery.R sidebar   # only these entries
#
# Set CHROME to the browser's path if it isn't found.
#
# An entry is a folder holding `config.yml`, merged over this package's
# _quartodown/_quartodown.yml, and any other files (such as extra.scss),
# copied into _quartodown/. Its screenshots are written next to them as
# light.png and dark.png.

gallery_dir <- "vignettes/articles/gallery"

# The page each entry is shown with, if not the home page.
pages <- c(
  sidebar = "reference/build_site.html",
  custom = "reference/index.html"
)

# Quarto's freeze cache, shared by the builds: only the styling differs
# between entries, so the vignettes' code runs in the first build only.
freeze_cache <- file.path(tempfile("gallery-freeze-"), "_freeze")

window_size <- "1280,800"

chrome <- function() {
  path <- Sys.getenv("CHROME")
  if (nzchar(path)) {
    return(path)
  }
  candidates <- c(
    "google-chrome",
    "google-chrome-stable",
    "chromium",
    "chromium-browser",
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  )
  found <- Sys.which(candidates)
  found <- c(found[nzchar(found)], candidates[file.exists(candidates)])
  if (length(found) == 0) {
    stop("Can't find Chrome; set CHROME to its path.", call. = FALSE)
  }
  found[[1]]
}

# A copy of the package, without its build output, `.git` or the gallery
# (whose page reads the entries, which the copy leaves out).
copy_package <- function(to) {
  files <- list.files(".", recursive = TRUE, all.files = TRUE)
  skip <- "^(\\.git/|docs/|_quarto/|vignettes/articles/gallery(/|\\.qmd$))"
  files <- files[!grepl(skip, files)]
  for (dir in unique(file.path(to, dirname(files)))) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.copy(files, file.path(to, files))
  invisible(to)
}

build_entry <- function(entry) {
  entry_dir <- file.path(gallery_dir, entry)
  pkg <- file.path(tempfile("gallery-"), "pkg")
  copy_package(pkg)
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  config_file <- file.path(pkg, "_quartodown", "_quartodown.yml")
  config <- utils::modifyList(
    yaml::read_yaml(config_file),
    as.list(yaml::read_yaml(file.path(entry_dir, "config.yml")))
  )
  yaml::write_yaml(config, config_file)
  extra <- setdiff(
    list.files(entry_dir),
    c("config.yml", "light.png", "dark.png")
  )
  file.copy(file.path(entry_dir, extra), file.path(pkg, "_quartodown"))

  project <- file.path(pkg, "_quarto")
  dir.create(project)
  if (dir.exists(freeze_cache)) {
    file.copy(freeze_cache, project, recursive = TRUE)
  }
  # `devel` skips installing the package.
  quartodown::build_site(pkg, freeze = TRUE, devel = TRUE, preview = FALSE)
  if (!dir.exists(freeze_cache)) {
    dir.create(dirname(freeze_cache))
    file.copy(
      file.path(project, "_freeze"),
      dirname(freeze_cache),
      recursive = TRUE
    )
  }

  # Over HTTP, not file://, which has no localStorage: the dark screenshot
  # relies on the site storing the browser's preferred color scheme. httpuv
  # serves static files on its own thread, so while R waits for Chrome too.
  port <- httpuv::randomPort()
  server <- httpuv::startServer(
    "127.0.0.1",
    port,
    list(staticPaths = list("/" = file.path(pkg, "docs")))
  )
  on.exit(server$stop(), add = TRUE)
  page <- if (entry %in% names(pages)) pages[[entry]] else "index.html"
  url <- sprintf("http://127.0.0.1:%d/%s", port, page)

  for (mode in c("light", "dark")) {
    status <- system2(
      chrome(),
      c(
        "--headless=new",
        "--disable-gpu",
        "--no-sandbox",
        "--hide-scrollbars",
        paste0("--window-size=", window_size),
        "--virtual-time-budget=3000",
        # 0 is dark, 1 light.
        paste0("--blink-settings=preferredColorScheme=", +(mode == "light")),
        paste0("--user-data-dir=", tempfile("chrome-")),
        paste0("--screenshot=", normalizePath(entry_dir), "/", mode, ".png"),
        url
      ),
      stdout = FALSE,
      stderr = FALSE,
      timeout = 60
    )
    if (status != 0) {
      stop("Chrome failed to screenshot ", url, call. = FALSE)
    }
  }
  message("Wrote ", entry_dir, "/{light,dark}.png")
}

pkgload::load_all(".", quiet = TRUE)
entries <- commandArgs(trailingOnly = TRUE)
if (length(entries) == 0) {
  entries <- list.files(gallery_dir)
  entries <- entries[dir.exists(file.path(gallery_dir, entries))]
}
for (entry in entries) {
  build_entry(entry)
}
