# Build script for altdown's own site.
#
# Usage, from the package root:
#   Rscript demo/build-site.R
#
# altdown's site needs a toy API to show off what a documented package looks
# like, so this stages demo/ into the package around altdown::build_site().
# Any other package just calls altdown::build_site() directly.

pkg_path <- "."

# ---- toy API ----
#
# The vignette render happens in a separate process (quarto shells out to
# Rscript per document), so `library(pkg)` there needs an actually installed
# package that exports the toy functions - staging roundtrips through a real
# install: copy the toy R file into R/ and matching man/*.Rd and
# vignettes/*.qmd, add temporary export()s to NAMESPACE.

install_demo_pkg <- function(path = pkg_path, lib) {
  dir.create(lib, showWarnings = FALSE, recursive = TRUE)
  install.packages(path, repos = NULL, type = "source", lib = lib, quiet = TRUE)
  invisible()
}

stage_demo <- function(path = pkg_path) {
  demo_r <- file.path(path, "demo", "demo.R")
  if (!file.exists(demo_r)) {
    return(NULL)
  }

  r_dest <- file.path(path, "R", "zzz-demo.R")
  file.copy(demo_r, r_dest, overwrite = TRUE)

  demo_src <- readLines(demo_r, warn = FALSE)
  rd <- roxygen2::roc_proc_text(
    roxygen2::rd_roclet(),
    paste(demo_src, collapse = "\n")
  )
  man_dir <- file.path(path, "man")
  rd_paths <- file.path(man_dir, names(rd))
  for (nm in names(rd)) {
    writeLines(format(rd[[nm]]), file.path(man_dir, nm))
  }

  aliases <- unlist(lapply(rd_paths, function(rd_file) {
    parsed <- tools::parse_Rd(rd_file)
    tags <- vapply(parsed, function(x) attr(x, "Rd_tag"), character(1))
    vapply(
      parsed[tags == "\\alias"],
      function(x) as.character(x[[1]]),
      character(1)
    )
  }))

  namespace_path <- file.path(path, "NAMESPACE")
  original_namespace <- readLines(namespace_path, warn = FALSE)
  writeLines(
    c(original_namespace, sprintf("export(%s)", aliases)),
    namespace_path
  )

  demo_qmds <- Sys.glob(file.path(path, "demo", "*.qmd"))
  vig_dir <- file.path(path, "vignettes")
  vig_existed <- dir.exists(vig_dir)
  qmd_paths <- character()
  if (length(demo_qmds) > 0) {
    if (!vig_existed) {
      dir.create(vig_dir)
    }
    qmd_paths <- file.path(vig_dir, basename(demo_qmds))
    file.copy(demo_qmds, qmd_paths, overwrite = TRUE)
  }

  lib <- tempfile("altdown-demo-lib-")
  install_demo_pkg(path, lib)

  original_r_libs <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = paste(c(lib, .libPaths()), collapse = .Platform$path.sep))

  list(
    path = path,
    r_dest = r_dest,
    rd_paths = rd_paths,
    qmd_paths = qmd_paths,
    vig_dir = vig_dir,
    vig_existed = vig_existed,
    namespace_path = namespace_path,
    original_namespace = original_namespace,
    lib = lib,
    original_r_libs = original_r_libs
  )
}

unstage_demo <- function(staged) {
  if (is.null(staged)) {
    return(invisible())
  }
  unlink(staged$r_dest)
  unlink(staged$rd_paths)
  unlink(staged$qmd_paths)
  if (!staged$vig_existed && length(list.files(staged$vig_dir)) == 0) {
    unlink(staged$vig_dir, recursive = TRUE)
  }
  writeLines(staged$original_namespace, staged$namespace_path)
  if (is.na(staged$original_r_libs)) {
    Sys.unsetenv("R_LIBS")
  } else {
    Sys.setenv(R_LIBS = staged$original_r_libs)
  }
  unlink(staged$lib, recursive = TRUE)
  invisible()
}


# ---- entry point --------------------------------------------------------------

render_readme_qmd <- function(path = pkg_path) {
  readme_qmd <- file.path(path, "README.qmd")
  if (file.exists(readme_qmd)) {
    quarto::quarto_render(readme_qmd, output_format = "gfm", quiet = TRUE)
  }
  invisible()
}

build_demo_site <- function(path = pkg_path, ...) {
  render_readme_qmd(path)

  staged <- stage_demo(path)
  on.exit(unstage_demo(staged), add = TRUE)

  pkgload::load_all(path, quiet = TRUE)
  altdown::build_site(path = path, ...)
}

if (identical(environment(), globalenv()) && sys.nframe() == 0) {
  build_demo_site()
}
