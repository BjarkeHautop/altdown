# ---- vignettes in the navbar --------------------------------------------------
#
# Vignettes are identified by their path relative to vignettes/, e.g.
# "getting-started.qmd" or "articles/technical.qmd". altdoc renders both
# top-level vignettes and those in vignettes/articles/.

vignette_pattern <- "\\.(qmd|[Rr]md)$"

# Top-level vignettes first, then vignettes/articles/, each sorted.
find_vignettes <- function(path) {
  vig_dir <- file.path(path, "vignettes")
  top <- sort(list.files(vig_dir, pattern = vignette_pattern))
  articles <- sort(list.files(file.path(vig_dir, "articles"), pattern = vignette_pattern))
  c(top, file.path("articles", articles))
}

# The top-level vignette named after the package, or the only top-level
# vignette if there's just one. NULL if neither exists.
find_getting_started_vignette <- function(path, vignettes = find_vignettes(path)) {
  top <- vignettes[dirname(vignettes) == "."]
  if (length(top) == 1) {
    return(top)
  }

  pkg_name <- desc::desc_get_field("Package", file = path)
  named <- top[strip_ext(top) == pkg_name]
  if (length(named) == 1) {
    return(named)
  }

  if (length(top) > 1) {
    cli::cli_inform(c(
      "No \"Get started\" link: found several top-level vignettes and none is named {.file {pkg_name}.qmd}.",
      "i" = "They're all listed under \"Articles\" instead."
    ))
  }
  NULL
}

# `order` is the `articles:` field of altdown.yml: vignette paths relative to
# vignettes/, without extension.
select_articles <- function(vignettes, order = NULL) {
  if (is.null(order)) {
    return(vignettes)
  }

  order <- as.character(unlist(order))
  keys <- strip_ext(vignettes)
  unknown <- setdiff(order, keys)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "altdoc/altdown.yml `articles:` lists {.val {unknown}}, which {?is not a vignette/are not vignettes}.",
      "i" = "Known vignettes: {.val {keys}}."
    ))
  }

  unlisted <- setdiff(keys, order)
  if (length(unlisted) > 0) {
    cli::cli_warn(
      "{.val {unlisted}} {?is/are} not listed under `articles:` in altdoc/altdown.yml and won't appear in the navbar."
    )
  }

  vignettes[match(order, keys)]
}

strip_ext <- function(x) sub("\\.[^./]*$", "", x)

vignette_title <- function(vignette_path) {
  fallback <- strip_ext(basename(vignette_path))

  lines <- readLines(vignette_path, warn = FALSE, n = 40)
  fence <- which(lines == "---")
  if (length(fence) < 2) {
    return(fallback)
  }

  header <- lines[(fence[1] + 1):(fence[2] - 1)]
  title_line <- grep("^title:", header, value = TRUE)
  if (length(title_line) == 0) {
    return(fallback)
  }

  gsub('^title:\\s*"?|"?\\s*$', "", trimws(title_line[1]))
}
