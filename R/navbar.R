# ---- navbar -------------------------------------------------------------------
#
# `navbar:` in quartodown.yml lists the items on the navbar's `left` and
# `right`. An item is the name of a built-in one below, or a Quarto navbar
# item. A built-in item the package has nothing for (e.g. "news" without
# NEWS.md) is left out.

builtin_navbar_items <- c("intro", "reference", "articles", "news", "github")

default_navbar <- list(
  left = list("intro", "reference", "articles", "news"),
  right = list("github")
)

# A list of `left` and `right` Quarto navbar items.
navbar_items <- function(path, config = list()) {
  layout <- default_navbar
  for (side in names(config$navbar)) {
    if (!side %in% names(default_navbar)) {
      cli::cli_abort(
        "_quartodown.yml `navbar:` has {.field {side}}, but only {.field left} and {.field right}."
      )
    }
    layout[[side]] <- as.list(config$navbar[[side]])
  }

  # Other items first: "Get started" and "Articles" skip the vignettes they
  # link.
  vignettes <- find_vignettes(path)
  layout <- lapply(layout, lapply, function(item) {
    if (is_builtin_item(item)) item else nav_item(item, path, vignettes)
  })
  items <- unlist(layout, recursive = FALSE)
  used <- unlist(Filter(is.character, items))
  listed <- nav_vignettes(Filter(Negate(is.character), items))

  builtins <- list()
  if ("intro" %in% used) {
    intro <- find_getting_started_vignette(path, setdiff(vignettes, listed))
    if (!is.null(intro)) {
      builtins$intro <- nav_vignette(path, intro, text = "Get started")
      listed <- c(listed, intro)
    }
  }
  if ("reference" %in% used) {
    builtins$reference <- list(text = "Reference", file = "reference/index.qmd")
  }
  unlisted <- setdiff(vignettes, listed)
  if ("articles" %in% used) {
    if (length(unlisted) > 0) {
      builtins$articles <- list(
        text = "Articles",
        menu = lapply(unlisted, nav_vignette, path = path)
      )
    }
  } else if (length(unlisted) > 0) {
    cli::cli_warn(c(
      "{.val {strip_ext(unlisted)}} {?is/are} not in the navbar.",
      "i" = "List vignettes under `navbar:` in _quartodown.yml, or add {.val articles} for those no other item links."
    ))
  }
  if ("news" %in% used && file.exists(file.path(path, "NEWS.md"))) {
    builtins$news <- list(text = "Changelog", file = "NEWS.md")
  }
  if ("github" %in% used) {
    gh_url <- pkg_github_url(path)
    if (!is.na(gh_url)) {
      builtins$github <- list(
        icon = "github",
        href = gh_url,
        `aria-label` = "GitHub"
      )
    }
  }

  lapply(layout, function(side) {
    side <- lapply(side, function(item) {
      if (is.character(item)) builtins[[item]] else item
    })
    Filter(Negate(is.null), side)
  })
}

is_builtin_item <- function(item) {
  is.character(item) && length(item) == 1 && item %in% builtin_navbar_items
}

# A `navbar:` item as a Quarto navbar item. A string is a vignette (its path
# relative to vignettes/, without extension); in a `menu:`, a string ending in
# "/" is every vignette in that folder. Other items are Quarto's own.
nav_item <- function(item, path, vignettes) {
  if (is.character(item) && length(item) == 1) {
    return(nav_vignette(path, match_vignette(item, vignettes)))
  }
  if (!is.null(item$menu)) {
    item$menu <- unlist(
      lapply(as.list(item$menu), function(entry) {
        if (is.character(entry) && endsWith(entry, "/")) {
          lapply(match_folder(entry, vignettes), nav_vignette, path = path)
        } else {
          list(nav_item(entry, path, vignettes))
        }
      }),
      recursive = FALSE
    )
  }
  item
}

nav_vignette <- function(path, vignette, text = NULL) {
  if (is.null(text)) {
    text <- vignette_title(file.path(path, "vignettes", vignette))
  }
  list(text = text, file = file.path("vignettes", vignette))
}

# The vignettes the navbar items link to.
nav_vignettes <- function(items) {
  files <- as.character(unlist(lapply(items, function(item) {
    c(item$file, unlist(lapply(item$menu, function(m) m$file)))
  })))
  sub("^vignettes/", "", files[startsWith(files, "vignettes/")])
}

match_vignette <- function(name, vignettes) {
  hit <- vignettes[strip_ext(vignettes) == strip_ext(name)]
  if (length(hit) == 0) {
    cli::cli_abort(c(
      "_quartodown.yml `navbar:` lists {.val {name}}, which is not a vignette.",
      "i" = "Known vignettes: {.val {strip_ext(vignettes)}}.",
      "i" = "Built-in items: {.val {builtin_navbar_items}}."
    ))
  }
  hit[[1]]
}

match_folder <- function(folder, vignettes) {
  hit <- vignettes[startsWith(vignettes, folder)]
  if (length(hit) == 0) {
    cli::cli_abort(
      "_quartodown.yml `navbar:` lists {.val {folder}}, but {.path vignettes/{folder}} has no vignettes."
    )
  }
  hit
}
