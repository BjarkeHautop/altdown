# ---- altdoc/reference.qmd generation ---------------------------------------
#
# altdoc has no pkgdown-style `reference:` field for grouping/ordering the
# function index (see https://github.com/etiennebacher/altdoc/issues/326),
# so we build one ourselves: the `reference:` field of `altdoc/altdown.yml`
# uses the same shape as pkgdown's `_pkgdown.yml` `reference:` field, and
# this reads it plus each man/*.Rd's \title{}/\alias{}/\concept{}/\keyword{}
# to regenerate `altdoc/reference.qmd` before every build.
#
# `contents:` entries support the same selectors pkgdown does: a plain alias
# name, `starts_with()`/`ends_with()`/`contains()`/`matches()` matched
# against every \alias{}, `has_concept()`/`has_keyword()` matched against
# \concept{}/\keyword{} tags, `-` to exclude, and `c()` to combine several
# selectors on one line.

# Reads every man/*.Rd into a topic record - basename (the name
# .render_one_man() uses for the rendered man/<basename>.qmd target),
# \title{}, and every \alias{}/\concept{}/\keyword{} tag - so `reference:`
# entries can select a function by any of its aliases, and selector
# functions have something to match against.
rd_topics <- function(path) {
  rd_files <- fs::dir_ls(file.path(path, "man"), regexp = "\\.Rd$")

  lapply(rd_files, function(rd_file) {
    rd <- tools::parse_Rd(rd_file)
    tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
    tag_text <- function(tag) {
      trimws(vapply(
        rd[tags == tag],
        function(x) as.character(x[[1]]),
        character(1)
      ))
    }

    alias <- tag_text("\\alias")
    names <- usage_names(rd[tags == "\\usage"])
    list(
      basename = fs::path_ext_remove(basename(rd_file)),
      title = trimws(paste(unlist(rd[tags == "\\title"]), collapse = "")),
      alias = alias,
      names = if (length(names) > 0) names else alias,
      concepts = tag_text("\\concept"),
      keywords = tag_text("\\keyword")
    )
  })
}

# ---- index names from \usage ------------------------------------------------
#
# A topic's index row lists every function in
# its \usage section, side by side: `f()` for a function, `name` for data or
# an infix operator, `generic(<class>)` for a method (dropped when the
# generic is documented in the same topic). A topic without a usage, such as
# a *-package topic, falls back to its aliases.

# Turns the Rd markup of a \usage section into parseable R code, writing
# \method{generic}{class} as S3method(`generic`, `class`).
usage_code <- function(x) {
  if (is.character(x) && is.null(attr(x, "Rd_tag"))) {
    return(paste(x, collapse = ""))
  }
  collapse <- function(x) {
    paste(vapply(x, usage_code, character(1)), collapse = "")
  }
  tag <- attr(x, "Rd_tag")
  switch(
    if (is.null(tag)) "" else tag,
    TEXT = ,
    RCODE = ,
    VERB = paste(as.character(x), collapse = ""),
    COMMENT = "",
    "\\dots" = ,
    "\\ldots" = "...",
    "\\method" = ,
    "\\S3method" = sprintf(
      "S3method(`%s`, `%s`)",
      usage_code(x[[1]]),
      usage_code(x[[2]])
    ),
    "\\S4method" = sprintf(
      "S4method(`%s`, list(%s))",
      usage_code(x[[1]]),
      paste0("`", strsplit(usage_code(x[[2]]), ",")[[1]], "`", collapse = ",")
    ),
    collapse(x)
  )
}

is_infix <- function(name) {
  grepl("^%.*%$", name) ||
    name %in%
      c(
        "+",
        "-",
        "*",
        "^",
        "/",
        "==",
        ">",
        "<",
        "!=",
        "<=",
        ">=",
        "&",
        "|",
        "[[",
        "[",
        "$"
      )
}

# Returns list(type, name, signature) for one usage expression, with type
# "data", "fun", "s3" or "s4", or NULL for anything else.
usage_type <- function(x) {
  if (is.symbol(x)) {
    return(list(type = "data", name = as.character(x)))
  }
  if (!is.call(x)) {
    return(NULL)
  }
  if (identical(x[[1]], quote(data))) {
    return(list(type = "data", name = as.character(x[[2]])))
  }
  replacement <- identical(x[[1]], quote(`<-`))
  if (replacement) {
    x <- x[[2]]
    if (!is.call(x)) return(NULL)
  }

  fun <- x[[1]]
  out <- if (is.symbol(fun)) {
    list(type = "fun", name = as.character(fun))
  } else if (identical(fun[[1]], quote(S3method))) {
    list(
      type = "s3",
      name = as.character(fun[[2]]),
      signature = as.character(fun[[3]])
    )
  } else if (identical(fun[[1]], quote(S4method))) {
    sig <- vapply(as.list(fun[[3]])[-1], as.character, character(1))
    list(type = "s4", name = as.character(fun[[2]]), signature = sig)
  } else if (
    identical(fun[[1]], quote(`::`)) || identical(fun[[1]], quote(`:::`))
  ) {
    list(type = "fun", name = as.character(fun[[3]]))
  } else {
    return(NULL)
  }
  if (replacement) {
    out$name <- paste0(out$name, "<-")
  }
  out
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

short_name <- function(u) {
  name <- escape_html(u$name)
  if (make.names(u$name) != u$name) {
    name <- paste0("`", name, "`")
  }
  switch(
    u$type,
    data = name,
    fun = if (is_infix(u$name)) name else paste0(name, "()"),
    paste0(
      name,
      "(",
      paste0("<i>&lt;", escape_html(u$signature), "&gt;</i>", collapse = ","),
      ")"
    )
  )
}

# `usage` is the (possibly empty) list of \usage sections of one Rd file.
usage_names <- function(usage) {
  if (length(usage) == 0) {
    return(character())
  }
  code <- paste(vapply(usage, usage_code, character(1)), collapse = "\n")
  exprs <- tryCatch(
    as.list(parse(text = code, keep.source = FALSE)),
    error = function(e) {
      cli::cli_warn("Failed to parse usage: {.code {code}}")
      list()
    }
  )
  funs <- Filter(Negate(is.null), lapply(exprs, usage_type))
  if (length(funs) == 0) {
    return(character())
  }

  # Drop methods for generics documented in the same topic.
  name <- vapply(funs, `[[`, character(1), "name")
  type <- vapply(funs, `[[`, character(1), "type")
  self_meth <- name %in% name[type == "fun"] & type %in% c("s3", "s4")
  unique(vapply(funs[!self_meth], short_name, character(1)))
}

# Builds the environment `contents:` entries are evaluated in: every
# \alias{} bound to its topic's integer position, plus the selector
# functions, mirroring pkgdown's `match_env()`.
match_env <- function(topics) {
  env <- new.env(parent = emptyenv())
  assign("-", function(x) -x, envir = env)
  assign("c", function(...) c(...), envir = env)

  alias_vecs <- lapply(topics, `[[`, "alias")
  for (i in seq_along(topics)) {
    for (alias in alias_vecs[[i]]) {
      assign(alias, i, envir = env)
    }
  }

  any_alias <- function(f) {
    which(vapply(alias_vecs, function(x) any(f(x)), logical(1)))
  }

  assign(
    "starts_with",
    function(x) any_alias(function(a) grepl(paste0("^", x), a)),
    envir = env
  )
  assign(
    "ends_with",
    function(x) any_alias(function(a) grepl(paste0(x, "$"), a)),
    envir = env
  )
  assign(
    "contains",
    function(x) any_alias(function(a) grepl(x, a, fixed = TRUE)),
    envir = env
  )
  assign("matches", function(x) any_alias(function(a) grepl(x, a)), envir = env)
  assign(
    "has_concept",
    function(x) {
      which(vapply(topics, function(t) any(t$concepts == x), logical(1)))
    },
    envir = env
  )
  assign(
    "has_keyword",
    function(x) {
      which(vapply(topics, function(t) any(t$keywords %in% x), logical(1)))
    },
    envir = env
  )

  env
}

match_eval <- function(string, env) {
  # Early return in case `string` is already a known alias verbatim.
  literal <- mget(string, envir = env, ifnotfound = list(NULL))[[1]]
  if (is.numeric(literal)) {
    return(as.integer(literal))
  }

  expr <- tryCatch(str2lang(string), error = function(e) NULL)
  if (is.null(expr)) {
    cli::cli_abort(
      "altdoc/altdown.yml `reference:` entry {.val {string}} must be valid R code."
    )
  }

  if (is.symbol(expr)) {
    val <- mget(as.character(expr), envir = env, ifnotfound = list(NULL))[[1]]
    if (!is.numeric(val)) {
      cli::cli_abort(
        "altdoc/altdown.yml `reference:` entry {.val {string}} must be a known \\alias{{}} or selector."
      )
    }
    as.integer(val)
  } else {
    tryCatch(
      as.integer(eval(expr, envir = env)),
      error = function(e) {
        cli::cli_abort(
          "altdoc/altdown.yml `reference:` entry {.val {string}} failed to evaluate.",
          parent = e
        )
      }
    )
  }
}

select_topics <- function(match_strings, topics) {
  if (length(match_strings) == 0) {
    return(integer())
  }

  env <- match_env(topics)
  indexes <- lapply(match_strings, match_eval, env = env)

  all_sign <- function(x, text) {
    if (all(x > 0)) {
      return("+")
    }
    if (all(x < 0)) {
      return("-")
    }
    cli::cli_abort(
      "altdoc/altdown.yml `reference:` entry {.val {text}} must be all positive or all negative."
    )
  }

  sign1 <- all_sign(indexes[[1]], match_strings[[1]])
  sel <- switch(sign1, "+" = integer(), "-" = seq_along(topics))

  for (i in seq_along(indexes)) {
    sign <- all_sign(indexes[[i]], match_strings[[i]])
    sel <- switch(
      sign,
      "+" = union(sel, indexes[[i]]),
      "-" = setdiff(sel, -indexes[[i]])
    )
  }
  sel
}

build_reference_qmd <- function(path, config = list()) {
  out_path <- file.path(path, "altdoc", "reference.qmd")
  topics <- rd_topics(path)

  # Without a `reference:` field, list every topic in one section.
  if (is.null(config$reference)) {
    config$reference <- list(list(
      title = "All functions",
      contents = vapply(topics, function(t) t$alias[[1]], character(1))
    ))
  }

  block_indexes <- lapply(
    config$reference,
    function(block) select_topics(as.character(block$contents), topics)
  )

  unlisted <- setdiff(seq_along(topics), unique(unlist(block_indexes)))
  if (length(unlisted) > 0) {
    unlisted_names <- vapply(
      topics[unlisted],
      function(t) t$alias[[1]],
      character(1)
    )
    cli::cli_warn(
      "man/*.Rd defines {.val {unlisted_names}}, which {?is/are} not listed under `reference:` in altdoc/altdown.yml and won't appear on the reference page."
    )
  }

  sections <- Map(
    function(block, idx) {
      entries <- vapply(
        idx,
        function(i) {
          topic <- topics[[i]]
          links <- sprintf(
            '<code><a href="man/%s.qmd">%s</a></code>',
            topic$basename,
            topic$names
          )
          sprintf(
            "<dt>%s</dt>\n<dd>%s</dd>",
            paste(links, collapse = " "),
            topic$title
          )
        },
        character(1)
      )
      c(
        paste0("## ", block$title),
        "",
        if (!is.null(block$desc)) c(trimws(block$desc), ""),
        '<dl class="ref-index">',
        paste(entries, collapse = "\n\n"),
        "</dl>",
        ""
      )
    },
    config$reference,
    block_indexes
  )

  lines <- c('---', 'title: "Function reference"', '---', "", unlist(sections))
  lines <- lines[-length(lines)] # drop trailing blank line
  writeLines(lines, out_path)
}
