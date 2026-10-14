test_that("use_altdown() errors outside a package root", {
  dir <- withr::local_tempdir()
  expect_error(use_altdown(dir), "DESCRIPTION")
})

test_that("use_altdown() writes a starter altdown.yml listing every topic", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  dir.create(file.path(dir, "man"))
  writeLines(
    c("\\name{foo}", "\\alias{foo}", "\\title{Foo}", "\\usage{foo(x)}"),
    file.path(dir, "man", "foo.Rd")
  )

  expect_message(use_altdown(dir), "Wrote")

  expect_identical(list.files(file.path(dir, "altdoc")), "altdown.yml")
  config <- yaml::yaml.load_file(file.path(dir, "altdoc", "altdown.yml"))
  expect_identical(config$reference[[1]]$contents, "foo")
})

test_that("use_altdown() takes `reference:` over from an earlier altdoc/reference.yml", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  dir.create(file.path(dir, "altdoc"))
  writeLines(
    c("## old comment", "", "reference:", "  - title: Grouped", "    contents:", "      - foo"),
    file.path(dir, "altdoc", "reference.yml")
  )

  expect_message(use_altdown(dir), "no longer used")

  config <- yaml::yaml.load_file(file.path(dir, "altdoc", "altdown.yml"))
  expect_identical(config$reference[[1]]$title, "Grouped")
  expect_identical(config$reference[[1]]$contents, "foo")
})

test_that("use_altdown() adds the .gitignore/.Rbuildignore entries the build needs", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))

  use_altdown(dir)

  gitignore <- readLines(file.path(dir, ".gitignore"))
  expect_true(all(
    c(
      "altdoc/freeze.rds",
      "altdoc/pkgdown.yml",
      "_quarto/*",
      "!_quarto/_freeze/",
      file.path("altdoc", generated_files)
    ) %in%
      gitignore
  ))
  expect_false("altdoc/altdown.yml" %in% gitignore)

  rbuildignore <- readLines(file.path(dir, ".Rbuildignore"))
  expect_true(all(c("^docs$", "^altdoc$", "^_quarto$") %in% rbuildignore))
})

test_that("use_altdown() doesn't duplicate .gitignore/.Rbuildignore entries on repeat calls", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  writeLines("some/existing/entry", file.path(dir, ".gitignore"))

  use_altdown(dir)
  use_altdown(dir)

  gitignore <- readLines(file.path(dir, ".gitignore"))
  expect_identical(sum(gitignore == "altdoc/freeze.rds"), 1L)
  expect_identical(sum(gitignore == "some/existing/entry"), 1L)
})

test_that("use_altdown() never overwrites an existing altdown.yml", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  use_altdown(dir)

  config <- yaml::yaml.load_file(file.path(dir, "altdoc", "altdown.yml"))
  expect_identical(config$reference[[1]]$contents, list())

  dir.create(file.path(dir, "man"))
  writeLines(
    c("\\name{bar}", "\\alias{bar}", "\\title{Bar}", "\\usage{bar(x)}"),
    file.path(dir, "man", "bar.Rd")
  )

  expect_message(use_altdown(dir), "Skipping.*altdown\\.yml")
  config <- yaml::yaml.load_file(file.path(dir, "altdoc", "altdown.yml"))
  expect_identical(config$reference[[1]]$contents, list())
})
