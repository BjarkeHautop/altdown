test_that("template files shipped in inst/ match altdown's own altdoc/ (no drift)", {
  # altdoc/ (altdown's own site source) is .Rbuildignore'd, so it only
  # exists in a dev checkout, not in the built package R CMD check runs
  # against - skip there rather than erroring on a missing directory.
  pkg_root <- testthat::test_path("..", "..")
  altdoc_dir <- file.path(pkg_root, "altdoc")
  skip_if_not(dir.exists(altdoc_dir), "altdoc/ not present (not a dev checkout)")

  template_dir <- file.path(pkg_root, "inst", "altdoc-template")

  for (f in c("build-site.R", "altdown.scss", "quarto_website_static.yml")) {
    expect_identical(
      readLines(file.path(template_dir, f)),
      readLines(file.path(altdoc_dir, f)),
      info = sprintf(
        "inst/altdoc-template/%s has drifted from altdoc/%s - update whichever is stale.",
        f, f
      )
    )
  }
})

test_that("use_altdown() errors outside a package root", {
  dir <- withr::local_tempdir()
  expect_error(use_altdown(dir), "DESCRIPTION")
})

test_that("use_altdown() copies the template and writes a starter reference.yml", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  dir.create(file.path(dir, "man"))
  writeLines(
    c("\\name{foo}", "\\alias{foo}", "\\title{Foo}", "\\usage{foo(x)}"),
    file.path(dir, "man", "foo.Rd")
  )

  expect_message(use_altdown(dir), "Wrote")

  out_dir <- file.path(dir, "altdoc")
  expect_true(file.exists(file.path(out_dir, "build-site.R")))
  expect_true(file.exists(file.path(out_dir, "altdown.scss")))
  expect_true(file.exists(file.path(out_dir, "quarto_website_static.yml")))

  reference <- yaml::yaml.load_file(file.path(out_dir, "reference.yml"))
  expect_identical(reference$reference[[1]]$contents, "foo")
})

test_that("use_altdown() doesn't overwrite existing files unless asked", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  use_altdown(dir)

  dir.create(file.path(dir, "man"))
  writeLines(
    c("\\name{bar}", "\\alias{bar}", "\\title{Bar}", "\\usage{bar(x)}"),
    file.path(dir, "man", "bar.Rd")
  )

  expect_message(use_altdown(dir), "Skipping")
  reference <- yaml::yaml.load_file(file.path(dir, "altdoc", "reference.yml"))
  expect_identical(reference$reference[[1]]$contents, list())

  expect_message(use_altdown(dir, overwrite = TRUE), "Wrote")
  reference <- yaml::yaml.load_file(file.path(dir, "altdoc", "reference.yml"))
  expect_identical(reference$reference[[1]]$contents, "bar")
})
