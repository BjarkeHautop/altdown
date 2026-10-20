# ---- Rd macros ------------------------------------------------------------------
#
# Rd files are parsed with the package's own Rd macros (`RdMacros:` in
# DESCRIPTION, such as Rdpack's `\insertRef{}`, or man/macros/*.Rd) as well
# as R's system ones, as `R CMD build` does.

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

# The macros for `tools::parse_Rd()`: R's system macros plus the package's own.
rd_macros <- function(path) {
  system_macros <- file.path(R.home("share"), "Rd", "macros", "system.Rd")
  if (has_rd_macros(path)) {
    tools::loadPkgRdMacros(path)
  } else {
    tools::loadRdMacros(system_macros)
  }
}
