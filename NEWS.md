# altdown 0.0.0.9000

* New `build_site()` builds the site; the build logic, theme and scripts now
  live in the package instead of being copied into `altdoc/`.
* `use_altdown()` now only writes `altdoc/altdown.yml`, which holds the
  `reference:` grouping (taken over from an existing `altdoc/reference.yml`)
  and optional `articles:` and `quarto:` fields. Its `overwrite` argument is
  gone.
* `build_site()` no longer modifies `README.md`, and lists every vignette other
  than the "Get started" one under "Articles".
* `build_site()` adds reftip tooltips when reftip is installed.
* Initial release.
