# Changelog

## RESIDE 0.4.0

### Breaking changes

- [`export_empty_cor_matrix()`](https://hehta.github.io/RESIDE/reference/export_empty_cor_matrix.md)
  and
  [`import_cor_matrix()`](https://hehta.github.io/RESIDE/reference/import_cor_matrix.md)
  are defunct, correlations are now specified using the `correlations`
  parameter of
  [`synthesise_data()`](https://hehta.github.io/RESIDE/reference/synthesise_data.md)
  with the new
  [`correlation()`](https://hehta.github.io/RESIDE/reference/correlation.md)
  function.
- The `correlation_matrix` parameter of
  [`synthesise_data()`](https://hehta.github.io/RESIDE/reference/synthesise_data.md)
  is no longer supported.
- [`synthesise_data()`](https://hehta.github.io/RESIDE/reference/synthesise_data.md)
  returns a named list of data frames for marginals obtained from
  multiple data frames.
- Requires R \>= 4.1.0.

### New features

- Marginal distributions can be obtained from multiple related data
  frames, linked by a subject identifier, using
  [`get_marginal_distributions()`](https://hehta.github.io/RESIDE/reference/get_marginal_distributions.md).
- [`correlation()`](https://hehta.github.io/RESIDE/reference/correlation.md)
  creates a correlation between two variables, categorical variables are
  correlated using a single category (`factor_name.x` / `factor_name.y`)
  and variables of multiple data frames using `df_name`, `df_name.x` and
  `df_name.y`, including correlations between data frames.
- [`summary()`](https://rdrr.io/r/base/summary.html) method for RESIDE
  objects, a higher level summary than
  [`print()`](https://rdrr.io/r/base/print.html).
- [`print()`](https://rdrr.io/r/base/print.html) shows the 10 most
  common categories of each categorical variable, use
  `print(x, full = TRUE)` to print every category.
- New vignette, a worked example using multiple tables from the
  pharmaversesdtm package.

### Bug fixes

- [`synthesise_data()`](https://hehta.github.io/RESIDE/reference/synthesise_data.md)
  keeps whitespace in categories.
- [`synthesise_data()`](https://hehta.github.io/RESIDE/reference/synthesise_data.md)
  maintains the number of subjects of each data frame.
- Categorical marginal distributions are maintained when synthesising
  with correlations.

## RESIDE 0.3.3

- Update to vignette
- Add logo and update README

## RESIDE 0.3.2

CRAN release: 2024-10-17

- Further fixes for CRAN Submission

## RESIDE 0.3.1

- Fixes for CRAN Submission

## RESIDE 0.3.0

- Initial CRAN submission.
