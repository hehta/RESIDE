# RESIDE 0.4.0

## Breaking changes

* `export_empty_cor_matrix()` and `import_cor_matrix()` are defunct,
  correlations are now specified using the `correlations` parameter of
  `synthesise_data()` with the new `correlation()` function.
* The `correlation_matrix` parameter of `synthesise_data()` is no longer
  supported.
* `synthesise_data()` returns a named list of data frames for marginals
  obtained from multiple data frames.
* Requires R >= 4.1.0.

## New features

* Marginal distributions can be obtained from multiple related data frames,
  linked by a subject identifier, using `get_marginal_distributions()`.
* `correlation()` creates a correlation between two variables, categorical
  variables are correlated using a single category (`factor_name.x` /
  `factor_name.y`) and variables of multiple data frames using `df_name`,
  `df_name.x` and `df_name.y`, including correlations between data frames.
* `summary()` method for RESIDE objects, a higher level summary than `print()`.
* `print()` shows the 10 most common categories of each categorical variable,
  use `print(x, full = TRUE)` to print every category.
* New vignette, a worked example using multiple tables from the
  pharmaversesdtm package.

## Bug fixes

* `synthesise_data()` keeps whitespace in categories.
* `synthesise_data()` maintains the number of subjects of each data frame.
* Categorical marginal distributions are maintained when synthesising with
  correlations.

# RESIDE 0.3.3

* Update to vignette
* Add logo and update README

# RESIDE 0.3.2

* Further fixes for CRAN Submission

# RESIDE 0.3.1

* Fixes for CRAN Submission

# RESIDE 0.3.0

* Initial CRAN submission.