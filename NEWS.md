# RESIDE 0.5.0

## Breaking changes

* `get_marginal_distributions()` merges categories with fewer than
  `min_category_n` rows (default 10) into a single `"(Other)"` category, to
  avoid disclosing rare values. `"(Other)"` appears as a value in synthesised
  data. Set `min_category_n = 0` to keep every category. Marginals of small
  data sets may differ from previous versions.

## Bug fixes

* `synthesise_data()` no longer copies whole rows over other rows when adding
  missing values to binary and continuous variables. Previously, rows were
  duplicated or lost and missing values were added to the wrong rows,
  including the subject identifier when synthesising with correlations.
  Synthetic data generated with earlier versions from marginals with missing
  values should be regenerated.
* `synthesise_data()` only removes the `"missing"` and `"NA's"` placeholder
  categories when they are the whole value, previously `"missing"` was
  removed from within values, e.g. `"missing dose"` became `" dose"`.
* `get_marginal_distributions()` keeps more than 100 categories, previously
  the remaining categories were merged into `"(Other)"`.
* `get_marginal_distributions()` only reports columns as common to multiple
  data frames when their values match for the shared subjects, previously
  any numeric column with the same name was reported as common, as were
  columns of data frames without shared subjects.
* `synthesise_data()` keeps the types of columns common to multiple data
  frames, previously every common column was converted to character when any
  of them was character, e.g. a numeric `age` became `"34"`.

## Performance

* `get_marginal_distributions()` and `synthesise_data()` are considerably
  faster and use less memory for large data, e.g. 500,000 rows reduced from
  21 to 10 seconds and from 19 to 2.5 seconds respectively:
  * Dates are converted using vectorised parsing of each distinct value.
  * The maximum number of decimal places of continuous variables is
    calculated once per distinct value, skipping whole numbers.
  * Categorical variables are sampled directly, rather than using a
    probability matrix of every row and category, which could run out of
    memory for large data with many categories.
  * When synthesising with correlations, rows with more than one correlated
    category selected are resolved without a loop over each row.

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