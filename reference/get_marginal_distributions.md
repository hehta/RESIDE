# Generate Marginal Distributions for a given data frame

Generate Marginal Distributions from a given data frame with options to
specify which variables to use.

## Usage

``` r
get_marginal_distributions(
  df,
  subject_identifier = "",
  variables = c(),
  print = FALSE,
  retype = TRUE
)
```

## Arguments

- df:

  Data frame or a `"list"` of data frames to get the marginal
  distributions from

- subject_identifier:

  (Optional) Subject identifier required if a list of data frames is
  provided, Default: ""

- variables:

  (Optional) variable (columns) to select, Default: c()

- print:

  Whether to print the marginal distributions to the console, Default:
  FALSE

- retype:

  Whether to re-type the data frame, Default: TRUE

## Value

A list of marginal distributions of an S3 RESIDE Class

## Details

A function to generate marginal distributions from a given data frame,
depending on the variable type the marginals will differ, for binary
variables a mean and number of missing is generated for continuous
variables, they are first transformed and both mean and sd of the
transformed variables are stored along with the quantile mapping for
back transformation. For categorical variables, the number of each
category is stored, missing values are categorise as "missing".

## See also

[`export_marginal_distributions`](https://hehta.github.io/RESIDE/reference/export_marginal_distributions.md)

## Examples

``` r
marginal_distributions <- get_marginal_distributions(
  IST,
  variables = c(
    "SEX",
    "AGE",
    "ID14",
    "RSBP",
    "RATRIAL"
  )
)
```
