# Synthesise data from marginal distributions

Allows the synthesis of data from marginal distributions obtained from a
Trusted Research Environment (TRE)

## Usage

``` r
synthesise_data(marginals, correlation_matrix = NULL, correlations = NULL, ...)

synthesize_data(marginals, correlation_matrix = NULL, correlations = NULL, ...)
```

## Arguments

- marginals:

  an object of class RESIDE

- correlation_matrix:

  No longer supported, use `correlations`. Default: NULL

- correlations:

  A list of correlations created with the
  [`correlation`](https://hehta.github.io/RESIDE/reference/correlation.md)
  function, Default: NULL

- ...:

  Additional parameters currently none are used.

## Value

a data frame of simulated data, or a named list of data frames for
marginals from multiple data frames.

## Details

This function will synthesise a dataset from marginals imported using
[`import_marginal_distributions`](https://hehta.github.io/RESIDE/reference/import_marginal_distributions.md).
By default the dataset will not contain correlations, however user
specified correlations can be added using the `correlations` parameter,
see
[`correlation`](https://hehta.github.io/RESIDE/reference/correlation.md).
Categorical variables are correlated using a single category, specified
with `factor_name.x` or `factor_name.y`. Correlated variables are
synthesised together, one row per subject, and joined to each data frame
by subject. Correlated variables therefore take a single value per
subject within each data frame. It is not possible to entirely maintain
the marginal distributions when specifying correlations.

## See also

[`correlation`](https://hehta.github.io/RESIDE/reference/correlation.md)

## Examples

``` r
marginals <- get_marginal_distributions(
  IST,
  variables = c("SEX", "AGE", "RSBP", "RATRIAL")
)
df <- synthesise_data(marginals)
df_cor <- synthesise_data(
  marginals,
  correlations = list(
    correlation("AGE", "RSBP", 0.3),
    correlation("SEX", "AGE", -0.2, factor_name.x = "M")
  )
)
```
