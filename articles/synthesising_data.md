# Synthesising Data from Marginal Distributions

Data is synthesised by sampling from a multivariate cumulative
distribution (Copula), using the `simstudy` package.

## Without Correlations

Data can be synthesised from marginal distributions using the
[`synthesise_data()`](https://hehta.github.io/RESIDE/reference/synthesise_data.md)
function:

``` r

library(RESIDE)
marginals <- import_marginal_distributions()
simulated_data <- synthesise_data(marginals)
```

## With correlations

User specified correlations can be added to the synthesised data by
supplying a list of correlations to the `correlations` parameter. Each
correlation is created with the
[`correlation()`](https://hehta.github.io/RESIDE/reference/correlation.md)
function, giving the names of the two variables and the correlation
(rho) between them:

``` r

library(RESIDE)
marginals <- import_marginal_distributions()
simulated_data <- synthesise_data(
  marginals,
  correlations = list(
    correlation("AGE", "RSBP", 0.3)
  )
)
```

Correlations should be between -1 and 1 and must be consistent with each
other (the resulting correlation matrix must be positive semi definite),
otherwise
[`synthesise_data()`](https://hehta.github.io/RESIDE/reference/synthesise_data.md)
will produce an error.

### Categorical variables

Categorical variables are correlated using a single category, specified
with the `factor_name.x` or `factor_name.y` parameters for the first or
second variable respectively. For example to correlate male patients
(`SEX` of `M`) with age:

``` r

simulated_data <- synthesise_data(
  marginals,
  correlations = list(
    correlation("SEX", "AGE", -0.2, factor_name.x = "M"),
    correlation("RATRIAL", "RSBP", 0.1, factor_name.x = "Y")
  )
)
```

A categorical variable without a factor name will produce an error.

### Multiple tables

When the marginals were obtained from multiple data frames, variables
that are present in more than one data frame (other than the common
columns) must specify which data frame they belong to. Use `df_name`
when both variables belong to the same data frame, or `df_name.x` and
`df_name.y` when they belong to different data frames:

``` r

simulated_data <- synthesise_data(
  marginals,
  correlations = list(
    # Both variables in the same data frame
    correlation("DOMAIN", "AESTDY", 0.2, df_name = "ae", factor_name.x = "AE"),
    # Variables in different data frames
    correlation(
      "SEX",
      "AESEV",
      0.3,
      df_name.x = "dm",
      df_name.y = "ae",
      factor_name.x = "M",
      factor_name.y = "SEVERE"
    )
  )
)
```

A data frame name specified for a variable that only appears in a single
data frame, but not that data frame, will be ignored with a warning.

Correlated variables are synthesised together, with one row per subject,
and joined to each data frame by subject. Correlations between data
frames are therefore between subjects, and a correlated variable takes a
single value for each subject within a data frame.

**NB** The correlation specified is that of the underlying multivariate
normal distribution (Copula), the correlation observed in the
synthesised data will be weaker, particularly for binary and categorical
variables. It is not possible to entirely maintain all the marginal
distributions when specifying correlations, this is a known limitation
and is not likely to change.
