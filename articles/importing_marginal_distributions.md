# Importing Marginal Distributions

Marginal distributions provided by a TRE should contain the following
files:

- *binary_variables.csv*
- *categorical_variables.csv*
- *continuous_variables.csv*
- *continuous_quantiles.csv*
- *summary.csv*

Depending on the data from the TRE some of the files may not be present.

The marginal distributions can be imported using the
[`import_marginal_distributions()`](https://hehta.github.io/RESIDE/reference/import_marginal_distributions.md)
function. By default the marginal distributions will be imported from
the current working directory:

``` r

library(RESIDE)
marginals <- import_marginal_distributions()
```

The folder of the marginals can be changes using the `folder_path`
parameter of the
[`import_marginal_distributions()`](https://hehta.github.io/RESIDE/reference/import_marginal_distributions.md)
function:

``` r

library(RESIDE)
marginals <- import_marginal_distributions(
  folder_path = "/Users/ryan/marginals"
)
```

Additionally any of the file names can be changed using the relevant
[`import_marginal_distributions()`](https://hehta.github.io/RESIDE/reference/import_marginal_distributions.md)

parameters:

- `binary_variables_file`
- `categorical_variables_file`
- `continuous_variables_file`
- `continuous_quantiles_file`
- `summary_file`

However this is generally not advised.
