# Import Marginal Distributions

Import the marginal distribution as exported from a Trusted Research
Environment (TRE)

## Usage

``` r
import_marginal_distributions(
  folder_path = ".",
  binary_variables_file = "",
  categorical_variables_file = "",
  continuous_variables_file = "",
  summary_file = "summary.csv"
)
```

## Arguments

- folder_path:

  Where the marginal distribution files are located, Default: '.' see
  details.

- binary_variables_file:

  filename for the binary_variables file, Default: ” see details.

- categorical_variables_file:

  filename for the categorical variables file , Default: ” see details.

- continuous_variables_file:

  filename for the continuous variables file, Default: ” see details.

- summary_file:

  filename for the summary file, Default: 'summary.csv' see details.

## Value

Returns an object of a RESIDE class

## Details

This function will import marginal distributions as generated within a
Trusted Research Environment (TRE) using the function
[`export_marginal_distributions`](https://hehta.github.io/RESIDE/reference/export_marginal_distributions.md).
The folder_path allows the path of the files provided by the TRE to be
imported, this will default to the current working directory. The file
parameters will provide the default file names if no filenames are
specified.

## See also

[`synthesise_data`](https://hehta.github.io/RESIDE/reference/synthesise_data.md)

## Examples

``` r
# Export marginal distributions to a temporary folder
folder_path <- file.path(tempdir(), "marginals")
export_marginal_distributions(
  get_marginal_distributions(
    IST,
    variables = c("SEX", "AGE", "RSBP", "RATRIAL")
  ),
  folder_path = folder_path,
  create_folder = TRUE,
  force = TRUE
)
#> Exporting  Categorical variables to:  /tmp/RtmpMyIM1J/marginals/categorical_variables.csv
#> Exporting  Continuous variables to:  /tmp/RtmpMyIM1J/marginals/continuous_variables.csv
#> Exporting  Summary to:  /tmp/RtmpMyIM1J/marginals/summary.csv
# Import the marginal distributions
marginals <- import_marginal_distributions(folder_path = folder_path)
#> Info: No file for binary variables found
```
