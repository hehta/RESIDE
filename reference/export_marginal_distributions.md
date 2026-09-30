# Export Marginal Distributions

Export the marginal distributions to CSV files

## Usage

``` r
export_marginal_distributions(
  marginals,
  folder_path,
  create_folder = FALSE,
  force = FALSE
)
```

## Arguments

- marginals:

  an Object of type RESIDE from
  [`get_marginal_distributions`](https://hehta.github.io/RESIDE/reference/get_marginal_distributions.md)

- folder_path:

  path to folder where to save files.

- create_folder:

  if the folder does not exist should it be created, Default: FALSE

- force:

  if the folder already contains marginal distribution files should they
  be removed, Default: FALSE

## Value

No return value, called for exportation of files.

## Details

Exports each of the marginal distributions to CSV files within a given
folder, along with the continuous quantiles.

## See also

[`get_marginal_distributions`](https://hehta.github.io/RESIDE/reference/get_marginal_distributions.md)

## Examples

``` r
marginal_distributions <- get_marginal_distributions(
  IST,
  variables = c("SEX", "AGE", "RSBP", "RATRIAL")
)
#> Registered S3 method overwritten by 'butcher':
#>   method                 from    
#>   as.character.dev_topic generics
export_marginal_distributions(
  marginal_distributions,
  folder_path = file.path(tempdir(), "marginals"),
  create_folder = TRUE,
  force = TRUE
)
#> Exporting  Categorical variables to:  /tmp/RtmpJvn88q/marginals/categorical_variables.csv
#> Exporting  Continuous variables to:  /tmp/RtmpJvn88q/marginals/continuous_variables.csv
#> Exporting  Summary to:  /tmp/RtmpJvn88q/marginals/summary.csv
```
