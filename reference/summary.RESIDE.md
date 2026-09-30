# summary.RESIDE

S3 override for summary RESIDE

## Usage

``` r
# S3 method for class 'RESIDE'
summary(object, ...)
```

## Arguments

- object:

  an object of class RESIDE

- ...:

  Other parameters currently none are used

## Value

An object of class summary.RESIDE, a list containing `overall`, a data
frame of the overall summary, and `data_frames`, a data frame with a row
for each data frame.

## Details

S3 Override for RESIDE Class, a higher level summary than
[`print.RESIDE`](https://hehta.github.io/RESIDE/reference/print.RESIDE.md).
For each data frame it gives the number of rows, subjects and variables,
the number of each type of variable, the number of date variables and
the number of variables with missing data.

## See also

[`print.RESIDE`](https://hehta.github.io/RESIDE/reference/print.RESIDE.md)

## Examples

``` r
summary(
  get_marginal_distributions(
    IST,
    variables = c(
      "SEX",
      "AGE",
      "ID14",
      "RSBP",
      "RATRIAL"
    )
  )
)
#> Summary of Marginal Distributions
#> Number of Subjects: 19435 
#> 
#>   Rows Subjects Variables Categorical Binary Continuous Dates Missing
#>  19435    19435         5           2      1          2     0       1
```
