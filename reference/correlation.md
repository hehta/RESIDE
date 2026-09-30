# Create a correlation object

A helper function to create a correlation object

## Usage

``` r
correlation(x, y, rho, ...)
```

## Arguments

- x:

  The name of the first variable

- y:

  The name of the second variable

- rho:

  The correlation between the two variables

- ...:

  Additional arguments to specify data frame names and factor names See
  details for more information on the additional arguments.

## Value

A list containing the correlation information

## Details

This function is a helper function to create a correlation object that
can be used to specify correlations between variables when synthesising
data using the
[`synthesise_data`](https://hehta.github.io/RESIDE/reference/synthesise_data.md)
function. Additional Arguments:

- df_name: The name of the data frame containing both variables

- df_name.x: The name of the data frame containing the first variable

- df_name.y: The name of the data frame containing the second variable

- factor_name.x: The name of the factor variable for the first variable

- factor_name.y: The name of the factor variable for the second variable

## Examples

``` r
 correlation("age", "bmi", 0.5)
#> $x
#> [1] "age"
#> 
#> $y
#> [1] "bmi"
#> 
#> $rho
#> [1] 0.5
#> 
#> attr(,"class")
#> [1] "correlation"
```
