# print.RESIDE

S3 override for print RESIDE

## Usage

``` r
# S3 method for class 'RESIDE'
print(x, ...)
```

## Arguments

- x:

  an object of class RESIDE

- ...:

  Other parameters, `full = TRUE` prints every category of the
  categorical variables, Default: FALSE

## Value

The RESIDE object, invisibly. Called to print to the terminal.

## Details

S3 Override for RESIDE Class, prints the overall summary followed by the
marginal distributions of each data frame. By default categorical
variables with more than 10 categories only print the 10 most common
categories, use `full = TRUE` to print every category.

## Examples

``` r
print(
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
)
#> Summary of Marginal Distributions
#> Number of Subjects: 19435 
#> 
#> Number of Rows: 19435 
#> Number of Columns: 5 
#> Variables: SEX, AGE, RATRIAL, RSBP, ID14 
#> 
#> Summary of Categorical Variables
#> Variable: SEX 
#>   F : 9028 
#>   M : 10407 
#> Variable: RATRIAL 
#>   N : 15282 
#>   Y : 3169 
#>   missing : 984 
#> 
#> Summary of Binary Variables
#> Variable: ID14 
#>   Mean: 0.0916388 
#>   Missing: 0 
#> 
#> Summary of Continuous Variables
#> Variable: AGE 
#>   Date: FALSE 
#>   Missing: 0 
#>   Decimal Places: 0 
#>   Quantiles:
#>   Original Transformed
#> 1       16  -3.8836332
#> 2       29  -2.8894619
#> 3       64  -0.7447992
#> 4       71  -0.2060679
#> 5       79   0.5263253
#> 6       86   1.4063880
#> 7       95   2.8388310
#> 8       96   3.0207824
#> 9       99   4.0489244
#> Variable: RSBP 
#>   Date: FALSE 
#>   Missing: 0 
#>   Decimal Places: 0 
#>   Quantiles:
#>   Original Transformed
#> 1       70 -4.04892439
#> 2      122 -1.37368502
#> 3      128 -1.31679183
#> 4      152 -0.13824887
#> 5      159 -0.09474469
#> 6      161  0.22957714
#> 7      295  4.04892439
print(marginal_distributions, full = TRUE)
#> Summary of Marginal Distributions
#> Number of Subjects: 19435 
#> 
#> Number of Rows: 19435 
#> Number of Columns: 5 
#> Variables: SEX, AGE, RATRIAL, RSBP, ID14 
#> 
#> Summary of Categorical Variables
#> Variable: SEX 
#>   F : 9028 
#>   M : 10407 
#> Variable: RATRIAL 
#>   N : 15282 
#>   Y : 3169 
#>   missing : 984 
#> 
#> Summary of Binary Variables
#> Variable: ID14 
#>   Mean: 0.0916388 
#>   Missing: 0 
#> 
#> Summary of Continuous Variables
#> Variable: AGE 
#>   Date: FALSE 
#>   Missing: 0 
#>   Decimal Places: 0 
#>   Quantiles:
#>   Original Transformed
#> 1       16  -3.8836332
#> 2       29  -2.8894619
#> 3       64  -0.7447992
#> 4       71  -0.2060679
#> 5       79   0.5263253
#> 6       86   1.4063880
#> 7       95   2.8388310
#> 8       96   3.0207824
#> 9       99   4.0489244
#> Variable: RSBP 
#>   Date: FALSE 
#>   Missing: 0 
#>   Decimal Places: 0 
#>   Quantiles:
#>   Original Transformed
#> 1       70 -4.04892439
#> 2      122 -1.37368502
#> 3      128 -1.31679183
#> 4      152 -0.13824887
#> 5      159 -0.09474469
#> 6      161  0.22957714
#> 7      295  4.04892439
```
