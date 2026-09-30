# Export an empty correlation matrix (removed)

This function has been removed. Correlations should now be specified
using the
[`correlation`](https://hehta.github.io/RESIDE/reference/correlation.md)
function.

## Usage

``` r
export_empty_cor_matrix(...)
```

## Arguments

- ...:

  Ignored, retained for backwards compatibility.

## Value

No return value, always throws an error.

## Details

Previously this function exported an empty correlation matrix as a csv
file. Correlations are now supplied directly to
[`synthesise_data`](https://hehta.github.io/RESIDE/reference/synthesise_data.md)
as a list of objects created with the
[`correlation`](https://hehta.github.io/RESIDE/reference/correlation.md)
function.

## See also

[`correlation`](https://hehta.github.io/RESIDE/reference/correlation.md)

## Examples

``` r
 try(export_empty_cor_matrix())
#> Error in export_empty_cor_matrix() : 
#>   export_empty_cor_matrix() has been removed. Use the correlation() function to specify correlations instead, e.g. correlation("age", "bmi", 0.5).
```
