# Get Missing Variables

Returns a list of missing variables from a list of data frames

## Usage

``` r
get_missing_variables(dfs, variables)
```

## Arguments

- dfs:

  A list of data frames

- variables:

  A vector of variable names

## Value

A vector of missing variable names

## Details

This function checks if each variable in the input vector is present in
any of the data frames.
