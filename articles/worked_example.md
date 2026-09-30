# Worked Example Using the IST Dataset

## Introduction

This R Markdown document illustrates example usage of the RESIDE package
using the IST dataset.

## Setup

Load the RESIDE package and set a seed for reproducibility and store the
folder directory for export / import.

``` r

# Load the Library
library(RESIDE)
# Set the seed
set.seed(1234)
# Store the folder path used for import / export
folder_path <- tempdir()
```

## Summarise original data

Select the variables of interest and summarise. Selection of variables
is optional, if the variables are known. They may not be known until the
marginal distributions have been received.

``` r

# Select variables of interest from the IST dataset.
IST_original <- IST |> dplyr::select(
  AGE, # AGE at Randomisation
  SEX, # SEX M/F
  RATRIAL, # Atrial Fibrillation Y/N at Randomisation 
  # (not coded for 984 patients in the pilot phase)
  RSBP, # Systolic Blood Pressure at Randomisation
  STRK14 # Indicator of Any Stroke at 14 days
)

# Convert the character variables to factors (to allow for summary)
IST_original <- IST_original |> dplyr::mutate_if(is.character, factor)

# Produce a summary of the variables
summary(IST_original)
#>       AGE        SEX       RATRIAL        RSBP           STRK14       
#>  Min.   :16.00   F: 9028    :  984   Min.   : 70.0   Min.   :0.00000  
#>  1st Qu.:65.00   M:10407   N:15282   1st Qu.:140.0   1st Qu.:0.00000  
#>  Median :73.00             Y: 3169   Median :160.0   Median :0.00000  
#>  Mean   :71.72                       Mean   :160.2   Mean   :0.04152  
#>  3rd Qu.:80.00                       3rd Qu.:180.0   3rd Qu.:0.00000  
#>  Max.   :99.00                       Max.   :295.0   Max.   :1.00000
```

## Fit a cox model on the original data

``` r

# Load survival and dplyr libraries
library(survival) # For Cox PH model
library(dplyr) # For data manipulation
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# Stroke event is measured at 14 days, so set this for patients
IST_original$DAY <- 14

# Illustrate the 984 missing values
sum(IST_original$RATRIAL == "")
#> [1] 984

# Remove the missing values
IST_original <- IST_original[!IST_original$RATRIAL == "",]

# Drop the factor name for the missing values
IST_original$RATRIAL <- droplevels(IST_original$RATRIAL)

# Summarise the variable to show there are no longer missing values
summary(IST_original$RATRIAL)
#>     N     Y 
#> 15282  3169

# Fit a Cox PH model
cox.ph <- coxph(Surv(DAY, STRK14) ~ AGE + SEX + RATRIAL + RSBP, data = IST_original) 

# Output the summary of the Cox PH Model
cox.ph
#> Call:
#> coxph(formula = Surv(DAY, STRK14) ~ AGE + SEX + RATRIAL + RSBP, 
#>     data = IST_original)
#> 
#>              coef exp(coef) se(coef)     z      p
#> AGE      0.005237  1.005251 0.003387 1.546 0.1220
#> SEXM     0.077489  1.080570 0.074388 1.042 0.2976
#> RATRIALY 0.231692  1.260732 0.091818 2.523 0.0116
#> RSBP     0.000994  1.000995 0.001310 0.759 0.4479
#> 
#> Likelihood ratio test=11.8  on 4 df, p=0.01893
#> n= 18451, number of events= 764
```

## Get the marginal distributions

Use the
[`get_marginal_distributions()`](https://hehta.github.io/RESIDE/reference/get_marginal_distributions.md)
function to get the marginal distributions, additionally selecting which
variables using the `variables` parameter.

``` r

# Get the Marginal Distributions for the selected variables
marginals <- get_marginal_distributions(
  IST,
  variables = c(
    "AGE",
    "SEX",
    "RATRIAL",
    "RSBP",
    "STRK14"
  )
)
#> Registered S3 method overwritten by 'butcher':
#>   method                 from    
#>   as.character.dev_topic generics
```

## Export the marginal distributions

Export the marginal distributions using the
[`export_marginal_distributions()`](https://hehta.github.io/RESIDE/reference/export_marginal_distributions.md)
function, using the `force` parameter to override any existing files.

``` r

# Export the Marginal Distributions
export_marginal_distributions(marginals,
                              folder_path = folder_path,
                              force = TRUE)
#> Exporting  Categorical variables to:  /tmp/Rtmptd4RWt/categorical_variables.csv
#> Exporting  Binary variables to:  /tmp/Rtmptd4RWt/binary_variables.csv
#> Exporting  Continuous variables to:  /tmp/Rtmptd4RWt/continuous_variables.csv
#> Exporting  Summary to:  /tmp/Rtmptd4RWt/summary.csv
```

## Reimport marginal distributions

Import the exported marginal distributions using the
[`import_marginal_distributions()`](https://hehta.github.io/RESIDE/reference/import_marginal_distributions.md)
function

``` r

# Import the Marginal Distributions
imported_marginals <- import_marginal_distributions(folder_path = folder_path)
```

## Synthesise data from marginal distributions (without correlations)

Synthesise data from the imported marginals using the `synthesise_data`
function.

``` r

# Synthesise a dataset from the imported Marginal Distributions (without correlations)
sim_df <- synthesise_data(imported_marginals)
```

## Summarise the synthesised data

Summarise the simulated data

``` r

# Convert any Character variables to Factors
sim_df <- sim_df |> dplyr::mutate_if(is.character, factor)
# Summarise the synthesised data
summary(sim_df)
#>        id        SEX       RATRIAL       STRK14             AGE       
#>  Min.   :    1   F: 9086    :  982   Min.   :0.00000   Min.   :17.00  
#>  1st Qu.: 4860   M:10349   N:15266   1st Qu.:0.00000   1st Qu.:65.00  
#>  Median : 9718             Y: 3187   Median :0.00000   Median :73.00  
#>  Mean   : 9718                       Mean   :0.04229   Mean   :71.57  
#>  3rd Qu.:14576                       3rd Qu.:0.00000   3rd Qu.:80.00  
#>  Max.   :19435                       Max.   :1.00000   Max.   :98.00  
#>       RSBP      
#>  Min.   : 79.0  
#>  1st Qu.:141.0  
#>  Median :160.0  
#>  Mean   :159.8  
#>  3rd Qu.:176.0  
#>  Max.   :290.0
```

## Fit a cox model on the simulated data

Fit the same cox model as earlier except this time on the simulated
data.

``` r


# As before the events are measured at day 14
sim_df$DAY <- 14

# Show that the missing observations are in the data
sum(sim_df$RATRIAL == "")
#> [1] 982

# Remove the missing observations
sim_df <- sim_df[!sim_df$RATRIAL == "",]

# Remove the missing factor name
sim_df$RATRIAL <- droplevels(sim_df$RATRIAL)

# Show that there are no missing observations
summary(sim_df$RATRIAL)
#>     N     Y 
#> 15266  3187

# Fit the model on the synthesised data
cox.ph.sim <- coxph(Surv(DAY, STRK14) ~ AGE + SEX + RATRIAL + RSBP, data = sim_df) 

# Show a summary of the model
cox.ph.sim
#> Call:
#> coxph(formula = Surv(DAY, STRK14) ~ AGE + SEX + RATRIAL + RSBP, 
#>     data = sim_df)
#> 
#>               coef exp(coef)  se(coef)      z      p
#> AGE       0.006556  1.006578  0.003153  2.079 0.0376
#> SEXM     -0.128158  0.879714  0.072031 -1.779 0.0752
#> RATRIALY -0.001377  0.998624  0.095327 -0.014 0.9885
#> RSBP     -0.002878  0.997126  0.001318 -2.183 0.0290
#> 
#> Likelihood ratio test=12.43  on 4 df, p=0.01443
#> n= 18453, number of events= 771
```

## Synthesise data with correlations

Synthesise data from the imported marginals with correlations, using the
`correlations` parameter to specify a list of assumed correlations
created with the
[`correlation()`](https://hehta.github.io/RESIDE/reference/correlation.md)
function. Categorical variables are correlated using a single category,
specified with `factor_name.x` or `factor_name.y`.

``` r

# Synthesise data specifying assumed correlations
sim_df_cor <- synthesise_data(
  imported_marginals,
  correlations = list(
    # Patients without atrial fibrillation are less likely to have a stroke
    correlation("RATRIAL", "STRK14", -0.2, factor_name.x = "N"),
    # Older patients have a higher systolic blood pressure
    correlation("AGE", "RSBP", 0.3)
  )
)
```

## Summarise the synthesised data

Summarise the synthesised data (with correlations)

``` r

# Convert to Factors from Character variables
sim_df_cor <- sim_df_cor |> dplyr::mutate_if(is.character, factor)
# Summarise the synthesised dataset
summary(sim_df_cor)
#>        id        SEX       RATRIAL       STRK14             AGE       
#>  Min.   :    1   F: 8946    : 1003   Min.   :0.00000   Min.   :18.00  
#>  1st Qu.: 4860   M:10489   N:15336   1st Qu.:0.00000   1st Qu.:65.00  
#>  Median : 9718             Y: 3096   Median :0.00000   Median :73.00  
#>  Mean   : 9718                       Mean   :0.03998   Mean   :71.64  
#>  3rd Qu.:14576                       3rd Qu.:0.00000   3rd Qu.:80.00  
#>  Max.   :19435                       Max.   :1.00000   Max.   :99.00  
#>       RSBP      
#>  Min.   : 78.0  
#>  1st Qu.:141.0  
#>  Median :160.0  
#>  Mean   :160.2  
#>  3rd Qu.:177.0  
#>  Max.   :293.0
```

## Fit a cox model on the simulated data with correlations

Using the synthesised data (with correlations) fit a Cox PH model, with
the same parameters as earlier.

``` r


# Again events are measured at 14 days
sim_df_cor$DAY <- 14

# Again check that the missing values where added
sum(sim_df_cor$RATRIAL == "")
#> [1] 1003

# Again remove the missing values
sim_df_cor <- sim_df_cor[!sim_df_cor$RATRIAL == "",]

# Again drop the missing factor
sim_df_cor$RATRIAL <- droplevels(sim_df_cor$RATRIAL)

# Show there are no missing values
summary(sim_df_cor$RATRIAL)
#>     N     Y 
#> 15336  3096

# Fit the model on the synthesised data (with correlations)
cox.ph.sim.cor <- coxph(Surv(DAY, STRK14) ~ AGE + SEX + RATRIAL + RSBP, data = sim_df_cor)

# Show a summary of the model
cox.ph.sim.cor
#> Call:
#> coxph(formula = Surv(DAY, STRK14) ~ AGE + SEX + RATRIAL + RSBP, 
#>     data = sim_df_cor)
#> 
#>               coef exp(coef)  se(coef)     z      p
#> AGE      0.0004601 1.0004602 0.0033498 0.137  0.891
#> SEXM     0.0211164 1.0213409 0.0755433 0.280  0.780
#> RATRIALY 0.7220032 2.0585528 0.0828990 8.709 <2e-16
#> RSBP     0.0001810 1.0001810 0.0013996 0.129  0.897
#> 
#> Likelihood ratio test=67.8  on 4 df, p=6.61e-14
#> n= 18432, number of events= 707
```
