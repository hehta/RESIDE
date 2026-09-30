# Worked Example Using Multiple Tables

## Introduction

This R Markdown document illustrates example usage of the RESIDE package
with multiple related tables, using the Demographics (DM) and Adverse
Events (AE) SDTM domains from the
[pharmaversesdtm](https://pharmaverse.github.io/pharmaversesdtm/)
package. The tables are linked by a subject identifier (`USUBJID`), DM
has one row per subject and AE has one row per adverse event.

## Setup

Load the RESIDE package and set a seed for reproducibility and store the
folder directory for export / import.

``` r

# Load the Library
library(RESIDE)
# Load dplyr for data manipulation
library(dplyr)
# Set the seed
set.seed(1234)
# Store the folder path used for import / export
folder_path <- tempdir()
```

## Summarise original data

Store the tables in a named list, the names are used to identify the
tables in the marginal distributions.

``` r

# Store the tables in a named list
dfs <- list(
  dm = pharmaversesdtm::dm,
  ae = pharmaversesdtm::ae
)

# Number of rows in each table
sapply(dfs, nrow)
#>   dm   ae 
#>  306 1191

# Number of subjects in each table
sapply(dfs, function(df) length(unique(df$USUBJID)))
#>  dm  ae 
#> 306 225

# Treatment arms of the subjects
table(dfs$dm$ARM)
#> 
#>              Placebo       Screen Failure Xanomeline High Dose 
#>                   86                   52                   84 
#>  Xanomeline Low Dose 
#>                   84

# Severity of the adverse events
table(dfs$ae$AESEV)
#> 
#>     MILD MODERATE   SEVERE 
#>      770      378       43
```

## Fit a logistic regression model on the original data

Join the adverse events to the demographics of the subject and fit a
logistic regression model for the odds of an adverse event being
moderate or severe, by age, sex and treatment arm. The same function is
used for the original and synthesised data.

``` r

# Join the adverse events to the demographics of each subject
prepare_ae_data <- function(dm, ae) {
  ae |>
    select(USUBJID, AESEV) |>
    inner_join(select(dm, USUBJID, AGE, SEX, ARM), by = "USUBJID") |>
    # Remove screen failures and adverse events without a severity
    filter(ARM != "Screen Failure", AESEV != "") |>
    mutate(
      MOD_SEV = AESEV %in% c("MODERATE", "SEVERE"),
      ARM = relevel(factor(ARM), "Placebo")
    )
}

ae_original <- prepare_ae_data(dfs$dm, dfs$ae)

# Proportion of moderate or severe adverse events by treatment arm
prop.table(table(ae_original$ARM, ae_original$MOD_SEV), 1)
#>                       
#>                            FALSE      TRUE
#>   Placebo              0.7275748 0.2724252
#>   Xanomeline High Dose 0.6725275 0.3274725
#>   Xanomeline Low Dose  0.5632184 0.4367816

# Fit a logistic regression model
glm.original <- glm(
  MOD_SEV ~ AGE + SEX + ARM,
  data = ae_original,
  family = binomial
)

# Output the coefficients of the model
summary(glm.original)$coefficients
#>                             Estimate  Std. Error    z value     Pr(>|z|)
#> (Intercept)             -0.305106216 0.611013964 -0.4993441 6.175370e-01
#> AGE                     -0.006640127 0.007838388 -0.8471291 3.969232e-01
#> SEXM                    -0.435382516 0.126327520 -3.4464582 5.679865e-04
#> ARMXanomeline High Dose  0.331225289 0.167062099  1.9826477 4.740679e-02
#> ARMXanomeline Low Dose   0.740406571 0.162802809  4.5478734 5.419071e-06
```

**NB** Adverse events from the same subject are not independent, a mixed
effects model would be more appropriate, however a logistic regression
model is used here for simplicity.

## Get the marginal distributions

Use the
[`get_marginal_distributions()`](https://hehta.github.io/RESIDE/reference/get_marginal_distributions.md)
function to get the marginal distributions of the tables, specifying the
column that identifies each subject using the `subject_identifier`
parameter.

``` r

# Get the Marginal Distributions of both tables
marginals <- get_marginal_distributions(
  dfs,
  subject_identifier = "USUBJID"
)
#> Registered S3 method overwritten by 'butcher':
#>   method                 from    
#>   as.character.dev_topic generics

# Summarise the Marginal Distributions
summary(marginals)
#> Summary of Marginal Distributions
#> Number of Data Frames: 2 
#> Subject Identifier: USUBJID 
#> Number of Subjects: 306 
#> Common Columns: STUDYID 
#> 
#>  Data Frame Rows Subjects Variables Categorical Binary Continuous Dates Missing
#>          dm  306      306        27          17      0         10     8       8
#>          ae 1191      225        34          28      0          6     3      11
```

Columns that are present in every table (other than the subject
identifier), are identified as common columns, in this case the study
identifier `STUDYID`.

## Export the marginal distributions

Export the marginal distributions using the
[`export_marginal_distributions()`](https://hehta.github.io/RESIDE/reference/export_marginal_distributions.md)
function, using the `force` parameter to override any existing files.

``` r

# Export the Marginal Distributions
export_marginal_distributions(marginals,
                              folder_path = folder_path,
                              force = TRUE)
#> Exporting  Categorical variables to:  /tmp/RtmpPvF0B2/categorical_variables.csv
#> Exporting  Continuous variables to:  /tmp/RtmpPvF0B2/continuous_variables.csv
#> Exporting  Summary to:  /tmp/RtmpPvF0B2/summary.csv
```

## Reimport marginal distributions

Import the exported marginal distributions using the
[`import_marginal_distributions()`](https://hehta.github.io/RESIDE/reference/import_marginal_distributions.md)
function

``` r

# Import the Marginal Distributions
imported_marginals <- import_marginal_distributions(folder_path = folder_path)
#> Info: No file for binary variables found
```

## Synthesise data from marginal distributions (without correlations)

Synthesise data from the imported marginals using the `synthesise_data`
function, for multiple tables a named list of data frames is returned.

``` r

# Synthesise the tables from the imported Marginal Distributions (without correlations)
sim_dfs <- synthesise_data(imported_marginals)

# Number of rows in each table
sapply(sim_dfs, nrow)
#>   dm   ae 
#>  306 1191

# Number of subjects in each table
sapply(sim_dfs, function(df) length(unique(df$USUBJID)))
#>  dm  ae 
#> 306 225
```

The number of rows and subjects of each table are maintained,
synthesised subjects are identified by a number, which links the
subjects between the tables.

## Fit a logistic regression model on the synthesised data

Fit the same logistic regression model as earlier except this time on
the synthesised data.

``` r

ae_sim <- prepare_ae_data(sim_dfs$dm, sim_dfs$ae)

# Proportion of moderate or severe adverse events by treatment arm
prop.table(table(ae_sim$ARM, ae_sim$MOD_SEV), 1)
#>                       
#>                            FALSE      TRUE
#>   Placebo              0.6265664 0.3734336
#>   Xanomeline High Dose 0.6234310 0.3765690
#>   Xanomeline Low Dose  0.5913043 0.4086957

# Fit a logistic regression model on the synthesised data
glm.sim <- glm(
  MOD_SEV ~ AGE + SEX + ARM,
  data = ae_sim,
  family = binomial
)

# Output the coefficients of the model
summary(glm.sim)$coefficients
#>                             Estimate  Std. Error    z value  Pr(>|z|)
#> (Intercept)             -0.630833056 0.601786974 -1.0482664 0.2945159
#> AGE                      0.002193439 0.007856429  0.2791903 0.7800988
#> SEXM                    -0.108603407 0.131271564 -0.8273186 0.4080565
#> ARMXanomeline High Dose  0.017967552 0.169169777  0.1062102 0.9154156
#> ARMXanomeline Low Dose   0.147867959 0.151204267  0.9779351 0.3281064
```

Without correlations the variables are synthesised independently, so
there is no relationship between the treatment arm and the severity of
the adverse events.

## Synthesise data with correlations

Synthesise data from the imported marginals with assumed correlations,
using the `correlations` parameter to specify a list of correlations
created with the
[`correlation()`](https://hehta.github.io/RESIDE/reference/correlation.md)
function. Categorical variables are correlated using a single category,
specified with `factor_name.x` or `factor_name.y`, and the tables of the
variables can be specified with `df_name.x` and `df_name.y`.

``` r

# Synthesise the tables specifying assumed correlations
sim_dfs_cor <- synthesise_data(
  imported_marginals,
  correlations = list(
    # Subjects on placebo are more likely to have mild adverse events
    correlation(
      "ARM",
      "AESEV",
      0.2,
      df_name.x = "dm",
      df_name.y = "ae",
      factor_name.x = "Placebo",
      factor_name.y = "MILD"
    ),
    # Male subjects are younger
    correlation("AGE", "SEX", -0.2, factor_name.y = "M")
  )
)
```

Correlated variables are synthesised together, one row per subject, and
joined to each table by subject. Therefore correlations between tables
are between subjects, and a correlated variable takes a single value for
each subject within a table, in this case every adverse event of a
subject has the same severity.

## Fit a logistic regression model on the synthesised data with correlations

Using the synthesised data (with correlations) fit the same logistic
regression model as earlier.

``` r

ae_sim_cor <- prepare_ae_data(sim_dfs_cor$dm, sim_dfs_cor$ae)

# Proportion of moderate or severe adverse events by treatment arm
prop.table(table(ae_sim_cor$ARM, ae_sim_cor$MOD_SEV), 1)
#>                       
#>                            FALSE      TRUE
#>   Placebo              0.7005988 0.2994012
#>   Xanomeline High Dose 0.6027778 0.3972222
#>   Xanomeline Low Dose  0.6548387 0.3451613

# Fit a logistic regression model on the synthesised data (with correlations)
glm.sim.cor <- glm(
  MOD_SEV ~ AGE + SEX + ARM,
  data = ae_sim_cor,
  family = binomial
)

# Output the coefficients of the model
summary(glm.sim.cor)$coefficients
#>                             Estimate  Std. Error    z value    Pr(>|z|)
#> (Intercept)             -1.434536276 0.653056342 -2.1966501 0.028045445
#> AGE                      0.006320768 0.008338343  0.7580365 0.448429162
#> SEXM                     0.240313969 0.136579469  1.7595175 0.078489647
#> ARMXanomeline High Dose  0.417586363 0.161300094  2.5888786 0.009628903
#> ARMXanomeline Low Dose   0.244232900 0.170323799  1.4339329 0.151591410
```

With the assumed correlation, adverse events of subjects on placebo are
less likely to be moderate or severe, allowing the analysis to be tested
before access to the original data is granted.

**NB** The correlation specified is that of the underlying multivariate
normal distribution (Copula), the correlation observed in the
synthesised data will differ, particularly for categorical variables. It
is not possible to entirely maintain all the marginal distributions when
specifying correlations.
