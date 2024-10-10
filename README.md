

[![Static Badge](https://img.shields.io/badge/docs-RESIDE-blue)](https://hehta.github.io/RESIDE/)
[![DOI](https://zenodo.org/badge/841077745.svg)](https://zenodo.org/doi/10.5281/zenodo.13693881)
[![codecov](https://codecov.io/gh/hehta/RESIDE/graph/badge.svg?token=17ZQNFWA40)](https://codecov.io/gh/hehta/RESIDE)
[![R-CMD-Check](https://github.com/hehta/RESIDE/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hehta/RESIDE/actions/workflows/R-CMD-check.yaml)

# RESIDE: Rapid Easy Synthesis to Inform Data Extraction

An R package which allows data to be simulation from marginal distributions obtained from Trusted Research Environments (TREs).

# Installation
Currently this package can be installed using `devtools` or `remotes`

## Devtools
```
install.packages("devtools")
devtools::install_github("hehta/RESIDE")
```

## Remotes
```
install.packages("remotes")
remotes::install_github("hehta/RESIDE")
```

# Usage

## Trusted Research Environments (TRE's)
There are two main functions:

### `get_marginal_distributions()`
This function will get the marginal distributions and store them in an R object.

### `export_marginal_distributions()`
This function will export the marginal distributions to csv files.

## End Users
Once you have obtained the marginal distributions from a TRE, you can import them and simulated data using the following functions:

### `import_marginal_distributions()`
This function will import the marginal distributions from the csv files provided by a TRE and store them in an R object.

### `synthesise_data()`
This function will synthesise data base on the imported marginal distributions.

# Worked Example Using the Internation Stroke Trial
The following is a worked example using the [International Stroke Trial (IST)](https://doi.org/10.7488/ds/104) which has been included as a dataset in the package under ODC-by licence.

```
# Load the package
library(RESIDE)

# Get the marginal distributions
marginals <- get_marginal_distributions(IST)

# Export the marginal distributions
export_marginal_distributions(marginals)

# Import the exported marginal distributions
marginals_imported <- import_marginal_distributions()

# Synthesise data from the imported marginal distributions
synthesise_data(marginals_imported)
```

# Funding
This work was supported by the UKRI Strength in Places Fund (SIPF) Competition, project number 107140.  The project title is SIPF The Living Laboratory driving economic growth in Glasgow through real world implementation of precision medicine.