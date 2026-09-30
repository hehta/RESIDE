# RESIDE: Rapid Easy Synthesis to Inform Data Extraction

An R package which allows data to be simulation from marginal
distributions obtained from Trusted Research Environments (TREs).

# Installation

## CRAN

The latest release of this package can be installed from CRAN:

    install.packages("RESIDE")

Additionally the development version of this package can be installed
using `devtools` or `remotes`:

## Devtools

    install.packages("devtools")
    devtools::install_github("hehta/RESIDE")

## Remotes

    install.packages("remotes")
    remotes::install_github("hehta/RESIDE")

# Usage

## Trusted Research Environments (TRE’s)

There are two main functions:

### `get_marginal_distributions()`

This function will get the marginal distributions and store them in an R
object. See the documentation on [exporting marginal
distributions](https://hehta.github.io/RESIDE/articles/exporting_marginal_distributions.html)
for further information.

### `export_marginal_distributions()`

This function will export the marginal distributions to csv files. See
the documentation on [exporting marginal
distributions](https://hehta.github.io/RESIDE/articles/exporting_marginal_distributions.html)
for further information.

## End Users

Once you have obtained the marginal distributions from a TRE, you can
import them and simulated data using the following functions:

### `import_marginal_distributions()`

This function will import the marginal distributions from the csv files
provided by a TRE and store them in an R object. See the documentation
on [importing marginal
distributions](https://hehta.github.io/RESIDE/articles/importing_marginal_distributions.html)
for further information.

### `synthesise_data()`

This function will synthesise data based on the imported marginal
distributions. See the documentation on [synthesising
data](https://hehta.github.io/RESIDE/articles/synthesising_data.html)
for further information.

# Worked Example Using the International Stroke Trial

A worked example using the International Stroke Trial is available in
the
[documentation](https://hehta.github.io/RESIDE/articles/worked_example.html).

# Worked Example Using Multiple Tables

A worked example using multiple related tables, the Demographics and
Adverse Events SDTM domains from the pharmaversesdtm package, is
available in the
[documentation](https://hehta.github.io/RESIDE/articles/pharmaverse_example.html).

# Funding

This work was supported by the UKRI Strength in Places Fund (SIPF)
Competition, project number 107140.  The project title is SIPF The
Living Laboratory driving economic growth in Glasgow through real world
implementation of precision medicine.
