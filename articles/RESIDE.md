# The RESIDE Package

## Introduction

The Rapid Easy Synthesis to Inform Data Extraction (RESIDE) Package was
developed to assist researchers with planning analysis, prior to
obtaining data from Trusted Research Environments (TREs) also known as
safe havens (Hubbard et al. 2021; Gao et al. 2022; Lea et al. 2016).

Obtaining data from TREs is can be a lengthy process due to data
governance (Gao et al. 2022), in which time a researcher may not be able
to start their analysis. With the RESIDE package, a researcher can
obtain the marginal distributions of a dataset held by a trusted
research environment and simulate a synthetic dataset. Thus allowing
them to explore what the data looks like prior to gaining access to it.
This approach allows the researched to explore variables, including
their missingness and plan their analysis appropriately. Additionally,
it allows the research to start writing code for their analysis as the
structure of the synthetic data will be the same as that on the TRE. The
RESIDE package also allows the researcher to specify **assumed**
correlations based on expertise or previous research when simulating the
data to also allow the researcher to test analysis.

**NB** When using correlations it is not entirely possible to maintain
all the marginal distributions.

## Methods

## For TRE’s

### Generating Marginal Distributions

Marginal distributions can be generated for a given dataset within a TRE
using the
[`get_marginal_distributions()`](https://hehta.github.io/RESIDE/reference/get_marginal_distributions.md)
function. Marginal distributions will be created for all variable types
including:

- Binary
- Categorical
- Continuous

Binary variables are detected automatically by testing the min and
maximum values of numeric variables, if they fall between 0 and 1, the
variable is assumed binary; currently there is no way of manually
overriding this, but this is planned for a future update. For binary
variables the mean is used to represent the marginal distributions,
allowing this to be used as a probability when simulating data.

Currently any factor variable or variable containing non numeric
characters will be treated as categorical, this however means that the
burden is on the TRE to ensure that any variables containing non numeric
characters are categorical. For categorical variables, the number of
each category is used to represent the marginal distributions, allowing
the probability of each category to be calculated when simulating data.

**NB** Currently there is no automated checking to see if a category
contains less than a certain percent, or if there are an excess number
of categories therefore it is up to the TRE to ensure that this is
checked and if necessary adjusted prior to generating the marginal
distributions.

Any other numeric variable is treated as a continuous variable. To
prevent the need to specify a distribution when simulating data,
continuous variables are transformed using Ordered Quantile
normalisation from the `bestNormalize` package. The mean and standard
deviation of the transformed variable(s) are used as the marginal
distributions to allow the simulation of the variable using a normal
distribution. Additionally the mapping table is exported to allow the
variables to be back transformed.

### Exporting Marginal Distributions

Marginal distributions are exported using the
`export_marginal_distributions` function. Marginal distributions can be
printed prior to export using the `print` function. See [Exporting
Marginal
Distributions](https://hehta.github.io/RESIDE/articles/exporting_marginal_distributions.md)
for more details.

## For End Users

### Importing Marginal Distributions

Marginal distributions can be imported using the
`import_marginal_distributions` function. See [Importing Marginal
Distributions](https://hehta.github.io/RESIDE/articles/importing_marginal_distributions.md)
for more details.

### Data Simulation

Data is simulated from a cumulative multivariate copula distribution
using the `simstudy` package. Data can be simulated from marginal
distributions using the `synthesise_data` function. See [Synthesising
Data and Synthesising Data with
Correlations](https://hehta.github.io/RESIDE/articles/synthesising_data.md)
for more details.

## Limitations

This package has several limitations, some of which will be overcome
with future updates.

### Binary Variables

The package currently assumes that numeric variables which lie between 0
and 1 are binary, with no current way to override this. This will be
address in a future update.

### Categorical Variables

The package currently assumes any variable of a character type is a
categorical variable, this may cause issues if a dataset has erroneous
characters. Additionally there is no limit on the number of categories
or the minimum number within a category, leaving it up to the TRE to
check this and preprocess these variables. Both these limitations will
be addressed in future updates.

### Correlations

It is not entirely possible to maintain all the marginal distributions
and allow correlations to be specified, this is a known problem and is
unlikely to be addressed.

Gao, Chuang, Mark McGilchrist, Shahzad Mumtaz, et al. 2022. “A National
Network of Safe Havens: Scottish Perspective.” *Journal of Medical
Internet Research* 24 (3): e31684. <https://doi.org/10.2196/31684>.

Hubbard, Tim, Gerry Reilly, Susheel Varma, and David Seymour. 2021.
*Trusted Research Environments (TRE) Green Paper*. Zenodo.
<https://doi.org/10.5281/zenodo.4594704>.

Lea, Nathan Christopher, Jacqueline Nicholls, Christine Dobbs, et al.
2016. “Data Safe Havens and Trust: Toward a Common Understanding of
Trusted Research Platforms for Governing Secure and Ethical Health
Research.” *JMIR Medical Informatics* 4 (2): e22.
<https://doi.org/10.2196/medinform.5571>.
