<!-- badges: start -->
[![R-CMD-check](https://github.com/entjos/ExclusionTable/workflows/R-CMD-check/badge.svg)](https://github.com/entjos/ExclusionTable/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/ExclusionTable)](https://CRAN.R-project.org/package=ExclusionTable)
[![](https://cranlogs.r-pkg.org/badges/ExclusionTable)](https://cran.r-project.org/package=ExclusionTable)
<!-- badges: end -->

# Description
This package provides an exclusion table in R with which you can keeping track of inclusion and exclusion criteria that you apply to your datasets. Using the `exclusion_table()` function you can obtain a table listing the number of excluded observation for each inclusion and exclusion criteria. Additionally, if you use the option `keep_data == TRUE` you can also obtain the dataset without the excluded observations.

# Quick Example
This is a quick example to illustrate what `ExclusionTable` offers. We will use the `penguins` dataset included in the `{palmerpenguins}` package for the example. So make sure you install the package before running the example below.

```
# Load Penguine dataset
library(palmerpenguins)

# Apply some exclusions
exclusion_table(penguins, 
                exclusion_criteria = c("year == 2007", 
                                       "sex  == 'male'",
                                       "bill_length_mm <= 40 & 
                                       !is.na(bill_length_mm)"),
                labels_exclusion   = c("Measured in 2007",
                                       "Males",
                                       "Bill length <= 40mm"))
```

The code above produces the following table, which offers a nice overview of the number of observations that were removed from the dataset for each exclusion criteria.

```
================================================
Excluded the following observations:
================================================
Exclusions based on EXCLUSION criteria

            exclusion n_prior n_post n_excluded
1    Measured in 2007     344    234        110
2               Males     234    114        120
3 Bill length <= 40mm     114     66         48
4               TOTAL     344     66        278

================================================
```
Please take a look at the [vigniett](https://www.joshua-entrop.com/post/exclusion_table/) on my website for more information on how to use the `{ExclusionTable}` package.

# Installation
You can download the latest release of this package from CRAN using 
`install.packages(ExclusionTable)` or the latest development version 
using `{remotes}`.

```
remotes::install_github("entjos/ExclusionTable")
library(ExclusionTable)
```
# Bugs
If you find any bugs or have any suggestions please don't hesitate to file an issue on GitHub.
