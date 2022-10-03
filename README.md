# Description

This package provides an exclusion table in R with which you can keeping track of inclusion and exclusion criteria that you apply to your datasets. Using the `exclusion_table()` function you can obtain a table listing the number of excluded observation for each inclusion and exclusion criteria. Additionally, if you use the option `keep_data == TRUE` you can also obtain the dataset without the excluded observations.

# Installation
You can download the latest release of this package from CRAN using 
`install.packages(ExclusionTable)` or the latest development version 
using `{remotes}`.

```
remotes::install_github("entjos/ExclusionTable")
library(ExclusionTable)
```
# Bugs
If you find any bugs or have any suggestions please don't hesitate to file an issue on GitHub or contact me via [my website](https://www.joshua-entrop.com/), [Twitter](https://twitter.com/entjos) or [email](mailto:joshua.entrop@ki.se).
