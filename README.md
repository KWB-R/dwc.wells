[![R-CMD-check](https://github.com/KWB-R/dwc.wells/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/dwc.wells/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/dwc.wells/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/dwc.wells/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/dwc.wells/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/dwc.wells)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/dwc.wells)]()

# dwc.wells

This package allows to predict the condition
of a drinking water well based on ML models. The models are trained
with results from pump tests and a large set of input variables e.g.
the well material, the age and the number of regenerations.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'dwc.wells' from GitHub
remotes::install_github("KWB-R/dwc.wells")
```

## Documentation

Release: [https://kwb-r.github.io/dwc.wells](https://kwb-r.github.io/dwc.wells)

Development: [https://kwb-r.github.io/dwc.wells/dev](https://kwb-r.github.io/dwc.wells/dev)
