# documentation of setting up package ------------------------------------------

# it is recommended to first built a git hub repository and then clone it via
# File -> New Project -> Version Control -> Git


# MAIN 1: setting up package ---------------------------------------------------

if (FALSE) {
  
  package_name <- "dwc.wells"
  rprojects_dir <- "C:/Users/mriech/Documents/R_Projects"
  pkg_dir <- file.path(rprojects_dir, package_name)
  
  withr::with_dir(pkg_dir, {kwb.pkgbuild::use_pkg_skeleton(package_name)})
  
  author <- list(name = "Mathias Riechel",
                 orcid = "0000-0002-1191-5877")
  
  #remotes::install_github("kwb-r/kwb.orcid")
  kwb.orcid::get_kwb_orcids
  
  description <- list(
    name = "dwc.wells",
    title = paste("A package for condition predictions for drinking water wells"),
    desc = paste("This package allows to predict the condition of a drinking", 
                 "water well based on ML models. The models are trained with", 
                 "results from pump tests and a large set of input variables", 
                 "e.g. the well material, the age and the number of regenerations.")
  )
  
  setwd(pkg_dir)
  
  kwb.pkgbuild::use_pkg(author, description)
}


# MAIN 2: creating empty r script for functions --------------------------------

if (FALSE) {
  usethis::use_r("functions")
}