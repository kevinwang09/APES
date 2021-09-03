## Test environments
* ubuntu 16.04 (GitHub Actions), R devel
* MacOS (GitHub Actions), R devel
* Windows (GitHub Actions), R devel

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* This package has a `Suggests` dependence on a GitHub package (ryantibs/best-subset/bestsubset), which depends on a commercial optimisation package `gurobi`. However, the core functions of APES package can operate independent of both packages. There is a vignette (install_bestsubset.Rmd) that details the installation instructions of both packages if an user wants to extend the use of this APES package. 
