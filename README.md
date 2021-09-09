
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jmpwashdata

<!-- badges: start -->
<!-- badges: end -->

The goal of jmpwashdata is to provide a snapshot of the JMP WASH
household, WASH in schools and WASH in health care facilities data that
is normally available in Excel sheets on <https://washdata.org>. The
data is loaded in the package since the data is updated at the moment no
more often than once a year. Helper functions are provided to access
specific data and indicators.

## Installation

You cannot yet install the (unreleased) version of jmpwashdata from
[CRAN](https://CRAN.R-project.org). You must build it from source and
install that.

# Install JMP package

Once you have built a binary file, install it locally. For example:

``` r
if (length(grep(pattern = "jmpwashdata", x = installed.packages()[,"Package"])) == 0) {
  devtools::install_local("~/RStudio/packages/jmpwashdata_0.1.1.tar.gz")
}
```
