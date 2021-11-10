
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jmpwashdata

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to facilite the use and analysis of data
form the WHO/UNICEF Joint Monitoring Programme for Water and Sanitation.
It provides a tidy snapshot of the JMP WASH household, WASH in schools
and WASH in health care facilities data that is normally available in
Excel sheets on <https://washdata.org>. The excel sheets filenames and
date downloaded are stored in the jmpwashdata::jmp\_files data frame as
a reference. The last download for jmpwashdata version 0.1.3 took place
on 2021-10-22.

The goal is to keep the package up to date with changes on the JMP
website and eventually to automate this process. If data are out of data
with the main JMP website, please feel free to post an issue so we can
rebuild it: <https://github.com/WASHNote/jmpwashdata/issues>

Please support the development and maintenance of this package. The
simplest way to do this is to provide us with attribution.

``` r
citation(package = "jmpwashdata")
#> 
#> To cite package 'jmpwashdata' in publications use:
#> 
#>   Nicolas Dickinson (2021). jmpwashdata: WHO/UNICEF Joint Monitoring
#>   Programme Water and Sanitation Data. R package version 0.1.3.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {jmpwashdata: WHO/UNICEF Joint Monitoring Programme Water and Sanitation Data},
#>     author = {Nicolas Dickinson},
#>     year = {2021},
#>     note = {R package version 0.1.3},
#>   }
#> 
#> ATTENTION: This citation information has been auto-generated from the
#> package DESCRIPTION file and may need manual editing, see
#> 'help("citation")'.
```

## Installation

The easiest way to install this is by using devtools. You may install
devtools as follows:

``` r
install.packages("devtools")
```

### Install with devtools

Simply run the following code.

``` r
devtools::install_github("WASHNote/jmpwashdata")
```

You cannot yet install from [CRAN](https://CRAN.R-project.org). The
package will be submitted to CRAN as soon as the documentation has been
completed. Rather. you must build it from source and the easiest way to
do this is with devtools.

### Build and develop the package (advanced)

For those interested in contributing to the development of the package,
you may also clone the repository and open it in RStudio.

# Changes

-   v.0.1.3 November 2021 Addition of extraction of regional and world
    school and healthcare facility datasets.
-   v.0.1.2 October 2021 Update of data files to include the new world
    and region files and changes in other files and to add more error
    handling. Includes now the data summary sheets found in the
    inequality files parsed to be in a cleaner long format.
-   v.0.1.1 July 2021 New published data files extracted with the 2019
    and 2020 data sets from JMP Excel sheets.
-   v.0.1.0 June 2021 Extraction of 2017 JMP files.

# Wish list / roadmap

-   Complete codebook of all jmp datasets and of the package
-   Complete labeling of all of the datasets
-   Complete how-to documentation and several case studies to
    demonstrate use
-   Add WASH in Schools and WASH in Health Care Facilities country
    files.
-   Add use cases on combining with other data sets (national monitoring
    data, country TrackFin studies, etc.)
-   Add tests for data extraction and validation to cross validate
    country files against world files and different sheets against one
    another (as an extraction test and internal validation of the data
    sets).
-   Add helper functions to transform world and regional data between
    the original wide format and a long format.
-   Standardize the (long) data format used by datasets in the package.
-   Automate rebuilds using file hashes and sampling and a periodic poll
    of the JMP website
-   Post article on “Enhancing the use and quality of official
    statistics using open source”
-   Python wrapper library for easy inclusion in Python projects
