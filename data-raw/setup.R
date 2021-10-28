# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


.setup <- function() {
  renv::activate()
  roxygen2::roxygenise()
  usethis::use_readme_rmd()
  usethis::use_data()
  usethis::use_data_raw()
}

<<<<<<< HEAD:data-raw/setup.R
.prep_for_build <- function() {
  renv::snapshot()
  devtools::build_readme()
=======
.install_requirements <- function() {
  utils::install.packages("renv")
  utils::install.packages("dplyr")
  utils::install.packages("usethis")
  utils::install.packages("rmarkdown")
  utils::install.packages("roxygen2")
  utils::install.packages("rio")
  utils::install.packages("XML")
  utils::install.packages("stringr")
  utils::install.packages("tidyr")
  utils::install.packages("testthat")
  utils::install.packages("devtools")
>>>>>>> 642b61a99b4d767989338d558fedfd8807624e19:R/setup.R
}
