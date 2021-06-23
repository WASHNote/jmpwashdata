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

.install_requirements <- function() {
  install.packages("renv")
  install.packages("dplyr")
  install.packages("usethis")
  install.packages("rmarkdown")
  install.packages("roxygen2")
  install.packages("rio")
  install.packages("XML")
  install.packages("stringr")
}

