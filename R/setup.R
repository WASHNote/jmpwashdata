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
  utils::install.packages("renv")
  utils::install.packages("dplyr")
  utils::install.packages("usethis")
  utils::install.packages("rmarkdown")
  utils::install.packages("roxygen2")
  utils::install.packages("rio")
  utils::install.packages("XML")
  utils::install.packages("stringr")
  utils::install.packages("tidyr")
}

.documentation <- function() {
  devtools::build_readme()
}
