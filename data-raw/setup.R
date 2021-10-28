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

.prep_for_build <- function() {
  renv::snapshot()
  devtools::build_readme()
}
