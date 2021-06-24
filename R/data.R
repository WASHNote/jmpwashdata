#' JMP Excel file metadata.
#'
#' A dataset containing the files that were downloaded from JMP to build this package.
#'
#' @format A tibble 5 variables:
#' \describe{
#'   \item{path}{path to the file download}
#'   \item{filename}{filename of the downloaded file}
#'   \item{type}{whether the file pertains to household, schools, or healthcare data based on the download path}
#'   \item{geo}{the three letter code of each country based on ISO 3166-1 alpha-3 or WLD for the world files and REG for the regional files}
#'   \item{date}{the date the file was last downloaded}
#' }
#' @source \url{http://www.washdata.org/}
"jmp_files"

#' JMP Hygiene inequalities in  per quintile data.
#'
#' A dataset containing the files that were downloaded from JMP to build this package.
#'
#' @format A tibble 5 variables:
#' \describe{
#'   \item{path}{path to the file download}
#'   \item{filename}{filename of the downloaded file}
#'   \item{type}{whether the file pertains to household, schools, or healthcare data based on the download path}
#'   \item{geo}{the three letter code of each country based on ISO 3166-1 alpha-3 or WLD for the world files and REG for the regional files}
#'   \item{date}{the date the file was last downloaded}
#' }
#' @source \url{http://www.washdata.org/}
"jmp_inequality_hygiene_source"

#' JMP Excel file metadata.
#'
#' A dataset containing the files that were downloaded from JMP to build this package.
#'
#' @format A tibble 5 variables:
#' \describe{
#'   \item{path}{path to the file download}
#'   \item{filename}{filename of the downloaded file}
#'   \item{type}{whether the file pertains to household, schools, or healthcare data based on the download path}
#'   \item{geo}{the three letter code of each country based on ISO 3166-1 alpha-3 or WLD for the world files and REG for the regional files}
#'   \item{date}{the date the file was last downloaded}
#' }
#' @source \url{http://www.washdata.org/}

"jmp_inequality_sanitation_source"
"jmp_inequality_water_source"

"jmp_inequality_hygiene_estimate"
"jmp_inequality_sanitation_estimate"
"jmp_inequality_water_estimate"

"jmp_inequality_hygiene_region"
"jmp_inequality_sanitation_region"
"jmp_inequality_water_region"

"jmp_wld_hygiene"
"jmp_wld_sanitation"
"jmp_wld_water"

"jmp_reg_hygiene"
"jmp_reg_sanitation"
"jmp_reg_water"


