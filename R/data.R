#' JMP Excel files online metadata.
#'
#' A dataset containing the files that were downloaded to build this package.
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
