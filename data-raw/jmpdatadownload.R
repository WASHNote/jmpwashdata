## code to prepare `jmp_files` and download the JMP excel files

library(dplyr)
library(rio)
library(httr)
library(XML)
library(stringr)


.refresh_data_files <- function() {
  .remove_previous_data()

  page <- .download_page()

  wld_data <- .download_aggregate_files(page, target = "WLD")
  reg_data <- .download_aggregate_files(page, target = "REG")

  country_data <- .download_country_files(
    .country_links(page)
    )

  jmp_files <- bind_rows(list(
    wld_data,
    reg_data,
    country_data
  ))

  usethis::use_data(jmp_files,
                    overwrite = TRUE,
                    compress = "bzip2")
}

.remove_previous_data <- function() {
  unlink("data-raw/healthcare",recursive=TRUE)
  unlink("data-raw/household",recursive=TRUE)
  unlink("data-raw/inequalities",recursive=TRUE)
  unlink("data-raw/REG",recursive=TRUE)
  unlink("data-raw/schools",recursive=TRUE)
  unlink("data-raw/WLD",recursive=TRUE)
}


## Data is updated no more than annually as of 2021-06-22
# target = "WLD" or "REG"
.download_aggregate_files <- function(page, target = "WLD") {
  links <- .relevant_links(page) %>% .[grepl(paste0("*/", target, "/*"), .)]
  .download_country_files(links, folder = target)
}

.collect_metadata <- function(links) {
  lapply(seq_along(links), function(i, x) {
    .wait_approx(.1)
    message("Getting filename for ", x[[i]])
    jmp_excel <- HEAD("https://washdata.org/", path=x[[i]])
    filename = str_extract(
      jmp_excel$headers$`content-disposition`,
      '(?<=attachment; filename\\=).{1,128}(?=)'
    )
    type = str_extract(
      x[[i]],
      paste0('(?<=/[A-Z]{3}/).*(?=/download)')
    )
    geo = str_extract(x[[i]],"(?<=/)[A-Z]{3}(?=/)")
    tibble(path = x[[i]], filename = filename, type = type, geo = geo, date = Sys.Date())
  },
  x = links)
}

.wait_approx <- function(seconds) {
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<seconds){
    Sys.sleep(0.1)
  }
}

.download_country_files <- function(links, folder = NA, overwrite = TRUE) {
  files <- .collect_metadata(links)
  lapply(files, function(x) {
    print(x[1,"filename"])
    print(paste0("https://washdata.org",
                 x[1,"path"]))
    target_dir <- paste0("data-raw/", if (is.na(folder)) x[1, "type"] else folder)
    if (!dir.exists(target_dir)) dir.create(target_dir)
    .wait_approx(0.5)
    print(jmp_excel <- RETRY("GET", paste0("https://washdata.org",
                            x[1,"path"]),
                     write_disk(path = paste0(target_dir,"/",x[1,"filename"]), overwrite = overwrite),
                     verbose()
    ), timeout(1))
  })
  bind_rows(files)
}

.download_page <- function() {
  page <- GET(
    "https://washdata.org/",
    path="data/downloads",
  )
  cat(content(page, "text"), file="data-raw/washdata_download.html")
  page
}

.relevant_links <- function(page) {
  page %>%
    content(as = "text") %>%
    htmlParse() %>%
    xpathSApply("//a/@href") %>%
    .[grepl("*/download/*", .)] %>%
    .[grepl("*/country/*", .)]
}

.country_links <- function(page) {
  .relevant_links(page) %>%
    .[!grepl("*/(REG|WLD)/*", .)]
}

.country_hh_links <- function(page) {
  .country_links(page) %>%
    .[grepl("*/household/*", .)]
}

.country_inequality_links <- function(page) {
  .country_links(page) %>%
    .[grepl("*/inequalities/*", .)]
}

.country_schools_files <- function(page) {
  .country_links(page) %>%
    .[grepl("*/schools/*", .)]
}

.download_country_healthcare_files <- function(page) {
  .country_links(page) %>%
    .[grepl("*/healthcare/*", .)]
}


