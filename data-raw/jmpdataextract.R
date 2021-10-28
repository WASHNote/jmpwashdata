## code to prepare the JMP datasets from downloaded files
## run first code to download the files and refresh the 'jmp_files' dataset

library(rio)
library(dplyr)
library(tidyr)

load("data/jmp_files.rda")

.get_jmp_tibble <- function(path, ...) {
  import(
    file = path,
    setclass = "tibble",
    ...)
}

# helper function for creating a codebook based on the columns in the compiled package
.get_all_package_columns <- function() {
  unlist_attr <- function(attr_list) {
    unlist(lapply(attr_list, function(x) {if (is.null(x)) {""} else {x}}))
  }

  lapply(data(package = "jmpwashdata")$results[, "Item"], function(x) {
      y <- get(x[[1]])

      label_list <- unlist_attr(var_attr(y, "label"))
      desc_list <- unlist_attr(var_attr(y, "description"))

      tibble(
        dataset = x,
        column = names(y),
        label = label_list,
        desc = desc_list
      )

    }) %>% bind_rows()
}

.save_codebook_template <- function() {
  write.csv(x = .get_all_package_columns(), file = "data-raw/codebook/codebook_template.csv")
}

.save_enriched_codebook_template <- function() {
  write.csv(x = .enriched_codebook_template(), file = "data-raw/codebook/codebook_template_enriched.csv")
}

.enriched_codebook_template <- function() {
  columns <- .get_all_package_columns()
  jmp_est_wide <- read.csv(file = "data-raw/codebook/jmp_codebook_estimate_wide.csv", header = TRUE)

  # need to add code to shorten names to remove the _[run]+

  columns %>% mutate(
    varname_short = str_match(string = columns$column, pattern = "(.*)_[run]+$")[,2]
  ) %>% left_join(jmp_est_wide, by = c("varname_short"))

}

# using error checking code same pattern  as the labelled package
# https://github.com/larmarange/labelled/blob/18b064a71f48644d4df79cfdfd61dcfc3aef9e80/R/var_label.R
`var_attr<-` <- function(x, attr_name, value) {
  if ((!is.character(value) & !is.null(value)) & !is.list(value) |
      (is.character(value) & length(value) > 1 & length(value) != ncol(x)))
    stop("`value` should be a named list, NULL, a single character string or a character vector of same length than the number of columns in `x`",
         call. = FALSE)
  if (is.character(value) & length(value) == 1) {
    value <- as.list(rep(value, ncol(x)))
    names(value) <- names(x)
  }
  if (is.character(value) & length(value) == ncol(x)) {
    value <- as.list(value)
    names(value) <- names(x)
  }
  if (is.null(value)) {
    value <- as.list(rep(1, ncol(x)))
    names(value) <- names(x)
    value <- lapply(value, function(x) {
      x <- NULL
    })
  }

  if (!all(names(value) %in% names(x)))
    stop("some variables not found in x")

  value <- value[names(value) %in% names(x)]

  for (var in names(value)) {
    if (length(value[[var]]) > 1)
      stop("each attribute `value` should be a single value or NULL",
           call. = FALSE)
    attr(x[[var]], attr_name) <- value[[var]]
  }
  x
}
# same as above
var_attr <- function(x, attr_name, unlist = FALSE) {
  r <- lapply(x, function(y, unlist = FALSE) {
    attr(y, attr_name, exact = TRUE)
  })
  if (unlist) {
    r <- lapply(r, function(y){if (is.null(y)) "" else y})
    base::unlist(r, use.names = TRUE)
  } else
    r
}

# will run all extraction functions and save messages to a table
# would be even better to generalize this to all types of messages into a log file
.extract_all_data <- function() {
  jmp_extraction_messages <- lapply(list(
    ".extract_wld_reg_data",
    ".extract_country_hh_summary_data",
    ".extract_inequalities_estimate_data",
    ".extract_inequalities_region_data",
    ".extract_inequalities_source_data",
    ".extract_inequalities_data_summary"
  ), function(fn) {
    error_txt <- character()
    tryCatch(expr = {
      do.call(fn, args = list())
    },
    error = function(e) {
      error_txt <<- paste0(e)
      message(paste0(e))
    })
    #message("length(error_txt) = ", length(error_txt))
    #message("length(names(warnings())) = ", length(names(warnings())))
    tibble(
      procedure = fn,
      message = c(names(warnings()), error_txt),
      message_type = c(rep("warning", times = length(names(warnings()))), rep("error", times = length(error_txt)))
    )
    assign("last.warning", NULL, envir = baseenv())
  }) %>% bind_rows()

  usethis::use_data(
    jmp_extraction_messages,
    overwrite = TRUE,
    compress = "bzip2")

  if (nrow(jmp_extraction_messages %>% filter(message_type == "error"))>0) {
    stop("Errors were produced during the extraction of the data sets. Please check the data/jmp_extraction_messages.rda to find the list of errors.")
  }
}

.extract_wld_reg_data <- function() {
  jmp_hh_wld_path <- paste0("data-raw/WLD/", filter(jmp_files, geo == "WLD", type == "household")$filename)
  jmp_hh_reg_path <- paste0("data-raw/REG/", filter(jmp_files, geo == "REG", type == "household")$filename)

  jmp_wld_sanitation <- .get_jmp_tibble(jmp_hh_wld_path, sheet = "san", col_names = TRUE)
  jmp_wld_hygiene <- .get_jmp_tibble(jmp_hh_wld_path, sheet = "hyg", col_names = TRUE)
  jmp_wld_water <- .get_jmp_tibble(jmp_hh_wld_path, sheet = "wat", col_names = TRUE)
  jmp_wld_menstrual_hygiene <- .get_jmp_tibble(jmp_hh_wld_path, sheet = "mh", col_names = TRUE)

  jmp_reg_sanitation <- .get_jmp_tibble(jmp_hh_reg_path, sheet = "san", col_names = TRUE)
  jmp_reg_hygiene <- .get_jmp_tibble(jmp_hh_reg_path, sheet = "hyg", col_names = TRUE)
  jmp_reg_water <- .get_jmp_tibble(jmp_hh_reg_path, sheet = "wat", col_names = TRUE)

  usethis::use_data(jmp_wld_sanitation,
                    jmp_wld_hygiene,
                    jmp_wld_water,
                    jmp_wld_menstrual_hygiene,
                    jmp_reg_sanitation,
                    jmp_reg_hygiene,
                    jmp_reg_water,
                    overwrite = TRUE,
                    compress = "bzip2")
}


## Example of extracting Burkina Faso regression data from a country file
# Keeping for future error checking
# In principle, we do not need this since it is already contained in the world file
# jmp_bfa_estimate_data <- .get_jmp_tibble(jmp_hh_bfa_path, sheet = "Regressions", range="A25:AD79", col_names = TRUE)
# jmp_bfa_estimate_data$iso3 <- "BFA"
# jmp_bfa_estimate_data$name <- jmp_bfa_estimate_data$country

.extract_country_hh_summary_data <- function() {

  # The survey summary data is unique to the country file and includes data sources and what is used - this is what we want to extract
  countries <- jmp_files %>% filter(type == "household", !(geo %in% c("WLD", "REG")))
  #%>% filter(geo == "AFG")

  jmp_household_watsan_sources <- lapply(countries$geo, function(x) {
    hh_path <- paste0("data-raw/household/", filter(jmp_files, geo == x, type == "household")$filename)

    print(paste0("Watsan summary from: ", hh_path))

    watsan_summary_data <- readxl::read_excel(hh_path, sheet = "Chart Data", range="A5:CL208", col_names = TRUE, col_types = c(rep("text", 2), rep("numeric", 88)))
    watsan_summary_data <- watsan_summary_data %>% filter(if_any(everything(), ~ (!is.na(.)&.!=0)))
    #watsan_use_summary_data <- readxl::read_excel(hh_path, sheet = "Chart Data", range="CV5:GD208", col_names = TRUE, col_types = rep("text", 87))
    #watsan_use_summary_data <- watsan_use_summary_data %>% filter(if_any(everything(), ~ (!is.na(.)&.!=0)))

    #watsan_summary <- bind_cols(watsan_summary_data, watsan_use_summary_data)
    watsan_summary <- watsan_summary_data

    watsan_summary$iso3 <- x

    watsan_summary %>% .lengthen_household_sources()
  }) %>% bind_rows()

  jmp_household_hygiene_sources <- lapply(countries$geo, function(x) {
    hh_path <- paste0("data-raw/household/", filter(jmp_files, geo == x, type == "household")$filename)

    print(paste0("Hygiene summary from: ", hh_path))

    hyg_summary_data <- readxl::read_excel(hh_path, sheet = "Chart Data", range="CM5:CU208", col_names = TRUE, col_types = c(rep("text", 2), rep("numeric", 7)))
    hyg_summary_data <- hyg_summary_data %>% filter(if_any(everything(), ~ (!is.na(.)&.!=0)))
    #hyg_use_summary_data <- readxl::read_excel(hh_path, sheet = "Chart Data", range="GE5:GJ208", col_names = TRUE, col_types = rep("text", 6))
    #hyg_use_summary_data <- hyg_use_summary_data %>% filter(if_any(everything(), ~ (!is.na(.)&.!=0)))

    #hyg_summary <- bind_cols(hyg_summary_data, hyg_use_summary_data)
    hyg_summary <- hyg_summary_data

    hyg_summary$iso3 <- x

    hyg_summary %>% .lengthen_household_sources()
  }) %>% bind_rows()

  usethis::use_data(jmp_household_watsan_sources,
                    jmp_household_hygiene_sources,
                    overwrite = TRUE,
                    compress = "bzip2")

}


### Procedure to extract inequality data

.extract_inequalities_estimate_data <- function() {
  countries <- jmp_files %>% filter(type == "inequalities", !(geo %in% c("WLD", "REG")))

  use_data <- usethis::use_data

  # 2 x estimates
  lapply(c("water", "sanitation"), function(service_type) {
    dataset_name <- paste0("jmp_inequality_",service_type,"_estimate")

    print(dataset_name)

    dataset <- lapply(countries$geo, function(x) {
      ineq_path <- paste0("data-raw/inequalities/", filter(countries, geo == x)$filename)

      print(ineq_path)

      .get_watsan_quintile_estimates(
        ineq_path = ineq_path,
        iso3 = x,
        service_type = service_type
      )
    }) %>% bind_rows()

    assign(dataset_name, dataset)

    do.call("use_data", list(as.name(dataset_name), overwrite = TRUE, compress = "bzip2"))

  })

}

.extract_inequalities_region_data <- function(verbose = FALSE) {
  countries <- jmp_files %>% filter(type == "inequalities", !(geo %in% c("WLD", "REG")))
  #%>% filter(geo == "NPL")

  use_data <- usethis::use_data

  # 3 x sources
  lapply(c("water", "sanitation", "hygiene"), function(service_type) {
    dataset_name <- paste0("jmp_inequality_",service_type,"_region")

    message(paste0("Prepping ", dataset_name))

    dataset <- lapply(countries$geo, function(x) {
      ineq_path <- paste0("data-raw/inequalities/", filter(countries, geo == x)$filename)

      message(ineq_path)

      .get_inequalities_region(
        ineq_path = ineq_path,
        iso3 = x,
        service_type = service_type,
        verbose = verbose
      )
    }) %>% bind_rows()

    assign(dataset_name, dataset)

    do.call("use_data", list(as.name(dataset_name), overwrite = TRUE, compress = "bzip2"))

  })

}

.extract_inequalities_source_data <- function() {
  countries <- jmp_files %>% filter(type == "inequalities", !(geo %in% c("WLD", "REG")))
  #%>% slice_head(n = 2)

  use_data <- usethis::use_data

  # 3 x sources
  lapply(c("water", "sanitation", "hygiene"), function(service_type) {
    dataset_name <- paste0("jmp_inequality_",service_type,"_source")

    message("Extracting data for ", dataset_name)

    dataset <- lapply(countries$geo, function(x) {
      ineq_path <- paste0("data-raw/inequalities/", filter(countries, geo == x)$filename)

      message(ineq_path)

      .get_inequalities_sources(
        ineq_path = ineq_path,
        iso3 = x,
        service_type = service_type
      )
    }) %>% bind_rows()

    assign(dataset_name, dataset)

    do.call("use_data", list(as.name(dataset_name), overwrite = TRUE, compress = "bzip2"))

  })

}


.extract_inequalities_data_summary <- function() {
  countries <- jmp_files %>% filter(type == "inequalities", !(geo %in% c("WLD", "REG"))) #%>% slice_head(n = 2)

  use_data <- usethis::use_data

  dataset_name_region <- paste0("jmp_inequality_summary_region_data")
  dataset_name_quintile <- paste0("jmp_inequality_summary_quintile_data")
  message("Extracting ", dataset_name)

  dataset <- lapply(countries$geo, function(x) {
    ineq_path <- paste0("data-raw/inequalities/", filter(countries, geo == x)$filename)

    message(ineq_path)

    .get_inequalities_data_summary(
      ineq_path = ineq_path
    )
  })

  assign(dataset_name_region, lapply(dataset, function(x) {
    x$region_data
  }) %>% bind_rows()) %>% distinct() %>% filter(!is.na(val))
  do.call("use_data", list(as.name(dataset_name_region), overwrite = TRUE, compress = "bzip2"))

  assign(dataset_name_quintile, lapply(dataset, function(x) {
    x$quintile_data
  }) %>% bind_rows()) %>% distinct() %>% filter(!is.na(val))
  do.call("use_data", list(as.name(dataset_name_quintile), overwrite = TRUE, compress = "bzip2"))
}


## open macro enabled excel read_excel
# c("Poorest", "Poor", "Middle", "Rich", "Richest")
## water quintile estimates
# offset + 5 per residence + 4 per quintile

.get_quintile_names <- function() {
  c("Poorest", "Poor", "Middle", "Rich", "Richest")
}

.shift <- function(old_anchor, x, y) {
  cell_limits(old_anchor[[1]] + c(x,y), old_anchor[[2]] + c(x,y))
}

.estimate_range_list <- function(x_anchor = "B32") {
  require(cellranger)
  anchor <- as.cell_limits(x_anchor)
  anchor[[2]] <- anchor[[1]] + c(2, 3)

  list(
    residence = c(
      as.range(.shift(anchor, 0,0)),
      as.range(.shift(anchor, 3,0)),
      as.range(.shift(anchor, 6,0))
    ),
    quintiles = list(
      list(
        "Poorest" = as.range(.shift(anchor, 0,4)),
        "Poor" = as.range(.shift(anchor, 0,8)),
        "Middle" = as.range(.shift(anchor, 0,12)),
        "Rich" = as.range(.shift(anchor, 0,16)),
        "Richest" = as.range(.shift(anchor, 0,20))
      ),
      list(
        "Poorest" = as.range(.shift(anchor, 3,4)),
        "Poor" = as.range(.shift(anchor, 3,8)),
        "Middle" = as.range(.shift(anchor, 3,12)),
        "Rich" = as.range(.shift(anchor, 3,16)),
        "Richest" = as.range(.shift(anchor, 3,20))
      ),
      list(
        "Poorest" = as.range(.shift(anchor, 6,4)),
        "Poor" = as.range(.shift(anchor, 6,8)),
        "Middle" = as.range(.shift(anchor, 6,12)),
        "Rich" = as.range(.shift(anchor, 6,16)),
        "Richest" = as.range(.shift(anchor, 6,20))
      )
    )
  )
}

.source_range_list <- function() {
  list(
    "summary" = list(
      "data" = "A1:ZZ2000"
    ),
    "water" = list(
      "var_list" = "A16:B26",
      "composite" = "C15:G26",
      "urban" = "H15:L26",
      "rural" = "M15:Q26",
      "region_anchor" = "R15",
      "region" = "R15:ZZ15"
    ),
    "sanitation" = list(
      "var_list" = "A122:B132",
      "composite" = "C121:G132",
      "urban" = "H121:L132",
      "rural" = "M121:Q132",
      "region_anchor" = "R121",
      "region" = "R121:ZZ121"
    ),
    "hygiene" = list(
      "var_list" = "A222:B224",
      "composite" = "C221:G224",
      "urban" = "H221:L224",
      "rural" = "M221:Q224",
      "region_anchor" = "R221",
      "region" = "R221:ZZ221"
    )
  )
}

.label_var_list <- function() {
  common <- list(
    "Improved" = "imp",
    "1 Improved" = "imp",
    "Not Improved" = "not_imp",
    "2 Not Improved" = "not imp",
    "Basic" = "bas",
    "1 Basic" = "bas",
    "1,00 Basic" = "bas",
    "Limited" = "lim",
    "2 Limited" = "lim",
    "2,00 Limited" = "lim",
    "Unimproved" = "unimp",
    "Other unimproved" = "unimp",
    "3 Other unimproved" = "unimp",
    "Open defecation" = "od",
    "4 Open defecation" = "od",
    "Surface water" = "sur",
    "4 Surface water" = "sur",
    "Improved wells" = "imp_wells",
    "Improved springs" = "imp_springs",
    "Other" = "other",
    "No facility" = "nfac",
    "3,00 No facility" = "nfac",
    "nowhere" = "nowhere",
    "in dwelling/yard/plot" = "premises",
    "somewhere else" = "else"
  )
  list(
    "water" = c(
      common,
      list(
        "Yes" = "gt30m",
        "No" = "ls30m"
      )
    ),
    "sanitation" = c(
      common,
      list(
        "Improved latrines" = "imp_lat",
        "Yes" = "shared",
        "No" = "not_shared",
        "1 Yes" = "shared",
        "2 No" = "not_shared"
      )
    ),
    "hygiene" = common
  )
}

# hygiene not yet included here as it is per survey so stored in sources dataset
.estimate_var_list <- function() {
  list(
    "water" = c("wat_bas", "wat_lim", "wat_unimp", "wat_sur"),
    "sanitation" = c("san_bas", "san_lim", "san_unimp", "san_od")
  )

}


.estimate_quintile_vars <- function(x, iso3) {
  names(x) <- c("residence", "drop", "drop2", "year")
  x %>%
    select(-starts_with("drop")) %>%
    fill(residence, .direction = "down") %>%
    mutate(
      residence = stringr::str_to_title(residence),
      iso3 = iso3
    )
}

.get_watsan_quintile_estimates <- function(ineq_path, iso3, service_type = "water") {
  sheet = stringr::str_to_title(service_type)
  var_names = .estimate_var_list()[[service_type]]

  # for later - would be more readable to name the residence ranges
  lapply(1:3, function(x, ranges) {
    message(x)
    quin_vars <- suppressMessages(
      readxl::read_excel(ineq_path, sheet = sheet, range=ranges$residence[[x]], col_names = TRUE)
    ) %>%
      .estimate_quintile_vars(iso3)

    lapply(1:5, function(y, quintile_list) {
      print(y*1000)
      df_quin <- suppressMessages(
        readxl::read_excel(ineq_path, sheet = sheet, range=as.character(quintile_list[y]), col_names = TRUE)
      )
      names(df_quin) <- var_names
      df_quin <- df_quin %>%
        mutate(quintile = names(quintile_list[y]),
               quintile_n = y)
      bind_cols(quin_vars, df_quin)
    }, quintile_list = ranges$quintiles[[x]]) %>% bind_rows()
  }, ranges = .estimate_range_list()) %>% bind_rows()
}

.finish_quintile <- function(varx, quinx, verbose = FALSE) {
  bind_cols(varx, quinx) %>% pivot_longer(4:8, names_to = "quintile") %>% filter(!is.na(value))
}

.get_inequalities_sources <- function(ineq_path, iso3, service_type = "water", verbose = FALSE) {
  sheets <- .get_inequalities_source_sheets(ineq_path)

  r1 <- lapply(sheets, function(sheet_name) {
    if (verbose) {
      message("Starting on sheet ", sheet_name)
    }
    .get_inequalities_source_by_sheet(ineq_path, iso3, sheet_name, service_type, verbose)
  })
  r1 %>% bind_rows() %>% mutate(
    iso3 = iso3
  )

}

.get_inequalities_source_var_columns <- function(ineq_path, sheet_name, service_type) {
  label_var = .label_var_list()[[service_type]]
  locations <- .source_range_list()

  # get first var columns
  df_1 <- suppressMessages(
    readxl::read_excel(ineq_path, sheet = sheet_name, range=locations[[service_type]][["var_list"]], col_names = FALSE)
  ) %>% tidyr::fill(1, .direction = "down")
  names(df_1) <- c("var_type", "var_label")
  df_1$var <- as.character(label_var[df_1$var_label])

  df_1
}

# .get_quintile_names() is used to retrieve consistent column names; after examination, and probably due to the excel charts, the actual order of each quintile is consistent across sheets so this should be safe if it stays this way
# explicit renaming in case one needs to ever recover the original columns later on
.get_inequalities_source_by_sheet <- function(ineq_path, iso3, sheet_name, service_type, verbose = FALSE) {
  locations <- .source_range_list()

  df_1 <- .get_inequalities_source_var_columns(ineq_path, sheet_name, service_type)

  # get data per residence type
  df_composite <- suppressMessages(
    readxl::read_excel(ineq_path, sheet = sheet_name, range=locations[[service_type]][["composite"]], col_names = TRUE, col_types = rep("numeric", 5), na = c("","N/A","N/a"))
  )
  names(df_composite) <- .get_quintile_names()
  df_composite <- df_composite %>%
    mutate(residence = "National")

  if (nrow(df_composite)==0) {
    df_composite <- NULL
  }

  df_urban <- suppressMessages(
    readxl::read_excel(ineq_path, sheet = sheet_name, range=locations[[service_type]][["urban"]], col_names = TRUE, col_types = rep("numeric", 5), na = c("","N/A","N/a"))
  )
  names(df_urban) <- .get_quintile_names()
  df_urban <- df_urban %>%
    mutate(residence = "Urban")

  if (nrow(df_urban)==0) {
    df_urban <- NULL
  }


  df_rural <- suppressMessages(
    readxl::read_excel(ineq_path, sheet = sheet_name, range=locations[[service_type]][["rural"]], col_names = TRUE, col_types = rep("numeric", 5), na = c("","N/A","N/a"))
  )
  names(df_rural) <- .get_quintile_names()
  df_rural <- df_rural %>%
    mutate(residence = "Rural")

  if (nrow(df_rural)==0) {
    df_rural <- NULL
  }


  if (verbose) {
    #message("composite nrow = ", nrow(df_composite))
    #message(names(df_composite))
    #message(str(df_composite))
  }


  # combine all data and pivot_longer with .finish_quintile
  result <- list(df_composite, df_urban, df_rural) %>%
    lapply(function(x) {.finish_quintile(df_1, x, verbose)}) %>%
    bind_rows() %>% .add_survey_vars(sheet_name, iso3)

  if (verbose) {
    message("result nrow = ", nrow(result))
    message(names(result))
    message(str(result))
  }

  if (nrow(result)==0) {
    NULL
  } else {
    result
  }

}

# No notes are saved from the surveys
.add_survey_vars <- function(x, sheet_name, iso3) {

  year_code <- stringr::str_sub(sheet_name, -2, -1)
  main_source_code = stringr::str_sub(sheet_name, 1, -3)

  if (suppressWarnings(is.na(as.numeric(year_code)))) {
    year_code <- stringr::str_extract(sheet_name, "[0-9]+")
    main_source_code = stringr::str_replace_all(sheet_name, "[0-9]+", "")
  }

  x %>% mutate(
    main_source = sheet_name,
    main_source_code = main_source_code,
    year_code = year_code,
    iso3 = iso3
  ) %>%
    mutate(
      year = ifelse(as.integer(year_code)<as.numeric(format(Sys.Date(), "%Y"))-2000, as.integer(year_code)+2000, as.integer(year_code)+1900)
    )
}

.get_inequalities_source_sheets <- function(ineq_path) {
  readxl::excel_sheets(ineq_path) %>% .[!(. %in% c("Introduction", "Water", "Sanitation", "Hygiene", "data_summary"))]
}

.get_inequalities_region <- function(ineq_path, iso3, service_type = "water", verbose = FALSE) {
  sheets <- .get_inequalities_source_sheets(ineq_path)

  lapply(sheets, function(sheet_name) {
    if (verbose) {
      message("Getting region ", service_type, " data in sheet ", sheet_name)
    }
    .get_inequalities_region_by_sheet(ineq_path, iso3, sheet_name, service_type, verbose)
  }) %>% bind_rows() %>% mutate(
    iso3 = iso3
  )

}

.get_inequalities_region_by_sheet <- function(ineq_path, iso3, sheet_name, service_type, verbose = FALSE) {
  locations <- .source_range_list()

  df_1 <- .get_inequalities_source_var_columns(ineq_path, sheet_name, service_type)


  # the number of rows can vary per type
  cl <- cellranger::as.cell_limits(locations[[service_type]][["composite"]])
  var_length <- cl[[2]][1] - cl[[1]][1] + 1

  df_region_names <- suppressMessages(
    readxl::read_excel(ineq_path, sheet = sheet_name, range=locations[[service_type]][["region"]], col_names = TRUE, na = c("","N/A","N/a"))
  ) %>% names()

  # Only use non-empty column headers
  non_empty_columns <- grep("^\\.", df_region_names)

  if (length(non_empty_columns) == 0 || non_empty_columns[[1]] == 1) {
    #stop("Workbook ", ineq_path ," without any region headers in sheet ", sheet_name," ! Check if formatted correctly, otherwise will need to start returning an empty tibble here.")
    return(NULL)
  } else {
    df_region_names <- df_region_names[1:non_empty_columns[[1]]-1]
  }

  if (verbose) {
    message("Sheet: ", sheet_name)
    message("Region names: ", df_region_names)
  }

  if (length(df_region_names)>0) {
    region_range <- cellranger::anchored(locations[[service_type]][["region_anchor"]], dim = c(var_length, length(df_region_names))) %>%
      cellranger::as.range(fo = "A1")

    df_region <- suppressMessages(
      readxl::read_excel(ineq_path, sheet = sheet_name, range=region_range, col_names = TRUE, col_types = rep("numeric", length(df_region_names)), na = c("","N/A","N/a"))
    )

    bind_cols(df_1, df_region) %>%
      pivot_longer(4:(3+length(df_region_names)), names_to = "region", values_drop_na = TRUE) %>%
      .add_survey_vars(sheet_name, iso3)
  } else {
    df_1$region <- NA_character_
    df_1$value <- as.numeric(NA)
    df_1 %>% filter(!is.na(value)) %>%
      .add_survey_vars(sheet_name, iso3)
  }
}

.get_inequalities_data_summary <- function(ineq_path, verbose = FALSE) {
  locations <- .source_range_list()

  tryCatch(expr = {
    suppressMessages({
      data_summary <- readxl::read_excel(ineq_path, sheet = "data_summary", range=locations[["summary"]][["data"]], col_names = TRUE, na = c("","N/A","N/a")) %>%
        select(-starts_with("...")) %>%
        filter(!is.na(iso3))
    })

    #data_summary <- matches("(.*)(region_[0-9]+)(!_name)", perl = TRUE))
    region_names <- data_summary %>%
      names() %>%
      str_match("region_[0-9]+") %>%
      unique() %>%
      na.omit() %>%
      as.character()

    list(

      region_data = lapply(region_names, function(region) {
        tryCatch(
          expr = {data_summary %>%
              select(1:7, matches(paste0(region,"(_name)?$"))) %>%
              rename(
                region = .data[[paste0(region,"_name")]],
                val = .data[[region]],
                n = .data[[paste0("n_",region)]],
              ) %>%
              mutate(
                var = str_match(varname, "._(.*)")[,2],
                var_type = .prefix_to_sector()[str_match(varname, "(._).*")[,2]]
              ) %>%
              fill(var_type, .direction = "down")},
          error = function (e) {
            warning("Could not add ", region, " from data summary sheet in ", ineq_path," with error message ", e)
            return(NULL)
          })
      }) %>% bind_rows(),

      quintile_data = lapply(1:5,function(quintile) {
        q_str <- paste0("q", quintile)
        lapply(c("total", "urban", "rural"), function(context) {
          data_summary %>%
            select(1:7, matches(paste0(context,"_",q_str))) %>%
            rename(
              val = .data[[paste0(context,"_",q_str)]],
              n = .data[[paste0("n_",context,"_",q_str)]]
            ) %>%
            mutate(
              quintile = quintile,
              var = str_match(varname, "._(.*)")[,2],
              var_type = .prefix_to_sector()[str_match(varname, "(._).*")[,2]]
            ) %>%
            fill(var_type, .direction = "down")
        }) %>% bind_rows()
      }) %>% bind_rows

    )
  },
  error = function(e) {
    warning("Could not add summary sheet from ", ineq_path, " with error message ", e)
    return(NULL)
  })

}

.prefix_to_sector <- function() {
  c(
    `s_` = "Sanitation",
    `w_` = "Water",
    `h_` = "Hygiene"
  )
}

.suffix_to_residence <- function() {
  c(
    `_n` = "National",
    `_u` = "Urban",
    `_r` = "Rural"
  )
}

.lengthen_household_sources <- function(household_source) {
  lapply(names(.prefix_to_sector()), function(prefix) {
      lapply(names(.suffix_to_residence()), function(suffix) {
        if (length(names(household_source %>% select(starts_with(prefix)) %>% select(ends_with(suffix))))) {
          source_slice <- household_source
          if ("h_source" %in% names(source_slice)) {
            source_slice <- source_slice %>% rename(source = h_source, type = h_type, year = h_year)
          }
          source_slice <- source_slice %>%
            select(iso3, source, type, year, starts_with(prefix)) %>%
            select(iso3, source, type, year, ends_with(suffix)) %>%
            filter(!grepl("[]].", source))
          #str_match(string = names(jmp_household_hygiene_sources), pattern = "h_(.*)_[nur].")[,3]
          if (nrow(source_slice) > 0) {
            source_slice %>%
              pivot_longer(
                cols = ends_with(suffix),
                names_to = "varname",
                values_to = "val"
              ) %>%
              filter(!is.na(val)) %>%
              mutate(
                var = str_match(string = varname, pattern = "^(.*[hws]+_(.*)_[nur]+)$")[,3],
                sector = .prefix_to_sector()[prefix],
                residence = .suffix_to_residence()[suffix]
              )
          } else {NULL}
        } else {NULL}
      }) %>% bind_rows()
  }) %>% bind_rows()
}

