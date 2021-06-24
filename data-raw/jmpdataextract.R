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

.extract_wld_reg_data <- function() {
  jmp_hh_wld_path <- paste0("data-raw/WLD/", filter(jmp_files, geo == "WLD", type == "household")$filename)
  jmp_hh_reg_path <- paste0("data-raw/REG/", filter(jmp_files, geo == "REG", type == "household")$filename)

  jmp_wld_sanitation <- .get_jmp_tibble(jmp_hh_wld_path, sheet = "Sanitation Data", col_names = TRUE)
  jmp_wld_hygiene <- .get_jmp_tibble(jmp_hh_wld_path, sheet = "Hygiene Data", col_names = TRUE)
  jmp_wld_water <- .get_jmp_tibble(jmp_hh_wld_path, sheet = "Water Data", col_names = TRUE)

  jmp_reg_sanitation <- .get_jmp_tibble(jmp_hh_reg_path, sheet = "Sanitation Data", col_names = TRUE)
  jmp_reg_hygiene <- .get_jmp_tibble(jmp_hh_reg_path, sheet = "Hygiene Data", col_names = TRUE)
  jmp_reg_water <- .get_jmp_tibble(jmp_hh_reg_path, sheet = "Water Data", col_names = TRUE)

  usethis::use_data(jmp_wld_sanitation,
                    jmp_wld_hygiene,
                    jmp_wld_water,
                    jmp_reg_sanitation,
                    jmp_reg_hygiene,
                    jmp_reg_water,
                    overwrite = TRUE,
                    compress = "bzip2")
}


## Example of extracting Burkina Faso regression data from a country file
# We do not need this since it is already contained in the world file
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
    watsan_use_summary_data <- readxl::read_excel(hh_path, sheet = "Chart Data", range="CV5:GD208", col_names = TRUE, col_types = rep("text", 87))
    watsan_use_summary_data <- watsan_use_summary_data %>% filter(if_any(everything(), ~ (!is.na(.)&.!=0)))
    watsan_summary <- bind_cols(watsan_summary_data, watsan_use_summary_data)
    watsan_summary$iso3 <- x

    watsan_summary
  }) %>% bind_rows()

  jmp_household_hygiene_sources <- lapply(countries$geo, function(x) {
    hh_path <- paste0("data-raw/household/", filter(jmp_files, geo == x, type == "household")$filename)

    print(paste0("Hygiene summary from: ", hh_path))

    hyg_summary_data <- readxl::read_excel(hh_path, sheet = "Chart Data", range="CM5:CU208", col_names = TRUE, col_types = c(rep("text", 2), rep("numeric", 7)))
    hyg_summary_data <- hyg_summary_data %>% filter(if_any(everything(), ~ (!is.na(.)&.!=0)))
    hyg_use_summary_data <- readxl::read_excel(hh_path, sheet = "Chart Data", range="GE5:GJ208", col_names = TRUE, col_types = rep("text", 6))
    hyg_use_summary_data <- hyg_use_summary_data %>% filter(if_any(everything(), ~ (!is.na(.)&.!=0)))
    hyg_summary <- bind_cols(hyg_summary_data, hyg_use_summary_data)
    hyg_summary$iso3 <- x

    hyg_summary
  }) %>% bind_rows()

  usethis::use_data(jmp_household_watsan_sources,
                    jmp_household_hygiene_sources,
                    overwrite = TRUE,
                    compress = "bzip2")

}


### Procedure to extract inequality data

.extract_inequalities_data() {
  countries <- jmp_files %>% filter(type == "inequalities", !(geo %in% c("WLD", "REG")))

  quintile_vars <- function(x) {
    names(x) <- c("residence", "drop", "drop2", "year")
    x %>%
      select(-starts_with("drop")) %>%
      fill(residence, .direction = "down") %>%
      mutate(
        residence = stringr::str_to_title(residence),
        iso3 = "BFA"
      )
  }

}




## open macro enabled excel read_excel

# jmp_ineq_bfa_path <- paste0("data-raw/inequalities/", filter(jmp_files, geo == "BFA", type == "inequalities")$filename)

# list("Poorest" = 1, "Poor" = 2, "Middle" = 3, "Rich" = 4, "Richest" = 5)

## water quintiles

# offset + 5 per residence + 4 per quintile
#region_range <- cellranger::anchored("R15", dim = c(12, length(df_region_names))) %>%
#  cellranger::as.range(fo = "A1")

.range_list <- function(x_anchor = "B32") {
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

.shift <- function(old_anchor, x, y) {
  cell_limits(old_anchor[[1]] + c(x,y), old_anchor[[2]] + c(x,y))
}

.get_all_quintile_estimates <- function(ineq_path) {
  lapply(1:3, function(x, ranges) {
    print(x)
    quin_vars <- readxl::read_excel(ineq_path, sheet = "Water", range=ranges$residence[[x]], col_names = TRUE) %>%
      .quintile_vars()

    lapply(1:5, function(y, quintile_list) {
      print(y*1000)
      df_quin <- readxl::read_excel(ineq_path, sheet = "Water", range=as.character(quintile_list[y]), col_names = TRUE)
      names(df_quin) <- c("wat_bas", "wat_lim", "wat_unimp", "wat_sur")
      df_quin %>%
        mutate(quintile = names(quintile_list[y]))
      bind_cols(quin_vars, df_quin)
    }, quintile_list = ranges$quintiles[[x]]) %>% bind_rows()

  }, ranges = .range_list()) %>% bind_rows()
}

.get_inequalities_sources <- function() {
  # Get list of all sheets
  # Survey sheets are sheets not in c("Introduction", "Water", "Sanitation", "Hygiene")

  df_1 <- as_tibble(readxl::read_excel(jmp_ineq_bfa_path, sheet = "DHS03", range="A16:B26", col_names = FALSE)) %>% tidyr::fill(1, .direction = "down")
  names(df_1) <- c("var_type", "var_label")
  label_var <- list(
    "Improved" = "imp",
    "Not Improved" = "not_imp",
    "Basic" = "bas",
    "Limited" = "lim",
    "Other unimproved" = "unimp",
    "Surface water" = "sur",
    "Yes" = "gt30m",
    "No" = "ls30m",
    "Improved wells" = "imp_wells",
    "Improved springs" = "imp_springs",
    "Other" = "other"
  )
  df_1$var <- as.character(label_var[df_1$var_label])

  finish_quintile <- function(varx, quinx) {
    bind_cols(varx, quinx) %>% pivot_longer(4:8, names_to = "quintile")
  }

  df_composite <- readxl::read_excel(jmp_ineq_bfa_path, sheet = "DHS03", range="C15:G26", col_names = TRUE, col_types = rep("numeric", 5), na = c("","N/A")) %>%
    mutate(residence = "National")
  df_urban <- readxl::read_excel(jmp_ineq_bfa_path, sheet = "DHS03", range="H15:L26", col_names = TRUE, col_types = rep("numeric", 5), na = c("","N/A")) %>%
    mutate(residence = "Urban")
  df_rural <- readxl::read_excel(jmp_ineq_bfa_path, sheet = "DHS03", range="M15:Q26", col_names = TRUE, col_types = rep("numeric", 5), na = c("","N/A")) %>%
    mutate(residence = "Rural")

  list(df_composite, df_urban, df_rural) %>%
    lapply(function(x) {finish_quintile(df_1, x)}) %>%
    bind_rows() %>% add_survey_vars

  # No notes are saved from the surveys
  add_survey_vars <- function(x) {
    x %>% mutate(
      main_source = "DHS03",
      main_source_code = stringr::str_sub("DHS03", 1, -3),
      year_code = stringr::str_sub("DHS03", -2, -1),
      iso3 = "BFA"
    ) %>%
      mutate(
        year = ifelse(as.integer(year_code)<as.numeric(format(Sys.Date(), "%Y"))-2000, as.integer(year_code)+2000, as.integer(year_code)+1900)
      )
  }

  df_region_names <- readxl::read_excel(jmp_ineq_bfa_path, sheet = "DHS03", range="R15:ZZ15", col_names = TRUE, na = c("","N/A")) %>%
    select(-starts_with("...")) %>%
    names()

  region_range <- cellranger::anchored("R15", dim = c(12, length(df_region_names))) %>%
    cellranger::as.range(fo = "A1")

  df_region <- readxl::read_excel(jmp_ineq_bfa_path, sheet = "DHS03", range=region_range, col_names = TRUE, na = c("","N/A"))

  df_region <- bind_cols(df_1, df_region) %>% add_survey_vars()

}




### Surveys (including hygiene)

##
# A16:B26


### WatSan regressions can be found here:

## National water:

# Forecast period start: C99
# Forecast period end: C100
# Reg start year: C101
# Reg end year: C102

# Extract C99:C102 as list
# add names c("forecast_start", "forecast_end", "regression_start", "regression_end")

# c("wat_bas", "wat_lim", "wat_unimp", "wat_sur")

## Rural water: C299:C302, quintiles: D199:H200, I199:M200, N199:R200, S199:W200, X199:AB200
# Add "quintile" column with respective values: c("Poorest", "Poor", "Middle", "Rich", "Richest")
# Add column/var names to each dataset c("wat_bas", "wat_lim", "wat_unimp", "wat_sur")

## Urban water: C199:C202...

## National sanitation: same on "Sanitation" sheet ; repeat for same offsets as for water
# c("san_bas", "san_lim", "san_unimp", "san_od")

## Hygiene - just a representation of the surveys actually so left out

