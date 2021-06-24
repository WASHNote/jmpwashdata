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

jmp_hh_wld_path <- paste0("data-raw/WLD/", filter(jmp_files, geo == "WLD", type == "household")$filename)
jmp_hh_reg_path <- paste0("data-raw/REG/", filter(jmp_files, geo == "REG", type == "household")$filename)

jmp_wld_sanitation <- .get_jmp_tibble(jmp_hh_wld_path, sheet = "Sanitation Data", col_names = TRUE)
jmp_wld_hygiene <- .get_jmp_tibble(jmp_hh_wld_path, sheet = "Hygiene Data", col_names = TRUE)
jmp_wld_water_data <- .get_jmp_tibble(jmp_hh_wld_path, sheet = "Water Data", col_names = TRUE)

## Example of extracting Burkina Faso data from a country file
jmp_hh_bfa_path <- paste0("data-raw/household/", filter(jmp_files, geo == "BFA", type == "household")$filename)

# This is already contained in the world file
jmp_bfa_estimate_data <- .get_jmp_tibble(jmp_hh_bfa_path, sheet = "Regressions", range="A25:AD79", col_names = TRUE)
jmp_bfa_estimate_data$iso3 <- "BFA"
jmp_bfa_estimate_data$name <- jmp_bfa_estimate_data$country

# This is unique to the country file and includes data sources and what is used - this is what we want to extract
jmp_bfa_watsan_summary_data <- .get_jmp_tibble(jmp_hh_bfa_path, sheet = "Chart Data", range="A5:CL208", col_names = TRUE)
jmp_bfa_watsan_summary_data <- jmp_bfa_watsan_summary_data %>% filter(!is.na(source))
jmp_bfa_watsan_use_summary_data <- .get_jmp_tibble(jmp_hh_bfa_path, sheet = "Chart Data", range="CV5:GD208", col_names = TRUE)
jmp_bfa_watsan_use_summary_data <- jmp_bfa_watsan_use_summary_data %>% filter(!is.na(used_w_imp_n))
jmp_bfa_watsan_summary <- bind_cols(jmp_bfa_watsan_summary_data, jmp_bfa_watsan_use_summary_data)

jmp_bfa_hyg_summary_data <- .get_jmp_tibble(jmp_hh_bfa_path, sheet = "Chart Data", range="CM5:CU208", col_names = TRUE)
jmp_bfa_hyg_summary_data <- jmp_bfa_hyg_summary_data %>% filter(!is.na(hwsource))
jmp_bfa_hyg_use_summary_data <- .get_jmp_tibble(jmp_hh_bfa_path, sheet = "Chart Data", range="GE5:GJ208", col_names = TRUE)
jmp_bfa_hyg_use_summary_data <- jmp_bfa_hyg_use_summary_data %>% filter(!is.na(used_hw_fac_n))
jmp_bfa_hyg_summary <- bind_cols(jmp_bfa_hyg_summary_data, jmp_bfa_hyg_use_summary_data)

### Procedure to extract inequality data

## open macro enabled excel read_excel

jmp_ineq_bfa_path <- paste0("data-raw/inequalities/", filter(jmp_files, geo == "BFA", type == "inequalities")$filename)
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
    main_survey = "DHS03",
    main_survey_code = stringr::str_sub("DHS03", 1, -3),
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


list("Poorest" = 1, "Poor" = 2, "Middle" = 3, "Rich" = 4, "Richest" = 5)


# Get list of all sheets

# Survey sheets are sheets not in c("Introduction", "Water", "Sanitation", "Hygiene")

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

