################################################################################
rm(list = ls())
# Applying the cleaning log to clean the data
library(tidyverse)
library(lubridate)
library(glue)
library(magrittr)
library(stringi)
library(stringr)
library(supporteR)

source("R/composite_indicators.R")
options("openxlsx.dateFormat" = "dd/mm/yyyy")

# Read data and checking log 
# data

#df_cleaning_log1 <-read_csv("outputs/combined_checks_eth_lcsa_somali_GG2.csv", col_types = cols(sheet = "c", index = "i"))


data_path <- "outputs/combined_checks_eth_lcsa_somali_GG2_Carlos.xlsx"
df_cleaning_log <- readxl::read_excel(data_path, col_types = "text", na = "NA")|> 
  filter(!adjust_log %in% c("delete_log"), reviewed %in% c("1")) |>
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & str_detect(string = issue_id, pattern = "logic_c_"), "blank", value),
         value = ifelse(type %in% c("remove_survey"), "blank", value),
         name = ifelse(is.na(name) & type %in% c("remove_survey"), "point_number", name)
  ) |> 
  filter(!is.na(value), !is.na(uuid)) |>
  mutate(value = ifelse(value %in% c("blank"), NA, value),
          # sheet = NA,
          # index = NA,
         relevant = NA) |>
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

# raw data
data_path <- "inputs/REACH_ETH_LCSA_Somali_data .xlsx"

cols_to_escape <- c("index", "start", "end", "today", "starttime", "endtime", "_submission_time", "_submission__submission_time")

data_nms <- names(readxl::read_excel(path = data_path, n_max = 3000))
c_types <- case_when(str_detect(string = data_nms, pattern = "_other$") ~ "text", TRUE ~ "guess")

df_raw_data <- readxl::read_excel(path = data_path, col_types = c_types, na = c("NA", "N/A", "n/a"))#|> 
  # mutate(across(.cols = -c(contains(cols_to_escape)),
  #              .fns = ~ifelse(str_detect(string = .,
  #                                        pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA",.))) |>
  # rename_with(~str_replace(string = .x, pattern = "\\-|\\.", replacement = "_")) |>
  # rename_with(~str_replace(string = .x, pattern = "\\ ", replacement = "")) |>
  # mutate(across(.cols = -c(contains(cols_to_escape)),
  #              .fns = ~str_replace(string = ., pattern = "\\-|\\.", replacement = "_")))|>
  # mutate(across(.cols = -c(contains(cols_to_escape)),
  #              .fns = ~str_replace(string = ., pattern = "\\ ", replacement = "")))

# loops
# loop_hh_roster
loop_hh_roster <- readxl::read_excel(path = data_path, sheet = "hh_roster")

df_raw_data_loop_roster <- df_raw_data |> 
  select(-`_index`) |> 
  inner_join(loop_hh_roster, by = c("_uuid" = "_submission__uuid"))

# loop_hh_health
loop_hh_health <- readxl::read_excel(path = data_path, sheet = "health_loop")

df_raw_data_loop_health <- df_raw_data |> 
  select(-`_index`) |> 
  inner_join(loop_hh_health, by = c("_uuid" = "_submission__uuid"))

# tool
loc_tool <- "inputs/REACH_ETH_LCSA_Somali_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices") |> 
  mutate(label = `label::English`)
#         name = str_replace(string = name, pattern = "\\-|\\.", replacement = "_"),
#         name = str_replace(string = name, pattern = "\\ ", replacement = ""))

vars_to_remove_from_data = c("deviceid", "audit", "audit_URL", "instance_name", "person_name", "health_ind_name", 
                             "gps", "_gps_latitude", "_gps_longitude", "_gps_altitude", "_gps_precision") 

# main dataset ----------------------------------------------------------------

df_cleaning_log_main <-  df_cleaning_log |> 
  filter(is.na(sheet))

df_cleaning_step <- supporteR::cleaning_support(input_df_raw_data = df_raw_data |> select(-starts_with("healthcare_seek"
                                                                                                       )),
                                                input_df_survey = df_survey,
                                                input_df_choices = df_choices,
                                                input_df_cleaning_log = df_cleaning_log_main, 
                                                input_vars_to_remove_from_data = vars_to_remove_from_data) 


df_cleaned_data <- df_cleaning_step 
intermediate_cols <- c("lcsi_stress1", "lcsi_stress2", "lcsi_stress3", "lcsi_stress4", "lcsi_crisis1", "lcsi_crisis2", "lcsi_crisis3", "lcsi_emergency1", "lcsi_emergency2", "lcsi_emergency3", "lcsi_stress_yes", "lcsi_stress_exhaust", "lcsi_stress", "lcsi_crisis_yes", "lcsi_crisis_exhaust", "lcsi_crisis", "lcsi_emergency_yes", "lcsi_emergency_exhaust", "lcsi_emergency", "lcsi_cat_yes", "lcsi_cat_exhaust")

df_main_with_composites <- df_cleaned_data |> 
  create_composite_indicators() |> 
  select(-any_of(intermediate_cols)) |> 
  mutate(across(.cols = starts_with("i."), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .)))

# clean repeats ---------------------------------------------------------------

# roster
df_cleaned_data_log_roster <- df_raw_data_loop_roster |> 
  select(any_of(colnames(loop_hh_roster)), `_index`, `_submission__uuid` = "_uuid") |> 
  filter(`_submission__uuid` %in% df_cleaned_data$uuid)

# health
df_cleaning_log_health <- df_cleaning_log |> 
  filter(uuid %in% df_raw_data_loop_health$`_uuid`)
remove_index <- df_cleaning_log_health |> filter(!is.na(index))
df_cleaned_data_log_health <- supporteR::cleaning_support(input_df_raw_data = df_raw_data_loop_health |> select(-starts_with("healthcare_seek")),
                                                          input_df_survey = df_survey,
                                                          input_df_choices = df_choices,
                                                          input_df_cleaning_log = df_cleaning_log_health, 
                                                          input_vars_to_remove_from_data = vars_to_remove_from_data) |> 
  select(any_of(colnames(loop_hh_health)), `_index`= index, `_submission__uuid` = "uuid") |> 
  filter(`_submission__uuid` %in% df_cleaned_data$uuid)

# # deletion log --------------------------------------------------------------
# 
# df_deletion_log <- df_cleaning_log |> 
#   filter(type %in% c("remove_survey")) |> 
#   group_by(uuid) |> 
#   filter(row_number() == 1) |> 
#   ungroup()  

# write final datasets out ----------------------------------------------------

df_raw_data_final <- df_raw_data |> select(-starts_with("int.")) |>
  mutate(across(.cols = any_of(vars_to_remove_from_data), .fns = ~na_if(., .)))  

list_of_raw_datasets <- list("raw_main" = df_raw_data_final,
                             "raw_roster_loop" = loop_hh_roster |> select(-"person_name"),
                             "raw_health_loop" = loop_hh_health |> select(-"health_ind_name"))

openxlsx::write.xlsx(x = list_of_raw_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_raw_data_eth_lcsa_somali.xlsx"))

list_of_clean_datasets <- list("cleaned_main_data" = df_main_with_composites,
                               "cleaned_roster_loop" = df_cleaned_data_log_roster |> select(-"person_name"),
                               "cleaned_health_loop" = df_cleaned_data_log_health)

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_eth_lcsa_somali.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

################################################################################
