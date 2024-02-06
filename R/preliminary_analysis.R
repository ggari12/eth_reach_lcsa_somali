###############################################################################

library(tidyverse)
library(srvyr)
library(supporteR)  

source("R/composite_indicators.R")
getwd()
# packages to install incase
# devtools::install_github("zackarno/butteR")
# devtools::install_github("twesigye10/supporteR")

# clean data
data_path <- "inputs/20240202_clean_data_eth_lcsa_somali.xlsx"
weight_table <- readxl::read_excel("inputs/data_eth_lcsa_somali_weighted.xlsx")|>
  dplyr::group_by(hh_zone, pop_group)

data_nms <- names(readxl::read_excel(path = data_path, n_max = 3000, sheet = "cleaned_main_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_main_data", col_types = c_types, na = "NA") |> 
  dplyr::select(-starts_with("i.")) |> 
  create_composite_indicators() |> 
  dplyr::mutate(strata = hh_woreda) |> 
  dplyr::mutate(across(.cols = starts_with("i."), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .)))

# add weights to data
df_main_clean_data_with_weights <- df_main_clean_data |>
  dplyr::group_by(hh_zone, pop_group)|>
  left_join(weight_table, by = c("hh_zone", "pop_group"))|>
  dplyr::rename(zone1="hh_zone")


writexl::write_xlsx(df_main_clean_data_with_weights, "inputs/clean_data_eth_lcsa_somali_weighted.xlsx")

loop_support_data <- df_main_clean_data_with_weights|> 
  dplyr::select(uuid, hh_woreda, i.hoh_gender,strata, weights)

#Load loop

health_loop <- readxl::read_excel(path = data_path, sheet = "cleaned_health_loop", na = "NA")

df_health_clean_data <- loop_support_data |> 
  inner_join(health_loop, by = c("uuid" = "_submission__uuid") ) 


roster_loop <- readxl::read_excel(path = data_path, sheet = "cleaned_roster_loop", na = "NA")|> 
create_composite_indicators_roster()
df_roster_clean_data <- loop_support_data |> 
  inner_join(roster_loop, by = c("uuid" = "_submission__uuid") ) 

# tool
loc_tool <- "inputs/REACH_ETH_LCSA_Somali_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

df_tool_data_support <- df_survey |> 
  select(type, name, label = `label::English`) |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple")) |> 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" )

# dap
dap <- read_csv("inputs/r_dap_eth_lcsa_somali.csv")

# main dataset ------------------------------------------------------------

# set up design object
ref_svy <- as_survey(.data = df_main_clean_data_with_weights, strata = strata, weights = weights)

# analysis

df_main_analysis <- analysis_after_survey_creation(input_svy_obj = ref_svy,
                                                   input_dap = dap) |> 
  dplyr::mutate(level = "Household")


# health loop -------------------------------------------------------------



# set up design object
ref_svy_health_loop <- as_survey(.data = df_health_clean_data, strata = strata, weights = weights)

# analysis
df_analysis_health_loop <- analysis_after_survey_creation(input_svy_obj = ref_svy_health_loop,
                                                         input_dap = dap |>
                                                          filter(!variable %in% c("respondent_age"))
                                                         )|>
 mutate(level = "Individual")



# roster ------------------------------------------------------------------

df_dap_roster <- bind_rows(tibble::tribble(~variable,
                                           "i.individual_age_cat",
                                           "i.individual_genre_cat",
                                           "i.individual_age_school_cat",
                                           "i.individual_genre_age_cat")) |> 
  mutate(split = "all",
         subset_1 = "hh_woreda",
         subset_2 = "ind_gender"
  ) |> 
  pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
  filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
  select(-subset_no)



# df_main_pivot <- df_main_clean_data_with_weights |> 
#   pivot_longer(cols = num_males_0to6:num_females_66plusyrs, names_to = "i.num_gender_age", values_to = "i.hh_size_based_on_gender_age")
# 
# df_roster_extract <- df_main_pivot |> 
#   filter(i.hh_size_based_on_gender_age > 0) |> 
#   uncount(i.hh_size_based_on_gender_age) |> 
#   mutate(i.individual_gender = ifelse(str_detect(string = i.num_gender_age, pattern = "females"), "Female", "Male"),
#          i.individual_age_cat = case_when(str_detect(string = i.num_gender_age, pattern = "0to6|7to3yrs|4to6") ~ "cat_0_6",
#                                           str_detect(string = i.num_gender_age, pattern = "7to13|14to17") ~ "cat_7_17",
#                                           str_detect(string = i.num_gender_age, pattern = "18to49|50to65") ~ "cat_18_65",
#                                           str_detect(string = i.num_gender_age, pattern = "66plusyrs") ~ "cat_66+" ))

# set up design object
ref_svy_roster <- as_survey(.data = df_roster_clean_data, strata = strata, weights = weights)
# analysis
df_analysis_roster <- analysis_after_survey_creation(input_svy_obj = ref_svy_roster,
                                                     input_dap = df_dap_roster ) |> 
  mutate(level = "Individual")
view(df_analysis_roster)

# merge and format analysis ----------------------------------------------------------

combined_analysis <- bind_rows(df_main_analysis, df_analysis_roster)


integer_cols_i <- c("i.fcs", "i.rcsi", "i.hhs")
integer_cols_int <- c("int.fcs", "int.rcsi", "int.hhs")

# formatting the analysis, adding question labels
full_analysis_long <- combined_analysis |> 
  mutate(variable = ifelse(is.na(variable) | variable %in% c(""), variable_val, variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) |> 
  left_join(df_tool_data_support, by = c("int.variable" = "name")) |> 
  relocate(label, .after = variable) |> 
  mutate(variable = ifelse(variable %in% integer_cols_i, str_replace(string = variable, pattern = "i.", replacement = "int."), variable),
         select_type = ifelse(variable %in% integer_cols_int, "integer", select_type),
         label = ifelse(is.na(label), variable, label),
         # `mean/pct` = ifelse(select_type %in% c("integer") & !variable %in% integer_cols_i & !str_detect(string = variable, pattern = "^i\\."), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) |> 
  mutate(variable = ifelse(variable %in% integer_cols_int, str_replace(string = variable, pattern = "int.", replacement = "i."), variable),
         label = ifelse(label %in% integer_cols_int, str_replace(string = label, pattern = "int.", replacement = "i."), label)) |> 
  select(`Question`= label, 
         variable, 
         `choices/options` = variable_val, 
         `Results(mean/percentage)` = `mean/pct`, 
         n_unweighted, 
         population, 
         subset_1_name, 
         subset_1_val, 
         select_type,
         level)

# output analysis

write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_eth_lcsa_somali.csv"), na="")
write_csv(full_analysis_long, paste0("outputs/full_analysis_lf_eth_lcsa_somali.csv"), na="")
write_csv(df_main_analysis, paste0("outputs/", butteR::date_file_prefix(), "combined_analysis_lf_eth_somali.csv"), na="")
###############################################################################