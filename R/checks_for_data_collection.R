###############################################################################
# checks for data collection
# read packages
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

source("R/composite_indicators.R")

# read data and tool ----------------------------------------------------------
# data
data_path <- "inputs/REACH_ETH_LCSA_Somali_data.xlsx"

df_tool_data <- readxl::read_excel(data_path) |>  
  mutate(start = as_datetime(start),
         end = as_datetime(end),
         enumerator_id = ifelse(is.na(enumerator_id), enum_id, enumerator_id)) |> 
  checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                        input_location_col = "hh_kebele") |> 
  
  create_composite_indicators() |> 
  rowwise() |> 
  mutate( 
    #i.hh_tot_income = sum(c_across(last30_income_agriculture_livestock:last30_hh_other_income_amount), na.rm = T),
    #i.tot_expenditure = sum(c_across(expenditure_food:expenditure_other_frequent), na.rm = T),
    #int.hh_number_male = sum(c_across(c("hh_number_men_count", "hh_number_boys_count")), na.rm = T),
    #int.hh_number_female = sum(c_across(c("hh_number_women_count", "hh_number_girls_count")), na.rm = T)
  
    ) |>
  ungroup()

# loops -----------------------------------------------------------------------
# loop_hh_roster
loop_hh_roster <- readxl::read_excel(path = data_path, sheet = "hh_roster")

df_raw_data_loop_hh_roster <- df_tool_data |> 
  select(-`_index`) |> 
  inner_join(loop_hh_roster, by = c("_uuid" = "_submission__uuid"))

# loop_hh_health
loop_hh_health <- readxl::read_excel(path = data_path, sheet = "health_loop")

df_raw_data_loop_hh_health <- df_tool_data |> 
  select(-`_index`) |> 
  inner_join(loop_hh_health, by = c("_uuid" = "_submission__uuid"))

# tool
loc_tool <- "inputs/REACH_ETH_LCSA_Somali_tool.xlsx"

df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

# checks ----------------------------------------------------------------------

checks_output <- list()

# testing data ----------------------------------------------------------------

df_testing_data <- df_tool_data |> 
  filter(i.check.start_date < as_date("2023-11-11")) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check.")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_testing_data")

df_testing_data2 <- df_tool_data |> 
  filter(i.check.start_date == as_date("2023-12-15")) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check.")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_testing_data2")

# fix enumerator_id data ------------------------------------------------------

df_logic_c_enumerator_id_harmonization <- df_tool_data |> 
  filter(is.na(enumerator_id), i.check.start_date > as_date("2023-11-10")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "enumerator_id",
         i.check.current_value = "NA",
         i.check.value = enumerator_id,
         i.check.issue_id = "logic_c_enumerator_id_harmonization",
         i.check.issue = "enumerator_id_harmonization",
         i.check.other_text = "",
         i.check.checked_by = "AT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_enumerator_id_harmonization")

# Time checks -----------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 20
max_time_of_survey <- 120

df_c_survey_time <-  supporteR::check_survey_time(input_tool_data = df_tool_data, 
                                                  input_enumerator_id_col = "enumerator_id",
                                                  input_location_col = "hh_kebele",
                                                  input_min_time = min_time_of_survey, 
                                                  input_max_time = max_time_of_survey) |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_survey_time")

# check duplicate uuids -------------------------------------------------------

df_c_duplicate_uuid <-  supporteR::checks_duplicate_uuids(input_tool_data = df_tool_data) |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_duplicate_uuid")

# outliers --------------------------------------------------------------------

df_c_outliers <- supporteR::check_outliers_cleaninginspector(input_tool_data = df_tool_data,
                                                             input_enumerator_id_col = "enumerator_id",
                                                             input_location_col = "hh_kebele") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_outliers")

# other_specify ---------------------------------------------------------------

df_others_data <- supporteR::extract_other_specify_data(input_tool_data = df_tool_data |> select(-hh_income_other, -last30_income_other), 
                                                        input_enumerator_id_col = "enumerator_id",
                                                        input_location_col = "hh_kebele",
                                                        input_survey = df_survey,  
                                                        input_choices = df_choices) |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_others_data")

# repeat_other_specify --------------------------------------------------------

df_repeat_others_data <- supporteR::extract_other_specify_data_repeats(input_repeat_data = df_raw_data_loop_hh_health |> mutate(`_index.y` = `_index`), 
                                                                       input_enumerator_id_col = "enumerator_id", 
                                                                       input_location_col = "hh_kebele", 
                                                                       input_survey = df_survey, 
                                                                       input_choices = df_choices, 
                                                                       input_sheet_name = "health_loop", 
                                                                       input_repeat_cols = c("health_unmet_need_type", "healthcare_seek")) |> 
  mutate(hh_kebele = as.character(index))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_repeat_others_data")

# logical checks --------------------------------------------------------------
# Household size seems to be unusually low (below 2) or high (above 8); survey needs to be checked
df_logic_c_hh_size_seems_unusal <- df_tool_data |> 
  filter(hh_size <= 2 | hh_size > 8) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "hh_size",
         i.check.current_value = as.character(hh_size),
         i.check.value = "",
         i.check.issue_id = "hh_size_seems_unusal",
         i.check.issue = "household size seems unusal",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "care to be taken in deciding how to use this data", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check.")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_size_seems_unusal")

# The age of the hoh seems too high (80 years old or higher), please check the age of the hoh again.
df_logic_c_hoh_age_seems_too_high <- df_tool_data |> 
  filter(hoh %in% c("no"), hoh_age >= 80) |>
  mutate(i.check.type = "change_response",
         i.check.name = "hoh",
         i.check.current_value = hoh,
         i.check.value = "",
         i.check.issue_id = "hoh_age_seems_too_high",
         i.check.issue = glue("hoh: {hoh} but hoh_age:{hoh_age}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hoh_age_seems_too_high")

# chronic_illness_male
df_logic_c_chronic_illness_male <- df_tool_data |> 
  filter(chronic_illness_male  > hh_male_count) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "chronic_illness_male",
         i.check.current_value = as.character(chronic_illness_male),
         i.check.value = as.character(hh_male_count),
         i.check.issue_id = "logic_c_chronic_illness_male",
         i.check.issue = glue("chronic_illness_male greater than hh composition, hh_male_count: {hh_male_count}"),
         i.check.other_text = "",
         i.check.checked_by = "AT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_chronic_illness_male")

# chronic_illness_female
df_logic_c_chronic_illness_female <- df_tool_data |> 
  filter(chronic_illness_female  > hh_female_count) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "chronic_illness_female",
         i.check.current_value = as.character(chronic_illness_female),
         i.check.value = as.character(hh_female_count),
         i.check.issue_id = "logic_c_chronic_illness_female",
         i.check.issue = glue("chronic_illness_female greater than hh composition, hh_female_count: {hh_female_count}"),
         i.check.other_text = "",
         i.check.checked_by = "AT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_chronic_illness_female")

# pregnant_lac_women
df_logic_c_pregnant_lac_women <- df_tool_data |> 
  filter(pregnant_lac_women  > hh_female_count) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "pregnant_lac_women",
         i.check.current_value = as.character(pregnant_lac_women),
         i.check.value = as.character(hh_female_count),
         i.check.issue_id = "logic_c_pregnant_lac_women",
         i.check.issue = glue("pregnant_lac_women greater than hh composition, hh_female_count: {hh_female_count}"),
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_pregnant_lac_women")

# HH reports 'population come from IDP', but not describes in the household's situation
df_logic_c_pop_from_idp_but_not_describe_hh_situation <- df_tool_data |> 
  filter(pop_group %in%  c("idp"), hh_situation %in% c( "non_displaced_households")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "pop_group",
         i.check.current_value = pop_group,
         i.check.value = "",
         i.check.issue_id = "pop_from_idp_but_not_describe_hh_situation",
         i.check.issue = glue("pop_group: {pop_group} but hh_situation: {hh_situation}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_pop_from_idp_but_not_describe_hh_situation")

# If hh_size = 1 and respondent does not live in the house i.e. none, survey needs to be checked
df_logic_c_live_in_house_and_hh_size <- df_tool_data %>% 
  filter(hh_member_currently_notliving_with_family %in% c("none") , hh_size == 1) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "hh_member_currently_notliving_with_family",
         i.check.current_value = hh_member_currently_notliving_with_family,
         i.check.value = "",
         i.check.issue_id = "logic_c_hh_size_and_live_in_house_mismatch",
         i.check.issue = glue("hh_size: {hh_size}, but respondent does not live in the house i.e. hh_member_currently_notliving_with_family: {hh_member_currently_notliving_with_family}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |>  
  dplyr::select(starts_with("i.check"))  |>  
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_live_in_house_and_hh_size")

# HH reports 'sell more livestock than usual', but reports not owning any livestock
df_logic_c_sell_livestock_but_not_owning_any_livestock <- df_tool_data |> 
  filter(liv_stress_4 %in%  c("yes"), hh_own_livestock %in% c( "no")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "liv_stress_4",
         i.check.current_value = liv_stress_4,
         i.check.value = "",
         i.check.issue_id = "hh_response_sell_livestock_but_not_owning_any_livestock",
         i.check.issue = glue("liv_stress_4: {liv_stress_4} but hh_own_livestock: {hh_own_livestock}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_sell_livestock_but_not_owning_any_livestock")

# HH reports 'sell the last female animal', but reports not owning any livestock
df_logic_c_sell_female_animal_but_not_owning_any_livestock <- df_tool_data |> 
  filter(liv_emergency_2 %in%  c("yes"), hh_own_livestock %in% c( "no")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "liv_emergency_2",
         i.check.current_value = liv_emergency_2,
         i.check.value = "",
         i.check.issue_id = "hh_response_sell_female_animal_but_not_owning_any_livestock",
         i.check.issue = glue("liv_emergency_2: {liv_emergency_2} but hh_own_livestock: {hh_own_livestock}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_sell_female_animal_but_not_owning_any_livestock")

# If fs_fcs_condiment = 0 i.e. household has not eaten salt, spices, tea, or coffee in the past seven days, surveys should be checked
df_logic_c_hh_no_eating_condiments <- df_tool_data %>% 
  filter(fs_fcs_condiment == 0) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "fs_fcs_condiment",
         i.check.current_value = as.character(fs_fcs_condiment),
         i.check.value = "NA",
         i.check.issue_id = "hh_no_eating_condiments",
         i.check.issue = glue("fs_fcs_condiment: {fs_fcs_condiment}, it's unlikely that a household spent 7 days eating food without salt"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "enumerators misinterpreted question", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_no_eating_condiments")

# If fs_fcs_beans_nuts = 0 i.e. household has not eaten any beans/legumes, pulses or nuts in the past seven days, surveys should be checked
df_logic_c_hh_no_eating_beans_nuts <- df_tool_data %>% 
  filter(fs_fcs_beans_nuts == 0) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "fs_fcs_beans_nuts",
         i.check.current_value = as.character(fs_fcs_beans_nuts),
         i.check.value = "NA",
         i.check.issue_id = "hh_no_eating_beans_nuts",
         i.check.issue = glue("fs_fcs_beans_nuts: {fs_fcs_beans_nuts}, it's unlikely that a household spent 7 days eating food without any beans/legumes, pulses or nuts"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "enumerators misinterpreted question", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_no_eating_beans_nuts")

# same value of fcs components
df_fd_consumption_score_same <- df_tool_data |>  
  filter(if_all(c(fs_fcs_cereals_grains_roots_tubers, fs_fcs_beans_nuts, fs_fcs_vegetables_leaves, fs_fcs_fruit, fs_fcs_condiment, 
                  fs_fcs_meat_fish_eggs, fs_fcs_dairy, fs_fcs_sugar, fs_fcs_oil_fat_butter), ~ fs_fcs_cereals_grains_roots_tubers == .x))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fs_fcs_cereals_grains_roots_tubers",
         i.check.current_value = as.character(fs_fcs_cereals_grains_roots_tubers),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_fd_consumption_score_same",
         i.check.issue = glue("fs_fcs_cereals_grains_roots_tubers:{fs_fcs_cereals_grains_roots_tubers}, fs_fcs_beans_nuts:{fs_fcs_beans_nuts}, fs_fcs_vegetables_leaves:{fs_fcs_vegetables_leaves}, fs_fcs_fruit:{fs_fcs_fruit}, fs_fcs_condiment:{fs_fcs_condiment}, fs_fcs_meat_fish_eggs:{fs_fcs_meat_fish_eggs}, fs_fcs_dairy:{fs_fcs_dairy}, fs_fcs_sugar:{fs_fcs_sugar}, fs_fcs_oil_fat_butter:{fs_fcs_oil_fat_butter}"),
         i.check.other_text = "",
         i.check.checked_by = "AT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 9)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fs_fcs_cereals_grains_roots_tubers", 
                                  rank == 2 ~ "fs_fcs_beans_nuts",
                                  rank == 3 ~ "fs_fcs_vegetables_leaves", 
                                  rank == 4 ~ "fs_fcs_fruit", 
                                  rank == 5 ~ "fs_fcs_condiment", 
                                  rank == 6 ~ "fs_fcs_meat_fish_eggs", 
                                  rank == 7 ~ "fs_fcs_dairy", 
                                  rank == 8 ~ "fs_fcs_sugar", 
                                  TRUE ~ "fs_fcs_oil_fat_butter"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fs_fcs_cereals_grains_roots_tubers),
                                           rank == 2 ~ as.character(fs_fcs_beans_nuts),
                                           rank == 3 ~ as.character(fs_fcs_vegetables_leaves), 
                                           rank == 4 ~ as.character(fs_fcs_fruit), 
                                           rank == 5 ~ as.character(fs_fcs_condiment), 
                                           rank == 6 ~ as.character(fs_fcs_meat_fish_eggs), 
                                           rank == 7 ~ as.character(fs_fcs_dairy), 
                                           rank == 8 ~ as.character(fs_fcs_sugar), 
                                           TRUE ~ as.character(fs_fcs_oil_fat_butter))
  ) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_fd_consumption_score_same")

# same value of rcsi components and more than 0 times of rcsi. suspect that enumerators may have just filled
df_fd_rcsi_same <- df_tool_data |>  
  filter(if_all(c(rCSILessQlty, rCSIMealSize, rCSIMealAdult, rCSIMealNb,
                  rCSIBorrow), ~ rCSILessQlty == .x & rCSILessQlty > 0))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "rCSILessQlty",
         i.check.current_value = as.character(rCSILessQlty),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_fd_rcsi_same",
         i.check.issue = glue("rCSILessQlty :{rCSILessQlty}, rCSIMealSize :{rCSIMealSize}, rCSIMealAdult :{rCSIMealAdult}, rCSIMealNb :{rCSIMealNb}, rCSIBorrow :{rCSIBorrow}"),
         i.check.other_text = "",
         i.check.checked_by = "AT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 5)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "rCSILessQlty", 
                                  rank == 2 ~ "rCSIMealSize",
                                  rank == 3 ~ "rCSIMealAdult", 
                                  rank == 4 ~ "rCSIMealNb", 
                                  TRUE ~ "rCSIBorrow"),
         i.check.current_value = case_when(rank == 1 ~ as.character(rCSILessQlty),
                                           rank == 2 ~ as.character(rCSIMealSize),
                                           rank == 3 ~ as.character(rCSIMealAdult), 
                                           rank == 4 ~ as.character(rCSIMealNb), 
                                           TRUE ~ as.character(rCSIBorrow))
  ) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_fd_rcsi_same")

#HH experience difficulties or 'shocks in the past 3 months', but not reports affected shocks
df_logic_c_hh_exprience_shocks_but_not_shocks_affected_household <- df_tool_data |> 
  filter(hh_shocks %in%  c("yes"), fs_shock_3months %in% c( "no_shocks_affected_my_household")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "hh_shocks",
         i.check.current_value = hh_shocks,
         i.check.value = "",
         i.check.issue_id = "hh_exprience_shocks_but_not_shocks_affected_household",
         i.check.issue = glue("hh_shocks: {hh_shocks} but fs_shock_3months: {fs_shock_3months}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_exprience_shocks_but_not_shocks_affected_household")

#HH experience 'too much rain or flooding' in the past 3 months, but not reports affected too much rain/flooding
df_logic_c_hh_affected_by_flooding_but_not_reports_issue <- df_tool_data |> 
  filter(fs_shock_3months %in%  c("too_much_rain_flooding"), hh_floods_affect %in% c( "no")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fs_shock_3months",
         i.check.current_value = fs_shock_3months,
         i.check.value = "",
         i.check.issue_id = "hh_affected_by_flooding_but_not_reports_issue",
         i.check.issue = glue("fs_shock_3months: {fs_shock_3months} but hh_floods_affect: {hh_floods_affect}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_affected_by_flooding_but_not_reports_issue")

# HH take short time to access the nearest health facility but report barriers:"Health facility is too far away"
df_logic_c_hh_report_short_time_but_health_facility_far_away <- df_tool_data |> 
  filter(healthcare_barriers %in% c("health_facility_is_too_far_away"), health_facility_distance < 30) |>
  mutate(i.check.type = "change_response",
         i.check.name = "healthcare_barriers",
         i.check.current_value = healthcare_barriers,
         i.check.value = "",
         i.check.issue_id = "hh_report_short_time_but_health_facility_far_away",
         i.check.issue = glue("healthcare_barriers: {healthcare_barriers} but health_facility_distance: {health_facility_distance}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_report_short_time_but_health_facility_far_away")

# It takes HH short time (less than 30 minutes) to fetch water but from distant water points (too far)
df_logic_c_short_time_to_fetch_water_but_waterpoints_far <- df_tool_data |> 
  filter(wash_watertime %in%  c("below_30minutes"), water_coping_strategies %in% c( "fetch_from_distant_water_point")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "wash_watertime",
         i.check.current_value = wash_watertime,
         i.check.value = "",
         i.check.issue_id = "short_time_to_fetch_water_but_waterpoints_far",
         i.check.issue = glue("wash_watertime:{wash_watertime} but water_coping_strategies:{water_coping_strategies}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_short_time_to_fetch_water_but_waterpoints_far")

# It has been reported that the household is sharing sanitation facilities with a very high number of households
df_logic_c_hh_share_sanitation_facility_with_high_number <- df_tool_data |> 
  filter(wash_sanitationsharing %in% c("yes"), wash_sanitationsharing_number >= 50) |>
  mutate(i.check.type = "change_response",
         i.check.name = "wash_sanitationsharing",
         i.check.current_value = wash_sanitationsharing,
         i.check.value = "",
         i.check.issue_id = "hh_share_sanitation_facility_with_high_number",
         i.check.issue = glue("wash_sanitationsharing: {wash_sanitationsharing} but wash_sanitationsharing_number:{wash_sanitationsharing_number}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_share_sanitation_facility_with_high_number")

# check FSL -------------------------------------------------------------------
# check_FCS_high_HHS_high
# HH has a good diet score: > 38.5 but it was also reported that the household had no food: "yes"
df_logic_c_hh_has_good_diet_but_nofood <- df_tool_data |> 
  filter(fs_hhs_nofood %in% c("yes"),
         fs_hhs_sleephungry %in% c("yes"),
         fs_hhs_daynoteating %in% c("yes"), fs_calculated_fcs >= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = " fs_hhs_nofood",
         i.check.current_value = as.character(fs_hhs_nofood),
         i.check.value = "",
         i.check.issue_id = "hh_has_good_diet_but_nofood",
         i.check.issue = glue("fs_hhs_nofood :{fs_hhs_nofood}, fs_hhs_sleephungry :{fs_hhs_sleephungry}, fs_hhs_daynoteating :{fs_hhs_daynoteating}"),
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 3)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fs_hhs_nofood",
                                  rank == 2 ~ "fs_hhs_sleephungry",
                                  TRUE ~ "fs_hhs_daynoteating"),
         i.check.current_value = case_when(rank == 1 ~ as.character("fs_hhs_nofood"),
                                           rank == 2 ~ as.character("fs_hhs_sleephungry"),
                                           TRUE ~ as.character("fs_hhs_daynoteating")) )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_good_diet_but_nofood")

# check_FCS_low_HHS_low
# HH has an alarming diet score: < 38.5 but it was also reported that the household had no food: "no"
df_logic_c_hh_has_alarming_diet_but_nofood <- df_tool_data |>
  filter(fs_hhs_nofood %in% c("no"),
         fs_hhs_sleephungry %in% c("no"),
         fs_hhs_daynoteating %in% c("no"), fs_calculated_fcs <= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = " fs_hhs_nofood",
         i.check.current_value = as.character(fs_hhs_nofood),
         i.check.value = "",
         i.check.issue_id = "hh_has_alarming_diet_but_nofood",
         i.check.issue = glue("fs_hhs_nofood :{fs_hhs_nofood}, fs_hhs_sleephungry :{fs_hhs_sleephungry}, fs_hhs_daynoteating :{fs_hhs_daynoteating}"),
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 3)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fs_hhs_nofood",
                                  rank == 2 ~ "fs_hhs_sleephungry",
                                  TRUE ~ "fs_hhs_daynoteating"),
         i.check.current_value = case_when(rank == 1 ~ as.character("fs_hhs_nofood"),
                                           rank == 2 ~ as.character("fs_hhs_sleephungry"),
                                           TRUE ~ as.character("fs_hhs_daynoteating")) )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_alarming_diet_but_nofood")

#check_FCS_high_LCSI_high
# It was reported that the the household has a good diet score but reports stress, crisis or emergency strategies for accessing food
df_logic_c_hh_has_good_diet_score_but_stress_crisis_emergency <- df_tool_data |>
  filter(!liv_stress_1 %in% c("no_had_no_need"),
         !liv_stress_2 %in% c("no_had_no_need"),
         !liv_stress_3 %in% c("no_had_no_need"),
         !liv_stress_4 %in% c("no_had_no_need"),
         !liv_crisis_1 %in% c("no_had_no_need"),
         !liv_crisis_2 %in% c("no_had_no_need"),
         !liv_crisis_3 %in% c("no_had_no_need"),
         !liv_emergency_1 %in% c("no_had_no_need"),
         !liv_emergency_2 %in% c("no_had_no_need"),
         !liv_emergency_3 %in% c("no_had_no_need"), fs_calculated_fcs >= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = "liv_stress_1",
         i.check.current_value = as.character(liv_stress_1),
         i.check.value = "",
         i.check.issue_id = "hh_has_good_diet_score_but_stress_crisis_emergency",
         i.check.issue = glue("liv_stress_1 :{liv_stress_1}, liv_stress_2 :{liv_stress_2}, liv_stress_3 :{liv_stress_3}, liv_stress_4 :{liv_stress_4}, liv_crisis_1 :{liv_crisis_1}, liv_crisis_2 :{liv_crisis_2}, liv_crisis_3 :{liv_crisis_3}, liv_emergency_1 :{liv_emergency_1}, liv_emergency_2 :{liv_emergency_2}, liv_emergency_3 :{liv_emergency_3}"),
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 10)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "liv_stress_1", 
                                  rank == 2 ~ "liv_stress_2",
                                  rank == 3 ~ "liv_stress_3", 
                                  rank == 4 ~ "liv_stress_4", 
                                  rank == 5 ~ "liv_crisis_1", 
                                  rank == 6 ~ "liv_crisis_2", 
                                  rank == 7 ~ "liv_crisis_3", 
                                  rank == 8 ~ "liv_emergency_1", 
                                  rank == 9 ~ "liv_emergency_2", 
                                  TRUE ~ "liv_emergency_3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(liv_stress_1),
                                           rank == 2 ~ as.character(liv_stress_2),
                                           rank == 3 ~ as.character(liv_stress_3), 
                                           rank == 4 ~ as.character(liv_stress_4), 
                                           rank == 5 ~ as.character(liv_crisis_1), 
                                           rank == 6 ~ as.character(liv_crisis_2), 
                                           rank == 7 ~ as.character(liv_crisis_3), 
                                           rank == 8 ~ as.character(liv_emergency_1), 
                                           rank == 9 ~ as.character(liv_emergency_2), 
                                           TRUE ~ as.character(liv_emergency_3)) 
  )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_good_diet_score_but_stress_crisis_emergency")

#check_FCS_low_LCSI_low
# It was reported that the household has an alarming diet score but does not report stress, crisis or emergency strategies
df_logic_c_hh_has_alarming_diet_score_but_stress_crisis_emergency <- df_tool_data |>
  filter(liv_stress_1 %in% c("no_had_no_need"),
         liv_stress_2 %in% c("no_had_no_need"),
         liv_stress_3 %in% c("no_had_no_need"),
         liv_stress_4 %in% c("no_had_no_need"),
         liv_crisis_1 %in% c("no_had_no_need"),
         liv_crisis_2 %in% c("no_had_no_need"),
         liv_crisis_3 %in% c("no_had_no_need"),
         liv_emergency_1 %in% c("no_had_no_need"),
         liv_emergency_2 %in% c("no_had_no_need"),
         liv_emergency_3 %in% c("no_had_no_need"), fs_calculated_fcs <= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = "liv_stress_1",
         i.check.current_value = as.character(liv_stress_1),
         i.check.value = "",
         i.check.issue_id = "hh_has_alarming_diet_score_but_stress_crisis_emergency",
         i.check.issue = glue("liv_stress_1 :{liv_stress_1}, liv_stress_2 :{liv_stress_2}, liv_stress_3 :{liv_stress_3}, liv_stress_4 :{liv_stress_4}, liv_crisis_1 :{liv_crisis_1}, liv_crisis_2 :{liv_crisis_2}, liv_crisis_3 :{liv_crisis_3}, liv_emergency_1 :{liv_emergency_1}, liv_emergency_2 :{liv_emergency_2}, liv_emergency_3 :{liv_emergency_3}"),
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 10)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "liv_stress_1", 
                                  rank == 2 ~ "liv_stress_2",
                                  rank == 3 ~ "liv_stress_3", 
                                  rank == 4 ~ "liv_stress_4", 
                                  rank == 5 ~ "liv_crisis_1", 
                                  rank == 6 ~ "liv_crisis_2", 
                                  rank == 7 ~ "liv_crisis_3", 
                                  rank == 8 ~ "liv_emergency_1", 
                                  rank == 9 ~ "liv_emergency_2", 
                                  TRUE ~ "liv_emergency_3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(liv_stress_1),
                                           rank == 2 ~ as.character(liv_stress_2),
                                           rank == 3 ~ as.character(liv_stress_3), 
                                           rank == 4 ~ as.character(liv_stress_4), 
                                           rank == 5 ~ as.character(liv_crisis_1), 
                                           rank == 6 ~ as.character(liv_crisis_2), 
                                           rank == 7 ~ as.character(liv_crisis_3), 
                                           rank == 8 ~ as.character(liv_emergency_1), 
                                           rank == 9 ~ as.character(liv_emergency_2), 
                                           TRUE ~ as.character(liv_emergency_3)) 
  )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_alarming_diet_score_but_stress_crisis_emergency")

#High FCS score and rcsi used
# The household diet score is reported to be good but the household reports using reduced coping strategies.
df_logic_c_hh_has_good_diet_score_but_using_coping_strategies <- df_tool_data |>
  filter(rCSILessQlty > 0,
         rCSIMealSize > 0, 
         rCSIMealAdult > 0,
         rCSIMealNb > 0,
         rCSIBorrow > 0, fs_calculated_fcs >= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = "rCSILessQlty",
         i.check.current_value = as.character(rCSILessQlty),
         i.check.value = "",
         i.check.issue_id = "hh_has_good_diet_score_but_using_coping_strategies",
         i.check.issue = glue("rCSILessQlty :{rCSILessQlty}, rCSIMealSize :{rCSIMealSize}, rCSIMealAdult :{rCSIMealAdult}, rCSIMealNb :{rCSIMealNb}, rCSIBorrow :{rCSIBorrow}"),
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 5)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "rCSILessQlty", 
                                  rank == 2 ~ "rCSIMealSize", 
                                  rank == 3 ~ "rCSIMealAdult", 
                                  rank == 4 ~ "rCSIMealNb", 
                                  TRUE ~ "rCSIBorrow"),
         i.check.current_value = case_when(rank == 1 ~ as.character(rCSILessQlty), 
                                           rank == 2 ~ as.character(rCSIMealSize), 
                                           rank == 3 ~ as.character(rCSIMealAdult), 
                                           rank == 4 ~ as.character(rCSIMealNb), 
                                           TRUE ~ as.character(rCSIBorrow)) 
  )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_good_diet_score_but_using_coping_strategies")

#Low FCS score but rcsi not used
# The household diet score is reported to be alarming but the household does not report using reduced coping strategies.
df_logic_c_hh_has_alarming_diet_score_but_no_coping_strategies <- df_tool_data |>
  filter(rCSILessQlty == 0,
         rCSIMealSize == 0, 
         rCSIMealAdult == 0,
         rCSIMealNb == 0,
         rCSIBorrow == 0, fs_calculated_fcs <= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = "rCSILessQlty",
         i.check.current_value = as.character(rCSILessQlty),
         i.check.value = "",
         i.check.issue_id = "hh_has_alarming_diet_score_but_no_coping_strategies",
         i.check.issue = glue("rCSILessQlty :{rCSILessQlty}, rCSIMealSize :{rCSIMealSize}, rCSIMealAdult :{rCSIMealAdult}, rCSIMealNb :{rCSIMealNb}, rCSIBorrow :{rCSIBorrow}"),
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 5)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "rCSILessQlty", 
                                  rank == 2 ~ "rCSIMealSize", 
                                  rank == 3 ~ "rCSIMealAdult", 
                                  rank == 4 ~ "rCSIMealNb", 
                                  TRUE ~ "rCSIBorrow"),
         i.check.current_value = case_when(rank == 1 ~ as.character(rCSILessQlty), 
                                           rank == 2 ~ as.character(rCSIMealSize), 
                                           rank == 3 ~ as.character(rCSIMealAdult), 
                                           rank == 4 ~ as.character(rCSIMealNb), 
                                           TRUE ~ as.character(rCSIBorrow)) 
  )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_alarming_diet_score_but_no_coping_strategies")

# mismatch between FCS and HHS
df_logic_c_fcs_and_hhs_mismatch <- df_tool_data |> 
  filter((i.fcs_cat %in% c("Acceptable")), i.hhs_cat %in% c("Severe")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fs_fcs_cereals_grains_roots_tubers",
         i.check.current_value = as.character(fs_fcs_cereals_grains_roots_tubers),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_mismatch_btn_fcs_and_hhs",
         i.check.issue = "mismatch between FCS and HHS",
         i.check.other_text = "",
         i.check.checked_by = "AT",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |>
  slice(rep(1:n(), each = 15)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fs_fcs_cereals_grains_roots_tubers", 
                                  rank == 2 ~ "fs_fcs_beans_nuts",
                                  rank == 3 ~ "fs_fcs_vegetables_leaves", 
                                  rank == 4 ~ "fs_fcs_fruit", 
                                  rank == 5 ~ "fs_fcs_condiment", 
                                  rank == 6 ~ "fs_fcs_meat_fish_eggs", 
                                  rank == 7 ~ "fs_fcs_dairy", 
                                  rank == 8 ~ "fs_fcs_sugar", 
                                  rank == 9 ~ "fs_fcs_oil_fat_butter",
                                  rank == 10 ~ "fs_hhs_nofood",
                                  rank == 11 ~ "fs_hhs_nofood_freq",
                                  rank == 12 ~ "fs_hhs_sleephungry",
                                  rank == 13 ~ "fs_hhs_sleephungry_freq",
                                  rank == 14 ~ "fs_hhs_daynoteating",
                                  TRUE ~ "fs_hhs_daynoteating_freq"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fs_fcs_cereals_grains_roots_tubers),
                                           rank == 2 ~ as.character(fs_fcs_beans_nuts),
                                           rank == 3 ~ as.character(fs_fcs_vegetables_leaves), 
                                           rank == 4 ~ as.character(fs_fcs_fruit), 
                                           rank == 5 ~ as.character(fs_fcs_condiment), 
                                           rank == 6 ~ as.character(fs_fcs_meat_fish_eggs), 
                                           rank == 7 ~ as.character(fs_fcs_dairy), 
                                           rank == 8 ~ as.character(fs_fcs_sugar), 
                                           rank == 9 ~ as.character(fs_fcs_oil_fat_butter), 
                                           rank == 10 ~ as.character(fs_hhs_nofood), 
                                           rank == 11 ~ as.character(fs_hhs_nofood_freq), 
                                           rank == 12 ~ as.character(fs_hhs_sleephungry), 
                                           rank == 13 ~ as.character(fs_hhs_sleephungry_freq), 
                                           rank == 14 ~ as.character(fs_hhs_daynoteating), 
                                           TRUE ~ as.character(fs_hhs_daynoteating_freq))
  ) |> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) |> 
  filter(!is.na(current_value))|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_fcs_and_hhs_mismatch")

# - i.hhs_cat[severe] // i.rcsi_cat[0] *** take out all food indicators ***
df_logic_c_fd_hhs_severe_but_rcsi_low <- df_tool_data |>  
  filter(i.hhs_cat %in% c("Severe"), i.rcsi_cat %in% c("rcsi_0_3"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fs_fcs_cereals_grains_roots_tubers",
         i.check.current_value = as.character(fs_fcs_cereals_grains_roots_tubers),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_fd_hhs_severe_but_rcsi_low",
         i.check.issue = glue("hhs_severe_but_rcsi_low"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 20)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fs_fcs_cereals_grains_roots_tubers", 
                                  rank == 2 ~ "fs_fcs_beans_nuts",
                                  rank == 3 ~ "fs_fcs_vegetables_leaves", 
                                  rank == 4 ~ "fs_fcs_fruit", 
                                  rank == 5 ~ "fs_fcs_condiment", 
                                  rank == 6 ~ "fs_fcs_meat_fish_eggs", 
                                  rank == 7 ~ "fs_fcs_dairy", 
                                  rank == 8 ~ "fs_fcs_sugar", 
                                  rank == 9 ~ "fs_fcs_oil_fat_butter", 
                                  rank == 10 ~ "fs_hhs_nofood", 
                                  rank == 11 ~ "fs_hhs_nofood_freq", 
                                  rank == 12 ~ "fs_hhs_sleephungry", 
                                  rank == 13 ~ "fs_hhs_sleephungry_freq", 
                                  rank == 14 ~ "fs_hhs_daynoteating", 
                                  rank == 15 ~ "fs_hhs_daynoteating_freq", 
                                  rank == 16 ~ "rCSILessQlty", 
                                  rank == 17 ~ "rCSIMealSize", 
                                  rank == 18 ~ "rCSIMealAdult", 
                                  rank == 19 ~ "rCSIMealNb", 
                                  TRUE ~ "rCSIBorrow"),
         i.check.value = case_when(i.check.name %in% c("fs_hhs_nofood", 
                                                       "fs_hhs_sleephungry",
                                                       "fs_hhs_daynoteating") ~ "0", 
                                   TRUE ~ i.check.value),
         i.check.current_value = case_when(rank == 1 ~ as.character(fs_fcs_cereals_grains_roots_tubers),
                                           rank == 2 ~ as.character(fs_fcs_beans_nuts),
                                           rank == 3 ~ as.character(fs_fcs_vegetables_leaves), 
                                           rank == 4 ~ as.character(fs_fcs_fruit), 
                                           rank == 5 ~ as.character(fs_fcs_condiment), 
                                           rank == 6 ~ as.character(fs_fcs_meat_fish_eggs), 
                                           rank == 7 ~ as.character(fs_fcs_dairy), 
                                           rank == 8 ~ as.character(fs_fcs_sugar), 
                                           rank == 9 ~ as.character(fs_fcs_oil_fat_butter), 
                                           rank == 10 ~ as.character(fs_hhs_nofood), 
                                           rank == 11 ~ as.character(fs_hhs_nofood_freq), 
                                           rank == 12 ~ as.character(fs_hhs_sleephungry), 
                                           rank == 13 ~ as.character(fs_hhs_sleephungry_freq), 
                                           rank == 14 ~ as.character(fs_hhs_daynoteating), 
                                           rank == 15 ~ as.character(fs_hhs_daynoteating_freq), 
                                           rank == 16 ~ as.character(rCSILessQlty), 
                                           rank == 17 ~ as.character(rCSIMealSize), 
                                           rank == 18 ~ as.character(rCSIMealAdult), 
                                           rank == 19 ~ as.character(rCSIMealNb), 
                                           TRUE ~ as.character(rCSIBorrow))
  ) |> 
  filter(!is.na(i.check.current_value), !i.check.current_value %in% c("0")) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_fd_hhs_severe_but_rcsi_low")

# - low FCS and low rCSI. fcs [poor] // rcs[0] *** take out all the data **
df_logic_c_fd_fcs_poor_but_rcsi_low <- df_tool_data |>  
  filter(i.hhs_cat %in% c("Poor"), i.rcsi_cat %in% c("rcsi_0_3"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fs_fcs_cereals_grains_roots_tubers",
         i.check.current_value = as.character(fs_fcs_cereals_grains_roots_tubers),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_fd_hhs_severe_but_rcsi_low",
         i.check.issue = glue("hhs_severe_but_rcsi_low"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 20)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fs_fcs_cereals_grains_roots_tubers", 
                                  rank == 2 ~ "fs_fcs_beans_nuts",
                                  rank == 3 ~ "fs_fcs_vegetables_leaves", 
                                  rank == 4 ~ "fs_fcs_fruit", 
                                  rank == 5 ~ "fs_fcs_condiment", 
                                  rank == 6 ~ "fs_fcs_meat_fish_eggs", 
                                  rank == 7 ~ "fs_fcs_dairy", 
                                  rank == 8 ~ "fs_fcs_sugar", 
                                  rank == 9 ~ "fs_fcs_oil_fat_butter", 
                                  rank == 10 ~ "fs_hhs_nofood", 
                                  rank == 11 ~ "fs_hhs_nofood_freq", 
                                  rank == 12 ~ "fs_hhs_sleephungry", 
                                  rank == 13 ~ "fs_hhs_sleephungry_freq", 
                                  rank == 14 ~ "fs_hhs_daynoteating", 
                                  rank == 15 ~ "fs_hhs_daynoteating_freq", 
                                  rank == 16 ~ "rCSILessQlty", 
                                  rank == 17 ~ "rCSIMealSize", 
                                  rank == 18 ~ "rCSIMealAdult", 
                                  rank == 19 ~ "rCSIMealNb", 
                                  TRUE ~ "rCSIBorrow"),
         i.check.value = case_when(i.check.name %in% c("fs_hhs_nofood", 
                                                       "fs_hhs_sleephungry",
                                                       "fs_hhs_daynoteating") ~ "0", 
                                   TRUE ~ i.check.value),
         i.check.current_value = case_when(rank == 1 ~ as.character(fs_fcs_cereals_grains_roots_tubers),
                                           rank == 2 ~ as.character(fs_fcs_beans_nuts),
                                           rank == 3 ~ as.character(fs_fcs_vegetables_leaves), 
                                           rank == 4 ~ as.character(fs_fcs_fruit), 
                                           rank == 5 ~ as.character(fs_fcs_condiment), 
                                           rank == 6 ~ as.character(fs_fcs_meat_fish_eggs), 
                                           rank == 7 ~ as.character(fs_fcs_dairy), 
                                           rank == 8 ~ as.character(fs_fcs_sugar), 
                                           rank == 9 ~ as.character(fs_fcs_oil_fat_butter), 
                                           rank == 10 ~ as.character(fs_hhs_nofood), 
                                           rank == 11 ~ as.character(fs_hhs_nofood_freq), 
                                           rank == 12 ~ as.character(fs_hhs_sleephungry), 
                                           rank == 13 ~ as.character(fs_hhs_sleephungry_freq), 
                                           rank == 14 ~ as.character(fs_hhs_daynoteating), 
                                           rank == 15 ~ as.character(fs_hhs_daynoteating_freq), 
                                           rank == 16 ~ as.character(rCSILessQlty), 
                                           rank == 17 ~ as.character(rCSIMealSize), 
                                           rank == 18 ~ as.character(rCSIMealAdult), 
                                           rank == 19 ~ as.character(rCSIMealNb), 
                                           TRUE ~ as.character(rCSIBorrow))
  ) |> 
  filter(!is.na(i.check.current_value), !i.check.current_value %in% c("0")) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_fd_fcs_poor_but_rcsi_low")

# - high FCS and high rCSI
df_logic_c_fd_fcs_acceptable_but_rcsi_high <- df_tool_data |>  
  filter(i.fcs_cat %in% c("Acceptable"), i.rcsi_cat %in% c("rcsi_19+"))  |>
  mutate(i.check.type = "change_response",
         i.check.name = "fs_fcs_cereals_grains_roots_tubers",
         i.check.current_value = as.character(fs_fcs_cereals_grains_roots_tubers),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_fd_hhs_severe_but_rcsi_low",
         i.check.issue = glue("hhs_severe_but_rcsi_low"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 20)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fs_fcs_cereals_grains_roots_tubers", 
                                  rank == 2 ~ "fs_fcs_beans_nuts",
                                  rank == 3 ~ "fs_fcs_vegetables_leaves", 
                                  rank == 4 ~ "fs_fcs_fruit", 
                                  rank == 5 ~ "fs_fcs_condiment", 
                                  rank == 6 ~ "fs_fcs_meat_fish_eggs", 
                                  rank == 7 ~ "fs_fcs_dairy", 
                                  rank == 8 ~ "fs_fcs_sugar", 
                                  rank == 9~ "fs_fcs_oil_fat_butter", 
                                  rank == 10 ~ "fs_hhs_nofood", 
                                  rank == 11 ~ "fs_hhs_nofood_freq", 
                                  rank == 12 ~ "fs_hhs_sleephungry", 
                                  rank == 13 ~ "fs_hhs_sleephungry_freq", 
                                  rank == 14 ~ "fs_hhs_daynoteating", 
                                  rank == 15 ~ "fs_hhs_daynoteating_freq", 
                                  rank == 16 ~ "rCSILessQlty", 
                                  rank == 17 ~ "rCSIMealSize", 
                                  rank == 18 ~ "rCSIMealAdult", 
                                  rank == 19 ~ "rCSIMealNb", 
                                  TRUE ~ "rCSIBorrow"),
         i.check.value = case_when(i.check.name %in% c("fs_hhs_nofood", 
                                                       "fs_hhs_sleephungry",
                                                       "fs_hhs_daynoteating") ~ "0", 
                                   TRUE ~ i.check.value),
         i.check.current_value = case_when(rank == 1 ~ as.character(fs_fcs_cereals_grains_roots_tubers),
                                           rank == 2 ~ as.character(fs_fcs_beans_nuts),
                                           rank == 3 ~ as.character(fs_fcs_vegetables_leaves), 
                                           rank == 4 ~ as.character(fs_fcs_fruit), 
                                           rank == 5 ~ as.character(fs_fcs_condiment), 
                                           rank == 6 ~ as.character(fs_fcs_meat_fish_eggs), 
                                           rank == 7 ~ as.character(fs_fcs_dairy), 
                                           rank == 8 ~ as.character(fs_fcs_sugar), 
                                           rank == 9 ~ as.character(fs_fcs_oil_fat_butter), 
                                           rank == 10 ~ as.character(fs_hhs_nofood), 
                                           rank == 11 ~ as.character(fs_hhs_nofood_freq), 
                                           rank == 12 ~ as.character(fs_hhs_sleephungry), 
                                           rank == 13 ~ as.character(fs_hhs_sleephungry_freq), 
                                           rank == 14 ~ as.character(fs_hhs_daynoteating), 
                                           rank == 15 ~ as.character(fs_hhs_daynoteating_freq), 
                                           rank == 16 ~ as.character(rCSILessQlty), 
                                           rank == 17 ~ as.character(rCSIMealSize), 
                                           rank == 18 ~ as.character(rCSIMealAdult), 
                                           rank == 19 ~ as.character(rCSIMealNb), 
                                           TRUE ~ as.character(rCSIBorrow))
  ) |> 
  filter(!is.na(i.check.current_value), !i.check.current_value %in% c("0")) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_fd_fcs_acceptable_but_rcsi_high")

# LCSI ------------------------------------------------------------------------
# lcsi_stress_c1
df_logic_c_lcsi_stress_c1 <- df_tool_data |>  
  filter(liv_stress_1 %in% c("no_exhausted"), 
         liv_stress_2 %in% c("no_had_no_need"),
         liv_stress_3 %in% c("no_had_no_need"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "liv_stress_1",
         i.check.current_value = as.character(liv_stress_1),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_lcsi_stress_c1",
         i.check.issue = glue("lcsi_stress_c1"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 10)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "liv_stress_1", 
                                  rank == 2 ~ "liv_stress_2",
                                  rank == 3 ~ "liv_stress_3", 
                                  rank == 4 ~ "liv_stress_4", 
                                  rank == 5 ~ "liv_crisis_1", 
                                  rank == 6 ~ "liv_crisis_2", 
                                  rank == 7 ~ "liv_crisis_3", 
                                  rank == 8 ~ "liv_emergency_1", 
                                  rank == 9 ~ "liv_emergency_2", 
                                  TRUE ~ "liv_emergency_3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(liv_stress_1),
                                           rank == 2 ~ as.character(liv_stress_2),
                                           rank == 3 ~ as.character(liv_stress_3), 
                                           rank == 4 ~ as.character(liv_stress_4), 
                                           rank == 5 ~ as.character(liv_crisis_1), 
                                           rank == 6 ~ as.character(liv_crisis_2), 
                                           rank == 7 ~ as.character(liv_crisis_3), 
                                           rank == 8 ~ as.character(liv_emergency_1), 
                                           rank == 9 ~ as.character(liv_emergency_2), 
                                           TRUE ~ as.character(liv_emergency_3))
  ) |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_stress_c1")

# lcsi_stress_c2
df_logic_c_lcsi_stress_c2 <- df_tool_data |>  
  filter(liv_stress_1 %in% c("no_exhausted"), 
         liv_stress_2 %in% c("no_had_no_need"),
         liv_stress_3 %in% c("no_had_no_need"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "liv_stress_1",
         i.check.current_value = as.character(liv_stress_1),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_lcsi_stress_c2",
         i.check.issue = glue("lcsi_stress_c2"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 10)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "liv_stress_1", 
                                  rank == 2 ~ "liv_stress_2",
                                  rank == 3 ~ "liv_stress_3", 
                                  rank == 4 ~ "liv_stress_4", 
                                  rank == 5 ~ "liv_crisis_1", 
                                  rank == 6 ~ "liv_crisis_2", 
                                  rank == 7 ~ "liv_crisis_3", 
                                  rank == 8 ~ "liv_emergency_1", 
                                  rank == 9 ~ "liv_emergency_2", 
                                  TRUE ~ "liv_emergency_3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(liv_stress_1),
                                           rank == 2 ~ as.character(liv_stress_2),
                                           rank == 3 ~ as.character(liv_stress_3), 
                                           rank == 4 ~ as.character(liv_stress_4), 
                                           rank == 5 ~ as.character(liv_crisis_1), 
                                           rank == 6 ~ as.character(liv_crisis_2), 
                                           rank == 7 ~ as.character(liv_crisis_3), 
                                           rank == 8 ~ as.character(liv_emergency_1), 
                                           rank == 9 ~ as.character(liv_emergency_2), 
                                           TRUE ~ as.character(liv_emergency_3))
  ) |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_stress_c2")

# lcsi_stress_c3
df_logic_c_lcsi_stress_c3 <- df_tool_data |>  
  filter(liv_stress_1 %in% c("no_had_no_need"), 
         liv_stress_2 %in% c("no_exhausted"),
         liv_stress_3 %in% c("no_had_no_need"),
         liv_emergency_1 %in% c("no_exhausted"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "liv_stress_1",
         i.check.current_value = as.character(liv_stress_1),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_lcsi_stress_c3",
         i.check.issue = glue("lcsi_stress_c3"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 10)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "liv_stress_1", 
                                  rank == 2 ~ "liv_stress_2",
                                  rank == 3 ~ "liv_stress_3", 
                                  rank == 4 ~ "liv_stress_4", 
                                  rank == 5 ~ "liv_crisis_1", 
                                  rank == 6 ~ "liv_crisis_2", 
                                  rank == 7 ~ "liv_crisis_3", 
                                  rank == 8 ~ "liv_emergency_1", 
                                  rank == 9 ~ "liv_emergency_2", 
                                  TRUE ~ "liv_emergency_3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(liv_stress_1),
                                           rank == 2 ~ as.character(liv_stress_2),
                                           rank == 3 ~ as.character(liv_stress_3), 
                                           rank == 4 ~ as.character(liv_stress_4), 
                                           rank == 5 ~ as.character(liv_crisis_1), 
                                           rank == 6 ~ as.character(liv_crisis_2), 
                                           rank == 7 ~ as.character(liv_crisis_3), 
                                           rank == 8 ~ as.character(liv_emergency_1), 
                                           rank == 9 ~ as.character(liv_emergency_2), 
                                           TRUE ~ as.character(liv_emergency_3))
  ) |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")|> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_stress_c3")

# lcsi_stress_c4
df_logic_c_lcsi_stress4_but_no_emergency_c4 <- df_tool_data |>  
  filter(liv_stress_4 %in% c("no_exhausted"), 
         liv_emergency_2 %in% c("not_applicable"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "liv_stress_1",
         i.check.current_value = as.character(liv_stress_1),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_lcsi_stress4_but_no_emergency_c4",
         i.check.issue = glue("lcsi_stress4_but_no_emergency"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 10)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "liv_stress_1", 
                                  rank == 2 ~ "liv_stress_2",
                                  rank == 3 ~ "liv_stress_3", 
                                  rank == 4 ~ "liv_stress_4", 
                                  rank == 5 ~ "liv_crisis_1", 
                                  rank == 6 ~ "liv_crisis_2", 
                                  rank == 7 ~ "liv_crisis_3", 
                                  rank == 8 ~ "liv_emergency_1", 
                                  rank == 9 ~ "liv_emergency_2", 
                                  TRUE ~ "liv_emergency_3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(liv_stress_1),
                                           rank == 2 ~ as.character(liv_stress_2),
                                           rank == 3 ~ as.character(liv_stress_3), 
                                           rank == 4 ~ as.character(liv_stress_4), 
                                           rank == 5 ~ as.character(liv_crisis_1), 
                                           rank == 6 ~ as.character(liv_crisis_2), 
                                           rank == 7 ~ as.character(liv_crisis_3), 
                                           rank == 8 ~ as.character(liv_emergency_1), 
                                           rank == 9 ~ as.character(liv_emergency_2), 
                                           TRUE ~ as.character(liv_emergency_3))
  ) |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_stress4_but_no_emergency_c4")

# lcsi_stress_c5
df_logic_c_lcsi_no_stress4_but_emergency2_c5 <- df_tool_data |>  
  filter(liv_stress_4 %in% c("no_had_no_need", "not_applicable"), 
         liv_emergency_2 %in% c("yes", "no_exhausted"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "liv_emergency_2",
         i.check.current_value = as.character(liv_emergency_2),
         i.check.value = "not_applicable",
         i.check.issue_id = "logic_c_lcsi_no_stress4_but_emergency2_c5",
         i.check.issue = glue("no stress4 but emergency2"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")  |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_no_stress4_but_emergency2_c5")
  
# lcsi_stress_c6
df_logic_c_lcsi_no_stress_but_crisis_emergency_c6 <- df_tool_data |>  
  filter(liv_stress_1 %in% c("no_had_no_need", "not_applicable"), 
         liv_stress_2 %in% c("no_had_no_need", "not_applicable"),
         liv_stress_3 %in% c("no_had_no_need", "not_applicable"),
         liv_stress_4 %in% c("no_had_no_need", "not_applicable"),
         (liv_crisis_1 %in% c("yes", "no_exhausted")|liv_crisis_2 %in% c("yes", "no_exhausted")|liv_crisis_3 %in% c("yes", "no_exhausted")|
            liv_emergency_1 %in% c("yes", "no_exhausted")|liv_emergency_2 %in% c("yes", "no_exhausted")|liv_emergency_3 %in% c("yes", "no_exhausted")))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "liv_stress_1",
         i.check.current_value = as.character(liv_stress_1),
         i.check.value = "not_applicable",
         i.check.issue_id = "logic_c_lcsi_no_stress_but_crisis_emergency_c6",
         i.check.issue = glue("No stress reported but crisis and emergency"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 6)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "liv_crisis_1", 
                                  rank == 2 ~ "liv_crisis_2", 
                                  rank == 3 ~ "liv_crisis_3", 
                                  rank == 4 ~ "liv_emergency_1", 
                                  rank == 5 ~ "liv_emergency_2", 
                                  TRUE ~ "livh_emerg_lcsi_3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(liv_crisis_1), 
                                           rank == 2 ~ as.character(liv_crisis_2), 
                                           rank == 3 ~ as.character(liv_crisis_3), 
                                           rank == 4 ~ as.character(liv_emergency_1), 
                                           rank == 5 ~ as.character(liv_emergency_2), 
                                           TRUE ~ as.character(liv_emergency_3))
  ) |> 
  filter(!i.check.current_value %in% c("no_had_no_need", "not_applicable")) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_no_stress_but_crisis_emergency_c6")

# lcsi_stress_c7
df_logic_c_lcsi_no_livestock_but_stress4_c7 <- df_tool_data |>  
  filter(hh_own_livestock %in% c("no"), 
         liv_stress_4 %in% c("yes"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "liv_stress_4",
         i.check.current_value = as.character(liv_stress_4),
         i.check.value = "no_exhausted",
         i.check.issue_id = "logic_c_lcsi_no_livestock_but_stress4_c7",
         i.check.issue = glue("livestock but stress4"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_no_livestock_but_stress4_c7")

# lcsi_stress_c8
df_logic_c_lcsi_no_livestock_but_emergency2_c8 <- df_tool_data |>  
  filter(hh_own_livestock %in% c("no"), 
         liv_emergency_2 %in% c("yes"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "liv_emergency_2",
         i.check.current_value = as.character(liv_emergency_2),
         i.check.value = "no_exhausted",
         i.check.issue_id = "logic_c_lcsi_no_livestock_but_emergency2_c8",
         i.check.issue = glue("livestock but emergency2"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") |> 
  mutate(hh_kebele = as.character(hh_kebele))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_no_livestock_but_emergency2_c8")

# combined  checks ------------------------------------------------------------

df_combined_checks <- bind_rows(checks_output)

# output the log --------------------------------------------------------------

write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_eth_lcsa_somali.csv"), na = "")

###############################################################################

