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
  filter(i.check.start_date < as_date("2023-11-10")) |> 
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
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_testing_data")

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
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_enumerator_id_harmonization")

# Time checks -----------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 20
max_time_of_survey <- 120

df_c_survey_time <-  supporteR::check_survey_time(input_tool_data = df_tool_data, 
                                                  input_enumerator_id_col = "enumerator_id",
                                                  input_location_col = "hh_kebele",
                                                  input_min_time = min_time_of_survey, 
                                                  input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_survey_time")

# check duplicate uuids -------------------------------------------------------

df_c_duplicate_uuid <-  supporteR::checks_duplicate_uuids(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_duplicate_uuid")

# outliers --------------------------------------------------------------------

df_c_outliers <- supporteR::check_outliers_cleaninginspector(input_tool_data = df_tool_data,
                                                             input_enumerator_id_col = "enumerator_id",
                                                             input_location_col = "hh_kebele")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_outliers")

# other_specify ---------------------------------------------------------------

df_others_data <- supporteR::extract_other_specify_data(input_tool_data = df_tool_data |> select(-hh_income_other), 
                                                        input_enumerator_id_col = "enumerator_id",
                                                        input_location_col = "hh_kebele",
                                                        input_survey = df_survey,  
                                                        input_choices = df_choices)

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_others_data")

# logical checks --------------------------------------------------------------
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
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

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
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

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
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

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
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_pop_from_idp_but_not_describe_hh_situation")

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
  batch_select_rename()

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
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_sell_female_animal_but_not_owning_any_livestock")

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
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

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
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

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
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_exprience_shocks_but_not_shocks_affected_household")

#HH experience 'too much rain or flooding' in the past 3 months, but not reports affected too much rain/flooding
df_logic_c_hh_affected_by_flooding_but_not_reports_issue <- df_tool_data |> 
  filter(fs_shock_3months %in%  c("too_much_rain_flooding"), hh_floods_affect %in% c( "no")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fs_shock_3months",
         i.check.current_value = fs_shock_3months,
         i.check.value = "",
         i.check.issue_id = "hh_hh_affected_by_flooding_but_not_reports_issue",
         i.check.issue = glue("fs_shock_3months: {fs_shock_3months} but hh_floods_affect: {hh_floods_affect}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_affected_by_flooding_but_not_reports_issue")

# check FSL -------------------------------------------------------------------

# combined  checks ------------------------------------------------------------

df_combined_checks <- bind_rows(checks_output)

# output the log --------------------------------------------------------------

write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_eth_lcsa_somali.csv"), na = "")

###############################################################################
