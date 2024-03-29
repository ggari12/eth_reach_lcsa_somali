###############################################################################
# read packages
rm(list = ls())
library(tidyverse)

df_tool_survey <- readxl::read_excel(path = "inputs/REACH_ETH_LCSA_Somali_tool.xlsx", sheet = "survey")

vars_to_remove <- c("consent",
                    "team_leader_name",
                    "enumerator_id",
                    "enum_comment",
                    "person_name",
                    "health_ind_name",
                    "hh_kebele")

df_dap_file_data_composites <- df_tool_survey |> 
  filter(str_detect(string = type, pattern = "integer|date|select_one|select_multiple"),
         !str_detect(string = name, pattern = "^fs_hhs|^fs_fcs|^liv"),
         !name %in% vars_to_remove) |> 
  select(variable = name) |>
  bind_rows(tibble::tribble(~variable,
                            "i.fcs",
                            "i.fcs_cat",
                            "i.rcsi",
                            "i.rcsi_cat",
                            "i.hhs",
                            "i.hhs_cat",
                            "i.chronic_illness_male",
                            "i.chronic_illness_female",
                            "i.pregnant_lac_women",
                            "i.fs_meals_cat",
                            "i.fs_meals_U5",
                            "i.hh_zone",
                            "i.wash_warter_secure",
                            "i.wash_watertime",
                            "i.lcsi_cat",
                            "i.respondent_age",
                            "i.fewsnet_phase",
                            "i.expenditure_food30_days_total",
                            "i.expenditure_service6_month_total",
                            "i.last30days_income_sources_total",
                            "i.past3years_hh_income_sources_total",
                            "i.hh_least_needed_healthcare")) |> 
  mutate(split = "all",
         subset_1 = "zone1",
         subset_2 = "group_zone",
         subset_3 = "pop_group",
         subset_4 = "respondent_gender",
         subset_5 = "i.respondent_age") |> 
  pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
  filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
  select(-subset_no)

# output r_dap
write_csv(x = df_dap_file_data_composites, file = paste0("outputs/", butteR::date_file_prefix(), "_r_dap_eth_lcsa_somali.csv"), na = "NA") 
write_csv(x = df_dap_file_data_composites, file = "inputs/r_dap_eth_lcsa_somali.csv", na = "NA")

###############################################################################
