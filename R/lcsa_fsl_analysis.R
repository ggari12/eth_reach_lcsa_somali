##################################################################################
# TEMPLATE HEALHTYR ANALYSIS DOCUMENT for Food Security Outcomes Data
#
# For use by REACH Initiative HQ and Country Teams
# Drafted 01 May 2023 by Public Health Unit at IMPACT HQ
# If any issues with the scripts or troubleshooting needed,
# please contact saeed.rahman@impact-initiative.org
# or Olivia Falkowitz, IMPACT FSL Focal Point (olivia.falkowitz@impact-initiatives.org)

# Setup ####

rm(list = ls())

# remotes::install_github("SaeedR1987/healthyr")

library(tidyverse)
library(healthyr)

# Step 1: Load your Dataset ####

df <- readxl::read_excel(path = "inputs/20240202_clean_data_eth_lcsa_somali.xlsx", na = "NA") |> 
  mutate(across(.cols = liv_stress_1:liv_emergency_3, 
                .fns = ~case_when(.x %in% c("yes") ~ "1",
                                  .x %in% c("no_had_no_need") ~ "2",
                                  .x %in% c("no_exhausted") ~ "3",
                                  .x %in% c("not_applicable") ~ "4")))  
  
# Step 2: Format Your Dataset ####

df2 <- format_nut_health_indicators(df = df,
                                    cluster = "hh_kebele", 
                                    enum = "enumerator_id", 
                                    date_of_dc = "today",
                                    
                                    # FSL Outcome Indicators
                                    fcs_cereal = "fs_fcs_cereals_grains_roots_tubers", fcs_legumes = "fs_fcs_beans_nuts", fcs_dairy = "fs_fcs_dairy", fcs_meat = "fs_fcs_meat_fish_eggs", fcs_veg = "fs_fcs_vegetables_leaves", fcs_fruit = "fs_fcs_fruit", fcs_oil = "fs_fcs_oil_fat_butter", fcs_sugar = "fs_fcs_sugar",
                                    # hdds_cereals = "F011B", hdds_tubers = "F012B", hdds_dairy = "F03B", hdds_veg = "F05B", hdds_fish = "F043B", hdds_meat = "hdds_meats_any", hdds_eggs = "F044B", hdds_fruit = "F06B", hdds_legumes = "F02B", hdds_condiments = "F09B", hdds_sugars = "F08B", hdds_oils = "F07B",
                                    hhs_nofoodhh_1 = "fs_hhs_nofood", hhs_nofoodhh_1a = "fs_hhs_nofood_freq", hhs_sleephungry_2 = "fs_hhs_sleephungry", hhs_sleephungry_2a = "fs_hhs_sleephungry_freq", hhs_alldaynight_3 = "fs_hhs_daynoteating", hhs_alldaynight_3a = "fs_hhs_daynoteating_freq",
                                    rcsi_lesspreferred_1 = "rCSILessQlty", rcsi_borrowfood_2 = "rCSIBorrow", rcsi_limitportion_3 = "rCSIMealSize", rcsi_restrict_4 = "rCSIMealAdult", rcsi_reducemeals5 = "rCSIMealNb",
                                    
                                    # Livelihood Coping Strategy Indicators
                                    lcs_variables = c("liv_stress_1", "liv_stress_2", "liv_stress_3", "liv_stress_4", "liv_crisis_1", "liv_crisis_2", "liv_crisis_3", "liv_emergency_1", "liv_emergency_2", "liv_emergency_3"),
                                    
                                    # Income variables as in MSNA Indicator Bank
                                    livelihood_variables = c("income_salaried" = "hh_income_salaried_work", "income_casual" = "hh_income_casual_income", "income_trade" = "hh_income_own_business_income", "income_own_production" = "hh_income_agriculture_farming", "income_social_benefits" = "hh_income_govt_social_benefits",
                                                             "income_rent" = "hh_income_rent_income", "income_remittances" = "hh_income_remittances", "income_loans_family" = "hh_income_loans_or_family_support", #"income_loans_community",
                                                             "income_humanitarian_assistance" = "hh_income_hum_assistance"),
                                    
                                    # Expenditure Indicators as in MSNA Indicator Bank
                                    food_exp_col = "expenditure_food",
                                    health_exp_col = "expenditure_health",
                                    monthly_expenditures = c("exp_food" = "expenditure_food", "exp_rent" = "expenditure_rent", "exp_nfi_monthly" = "expenditure_nfi_frequent", "exp_utilities" = "expenditure_utilities", "exp_fuel" = "expenditure_fuel", "exp_transport" = "expenditure_transportation", "exp_comms" = "expenditure_communications", "exp_other_monthly" = "expenditure_other_frequent"),
                                    period_expenditures = c("exp_shelter" = "expenditure_shelter", "exp_nfi_infrequent" = "expenditure_nfi_infrequent", "exp_health" = "expenditure_health", "exp_education" = "expenditure_education", "exp_debt" = "expenditure_debt", "exp_other_infrequent" = "expenditure_other_infrequent"),
                                    num_period_months = 6)




# Step 3: Review a Quality Summary Report ####

 (create_fsl_quality_report(df = df2, short_report = TRUE))
# 
 (create_fsl_quality_report(df = df2, short_report = FALSE))
# 
 (create_fsl_quality_report(df = df2, grouping = "enum", short_report = TRUE))
# 
 (create_fsl_quality_report(df = df2, grouping = "enum", short_report = FALSE))

# export reports
healthyr::create_fsl_quality_report(df = df2, 
                                    short_report = FALSE,
                                    file_path = paste0("outputs/", butteR::date_file_prefix(), 
                                                       "_eth_lcsa_somali_healthyr_full_report.xlsx"))

# Step 4a: Evaluate Data with Visualizations ####

(plot_ridge_distribution(df2, numeric_cols = c("fcs_cereal", "fcs_dairy", "fcs_veg", "fcs_fruit", "fcs_legumes", "fcs_sugar", "fcs_oil"),
                         name_groups = "Food Groups", name_units = "Days"))

(plot_ridge_distribution(df2, numeric_cols = c("fcs_cereal", "fcs_dairy", "fcs_veg", "fcs_fruit", "fcs_legumes", "fcs_sugar", "fcs_oil"),
                         name_groups = "Food Groups", name_units = "Days", grouping = "enum"))

(plot_ridge_distribution(df2, numeric_cols = c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3","rcsi_restrict_4", "rcsi_reducemeals5"),
                         name_groups = "Food Coping Strategy", name_units = "Days"))

(plot_ridge_distribution(df2, numeric_cols = c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3","rcsi_restrict_4", "rcsi_reducemeals5"),
                         name_groups = "Food Coping Strategy", name_units = "Days", grouping = "enum"))

(plot_correlogram(df2, numeric_cols = c("fcs_score", "hdds_score", "rcsi_score",  "hhs_score")))

# Step 4b: Data Quality Checking Dashboard
# install and load these libraries below in order to run the dashboard
# for the paramater df, use the formatted dataset from format_nut_health_indicators
# for the paramater grouping_var,

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(DT)

healthyr::run_fsl_monitoring_dashboard(df = df2, grouping_var = "enum", filter_var1 = "date_dc")

# Step 5: Export Flagged Records to Cleaning Log + Cleaning ####

(flag_summary <- flag_summary_table(df = df2, grouping = "enum"))

cl <- create_cleaning_log_flags(df = df2, uuid_col = "uuid")
View(cl)

cl_food_related <- cl|> filter(!description %in% c("Other values, check if should be recoded."))

write_csv(cl_food_related, 
          file = paste0("outputs/", butteR::date_file_prefix(), "_eth_lcsa_somali_healthyr_cl_output.csv"))

# Step 6: Analyse Survey Results ####
# if cluster survey, use sample_design = "two_stage_cluster"
# if not cluster and multiple areas, use sample_design = "two_stage_stratified"

(res <- analyse_survey_results(df = df2 %>% dplyr::mutate(food_exp_share = as.numeric(food_exp_share)),
                               aggregation = "enum",
                               
                               #sample_design = "two_stage_stratified",
                               sample_design = "two_stage_cluster",
                               cluster = "cluster",
                               
                               proportions = c("fcs_cat", "hhs_cat","hdds_cat",  "rcsi_cat", "lcs_cat",
                                               "fc_phase", "fclc_phase"),
                               
                               means = c("fcs_score", "hhs_score", "rcsi_score", "food_exp_share")))

##################################################################################
