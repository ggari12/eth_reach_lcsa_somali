##################################################################################
# TEMPLATE HEALHTYR ANALYSIS DOCUMENT for Food Security Outcomes Data
#
# For use by REACH Initiative HQ and Country Teams
# Drafted 18 April 2022 by Cluster Support Unit (CSU)
# If any issues with the scripts or troubleshooting needed,
# please contact saeed.rahman@reach-initiative.org

# Setup ####

rm(list = ls())

# remotes::install_github("SaeedR1987/healthyr")

library(tidyverse)
library(healthyr)

# Step 1: Load your Dataset ####

df <- readxl::read_excel(path = "inputs/clean_data_eth_lcsa_somali.xlsx", na = "NA") |> 
df <- readxl::read_excel(data_path) |> 
  mutate(across(.cols = liv_stress_1:liv_emergency_3, 
                .fns = ~case_when(.x %in% c("yes") ~ "1",
                                  .x %in% c("no_had_no_need") ~ "2",
                                  .x %in% c("no_exhausted") ~ "3",
                                  .x %in% c("not_applicable") ~ "4")))  
  
# Step 2: Format Your Dataset ####

df2 <- format_nut_health_indicators(df = df,
                                    
                                    cluster = "hh_kebele", enum = "enumerator_id",
                                    
                                    fcs_cereal = "fs_fcs_cereals_grains_roots_tubers", fcs_legumes = "fs_fcs_beans_nuts", fcs_dairy = "fs_fcs_dairy", fcs_meat = "fs_fcs_meat_fish_eggs", fcs_veg = "fs_fcs_vegetables_leaves", fcs_fruit = "fs_fcs_fruit", fcs_oil = "fs_fcs_oil_fat_butter", fcs_sugar = "fs_fcs_sugar",
                                    
                                    # hdds_cereals = "F011B", hdds_tubers = "F012B", hdds_dairy = "F03B", hdds_veg = "F05B", hdds_fish = "F043B", hdds_meat = "hdds_meats_any", hdds_eggs = "F044B", hdds_fruit = "F06B", hdds_legumes = "F02B", hdds_condiments = "F09B", hdds_sugars = "F08B", hdds_oils = "F07B",
                                    
                                    hhs_nofoodhh_1 = "fs_hhs_nofood", hhs_nofoodhh_1a = "fs_hhs_nofood_freq", hhs_sleephungry_2 = "fs_hhs_sleephungry", hhs_sleephungry_2a = "fs_hhs_sleephungry_freq", hhs_alldaynight_3 = "fs_hhs_daynoteating", hhs_alldaynight_3a = "fs_hhs_daynoteating_freq",
                                    
                                    rcsi_lesspreferred_1 = "rCSILessQlty", rcsi_borrowfood_2 = "rCSIBorrow", rcsi_limitportion_3 = "rCSIMealSize", rcsi_restrict_4 = "rCSIMealAdult", rcsi_reducemeals5 = "rCSIMealNb",
                                    
                                    lcs_variables = c("liv_stress_1", "liv_stress_2", "liv_stress_3", "liv_stress_4", "liv_crisis_1", "liv_crisis_2", "liv_crisis_3", "liv_emergency_1", "liv_emergency_2", "liv_emergency_3")
)

# Step 3: Review a Quality Summary Report ####

# (create_fsl_quality_report(df = df2, short_report = TRUE))
# 
# (create_fsl_quality_report(df = df2, short_report = FALSE))
# 
# (create_fsl_quality_report(df = df2, grouping = "enum", short_report = TRUE))
# 
# (create_fsl_quality_report(df = df2, grouping = "enum", short_report = FALSE))

# export reports
healthyr::create_fsl_quality_report(df = df2, 
                                    short_report = FALSE, 
                                    file_path = paste0("outputs/", butteR::date_file_prefix(), 
                                                       "_eth_lcsa_somali_healthyr_full_report.xlsx"))

# Step 4: Evaluate Data with Visualizations ####

(plot_ridge_distribution(df2, numeric_cols = c("fcs_cereal", "fcs_dairy", "fcs_veg", "fcs_fruit", "fcs_legumes", "fcs_sugar", "fcs_oil"),
                         name_groups = "Food Groups", name_units = "Days"))

(plot_ridge_distribution(df2, numeric_cols = c("fcs_cereal", "fcs_dairy", "fcs_veg", "fcs_fruit", "fcs_legumes", "fcs_sugar", "fcs_oil"),
                         name_groups = "Food Groups", name_units = "Days", grouping = "enum"))

(plot_ridge_distribution(df2, numeric_cols = c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3","rcsi_restrict_4", "rcsi_reducemeals5"),
                         name_groups = "Food Coping Strategy", name_units = "Days"))

(plot_ridge_distribution(df2, numeric_cols = c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3","rcsi_restrict_4", "rcsi_reducemeals5"),
                         name_groups = "Food Coping Strategy", name_units = "Days", grouping = "enum"))

(plot_correlogram(df2, numeric_cols = c("fcs_score", "hdds_score", "rcsi_score",  "hhs_score")))

# Step 5: Export Flagged Records to Cleaning Log + Cleaning ####

(flag_summary <- flag_summary_table(df = df2, grouping = "enum"))

cl <- create_cleaning_log_flags(df = df2, uuid_col = "_uuid")

cl_food_related <- cl|> filter(!description %in% c("Other values, check if should be recoded."))

write_csv(cl_food_related, 
          file = paste0("outputs/", butteR::date_file_prefix(), "_eth_lcsa_somali_healthyr_cl_output.csv"))

# Step 6: Analyse Survey Results ####

(res <- analyse_survey_results(df = df2,
                               aggregation = "enum_id",
                               sample_design = "two_stage_cluster",
                               cluster = "cluster",
                               proportions = c("fcs_cat", "hhs_cat", "hdds_cat", "rcsi_cat", "lcs_cat",
                                               "fc_phase", "fclc_phase"),
                               means = c("fcs_score", "hhs_score", "rcsi_score")))

##################################################################################