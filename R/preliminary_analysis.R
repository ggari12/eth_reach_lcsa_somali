###############################################################################
rm(list = ls())
library(tidyverse)
library(srvyr)
library(supporteR)  

source("R/composite_indicators.R")
getwd()
# packages to install incase
# devtools::install_github("zackarno/butteR")
# devtools::install_github("twesigye10/supporteR")

# clean data
data_path <- "outputs/20240202_clean_data_eth_lcsa_somali.xlsx"
weight_table <- readxl::read_excel("inputs/data_eth_lcsa_somali_weighted.xlsx")|>
  dplyr::group_by(hh_zone, pop_group)|>
  mutate(hh_zone = case_when(hh_zone =="ET0508"~"Afder",
                             hh_zone == "ET0511"~	"Daawa",
                             hh_zone == "ET0507"~	"Doolo",
                             hh_zone == "ET0504"~	"Erer",
                             hh_zone == "ET0503"~	"Jarar",
                             hh_zone == "ET0505"~	"Korahe",
                             hh_zone == "ET0509"~	"Liban",
                             hh_zone == "ET0510"~	"Nogob",
                             hh_zone == "ET0506"~	"Shabelle"
    
  ))

data_nms <- names(readxl::read_excel(path = data_path, n_max = 3000, sheet = "cleaned_main_data"))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_main_clean_data <- readxl::read_excel(path = data_path, sheet = "cleaned_main_data", col_types = c_types, na = "NA") |> 
  # dplyr::select(-starts_with("i.")) |> 
  create_composite_indicators() |> 
  dplyr::mutate(strata = hh_woreda,
                hh_zone = case_when(hh_zone =="ET0508"~"Afder",
                                    hh_zone == "ET0511"~	"Daawa",
                                    hh_zone == "ET0507"~	"Doolo",
                                    hh_zone == "ET0504"~	"Erer",
                                    hh_zone == "ET0503"~	"Jarar",
                                    hh_zone == "ET0505"~	"Korahe",
                                    hh_zone == "ET0509"~	"Liban",
                                    hh_zone == "ET0510"~	"Nogob",
                                    hh_zone == "ET0506"~	"Shabelle"
                                    
                )) |>
  mutate(
    group_zone = case_when(
      hh_zone=="Afder" & pop_group == "host"~"Afder_host",
      hh_zone=="Afder" & pop_group == "idp"~"Afder_idp",
      
      hh_zone=="Daawa" & pop_group == "host"~"Daawa_host",
      hh_zone=="Daawa" & pop_group == "idp"~"Daawa_idp",
      
      hh_zone=="Doolo" & pop_group == "host"~"Doolo_host",
      hh_zone=="Doolo" & pop_group == "idp"~"Doolo_idp",
      
      hh_zone=="Erer" & pop_group == "host"~"Erer_host",
      hh_zone=="Erer" & pop_group == "idp"~"Erer_idp",
      
      hh_zone=="Jarar" & pop_group == "host"~"Jarar_host",
      hh_zone=="Jarar" & pop_group == "idp"~"Jarar_idp",
      
      hh_zone=="Korahe" & pop_group == "host"~"Korahe_host",
      hh_zone=="Korahe" & pop_group == "idp"~"Korahe_idp",
      
      hh_zone=="Liban" & pop_group == "host"~"Liban_host",
      hh_zone=="Liban" & pop_group == "idp"~"Liban_idp",
      
      hh_zone=="Nogob" & pop_group == "host"~"Nogob_host",
      hh_zone=="Nogob" & pop_group == "idp"~"Nogob_idp",
      
      hh_zone=="Shabelle" & pop_group == "host"~"Shabelle_host",
      hh_zone=="Shabelle" & pop_group == "idp"~"Shabelle_idp"
    ))|> 
  dplyr::mutate(across(.cols = starts_with("i."), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .)))

view(df_main_clean_data$expenditure_rent)
# |>
#   mutate(
#          ##in.wash_water_quantity
#          int.wash_water_quantity = case_when(wash_water_quantity=="never"~ 0,
#                                              wash_water_quantity=="rarely"~1,
#                                              wash_water_quantity=="sometimes"~2,
#                                              wash_water_quantity %in% c("often","always")~3
#                                              
#          ),
#          int.wash_water_quantity2 = case_when(wash_water_quantity2=="never"~0,
#                                               wash_water_quantity2=="rarely"~1,
#                                               wash_water_quantity2=="sometimes"~2,
#                                               wash_water_quantity2 %in% c("often","always")~3
#                                               
#          ),
#          int.wash_water_quantity3 = case_when(wash_water_quantity3=="never"~0,
#                                               wash_water_quantity3=="rarely"~1,
#                                               wash_water_quantity3=="sometimes"~2,
#                                               wash_water_quantity3 %in% c("often","always")~3
#                                               
#          ),
#          int.wash_water_quantity4 = case_when(wash_water_quantity4=="never"~0,
#                                               wash_water_quantity4=="rarely"~1,
#                                               wash_water_quantity4=="sometimes"~2,
#                                               wash_water_quantity4 %in% c("often","always")~3
#                                               
#          ),
#          int.wash_water_quantity5 = case_when(wash_water_quantity5=="never"~0,
#                                               wash_water_quantity5=="rarely"~1,
#                                               wash_water_quantity5=="sometimes"~2,
#                                               wash_water_quantity5 %in% c("often", "always")~3
#                                               
#          ),
#          int.wash_water_quantity6 = case_when(wash_water_quantity6=="never"~0,
#                                               wash_water_quantity6=="rarely"~1,
#                                               wash_water_quantity6=="sometimes"~2,
#                                               wash_water_quantity6 %in% c("often", "always")~3
#                                               
#          ),
#          int.wash_water_quantity7 = case_when(wash_water_quantity7=="never"~0,
#                                               wash_water_quantity7=="rarely"~1,
#                                               wash_water_quantity7=="sometimes"~2,
#                                               wash_water_quantity7 %in% c("often", "always")~3
#                                               
#          ),
#          int.wash_water_quantity8 = case_when(wash_water_quantity8=="never"~0,
#                                               wash_water_quantity8=="rarely"~1,
#                                               wash_water_quantity8=="sometimes"~2,
#                                               wash_water_quantity8 %in% c("often","always")~3
#                                               
#          ),
#          int.wash_water_quantity9 = case_when(wash_water_quantity9=="never"~0,
#                                               wash_water_quantity9=="rarely"~1,
#                                               wash_water_quantity9=="sometimes"~2,
#                                               wash_water_quantity9 %in% c("often", "always")~3
#                                               
#          ),
#          int.wash_water_quantity10 = case_when(wash_water_quantity10=="never"~0,
#                                                wash_water_quantity10=="rarely"~1,
#                                                wash_water_quantity10=="sometimes"~2,
#                                                wash_water_quantity10 %in% c("often","always")~3
#                                                
#          ),
#          int.wash_water_quantity11 = case_when(wash_water_quantity11=="never"~0,
#                                                wash_water_quantity11=="rarely"~1,
#                                                wash_water_quantity11=="sometimes"~2,
#                                                wash_water_quantity11 %in% c("often","always")~3
#                                                
#          ),
#          int.wash_water_quantity12 = case_when(wash_water_quantity12=="never"~0,
#                                                wash_water_quantity12=="rarely"~1,
#                                                wash_water_quantity12=="sometimes"~2,
#                                                wash_water_quantity12 %in% c("often", "always")~3
#                                                
#          )                                   
#          )
#    ###  wash_water_score      
# df_main_clean_data$wash_water_score = rowSums(df_main_clean_data[ ,c("int.wash_water_quantity",
#                                            "int.wash_water_quantity2",
#                                            "int.wash_water_quantity3",
#                                            "int.wash_water_quantity4",
#                                            "int.wash_water_quantity5",
#                                            "int.wash_water_quantity6",
#                                           "int.wash_water_quantity7",
#                                            "int.wash_water_quantity8",
#                                            "int.wash_water_quantity9",
#                                            "int.wash_water_quantity10",
#                                            "int.wash_water_quantity11",
#                                            "int.wash_water_quantity12")
#                                            ], na.rm = T)
# ##i.wash_water_secure         
# df_main_clean_data <- df_main_clean_data|>
#   mutate(i.wash_warter_secure = case_when(
#     wash_water_score < 11 ~ "water_secure",
#     wash_water_score>11~"water_insecure"
#   )
#   )

# add weights to data
df_main_clean_data_with_weights <- df_main_clean_data |>
  dplyr::group_by(hh_zone, pop_group)|>
  left_join(weight_table, by = c("hh_zone", "pop_group"))|>
  dplyr::rename(zone1="hh_zone")

writexl::write_xlsx(df_main_clean_data_with_weights, "inputs/clean_data_eth_lcsa_somali_weighted.xlsx")



loop_support_data <- df_main_clean_data_with_weights|> 
  dplyr::select(uuid,zone1,respondent_gender,i.respondent_age, group_zone, strata, weights)


#Load loop

health_loop <- readxl::read_excel(path = data_path, sheet = "cleaned_health_loop", na = "NA")
df_health_clean_data <- loop_support_data |> 
  inner_join(health_loop, by = c("uuid" = "_submission__uuid") ) 

### add some indicator form health_loop to df_main_clean_data

##recoding health needed healthcare in health loop

df_health_clean_data <- df_health_clean_data|>
   mutate(
     hh_least_needed_healthcare = case_when(
        health_needed_healthcare =="yes" ~ 1,
       health_needed_healthcare =="no" ~ 0
     )
   )

numeric_cols <- c(
  "hh_least_needed_healthcare")

df_health_clean_data[, numeric_cols] <- lapply(df_health_clean_data[, numeric_cols], as.numeric)

## Au moins un menbre de famille a besoi des soin
household_level = df_health_clean_data %>%
   group_by(uuid) %>%
   summarise(across(numeric_cols, 
                    list(max = ~max(., na.rm = TRUE))))
 
household_level <- household_level %>% mutate_all(~ifelse(. == "-Inf", NA, .)) ## change -Inf to NA
household_level <- household_level %>% rename_with(~str_remove(., '_max')) ## remove "_max"
 

 ### join results to  df_main_clean_data
df_main_clean_data_with_weights <-  df_main_clean_data_with_weights %>% 
   left_join(household_level)|>
   mutate(i.hh_least_needed_healthcare = case_when( 
     hh_least_needed_healthcare == 1 ~ "least_person_need_healthcare",
     hh_least_needed_healthcare == 0 ~ "no_dtw_dt")
     
   )

# roster_loop <- readxl::read_excel(path = data_path, sheet = "cleaned_roster_loop", na = "NA")|> 
# create_composite_indicators_roster()
# df_roster_clean_data <- loop_support_data |> 
#   inner_join(roster_loop, by = c("uuid" = "_submission__uuid") ) 

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


# health ------------------------------------------------------------------

df_dap_health <- bind_rows(tibble::tribble(~variable,
                                           "health_needed_healthcare",
                                           "health_received_healthcare",
                                           "healthcare_seek",
                                           "health_unmet_need_type")) |> 
  mutate(split = "all",
         subset_1 = "pop_group",
         subset_2 = "zone1",
         subset_3 = "respondent_gender",
         subset_4 = "i.respondent_age",
         subset_4 = "group_zone"
         
         
         )|> 
  pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
  filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
  select(-subset_no)

# set up design object
   ref_svy_health_loop <- as_survey(.data = df_health_clean_data, strata = strata, weights = weights)

# # analysis
df_analysis_health_loop <- analysis_after_survey_creation(input_svy_obj = ref_svy_health_loop,
                                                         input_dap = df_dap_health)|>


 mutate(level = "Individual")

# roster ------------------------------------------------------------------

# df_dap_roster <- bind_rows(tibble::tribble(~variable,
#                                            "i.individual_age_cat",
#                                            "i.individual_genre_cat",
#                                            "i.individual_age_school_cat",
#                                            "i.individual_genre_age_cat")) |> 
#   mutate(split = "all",
#          subset_2 = "ind_gender"
#   ) |> 
#   pivot_longer(cols = starts_with("subset"), names_to = "subset_no", values_to = "subset_1") |> 
#   filter(!is.na(subset_1), !subset_1 %in% c("NA")) |> 
#   select(-subset_no)



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
# ref_svy_roster <- as_survey(.data = df_roster_clean_data, strata = strata, weights = weights)
# # analysis
# df_analysis_roster <- analysis_after_survey_creation(input_svy_obj = ref_svy_roster,
#                                                      input_dap = df_dap_roster ) |> 
#   mutate(level = "Individual")
# 

# merge and format analysis ----------------------------------------------------------

combined_analysis <- bind_rows(df_main_analysis, df_analysis_health_loop)


integer_cols_i <- c("i.fcs", 
                    "i.rcsi", 
                    "i.hhs",
                    "i.chronic_illness_male",
                    "i.chronic_illness_female",
                    "i.pregnant_lac_women",
                    "i.fs_meals_cat",
                    "i.fs_meals_U5",
                    "i.hh_zone",
                    "i.wash_warter_secure",
                    "i.lcsi_cat",
                    "i.respondent_age",
                    "i.wash_watertime",
                    "i.expenditure_food30_days_total",
                    "i.expenditure_service6_month_total",
                    "i.last30days_income_sources_total",
                    "i.past3years_hh_income_sources_total",
                    "i.hh_least_needed_healthcare"
                    )
integer_cols_int <- c("int.fcs", 
                      "int.rcsi",
                      "int.hhs",
                      "int.chronic_illness_male",
                      "int.chronic_illness_female",
                      "int.pregnant_lac_women",
                      "int.fs_meals_cat",
                      "int.fs_meals_U5",
                      "int.hh_zone",
                      "int.wash_warter_secure",
                      "int.lcsi_cat",
                      "int.respondent_age",
                      "int.wash_watertime",
                      "int.expenditure_food30_days_total",
                      "int.expenditure_service6_month_total",
                      "int.last30days_income_sources_total",
                      "int.past3years_hh_income_sources_total",
                      "int.hh_least_needed_healthcare")

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
view(full_analysis_long)  
write_csv(full_analysis_long, paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_eth_lcsa_somali.csv"), na="")
write_csv(full_analysis_long, paste0("outputs/full_analysis_lf_eth_lcsa_somali.csv"), na="")
write_csv(df_main_analysis, paste0("outputs/", butteR::date_file_prefix(), "combined_analysis_lf_eth_somali.csv"), na="")
###############################################################################