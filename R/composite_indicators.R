###############################################################################

# creating composite indicators -----------------------------------------------

create_composite_indicators <- function(input_df) {
  input_df |> 
    dplyr::mutate(int.fcs_cereals_tubers = fs_fcs_cereals_grains_roots_tubers*2,
           int.fcs_pulses = fs_fcs_beans_nuts*3,
           int.fcs_vegetables = fs_fcs_vegetables_leaves,
           int.fcs_fruit = fs_fcs_fruit,
           int.fcs_meat_fish = fs_fcs_meat_fish_eggs*4,
           int.fcs_dairy = fs_fcs_dairy*4,
           int.fcs_sugar = fs_fcs_sugar*0.5,
           int.fcs_oils = fs_fcs_oil_fat_butter*0.5,
           int.rCSILessQlty = rCSILessQlty,
           int.rCSIBorrow = 2 * rCSIBorrow,
           int.rCSIMealSize = rCSIMealSize,
           int.rCSIMealAdult = 3 * rCSIMealAdult,
           int.rCSIMealNb = rCSIMealNb,
           int.freq_no_food_lack_resources = case_when(fs_hhs_nofood %in% c("no") ~ 0,
                                                       fs_hhs_nofood %in% c("yes") & fs_hhs_nofood_freq %in% c("rarely") ~ 0,
                                                       fs_hhs_nofood %in% c("yes") & fs_hhs_nofood_freq %in% c("sometimes") ~ 1,
                                                       fs_hhs_nofood %in% c("yes") & fs_hhs_nofood_freq %in% c("often") ~ 2),
           int.freq_sleep_hungry = case_when(fs_hhs_sleephungry %in% c("no") ~ 0,
                                             fs_hhs_sleephungry %in% c("yes") & fs_hhs_sleephungry_freq %in% c("rarely") ~ 0,
                                             fs_hhs_sleephungry %in% c("yes") & fs_hhs_sleephungry_freq %in% c("sometimes") ~ 1,
                                             fs_hhs_sleephungry %in% c("yes") & fs_hhs_sleephungry_freq %in% c("often") ~ 2),
           int.freq_day_and_night_no_food = case_when(fs_hhs_daynoteating %in% c("no") ~ 0,
                                                      fs_hhs_daynoteating %in% c("yes") & fs_hhs_daynoteating_freq %in% c("rarely") ~ 0,
                                                      fs_hhs_daynoteating %in% c("yes") & fs_hhs_daynoteating_freq %in% c("sometimes") ~ 1,
                                                      fs_hhs_daynoteating %in% c("yes") & fs_hhs_daynoteating_freq %in% c("often") ~ 2) 
    ) |> 
    rowwise() |> 
    dplyr::mutate(int.fcs = sum(c_across(int.fcs_cereals_tubers:int.fcs_oils), na.rm = T),
           int.rcsi = sum(c_across(int.rCSILessQlty:int.rCSIMealNb)),
           int.hhs = sum(c_across(int.freq_no_food_lack_resources:int.freq_day_and_night_no_food)),
           i.hh_size = case_when(hh_size <= 3 ~ "between_1_and_3_members",
                                 hh_size <= 6 ~ "between_4_and_6_members",
                                 hh_size <= 9 ~ "between_7_and_9_members",
                                 hh_size >= 10 ~ "10_or_more_members"),
           i.respondent_age = case_when(respondent_age <= 24 ~ "age_18_24",
                                        respondent_age <= 39 ~ "age_25_39",
                                        respondent_age <= 59 ~ "age_40_59",
                                        respondent_age > 59 ~ "age_60+")
           #int.hh_number_male = sum(c_across(c("hh_number_men_count", "hh_number_boys_count")), na.rm = T),
           #int.hh_number_female = sum(c_across(c("hh_number_women_count", "hh_number_girls_count")), na.rm = T)
           
    ) |>
    ungroup() |>
    dplyr::mutate(i.fcs = int.fcs,
           i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
                                 i.fcs <= 35 ~ "Borderline",
                                 i.fcs <= 112 ~ "Acceptable"),
           i.rcsi = int.rcsi,
           i.rcsi_cat = case_when(i.rcsi < 4 ~ "rcsi_0_3",
                                  i.rcsi < 19 ~ "rcsi_4_18",
                                  i.rcsi >= 19 ~ "rcsi_19+"),
           i.hhs = int.hhs,
           i.hhs_cat = case_when(i.hhs == 0 ~ "None",
                                 i.hhs == 1 ~ "Slight",
                                 i.hhs <= 3 ~ "Moderate",
                                 i.hhs == 4 ~ "Severe",
                                 i.hhs <= 6 ~ "Very severe"),
           
           #i.hh_composition_size = int.hh_size,
           i.hoh_gender = ifelse(is.na(hoh_gender), respondent_gender, hoh_gender),
           i.hoh_age = ifelse(is.na(hoh_age), respondent_age, hoh_age),
           i.chronic_illness_male = case_when(chronic_illness_male == 0 ~ "no",
                                              chronic_illness_male > 0 ~ "yes"), 
           i.chronic_illness_female = case_when(chronic_illness_female == 0 ~ "no",
                                                chronic_illness_female > 0 ~ "yes"), 
           i.pregnant_lac_women = case_when(pregnant_lac_women == 0 ~ "no",
                                            pregnant_lac_women > 0 ~ "yes"), 
           
           i.fs_meals_cat = case_when(fs_meals==1~"meals_1",
                                      fs_meals == 2~"meals_2",
                                      fs_meals == 3~"meals_3",
                                      fs_meals ==4~"meals_4",
                                      fs_meals ==5~"meals_5"),
           
           i.fs_meals_U5 = case_when(fs_meals_U5==1~"meals_U5_1",
                                     fs_meals_U5 == 2~"meals_U5_2",
                                     fs_meals_U5 == 3~"meals_U5_3",
                                     fs_meals_U5 ==4~"meals_U5_4",
                                     fs_meals_U5 ==5~"meals_U5_5"),
           ##expenditure food 30 days
           i.expenditure_food30_days_total = rowSums(across(c(expenditure_food,
                                                      expenditure_rent,
                                                      expenditure_water,
                                                      expenditure_nfi_frequent,
                                                      expenditure_utilities,
                                                      expenditure_fuel,
                                                      expenditure_transportation,
                                                      expenditure_communications,
                                                      expenditure_other_frequent
             
           )), na.rm = TRUE),

            ## expenditure service 6 months
           i.expenditure_service6_month_total = rowSums(across(c(
             expenditure_shelter,
             expenditure_nfi_infrequent,
             expenditure_health,
             expenditure_education,
             expenditure_debt,
             expenditure_other_infrequent
           )), na.rm = TRUE),

        ## income last 30 days
        i.last30days_income_sources_total = rowSums(across(c(last30_income_agriculture_livestock,
                                                    last30_income_agriculture_farming,
                                                    last30_income_fishing,
                                                    last30_income_salaried_work,
                                                    last30_income_casual_income,
                                                    last30_income_own_business_income,
                                                    last30_income_govt_social_benefits,
                                                    last30_income_remittances,
                                                    last30_income_rent_income,
                                                    last30_income_loans_or_family_support,
                                                    last30_income_hum_assistance,
                                                    last30_hh_other_income_amount

        )), na.rm = TRUE),
      
        ### income the past 3 years-2020
        i.past3years_hh_income_sources_total = rowSums(across(c( hh_income_agriculture_livestock,
                                                               hh_income_agriculture_farming,
                                                               hh_income_fishing,
                                                               hh_income_salaried_work,
                                                               hh_income_casual_income,
                                                               hh_income_own_business_income,
                                                               hh_income_govt_social_benefits,
                                                               hh_income_remittances,
                                                               hh_income_rent_income,
                                                               hh_income_loans_or_family_support,
                                                               hh_income_hum_assistance

        )), na.rm = TRUE),
        
        
           # ##i.wash_watertime
           # i.wash_watertime =  case_when(
           #   wash_watertime == "above_1hr_2hrs"~"above_1hr_2hrs",
           #   wash_watertime == "above_2hrs_below_3hrs"~"above_2hrs_below_3hrs",
           #   wash_watertime == "above_30_below_1hr"~"above_30_below_1hr",
           #   wash_watertime == "above_3hrs"~"above_3hrs",
           #   wash_watertime == "below_30minutes"~"below_30minutes",
           #   wash_watertime == "dk"~"dk",
           #   wash_watertime == "dwta"~"dwta"
           # ),
        int.wash_water_quantity = case_when(wash_water_quantity=="never"~ 0,
                                            wash_water_quantity=="rarely"~1,
                                            wash_water_quantity=="sometimes"~2,
                                            wash_water_quantity %in% c("often","always")~3
                                            
        ),
        int.wash_water_quantity2 = case_when(wash_water_quantity2=="never"~0,
                                             wash_water_quantity2=="rarely"~1,
                                             wash_water_quantity2=="sometimes"~2,
                                             wash_water_quantity2 %in% c("often","always")~3
                                             
        ),
        int.wash_water_quantity3 = case_when(wash_water_quantity3=="never"~0,
                                             wash_water_quantity3=="rarely"~1,
                                             wash_water_quantity3=="sometimes"~2,
                                             wash_water_quantity3 %in% c("often","always")~3
                                             
        ),
        int.wash_water_quantity4 = case_when(wash_water_quantity4=="never"~0,
                                             wash_water_quantity4=="rarely"~1,
                                             wash_water_quantity4=="sometimes"~2,
                                             wash_water_quantity4 %in% c("often","always")~3
                                             
        ),
        int.wash_water_quantity5 = case_when(wash_water_quantity5=="never"~0,
                                             wash_water_quantity5=="rarely"~1,
                                             wash_water_quantity5=="sometimes"~2,
                                             wash_water_quantity5 %in% c("often", "always")~3
                                             
        ),
        int.wash_water_quantity6 = case_when(wash_water_quantity6=="never"~0,
                                             wash_water_quantity6=="rarely"~1,
                                             wash_water_quantity6=="sometimes"~2,
                                             wash_water_quantity6 %in% c("often", "always")~3
                                             
        ),
        int.wash_water_quantity7 = case_when(wash_water_quantity7=="never"~0,
                                             wash_water_quantity7=="rarely"~1,
                                             wash_water_quantity7=="sometimes"~2,
                                             wash_water_quantity7 %in% c("often", "always")~3
                                             
        ),
        int.wash_water_quantity8 = case_when(wash_water_quantity8=="never"~0,
                                             wash_water_quantity8=="rarely"~1,
                                             wash_water_quantity8=="sometimes"~2,
                                             wash_water_quantity8 %in% c("often","always")~3
                                             
        ),
        int.wash_water_quantity9 = case_when(wash_water_quantity9=="never"~0,
                                             wash_water_quantity9=="rarely"~1,
                                             wash_water_quantity9=="sometimes"~2,
                                             wash_water_quantity9 %in% c("often", "always")~3
                                             
        ),
        int.wash_water_quantity10 = case_when(wash_water_quantity10=="never"~0,
                                              wash_water_quantity10=="rarely"~1,
                                              wash_water_quantity10=="sometimes"~2,
                                              wash_water_quantity10 %in% c("often","always")~3
                                              
        ),
        int.wash_water_quantity11 = case_when(wash_water_quantity11=="never"~0,
                                              wash_water_quantity11=="rarely"~1,
                                              wash_water_quantity11=="sometimes"~2,
                                              wash_water_quantity11 %in% c("often","always")~3
                                              
        ),
        int.wash_water_quantity12 = case_when(wash_water_quantity12=="never"~0,
                                              wash_water_quantity12=="rarely"~1,
                                              wash_water_quantity12=="sometimes"~2,
                                              wash_water_quantity12 %in% c("often", "always")~3
                                              
    ),
  ###  wash_water_score      
  wash_water_score = rowSums(across(c( int.wash_water_quantity,
                                           int.wash_water_quantity2,
                                           int.wash_water_quantity3,
                                           int.wash_water_quantity4,
                                           int.wash_water_quantity5,
                                           int.wash_water_quantity6,
                                          int.wash_water_quantity7,
                                           int.wash_water_quantity8,
                                           int.wash_water_quantity9,
                                           int.wash_water_quantity10,
                                           int.wash_water_quantity11,
                                           int.wash_water_quantity12)), na.rm = T),
  ##i.wash_water_secure         
  i.wash_warter_secure = case_when(
      wash_water_score < 12 ~ "water_secure",
      wash_water_score>11~"water_insecure"
    ),
        
           i.fc_matrix = case_when( 
             # 1 - 5
             i.hhs == 0 & i.rcsi < 4 & i.fcs > 35 ~ 1,
             i.hhs == 1 & i.rcsi < 4 & i.fcs > 35 ~ 2,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi < 4 & i.fcs > 35 ~ 3,
             i.hhs == 4 & i.rcsi < 4 & i.fcs > 35 ~ 4,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi < 4 & i.fcs > 35 ~ 5,
             # 6 - 10
             i.hhs == 0 & i.rcsi < 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 6,
             i.hhs == 1 & i.rcsi < 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 7,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi < 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 8,
             i.hhs == 4 & i.rcsi < 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 9,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi < 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 10,
             # 11 - 15
             i.hhs == 0 & i.rcsi < 4 & i.fcs  < 21.5 ~ 11,
             i.hhs == 1 & i.rcsi < 4 & i.fcs  < 21.5 ~ 12,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi < 4 & i.fcs  < 21.5 ~ 13,
             i.hhs == 4 & i.rcsi < 4 & i.fcs  < 21.5 ~ 14,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi < 4 & i.fcs  < 21.5 ~ 15,
             # 16 - 20
             i.hhs == 0 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs > 35 ~ 16,
             i.hhs == 1 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs > 35 ~ 17,
             (i.hhs >= 2 & i.hhs <= 3) & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs > 35 ~ 18,
             i.hhs == 4 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs > 35 ~ 19,
             (i.hhs >= 5 & i.hhs <= 6) & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs > 35 ~ 20,
             # 21 - 25
             i.hhs == 0 & (i.rcsi >= 4 & i.rcsi <= 18) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 21,
             i.hhs == 1 & (i.rcsi >= 4 & i.rcsi <= 18) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 22,
             (i.hhs >= 2 & i.hhs <= 3) & (i.rcsi >= 4 & i.rcsi <= 18) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 23,
             i.hhs == 4 & (i.rcsi >= 4 & i.rcsi <= 18) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 24,
             (i.hhs >= 5 & i.hhs <= 6) & (i.rcsi >= 4 & i.rcsi <= 18) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 25,
             # 26 - 30
             i.hhs == 0 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs  < 21.5 ~ 26,
             i.hhs == 1 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs  < 21.5 ~ 27,
             (i.hhs >= 2 & i.hhs <= 3) & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs  < 21.5 ~ 28,
             i.hhs == 4 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs  < 21.5 ~ 29,
             (i.hhs >= 5 & i.hhs <= 6) & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs  < 21.5 ~ 30,
             # 31 - 35
             i.hhs == 0 & i.rcsi > 18 & i.fcs > 35 ~ 31,
             i.hhs == 1 & i.rcsi > 18 & i.fcs > 35 ~ 32,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi > 18 & i.fcs > 35 ~ 33,
             i.hhs == 4 & i.rcsi > 18 & i.fcs > 35 ~ 34,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi > 18 & i.fcs > 35 ~ 35,
             # 36 - 40
             i.hhs == 0 & i.rcsi > 18 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 36,
             i.hhs == 1 & i.rcsi > 18 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 37,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi > 18 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 38,
             i.hhs == 4 & i.rcsi > 18 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 39,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi > 18 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 40,
             # 41 - 45
             i.hhs == 0 & i.rcsi > 18 & i.fcs  < 21.5 ~ 41,
             i.hhs == 1 & i.rcsi > 18 & i.fcs  < 21.5 ~ 42,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi > 18 & i.fcs  < 21.5 ~ 43,
             i.hhs == 4 & i.rcsi > 18 & i.fcs  < 21.5 ~ 44,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi > 18 & i.fcs  < 21.5 ~ 45),
           i.fc_matrix_cat = case_when(i.fc_matrix %in% c(1, 6) ~ "Phase 1",
                                       i.fc_matrix %in% c(2, 3, 7, 11, 12, 16, 17, 18, 21, 22, 26, 31, 32, 36) ~ "Phase 2",
                                       i.fc_matrix %in% c(4, 5, 8, 9, 13, 19, 20, 23, 24, 27, 28, 33, 34, 37, 38, 41, 42, 43) ~ "Phase 3",
                                       i.fc_matrix %in% c(10, 14, 15, 25, 29, 35, 39, 40, 44) ~ "Phase 4",
                                       i.fc_matrix %in% c(30, 45) ~ "Phase 5"),
           i.fc_matrix_fcs_hhs = case_when( 
             # 1 - 5
             i.hhs == 0 & i.fcs > 35 ~ 1,
             i.hhs == 1 & i.fcs > 35 ~ 2,
             (i.hhs >= 2 & i.hhs <= 3) & i.fcs > 35 ~ 3,
             i.hhs == 4 & i.fcs > 35 ~ 4,
             (i.hhs >= 5 & i.hhs <= 6) & i.fcs > 35 ~ 5,
             # 6 - 10
             i.hhs == 0 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 6,
             i.hhs == 1 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 7,
             (i.hhs >= 2 & i.hhs <= 3) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 8,
             i.hhs == 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 9,
             (i.hhs >= 5 & i.hhs <= 6) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 10,
             # 11 - 15
             i.hhs == 0 & i.fcs  < 21.5 ~ 11,
             i.hhs == 1 & i.fcs  < 21.5 ~ 12,
             (i.hhs >= 2 & i.hhs <= 3) & i.fcs  < 21.5 ~ 13,
             i.hhs == 4 & i.fcs  < 21.5 ~ 14,
             (i.hhs >= 5 & i.hhs <= 6) & i.fcs  < 21.5 ~ 15,
             # 16 - 20
             i.hhs == 0  & i.fcs > 35 ~ 16,
             i.hhs == 1  & i.fcs > 35 ~ 17,
             (i.hhs >= 2 & i.hhs <= 3)  & i.fcs > 35 ~ 18,
             i.hhs == 4  & i.fcs > 35 ~ 19,
             (i.hhs >= 5 & i.hhs <= 6)  & i.fcs > 35 ~ 20,
             # 21 - 25
             i.hhs == 0  & (i.fcs >= 21.5 & i.fcs <= 35) ~ 21,
             i.hhs == 1  & (i.fcs >= 21.5 & i.fcs <= 35) ~ 22,
             (i.hhs >= 2 & i.hhs <= 3)  & (i.fcs >= 21.5 & i.fcs <= 35) ~ 23,
             i.hhs == 4  & (i.fcs >= 21.5 & i.fcs <= 35) ~ 24,
             (i.hhs >= 5 & i.hhs <= 6)  & (i.fcs >= 21.5 & i.fcs <= 35) ~ 25,
             # 26 - 30
             i.hhs == 0  & i.fcs  < 21.5 ~ 26,
             i.hhs == 1  & i.fcs  < 21.5 ~ 27,
             (i.hhs >= 2 & i.hhs <= 3)  & i.fcs  < 21.5 ~ 28,
             i.hhs == 4  & i.fcs  < 21.5 ~ 29,
             (i.hhs >= 5 & i.hhs <= 6)  & i.fcs  < 21.5 ~ 30,
             # 31 - 35
             i.hhs == 0 & i.fcs > 35 ~ 31,
             i.hhs == 1 & i.fcs > 35 ~ 32,
             (i.hhs >= 2 & i.hhs <= 3) & i.fcs > 35 ~ 33,
             i.hhs == 4 & i.fcs > 35 ~ 34,
             (i.hhs >= 5 & i.hhs <= 6) & i.fcs > 35 ~ 35,
             # 36 - 40
             i.hhs == 0 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 36,
             i.hhs == 1 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 37,
             (i.hhs >= 2 & i.hhs <= 3) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 38,
             i.hhs == 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 39,
             (i.hhs >= 5 & i.hhs <= 6) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 40,
             # 41 - 45
             i.hhs == 0 & i.fcs  < 21.5 ~ 41,
             i.hhs == 1 & i.fcs  < 21.5 ~ 42,
             (i.hhs >= 2 & i.hhs <= 3) & i.fcs  < 21.5 ~ 43,
             i.hhs == 4 & i.fcs  < 21.5 ~ 44,
             (i.hhs >= 5 & i.hhs <= 6) & i.fcs  < 21.5 ~ 45),
           i.fc_matrix_fcs_hhs = case_when(i.fc_matrix_fcs_hhs %in% c(1, 6) ~ "Phase 1",
                                           i.fc_matrix_fcs_hhs %in% c(2, 3, 7, 11, 12, 16, 17, 18, 21, 22, 26, 31, 32, 36) ~ "Phase 2",
                                           i.fc_matrix_fcs_hhs %in% c(4, 5, 8, 9, 13, 19, 20, 23, 24, 27, 28, 33, 34, 37, 38, 41, 42, 43) ~ "Phase 3",
                                           i.fc_matrix_fcs_hhs %in% c(10, 14, 15, 25, 29, 35, 39, 40, 44) ~ "Phase 4",
                                           i.fc_matrix_fcs_hhs %in% c(30, 45) ~ "Phase 5"),
  
  #flag_lcsi_liv_livestock - LCSI coherence between the use of livestock-related coping mechanisms and livelihoods systems of the HH can be further reviewed if not already.
  
  liv_stress_4 = ifelse(hh_own_livestock2=="no"&liv_stress_4 == "yes","not_applicable", liv_stress_4),
  liv_emergency_2 = ifelse(hh_own_livestock2 =="no"&liv_emergency_2=="yes","not_applicable",liv_emergency_2),
  liv_crisis_1 = ifelse(hh_plant_crops=="did_not_plant"&liv_crisis_1=="yes", "not_applicable",liv_crisis_1),
  liv_emergency_1 = ifelse(hh_member_currently_notliving_with_family =="none"&liv_emergency_1=="yes","not_applicable", liv_emergency_1),
  
  ##flag_lcsi_coherence - LCSI entries where only emergency OR crisis coping are reported and all the other options are “no” or “NA”
  #liv_stress_1
  liv_stress_1 = ifelse(
    (liv_stress_1 %in% c("no_had_no_need", "not_applicable")| 
      liv_stress_2 %in% c("no_had_no_need", "not_applicable")|
     liv_stress_3 %in% c("no_had_no_need", "not_applicable")|
     liv_stress_4 %in% c("no_had_no_need", "not_applicable"))&
     ((liv_crisis_1 %in% c("yes", "no_exhausted")|liv_crisis_2 %in% c("yes", "no_exhausted")|liv_crisis_3 %in% c("yes", "no_exhausted")|
        liv_emergency_1 %in% c("yes", "no_exhausted")|liv_emergency_2 %in% c("yes", "no_exhausted")|liv_emergency_3 %in% c("yes", "no_exhausted"))),"not_applicable",liv_stress_1),
  
  # liv_stress_2
  liv_stress_2 = ifelse(
    (liv_stress_1 %in% c("no_had_no_need", "not_applicable")| 
       liv_stress_2 %in% c("no_had_no_need", "not_applicable")|
       liv_stress_3 %in% c("no_had_no_need", "not_applicable")|
       liv_stress_4 %in% c("no_had_no_need", "not_applicable"))&
       ((liv_crisis_1 %in% c("yes", "no_exhausted")|liv_crisis_2 %in% c("yes", "no_exhausted")|liv_crisis_3 %in% c("yes", "no_exhausted")|
          liv_emergency_1 %in% c("yes", "no_exhausted")|liv_emergency_2 %in% c("yes", "no_exhausted")|liv_emergency_3 %in% c("yes", "no_exhausted"))),"not_applicable",liv_stress_2),
  
  #liv_stress_3
  liv_stress_3 = ifelse(
    (liv_stress_1 %in% c("no_had_no_need", "not_applicable")| 
       liv_stress_2 %in% c("no_had_no_need", "not_applicable")|
       liv_stress_3 %in% c("no_had_no_need", "not_applicable")|
       liv_stress_4 %in% c("no_had_no_need", "not_applicable"))&
       ((liv_crisis_1 %in% c("yes", "no_exhausted")|liv_crisis_2 %in% c("yes", "no_exhausted")|liv_crisis_3 %in% c("yes", "no_exhausted")|
          liv_emergency_1 %in% c("yes", "no_exhausted")|liv_emergency_2 %in% c("yes", "no_exhausted")|liv_emergency_3 %in% c("yes", "no_exhausted"))),"not_applicable",liv_stress_3),
  
  #liv_stress_4
  liv_stress_4 = ifelse(
    (liv_stress_1 %in% c("no_had_no_need", "not_applicable")| 
       liv_stress_2 %in% c("no_had_no_need", "not_applicable")|
       liv_stress_3 %in% c("no_had_no_need", "not_applicable")|
       liv_stress_4 %in% c("no_had_no_need", "not_applicable"))&
       ((liv_crisis_1 %in% c("yes", "no_exhausted")|liv_crisis_2 %in% c("yes", "no_exhausted")|liv_crisis_3 %in% c("yes", "no_exhausted")|
          liv_emergency_1 %in% c("yes", "no_exhausted")|liv_emergency_2 %in% c("yes", "no_exhausted")|liv_emergency_3 %in% c("yes", "no_exhausted"))),"not_applicable",liv_stress_4),
  
  # liv_crisis_1
  liv_crisis_1 = ifelse(
    (liv_crisis_1 %in% c("no_had_no_need", "not_applicable")| 
       liv_crisis_2 %in% c("no_had_no_need", "not_applicable")|
       liv_crisis_3 %in% c("no_had_no_need", "not_applicable"))&
      ((liv_emergency_1 %in% c("yes", "no_exhausted")|liv_emergency_2 %in% c("yes", "no_exhausted")|liv_emergency_3 %in% c("yes", "no_exhausted"))),"not_applicable",liv_crisis_1),
  
  # liv_crisis_2
  liv_crisis_1 = ifelse(
    (liv_crisis_1 %in% c("no_had_no_need", "not_applicable")| 
       liv_crisis_2 %in% c("no_had_no_need", "not_applicable")|
       liv_crisis_3 %in% c("no_had_no_need", "not_applicable"))&
      ((liv_emergency_1 %in% c("yes", "no_exhausted")|liv_emergency_2 %in% c("yes", "no_exhausted")|liv_emergency_3 %in% c("yes", "no_exhausted"))),"not_applicable",liv_crisis_2),
  
  # liv_crisis_3
  liv_crisis_1 = ifelse(
    (liv_crisis_1 %in% c("no_had_no_need", "not_applicable")| 
       liv_crisis_2 %in% c("no_had_no_need", "not_applicable")|
       liv_crisis_3 %in% c("no_had_no_need", "not_applicable"))&
      ((liv_emergency_1 %in% c("yes", "no_exhausted")|liv_emergency_2 %in% c("yes", "no_exhausted")|liv_emergency_3 %in% c("yes", "no_exhausted"))),"not_applicable",liv_crisis_3),
  
  
  )|>
    ##entries where all answers are ‘NA’ can be removed for this indicator only – as it would indicate some misunderstanding at some stage of the data collection. flag_lcsi_na - LCSI
    # across(.cols = starts_with("liv_."), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .))|>
    addindicators::add_lcsi(lcsi_stress_vars = c("liv_stress_1", "liv_stress_2", "liv_stress_3", "liv_stress_4"),
                            lcsi_crisis_vars = c("liv_crisis_1", "liv_crisis_2", "liv_crisis_3"),
                            lcsi_emergency_vars = c("liv_emergency_1", "liv_emergency_2", "liv_emergency_3"),
                            yes_val = "yes",
                            no_val = "no_had_no_need",
                            exhausted_val = "no_exhausted",
                            not_applicable_val = "not_applicable") |> 
     rename(i.lcsi_cat = lcsi_cat ) |> 
    select(-c(starts_with("int.")))
}




###############################################################################