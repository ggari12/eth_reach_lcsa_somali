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
    dplyr::mutate(int.fcs = sum(c_across(int.fcs_cereals_tubers:int.fcs_oils)),
           int.rcsi = sum(c_across(int.rCSILessQlty:int.rCSIMealNb)),
           int.hhs = sum(c_across(int.freq_no_food_lack_resources:int.freq_day_and_night_no_food)),
           i.hh_size = case_when(hh_size <= 3 ~ "between_1_and_3_members",
                                 hh_size <= 6 ~ "between_4_and_6_members",
                                 hh_size <= 9 ~ "between_7_and_9_members",
                                 hh_size >= 10 ~ "10_or_more_members"),
           i.respondent_age = case_when(respondent_age <= 24 ~ "age_18_24",
                                        respondent_age <= 39 ~ "age_25_39",
                                        respondent_age <= 59 ~ "age_40_59",
                                        respondent_age > 59 ~ "age_60+"),
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
       
           ##in.wash_water_quantity
           int.wash_water_quantity = case_when(wash_water_quantity=="never"~0,
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
           ##wash_water_score
           wash_water_score = sum(c_across(int.wash_water_quantity:int.wash_water_quantity12), na.rm = T),
           
           ##i.wash_water_secure
           i.wash_warter_secure = case_when(
                                            wash_water_score < 11 ~ "water_secure",
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
    ) |> 
    addindicators::add_lcsi(lcsi_stress_vars = c("liv_stress_1", "liv_stress_2", "liv_stress_3", "liv_stress_4"),
                            lcsi_crisis_vars = c("liv_crisis_1", "liv_crisis_2", "liv_crisis_3"),
                            lcsi_emergency_vars = c("liv_emergency_1", "liv_emergency_2", "liv_emergency_3"),
                            yes_val = "yes",
                            no_val = "no_had_no_need",
                            exhausted_val = "no_exhausted",
                            not_applicable_val = "not_applicable") |> 
    # rename(i.lcsi_cat = lcsi_cat ) |> 
    select(-c(starts_with("int.")))
}

create_composite_indicators_roster <- function(input_df) {
  input_df |> 
    dplyr::mutate(i.individual_age_cat = case_when(
      ind_age<18~"children_17-",
      ind_age>=18~"adults_18+"
    ),
    i.individual_genre_cat=case_when(
      ind_age<13 & ind_gender =="female"~"women aged 12 and under",
      ind_age>=13 & ind_gender =="female"~"woman aged 13 over"
    ),
   i.individual_age_school_cat = case_when(
     ind_age<6~"NA",
      ind_age>=6 &ind_age<18 ~ "school age children 6-17",
      ind_age>18 ~ "other age"
    ),
   i.individual_genre_age_cat = case_when(
      ind_age>=6 & ind_age<18 & ind_gender =="female"~"girl school 6-17",
      ind_age>=6 & ind_age <18 & ind_gender == "male"~"boy school 6-17",
      
    )
    )
}


###############################################################################