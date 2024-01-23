rm(list = ls())
library(tidyverse)
library(srvyr)
library(fastDummies)
library(healthyr)

getwd()

# Load and process household data
df <- readxl::read_xlsx("inputs/clean_data_eth_lcsa_somali.xlsx", sheet = "cleaned_main_data")

table(df$hh_zone)

df2 <- df %>%
  dplyr::group_by(hh_zone, pop_group) %>%
  dplyr::summarise(num_households_data = n())

table(df2$pop_group)


# load and process sampling frame

sampling_frame_host <- readxl::read_xlsx("inputs/20231030_sample_host_idp_initial + replacement_FINAL.xlsx", sheet = "sample_host")|>
  select("Pcod", "pop_numbers","Type")

sampling_frame_idp <- readxl::read_xlsx("inputs/20231030_sample_host_idp_initial + replacement_FINAL.xlsx", sheet = "sample_idp")|>
  select("Pcod", "pop_numbers","type")

colnames(sampling_frame_host) <- colnames(sampling_frame_idp)

sampling_frame <- rbind(sampling_frame_host,sampling_frame_idp)

names(sampling_frame)

sampling_frame_2 <- sampling_frame |>
  dplyr::group_by(Pcod,type)|>
  dplyr::summarise(num_households_sf = sum(pop_numbers, na.rm = TRUE))|>
  dplyr::mutate(type = dplyr::case_when(
    type == "Host" ~ "host",
    type == "idp"~"idp"))|>
  dplyr::rename(hh_zone = Pcod,
                pop_group = type)

weights <- sampling_frame_2 %>% dplyr::left_join(df2, by = c("hh_zone", "pop_group")) |>
  dplyr::mutate(
    prop_sf = num_households_sf / sum(num_households_sf, na.rm = TRUE),
    prop_data = num_households_data / sum(num_households_data, na.rm = TRUE),
    pw = (prop_sf / prop_data))

writexl::write_xlsx(weights, "inputs/data_eth_lcsa_somali_weighted.xlsx")
