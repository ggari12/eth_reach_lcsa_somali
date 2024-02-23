###############################################################################
rm(list = ls())
library(tidyverse)
library(srvyr)
library(supporteR) 



data_path1 <- "outputs/20240202_clean_data_eth_lcsa_somali.xlsx"
data_path2 <- "outputs/20240222_clean_data_eth_lcsa_somali.xlsx"

data_nms1 <- names(readxl::read_excel(path = data_path1, n_max = 3000, sheet = "cleaned_main_data"))
c_types1 <- ifelse(str_detect(string = data_nms1, pattern = "_other$"), "text", "guess")

data_nms2 <- names(readxl::read_excel(path = data_path2, n_max = 3000, sheet = "cleaned_main_data"))
c_types2 <- ifelse(str_detect(string = data_nms2, pattern = "_other$"), "text", "guess")


df_main_clean_data1 <- readxl::read_excel(path = data_path1, sheet = "cleaned_main_data", col_types = c_types1, na = "NA") 
df_main_clean_data2 <- readxl::read_excel(path = data_path2, sheet = "cleaned_main_data", col_types = c_types2, na = "NA") 


col1<- names(df_main_clean_data1)
col2 <- names(df_main_clean_data2)

diff_col1 <- df_main_clean_data2|>
  select(-all_of(col1))
names(diff_col1)  

