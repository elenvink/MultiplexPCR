#PCR results data wrangling and analysis
#EHV
#20210831

library(readr)
library(tidyverse)
library(readxl)

#Import data

master_file <- read_excel("~/Viral Coinfection/Sample Lists/Sample Release/Glasgow Interim Release File_Elen Vink_June2021.xlsx") %>% 
  select(1:3,5,9)

#Need to delete title rows from Excel export sheet from ABI before 

SMSB_am <- read_excel("~/Viral Coinfection/PCR Results/Trial Run/20210827_trial_SMSBam_data.xls") %>% 
  select(1:3,5,7,13) %>% order('Target Name', 'Well')


HWB_pm <- read_excel("~/Viral Coinfection/PCR Results/Trial Run/20210827_trial_HWB_pm_data.xls")
hwb_am <- read_excel("~/Viral Coinfection/PCR Results/Trial Run/20210827_trial_hwb_am.xls")
