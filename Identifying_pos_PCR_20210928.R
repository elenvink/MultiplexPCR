#Identifying Positive PCR results - ISARIC URT Resp Viral PCR
#EHV
#20210928

library(readr)
library(tidyverse)
library(readxl)

#Import data

Sept7 <- read_excel("~/Viral Coinfection/PCR Results/20210907allRUTs_data_update.xls") %>% 
  select(2,3,4,7)

Sept7$PCR_date <- 'Sept7'

Sept8 <- read_excel("~/Viral Coinfection/PCR Results/20210908allRUTS_data_updated.xls") %>% 
  select(2,3,4,7)

Sept8$PCR_date <- 'Sept8'

Sept9am <- read_excel("~/Viral Coinfection/PCR Results/20210909am_allRUTs_data_updated.xls") %>% 
  select(2,3,4,7)

Sept9am$PCR_date <- 'Sept9am'

Sept9pm <- read_excel("~/Viral Coinfection/PCR Results/20210909pm_allRUTs_data_updated.xls") %>% 
  select(2,3,4,7)

Sept9pm$PCR_date <- 'Sept9pm'

Sept10am <- read_excel("~/Viral Coinfection/PCR Results/20210910am_allRUTs_data_updated.xls") %>% 
  select(2,3,4,7)

Sept10am$PCR_date <- 'Sept10am'

Sept10pm <- read_excel("~/Viral Coinfection/PCR Results/20210910pm_allRUTs_data_updated.xls") %>% 
  select(2,3,4,7)

Sept10pm$PCR_date <- 'Sept10pm'

Sept14am <- read_excel("~/Viral Coinfection/PCR Results/20210914am_allRUTs_data_all.xls") %>% 
  select(2,3,4,7)

Sept14am$PCR_date <- 'Sept14am'

Sept14pm <- read_excel("~/Viral Coinfection/PCR Results/20210914pm_allRUTs_data_all.xls") %>% 
  select(2,3,4,7)

Sept14pm$PCR_date <- 'Sept14pm'

Sept16 <- read_excel("~/Viral Coinfection/PCR Results/20210916allRUTs_data_all.xls") %>% 
  select(2,3,4,7)

Sept16$PCR_date <- 'Sept16'

Sept22am <- read_excel("~/Viral Coinfection/PCR Results/20210922am_allRUTs_data_all.xls") %>% 
  select(2,3,4,7)

Sept22am$PCR_date <- 'Sept22am'

Sept22pm <- read_excel("~/Viral Coinfection/PCR Results/20210922pm_allRUTs_data_all.xls") %>% 
  select(2,3,4,7)

Sept22pm$PCR_date <- 'Sept22pm'

Sept27 <- read_excel("~/Viral Coinfection/PCR Results/20210927_allRUTs_data.xls") %>% 
  select(2,3,4,7)

Sept27$PCR_date <- 'Sept27'


#Combine datasheets

PCR_run <- rbind(Sept7, Sept8, Sept9am, Sept9pm, Sept10am, Sept10pm, Sept14am, Sept14pm, Sept16, Sept22am, Sept22pm, Sept27)

names(PCR_run)[4] <- 'CT'

#Filter for positive sample results
  
PCR_run <- PCR_run %>% 
  filter(str_detect(PCR_run$`Target Name`,'IC EV') == FALSE) 

PCR_run <- PCR_run %>% 
  filter(str_detect(PCR_run$`Target Name`,'IC') == FALSE) 

PCR_run <- PCR_run %>% 
  filter(str_starts(PCR_run$`Sample Name`, 'p') == FALSE)

PCR_run <- PCR_run %>% 
  filter(str_starts(PCR_run$`Sample Name`, 'P') == FALSE) 

PCR_run <- PCR_run %>% 
  filter(str_detect(PCR_run$'CT', 'No PP') == FALSE)

PCR_run <- PCR_run %>% 
  filter(str_detect(PCR_run$`CT`, 'Undetermined') == FALSE)

PCR_run <- PCR_run %>% 
  filter(str_detect(PCR_run$`Task`, 'Neg') == FALSE)

#PCR_run <- PCR_run %>% 
  #sort(as.numeric('CT', FALSE))
         
         