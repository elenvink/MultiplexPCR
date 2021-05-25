#Checking COVID status
#by Elen Vink 25/5/2021

#Import ccp data

library(readr)
library(tidyverse)


ccp_data_20210419 <- read_csv("F:/ISARIC files/original data/Ultra download/ccp_data_20210419.csv")
oneline_20210419 <- read_csv("F:/ISARIC files/original data/Ultra download/oneline_20210419.csv")

covid_test_results <-  ccp_data_20210419 %>% 
  select('subjid', 'corna_mbcat', 'corna_mbcaty', 'mbdat', 'mbspec', 'other_mbspec', 'mbmethod', 'other_mbmethod', 'mborres', 'mbtestcd')


possible_covid_neg <- oneline_20210419 %>%
  select('subjid', 'corna_mbcat', 'corna_mbcaty') %>% 
  filter(corna_mbcat == "NO" | is.na(corna_mbcat)) %>% 
  select(subjid) %>% 
  left_join(covid_test_results, by = "subjid") 
  
check_covid_status <-  possible_covid_neg %>%   
  filter(mborres == "Negative")

#Export list to Excel and update manually

poss_covid_neg_excel <- possible_covid_neg %>% 
  select(subjid) %>% 
  distinct()

write_csv(poss_covid_neg_excel, "~/NGS Serology/Data/Updated severity score/Newest Severity SCore/poss_covid_neg_excel_20210525.csv")


