#Identifying Positive PCR results - ISARIC URT Resp Viral PCR
#EHV
#20210928

library(readr)
library(tidyverse)
library(readxl)

#Import data

URTsamples <- read_excel("~/Viral Coinfection/ESWI Abstract/Scottish URT Extraction PCR 20210928.xlsx") %>% 
  select(1,6,10)

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

Sept10am <- read_excel("~/Viral Coinfection/PCR Results/20210910am_allRUTs_data_updated_v2.xls") %>% 
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

Sept17 <- read_excel("~/Viral Coinfection/PCR Results/20210917allRUTs_data_all_repeatedRUT3_20210929.xls") %>% 
  select(2,3,4,7)

Sept17$PCR_date <- 'Sept17'

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

PCR_run <- rbind(Sept7, Sept8, Sept9am, Sept9pm, Sept10am, Sept10pm, Sept14am, Sept14pm, Sept16, Sept17, Sept22am, Sept22pm, Sept27)

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

#Add Patient ID

PCR_run$`Sample Name` <- as.numeric(PCR_run$`Sample Name`)

PCR_run_id <- PCR_run %>% 
  left_join(URTsamples, by = c('Sample Name' = 'Sample Barcode'))

#PCR_run <- PCR_run %>% 
  #sort(as.numeric('CT', FALSE))

#Adding oneline data

URTsamples_data_updated <- read_csv("~/Viral Coinfection/ESWI Abstract/URTsamples_data_20210928_update.csv") 

URTsamples_data_update <- URTsamples %>% 
  left_join(URTsamples_data_updated, by = 'Patient ID')

summary(URTsamples_data_update$calc_age)

Gender_table_1 <- table(URTsamples_data_update$sex)
prop.table(Gender_table_1)

#write_csv(URTsamples_data_update, "~/Viral Coinfection/ESWI Abstract/URTsamples_data_update_20210929.csv")

PCR_run_id_data <- PCR_run_id %>% 
  left_join(URTsamples_data_update, by = 'Patient ID')

PCR_run_id_data_severity <- PCR_run_id_data %>% 
  select('Target Name', 'CT', 'severity')

severity_table_1 <- table(URTsamples_data_update$severity)
severity_table_1
prop.table(severity_table_1)


severity_table <- table(PCR_run_id_data_severity$severity)
severity_table
prop.table(severity_table)




PCR_run_id_data_age <- PCR_run_id_data %>% 
  select('Target Name', 'CT', 'calc_age')

summary(PCR_run_id_data_age$calc_age)

Gender_table <- table(PCR_run_id_data$sex)
prop.table(Gender_table)

Immsupp_med_20210730 <- read_excel("~/NGS Serology/Data/Updated severity score/Newest Severity SCore/Immsupp_med_20210730.xlsx")

PCR_run_id_data <- PCR_run_id_data %>% 
  left_join(Immsupp_med_20210730, by = c('Patient ID' = 'updated_patient_id'))

immsupp_table <- table(PCR_run_id_data$immunosupp_med_updated)
immsupp_table
prop.table(immsupp_table)

# Sample day of symptoms

URTsamples_data_update$sample_collection_DoSymptoms <- (as.Date(as.character(URTsamples_data_update$'Date Taken.x'), format="%d/%m/%Y")) - as.Date(as.character(URTsamples_data_update$cestdat), format="%d/%m/%Y")

####Replace negative values with NA as incorrect

URTsamples_data_update$sample_collection_DoSymptoms <- replace(URTsamples_data_update$sample_collection_DoSymptoms, which(URTsamples_data_update$sample_collection_DoSymptoms < 0), NA)

library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

URTsamples_data_update$sample_collection_DoSymptoms_number <- numextract(URTsamples_data_update$sample_collection_DoSymptoms)

URTsamples_data_update$sample_collection_DoSymptoms_number <- as.numeric(URTsamples_data_update$sample_collection_DoSymptoms_number)

summary(URTsamples_data_update$sample_collection_DoSymptoms_number)

# Sample day of admission

URTsamples_data_update$sample_collection_DoAdmission <- (as.Date(as.character(URTsamples_data_update$'Date Taken.x'), format="%d/%m/%Y")) - as.Date(as.character(URTsamples_data_update$hostdat), format="%d/%m/%Y")

####Replace negative values with NA as incorrect

URTsamples_data_update$sample_collection_DoAdmission <- replace(URTsamples_data_update$sample_collection_DoAdmission, which(URTsamples_data_update$sample_collection_DoAdmission < 0), NA)

library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

URTsamples_data_update$sample_collection_DoAdmission_number <- numextract(URTsamples_data_update$sample_collection_DoAdmission)
URTsamples_data_update$sample_collection_DoAdmission_number <- as.numeric(URTsamples_data_update$sample_collection_DoAdmission_number)


summary(URTsamples_data_update$sample_collection_DoAdmission_number)


URTsamples_data_update_2 <- URTsamples_data_update %>% 
  select(`Patient ID`, 'sample_collection_DoAdmission_number')

#Number of people in cohort needing mechanical ventilation vs positive patients

imv_table <- table(URTsamples_data_update$any_invasive)
imv_table
prop.table(imv_table)

imv_pos_table <- table(PCR_run_id_data$any_invasive)
imv_pos_table
prop.table(imv_pos_table)

#Number of people in cohort dying vs positive patients

death_table <- table(URTsamples_data_update$dsterm)
death_table
prop.table(death_table)

death_pos_table <- table(PCR_run_id_data$dsterm)
death_pos_table
prop.table(death_pos_table)

#Length of stay neg vs pos for those alive

#add column of positive or negative to full cohort

PCR_run_id_pos_result <- PCR_run_id %>% 
  select(2,4,6)

PCR_run_id_pos_result$PCR_result <- 'Pos'

URTsamples_data_update_posneg <- URTsamples_data_update %>% 
  left_join(PCR_run_id_pos_result, by = 'Patient ID')

#filter by discharged alive

URTsamples_data_update_posneg <- URTsamples_data_update_posneg %>% 
  filter(dsterm == 'Discharged alive')

#calculate LOS

URTsamples_data_update_posneg$LoS <-  (as.Date(as.character(URTsamples_data_update_posneg$dsstdtc), format="%d/%m/%Y")) - as.Date(as.character(URTsamples_data_update_posneg$dsstdat), format="%d/%m/%Y")

####Replace negative values with NA as incorrect

URTsamples_data_update_posneg$LoS <- replace(URTsamples_data_update_posneg$LoS, which(URTsamples_data_update_posneg$LoS < 0), NA)

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

URTsamples_data_update_posneg$LoS_number <- numextract(URTsamples_data_update_posneg$LoS)

URTsamples_data_update_posneg$LoS_number <- as.numeric(URTsamples_data_update_posneg$LoS_number)

#Divide into pos neg and compare average LOS

URTsamples_data_update_pos <- URTsamples_data_update_posneg %>% 
  filter(PCR_result == 'Pos')

URTsamples_data_update_neg <- URTsamples_data_update_posneg %>% 
  filter(is.na(PCR_result == TRUE))

summary(URTsamples_data_update_pos$LoS_number)

summary(URTsamples_data_update_neg$LoS_number)
