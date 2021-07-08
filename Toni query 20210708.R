#Toni Breakdown of URT Day 1 samples by sample type and Day of Onset
#by Elen Vink
#20210708

library(readr)
library(tidyverse)
library(readxl)

#Import data

lims_data_URT <- read_csv("~/Ultra/lims_data_20210614_URT.csv")
oneline_severity <- read_csv("~/Ultra/oneline_severity_someupdatedfio2_20210527.csv")
covid_neg_list <- read_excel("~/NGS Serology/Data/Updated severity score/Newest Severity SCore/covid_neg_manual_list_elen_20210525_v2.xlsx")

#Add collection DoS to LIMS data

oneline_severity_DoS <- oneline_severity %>% 
  select('subjid', dsstdat, cestdat, hostdat)

lims_data <-  lims_data_URT %>% 
  left_join(oneline_severity_DoS, by = c('Patient_ID' = 'subjid'))

###Add column - specimen collection Day of symptoms etc

lims_data$sample_collection_DoSymptoms <- (as.Date(as.character(lims_data$Date_Collected), format="%Y%m%d")) - as.Date(as.character(lims_data$cestdat), format="%d/%m/%Y")
lims_data$sample_collection_DoAdmission <- (as.Date(as.character(lims_data$Date_Collected), format="%Y%m%d")) - as.Date(as.character(lims_data$hostdat), format="%d/%m/%Y")
lims_data$sample_collection_DoEnrolment <- (as.Date(as.character(lims_data$Date_Collected), format="%Y%m%d")) - as.Date(as.character(lims_data$dsstdat), format="%d/%m/%Y")

####Replace negative values with NA as incorrect

lims_data$sample_collection_DoSymptoms <- replace(lims_data$sample_collection_DoSymptoms, which(lims_data$sample_collection_DoSymptoms < 0), NA)
lims_data$sample_collection_DoAdmission <- replace(lims_data$sample_collection_DoAdmission, which(lims_data$sample_collection_DoAdmission < 0), NA)
lims_data$sample_collection_DoEnrolment <- replace(lims_data$sample_collection_DoEnrolment, which(lims_data$sample_collection_DoSymptoms < 0), NA)

#Add covid status

lims_data <-  lims_data %>% 
  left_join(covid_neg_list, by = c('Patient_ID' = 'subjid'))

lims_data_distinct <- lims_data %>% 
  select('Patient_ID') %>% 
  distinct()

#Filter by Day 1 samples

lims_data_filtered <- lims_data %>% 
  filter(Timepoint == "Day 1" | Timepoint == "Recruitment" | Timepoint == "Recruitment / Day 1" | Timepoint == "Unknown" | Timepoint == "Not Recorded")

#For unknowns select by corrected study day 0-1
lims_data_filtered <- subset(lims_data_filtered, !((Timepoint == "Unknown") & (sample_collection_DoEnrolment >1)))
lims_data_filtered <- subset(lims_data_filtered, !((Timepoint == "Not Recorded") & (sample_collection_DoEnrolment >1)))  

#Filter by Day 1 or Day 3 samples
#lims_data_filtered <- lims_data %>% 
  #filter(Timepoint == "Day 1" | Timepoint == "Recruitment" | Timepoint == "Recruitment / Day 1" | Timepoint == "Day 3" | Timepoint == "Unknown" | Timepoint == "Not Recorded")

#For unknowns select by corrected study day 0-4
#lims_data_filtered <- subset(lims_data_filtered, !((Timepoint == "Unknown") & (sample_collection_DoEnrolment >4)))
#lims_data_filtered <- subset(lims_data_filtered, !((Timepoint == "Not Recorded") & (sample_collection_DoEnrolment >4)))  

#Remove duplicates i.e 2 x day 1 throat sample per patient

lims_data_filtered_2 <- lims_data_filtered %>% 
  select(Patient_ID, Kit_ID, Timepoint, Sample_Type) %>% 
  distinct()

#Export to Excel to sort sample type labelling

write_csv(lims_data_filtered_2, '~/Viral Coinfection/Sample Lists/Toni query/URT_Day1_Samples_20210708.csv')

#Import back

lims_data_filtered_2_update <- read_excel('~/Viral Coinfection/Sample Lists/Toni query/URT_Day1_Samples_20210708ev.xlsx') %>% 
  select(Patient_ID, Kit_ID, Timepoint, Sample_Type_Updated) %>% 
  distinct()

lims_data_filtered_3 <- lims_data_filtered_2_update %>% 
  select(Patient_ID, Timepoint, Sample_Type_Updated) %>% 
  distinct()

#Count Sample Types available

Sample_Type <- as.data.frame(count(lims_data_filtered_3, vars = Sample_Type_Updated))


###Export to Excel for final tidy up!

write_csv(Sample_Type, '~/Viral Coinfection/Sample Lists/Toni query/URT_Day1_Sample_Type_20210708.csv')

##Samples sorted by Day from symptom onset

lims_data_filtered_4 <- lims_data_filtered_2_update %>% 
  
  
  ##left join DoS and then count
  select(Patient_ID, Kit_ID, Timepoint, Sample_Type, sample_collection_DoSymptoms) %>% 
  distinct()