#Sample Selection for URT Multiplex PCR
#by Elen Vink
#20210525

#Exclusion = covid neg, nosocomial onset
#Prioritise - earliest samples by symptoms, earliest samples per patient, phipseq patients - MicroScan, severity score NA

library(readr)
library(tidyverse)
library(readxl)

#Import data

lims_data_URT <- read_csv("~/Ultra/lims_data_20210419_URT.csv")
oneline_severity <- read_csv("~/Ultra/oneline_severity_someupdatedfio2_20210527.csv")
covid_neg_list <- read_excel("~/NGS Serology/Data/Updated severity score/Newest Severity SCore/covid_neg_manual_list_elen_20210525_v2.xlsx")

#Add covid status and severity score to LIMS data

oneline_severity_DoS <- oneline_severity %>% 
  select('subjid', dsstdat, cestdat, hostdat, outcome_DoAdmission, outcome_DoSymptoms, symptom_onset_DoAdmission, enrolment_DoSymptoms, enrolment_DoAdmission, 'severity')

lims_data <-  lims_data_URT %>% 
  left_join(oneline_severity_DoS, by = c('Patient_ID' = 'subjid'))

lims_data <-  lims_data %>% 
  left_join(covid_neg_list, by = c('Patient_ID' = 'subjid'))

#Add column for whether throat swab available - TRUE/FALSE (FALSE means nose or unknown site)

lims_data$throat_swab <- str_detect(lims_data$Sample_Type, '(?i)throat') | str_detect(lims_data$Sample_Type, '(?i)thoat') 

#Add column for those on full MicroScan list - d1/3&conv plus day1/3&9/conv





#Exclusion criteria
##Exclude those who are covid neg

lims_data_filtered <- lims_data %>% 
  filter(is.na(`Covid status`) == TRUE)

## Exclude nosocomial onset
###Add column - specimen collection Day of symptoms

lims_data_filtered$sample_collection_DoSymptoms <- (as.Date(as.character(lims_data_filtered$Date_Collected), format="%Y%m%d")) - as.Date(as.character(lims_data_filtered$cestdat), format="%d/%m/%Y")
lims_data_filtered$sample_collection_DoAdmission <- (as.Date(as.character(lims_data_filtered$Date_Collected), format="%Y%m%d")) - as.Date(as.character(lims_data_filtered$hostdat), format="%d/%m/%Y")
lims_data_filtered$sample_collection_DoEnrolment <- (as.Date(as.character(lims_data_filtered$Date_Collected), format="%Y%m%d")) - as.Date(as.character(lims_data_filtered$dsstdat), format="%d/%m/%Y")

####Replace negative values with NA as incorrect

lims_data_filtered$sample_collection_DoSymptoms <- replace(lims_data_filtered$sample_collection_DoSymptoms, which(lims_data_filtered$sample_collection_DoSymptoms < 0), NA)
lims_data_filtered$sample_collection_DoAdmission <- replace(lims_data_filtered$sample_collection_DoAdmission, which(lims_data_filtered$sample_collection_DoAdmission < 0), NA)
lims_data_filtered$sample_collection_DoEnrolment <- replace(lims_data_filtered$sample_collection_DoEnrolment, which(lims_data_filtered$sample_collection_DoSymptoms < 0), NA)


lims_data_filtered_2 <- lims_data_filtered %>% 
  filter(symptom_onset_DoAdmission < 7 | is.na(symptom_onset_DoAdmission == TRUE))

#Where symptom onset = NA further exclude nosocomial by filtering by collection date DoAdmission <7 - need to decide whether to include unknowns??

lims_data_filtered_3inc_unkwn_nosocomial <- lims_data_filtered_2 %>% 
  filter(is.na(symptom_onset_DoAdmission) == FALSE | sample_collection_DoAdmission < 7 | is.na(sample_collection_DoAdmission) == TRUE)

lims_data_filtered_3exclude_unkwn_nosocomial <- lims_data_filtered_2 %>% 
  filter(is.na(symptom_onset_DoAdmission) == FALSE | sample_collection_DoAdmission < 7 )

#Inclusion criteria
##Filter for Day 1 or Day 3 samples

lims_data_filtered_4 <- lims_data_filtered_3exclude_unkwn_nosocomial %>% 
  filter(Timepoint == "Day 1" | Timepoint == "Recruitment" | Timepoint == "Recruitment / Day 1" | Timepoint == "Day 3" | Timepoint == "Unknown" | Timepoint == "Not Recorded")

#For unknowns select by corrected study day 0-4
lims_data_filtered_4 <- subset(lims_data_filtered_4, !((Timepoint == "Unknown") & (sample_collection_DoEnrolment >4)))
lims_data_filtered_4 <- subset(lims_data_filtered_4, !((Timepoint == "Not Recorded") & (sample_collection_DoEnrolment >4)))  
  
#?Include throat swabs only ?Prioritise throat swabs
##Add column for whether throat available
lims_data_filtered_5 <- lims_data_filtered_4 %>% 
  filter(str_detect(Sample_Type, '(?i)throat') | str_detect(Sample_Type, '(?i)thoat') | str_detect(Sample_Type, 'Respiratory'))

#How many patients included at this point?

PCR_patient_list_1 <- lims_data_filtered_5 %>% 
  select('Patient_ID') %>% 
  distinct()





#Compare to PhipSeq plasma list

PhipSeq_plasma_lims_list_20210525_v2 <- read_csv("~/NGS Serology/Data/Updated severity score/Newest Severity SCore/PhipSeq_plasma_lims_list_20210525_v2.csv")

URT_vs_PhipSeq <- PCR_patient_list_1 %>% 
  full_join(PhipSeq_plasma_lims_list_20210525_v2, by = "Patient_ID")

##TODO





#Prioritise - those with severity score and also PhipSeq analysis, earliest samples

lims_data_filtered_2 <- lims_data_filtered %>% 
  filter(is.na(severity) == FALSE)