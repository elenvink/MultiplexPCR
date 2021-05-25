#Sample Selection for URT Multiplex PCR
#by Elen Vink
#20210525

#Exclusion = covid neg, severity score NA, nosocomial onset
#Prioritise - earliest samples by symptoms, earliest samples per patient, phipseq patients

library(readr)
library(tidyverse)
library(readxl)

#Import data

lims_data_URT <- read_csv("F:/ISARIC files/original data/Ultra download/lims_data_20210419_URT.csv")
oneline_severity <- read_csv("F:/ISARIC files/original data/Ultra download/oneline_severity_20210525.csv")
covid_neg_list <- read_excel("~/NGS Serology/Data/Updated severity score/Newest Severity SCore/covid_neg_manual_list_elen_20210525_v2.xlsx")

#Add covid status and severity score to LIMS data

oneline_severity_DoS <- oneline_severity %>% 
  select('subjid', cestdat, outcome_DoAdmission, outcome_DoSymptoms, symptom_onset_DoAdmission, enrolment_DoSymptoms, 'severity')

lims_data <-  lims_data_URT %>% 
  left_join(oneline_severity_DoS, by = c('Patient_ID' = 'subjid'))

lims_data <-  lims_data %>% 
  left_join(covid_neg_list, by = c('Patient_ID' = 'subjid'))

#Filter out those who are covid neg or severity = na

lims_data_filtered <- lims_data %>% 
  filter(is.na(`Covid status`) == TRUE)

lims_data_filtered_2 <- lims_data_filtered %>% 
  filter(is.na(severity) == FALSE)

#Add column - specimen collection Day of symptoms

lims_data_filtered_2$sample_collection_DoSymptoms <- (as.Date(as.character(lims_data_filtered_2$Date_Collected), format="%Y%m%d")) - as.Date(as.character(lims_data_filtered_2$cestdat), format="%d/%m/%Y")

# Exclude nosocomial onset

lims_data_filtered_3 <- lims_data_filtered_2 %>% 
  filter(symptom_onset_DoAdmission < 7 | is.na(symptom_onset_DoAdmission == TRUE))

#?further filtering needed for those where symptom onset = NA


#How many patients included at this point

PCR_patient_list_1 <- lims_data_filtered_3 %>% 
  select('Patient_ID') %>% 
  distinct()

PCR_patient_list_1$URT <- "Yes"

#Compare to PhipSeq plasma list

PhipSeq_plasma_lims_list_20210525_v2 <- read_csv("~/NGS Serology/Data/Updated severity score/Newest Severity SCore/PhipSeq_plasma_lims_list_20210525_v2.csv")

URT_vs_PhipSeq <- PCR_patient_list_1 %>% 
  full_join(PhipSeq_plasma_lims_list_20210525_v2, by = "Patient_ID")

#Need to also add list of usable PhipSeq samples that we already have at the CVR