#Add severity and days of outcome/symptoms etc to oneline
#by Elen Vink 
#20210525

library(readr)
library(tidyverse)
library(readxl)

#Import oneline data

oneline_data <- read_csv("F:/ISARIC files/original data/Ultra download/oneline_20210419.csv")

#Add additional columns

oneline_data$outcome_DoAdmission <- (as.Date(as.character(oneline_data$dsstdtc), format="%d/%m/%Y")) - as.Date(as.character(oneline_data$hostdat), format="%d/%m/%Y")
oneline_data$symptom_onset_DoAdmission <- (as.Date(as.character(oneline_data$cestdat), format="%d/%m/%Y")) - as.Date(as.character(oneline_data$hostdat), format="%d/%m/%Y")
oneline_data$outcome_DoSymptoms <- (as.Date(as.character(oneline_data$dsstdtc), format="%d/%m/%Y")) - as.Date(as.character(oneline_data$cestdat), format="%d/%m/%Y")
oneline_data$enrolment_DoSymptoms <- (as.Date(as.character(oneline_data$dsstdat), format="%d/%m/%Y")) - as.Date(as.character(oneline_data$cestdat), format="%d/%m/%Y")
oneline_data$"28d_alive_unknown" <- (((oneline$dsterm == "Transfer to other facility" | oneline$dsterm == "Unknown") & (oneline$outcome_DoAdmission <28))|((oneline$dsterm == "Transfer to other facility" | oneline$dsterm == "Unknown") & (oneline$outcome_DoAdmission >27) & (oneline$symptom_onset_DoAdmission >7) & (oneline$outcome_DoSymptoms <28)))
oneline_data$enrolment_DoAdmission <- (as.Date(as.character(oneline_data$dsstdat), format="%d/%m/%Y")) - as.Date(as.character(oneline_data$hostdat), format="%d/%m/%Y")

#Severity score automation

oneline_data <- oneline_data %>% 
  mutate(
    severity = case_when(
      ((dsterm == "Death" | dsterm == "Palliative discharge") & (outcome_DoAdmission <28))|((dsterm == "Death"|dsterm == "Palliative discharge") & (outcome_DoAdmission >27) & (symptom_onset_DoAdmission >7) & (outcome_DoSymptoms <28)) ~ "5",
      (any_invasive == "Yes" & (is.na(dsterm) == TRUE | dsterm == "Unknown" | (dsterm == "Death" & (is.na(outcome_DoAdmission) == TRUE)) | (((dsterm == "Transfer to other facility" ) & (outcome_DoAdmission <28 | is.na(outcome_DoAdmission) == TRUE))|((dsterm == "Transfer to other facility") & (outcome_DoAdmission >27) & (symptom_onset_DoAdmission >7) & (outcome_DoSymptoms <28))))) ~ "4+",
      (any_invasive == "Yes" & 
         ((dsterm == "Discharged alive" | dsterm == "Hospitalization") | ((dsterm == "Transfer to other facility" | dsterm == "Transfer to other facility" |dsterm == "Death" | dsterm == "Palliative discharge") & outcome_DoAdmission >27)|
            ((dsterm == "Transfer to other facility" | dsterm == "Death"|dsterm == "Palliative discharge") & (outcome_DoAdmission <28 & symptom_onset_DoAdmission >7 & outcome_DoSymptoms >27)))) ~ "4",
      (any_noninvasive == "Yes" | any_daily_nasaloxy_cmtrt == "Yes" | oxygenhf_cmoccur == "YES") &
        ((dsterm == "Discharged alive" | dsterm == "Hospitalization" | (((dsterm == "Transfer to other facility" | dsterm == "Death" | dsterm == "Palliative discharge") & outcome_DoAdmission >27)|
                                                                          ((dsterm == "Transfer to other facility" | dsterm == "Death"|dsterm == "Palliative discharge") & (outcome_DoAdmission <28 & symptom_onset_DoAdmission >7 & outcome_DoSymptoms >27))))) ~ "3",
      (any_oxygen == "Yes" | oxy_vsorresu == "Oxygen therapy") &
        (dsterm == "Discharged alive" | dsterm == "Hospitalization" | (((dsterm == "Transfer to other facility" | dsterm == "Death" | dsterm == "Palliative discharge") & outcome_DoAdmission >27)|
                                                                         ((dsterm == "Transfer to other facility" | dsterm == "Death"|dsterm == "Palliative discharge") & (outcome_DoAdmission <28 & symptom_onset_DoAdmission >7 & outcome_DoSymptoms >27)))) ~ "2",
      ((any_oxygen == "No" | is.na(any_oxygen) == TRUE) & (oxy_vsorresu == "Room air" | oxy_vsorresu == "N/A" | is.na(oxy_vsorresu) == TRUE) & (any_invasive == "No" | is.na(any_invasive) == TRUE)  & (any_noninvasive == "No" | is.na(any_noninvasive) == TRUE) & (any_daily_nasaloxy_cmtrt == "No" | is.na(any_daily_nasaloxy_cmtrt) == TRUE) & 
         (dsterm == "Discharged alive" | dsterm == "Hospitalization" | (((dsterm == "Transfer to other facility" | dsterm == "Death" | dsterm == "Palliative discharge") & outcome_DoAdmission >27)|
                                                                          ((dsterm == "Transfer to other facility" | dsterm == "Death"|dsterm == "Palliative discharge") & (outcome_DoAdmission <28 & symptom_onset_DoAdmission >7 & outcome_DoSymptoms >27))))) ~ "1")
    %>% 
      factor(levels = c("5", "4+", "4", "3", "2", "1")))

#Export updated version of oneline with severity scores

write_csv(oneline_data, "F:/ISARIC files/original data/Ultra download/oneline_severity_20210525.csv")
