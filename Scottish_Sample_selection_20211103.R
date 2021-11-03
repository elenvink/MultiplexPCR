#ESWI Abstract Scottish Sample Selection for URT Multiplex PCR
#by Elen Vink
#20211103

#Exclusion = covid neg, nosocomial onset, sample collected >14 days after admission
#Prioritise - earliest samples by symptoms, earliest samples per patient, phipseq patients - MicroScan, severity score NA

library(readr)
library(tidyverse)
library(readxl)

#Import data

lims_data_URT <- read_csv("~/Ultra/lims_data_20210614_URT.csv")
oneline_severity <- read_csv("~/Ultra/oneline_severity_someupdatedfio2_20210527.csv")
covid_neg_list <- read_excel("~/NGS Serology/Data/Updated severity score/Newest Severity SCore/covid_neg_manual_list_elen_20210525_v2.xlsx")
MicroScan_list_d1.3.conv <- read_csv("~/NGS Serology/Data/Sample Lists/MicroScan/MicroScan_list_d1.3.conv_20210622.csv")
MicroScan_list_d1.3.conv.day9 <- read_csv("~/NGS Serology/Data/Sample Lists/MicroScan/MicroScan_list_d1.3.conv.day9_20210622.csv")

#Add covid status and severity score to LIMS data

oneline_severity_DoS <- oneline_severity %>% 
  select('subjid', dsstdat, cestdat, hostdat, outcome_DoAdmission, outcome_DoSymptoms, symptom_onset_DoAdmission, enrolment_DoSymptoms, enrolment_DoAdmission, 'severity')

lims_data <-  lims_data_URT %>% 
  left_join(oneline_severity_DoS, by = c('Patient_ID' = 'subjid'))

lims_data <-  lims_data %>% 
  left_join(covid_neg_list, by = c('Patient_ID' = 'subjid'))

lims_data_distinct <- lims_data %>% 
  select('Patient_ID') %>% 
  distinct()

#Add column for whether throat swab available - TRUE/FALSE (FALSE means nose or unknown site)

lims_data$throat_swab <- str_detect(lims_data$Sample_Type, '(?i)throat') | str_detect(lims_data$Sample_Type, '(?i)thoat') 

#Add column for those on full MicroScan list - d1/3&conv plus day1/3&9/conv

lims_data <- lims_data %>% 
  left_join(MicroScan_list_d1.3.conv, by = c('Patient_ID' = 'joining_subjid'))

lims_data <- lims_data %>% 
  left_join(MicroScan_list_d1.3.conv.day9, by = c('Patient_ID' = 'joining_subjid'))

###Add column - specimen collection Day of symptoms

lims_data$sample_collection_DoSymptoms <- (as.Date(as.character(lims_data$Date_Collected), format="%Y%m%d")) - as.Date(as.character(lims_data$cestdat), format="%d/%m/%Y")
lims_data$sample_collection_DoAdmission <- (as.Date(as.character(lims_data$Date_Collected), format="%Y%m%d")) - as.Date(as.character(lims_data$hostdat), format="%d/%m/%Y")
lims_data$sample_collection_DoEnrolment <- (as.Date(as.character(lims_data$Date_Collected), format="%Y%m%d")) - as.Date(as.character(lims_data$dsstdat), format="%d/%m/%Y")

####Replace negative values with NA as incorrect

lims_data$sample_collection_DoSymptoms <- replace(lims_data$sample_collection_DoSymptoms, which(lims_data$sample_collection_DoSymptoms < 0), NA)
lims_data$sample_collection_DoAdmission <- replace(lims_data$sample_collection_DoAdmission, which(lims_data$sample_collection_DoAdmission < 0), NA)
lims_data$sample_collection_DoEnrolment <- replace(lims_data$sample_collection_DoEnrolment, which(lims_data$sample_collection_DoSymptoms < 0), NA)


#Inclusion criteria
##Filter for Day 1 or Day 3 samples

lims_data_filtered <- lims_data %>% 
  filter(Timepoint == "Day 1" | Timepoint == "Recruitment" | Timepoint == "Recruitment / Day 1" | Timepoint == "Day 3" | Timepoint == "Unknown" | Timepoint == "Not Recorded")

#For unknowns select by corrected study day 0-4
lims_data_filtered <- subset(lims_data_filtered, !((Timepoint == "Unknown") & (sample_collection_DoEnrolment >4)))
lims_data_filtered <- subset(lims_data_filtered, !((Timepoint == "Not Recorded") & (sample_collection_DoEnrolment >4)))  

lims_data_filtered_distinct <- lims_data_filtered %>% 
  select('Patient_ID') %>% 
  distinct()

#Select earliest samples for each patient
lims_data_filtered_2 <- lims_data_filtered %>% 
  arrange(Kit_ID, desc(throat_swab)) %>% 
  distinct(Kit_ID, .keep_all = TRUE) %>% 
  arrange(Patient_ID, desc(throat_swab)) %>% 
  distinct(Patient_ID, Timepoint, .keep_all = TRUE) %>% 
  arrange(Patient_ID, sample_collection_DoEnrolment) %>% 
  distinct(Patient_ID, .keep_all = TRUE)

##Filter for Scottish samples

lims_data_filtered_3 <- lims_data_filtered_2 %>% 
  filter(str_starts(lims_data_filtered_2$`Kit_ID`, 'CVR') == TRUE)



#Exclusion criteria
##Exclude those who are covid neg

lims_data_filtered_4 <- lims_data_filtered_3 %>% 
  filter(is.na(`Covid status`) == TRUE)



## Exclude nosocomial onset


lims_data_filtered_5 <- lims_data_filtered_4 %>% 
  filter(symptom_onset_DoAdmission < 7 | is.na(symptom_onset_DoAdmission == TRUE))

#Where symptom onset = NA further exclude nosocomial by filtering by collection date DoAdmission <7 

lims_data_filtered_5 <- lims_data_filtered_5 %>% 
  filter(is.na(symptom_onset_DoAdmission) == FALSE | sample_collection_DoAdmission < 7 )


## Exclude sample_collection_DoA >14

lims_data_filtered_6 <- lims_data_filtered_5 %>% 
  filter(sample_collection_DoAdmission < 15)



#How many patients included at this point?

PCR_patient_list_1 <- lims_data_filtered_6 %>% 
  select('Patient_ID') %>% 
  distinct()

#Compare to original list of sample release and to those with results - those lost to EMAG vs not found by Sarah
##Sample release
Sarah_samples_1 <- read_excel("~/Viral Coinfection/Sample Lists/Sample Release/Glasgow ISARIC 4C Sample Release_Vink_ThroatSwabs_July2021.xlsx") %>% 
  select(1,3) %>% 
  distinct()

Sarah_samples_2 <- read_excel("~/Viral Coinfection/Sample Lists/Sample Release/Interim Release File_Elen Vink_June2021 - Glasgow.xlsx") %>% 
  select(1,5) %>% 
  distinct()

Sarah_samples <- rbind(Sarah_samples_1, Sarah_samples_2)

##Samples PCRd

PCR_samples <- read_excel("~/Viral Coinfection/Sample Lists/Sample Release/Master_list_extraction_PCR.xlsx") %>% 
  select(4,14) %>% 
  filter(str_starts(PCR_samples$`Kit ID`, 'CVR') == TRUE)


##Compare lims list to sample release list - difference = not available

not_available <- lims_data_filtered_6 %>% 
  left_join(Sarah_samples, by = c('Kit_ID' = 'Kit ID'))

not_available_filtered <- not_available %>% 
  filter(is.na(`Patient ID` ) == TRUE)

###not available = 3

##Compare sample release list to PCR list - difference = lost to EMAG

lost_to_emag <- Sarah_samples %>% 
  left_join(PCR_samples, by = 'Kit ID')

lost_to_emag_filtered <-  lost_to_emag %>% 
  filter(is.na(`PCR batch name` ) == TRUE)

###lost to EMAg = 28
  ####failed extraction (IC undetermined x2) = 16
  ####

