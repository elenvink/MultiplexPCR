#Identifying Positive PCR results - ISARIC URT Resp Viral PCR
#EHV
#20210928

library(readr)
library(tidyverse)
library(readxl)


#Import data

URTsamples <- read_excel("~/Viral Coinfection/Sample Lists/Sample Release/Master_list_extraction_PCR.xlsx") %>% 
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

Oct20 <- read_excel("~/Viral Coinfection/PCR Results/20211020allRUTs_data.xls") %>% 
  select(2,3,4,7)

Oct20$PCR_date <- 'Oct20'

Oct21 <- read_excel("~/Viral Coinfection/PCR Results/20211021allRUTs_data.xls") %>% 
  select(2,3,4,7)

Oct21$PCR_date <- 'Oct21'

Oct26 <- read_excel("~/Viral Coinfection/PCR Results/20211026allRUTs_data.xls") %>% 
  select(2,3,4,7)

Oct26$PCR_date <- 'Oct26'

Oct27am <- read_excel("~/Viral Coinfection/PCR Results/20211027_am_allRUTs_data.xls") %>% 
  select(2,3,4,7)

Oct27am$PCR_date <- 'Oct27am'

Oct27pm <- read_excel("~/Viral Coinfection/PCR Results/20211027_pm_allRUTs_data.xls") %>% 
  select(2,3,4,7)

Oct27pm$PCR_date <- 'Oct27pm'

Oct28 <- read_excel("~/Viral Coinfection/PCR Results/20211028_allRUTs_data.xls") %>% 
  select(2,3,4,7)

Oct28$PCR_date <- 'Oct28'

Oct29am <- read_excel("~/Viral Coinfection/PCR Results/20211029_allRUTs_data.xls") %>% 
  select(2,3,4,7)

Oct29am$PCR_date <- 'Oct29am'

Oct29pm <- read_excel("~/Viral Coinfection/PCR Results/20211029_pm_allRUTs_data.xls") %>% 
  select(2,3,4,7)

Oct29pm$PCR_date <- 'Oct29pm'

Nov17 <- read_excel("~/Viral Coinfection/PCR Results/20211117allRUTs_data.xls") %>% 
  select(2,3,4,7)

Nov17$PCR_date <- 'Nov17'

Nov18am1 <- read_excel("~/Viral Coinfection/PCR Results/20211118allRUTs_data_am1.xls") %>% 
  select(2,3,4,7)

Nov18am1$PCR_date <- 'Nov18am1'

Nov18am2 <- read_excel("~/Viral Coinfection/PCR Results/20211118allRUTs_am2_data.xls") %>% 
  select(2,3,4,7)

Nov18am2$PCR_date <- 'Nov18am2'


Nov18pm <- read_excel("~/Viral Coinfection/PCR Results/20211118allRUTs_pm_data.xls") %>% 
  select(2,3,4,7)

Nov18pm$PCR_date <- 'Nov18pm'

Nov24am <- read_excel("~/Viral Coinfection/PCR Results/20211124allRUTs_am_data.xls") %>% 
  select(2,3,4,7)

Nov24am$PCR_date <- 'Nov24am'

Nov24pm <- read_excel("~/Viral Coinfection/PCR Results/20211124all_ruts_pm_data.xls") %>% 
  select(2,3,4,7)

Nov24pm$PCR_date <- 'Nov24pm'

#Combine datasheets

PCR_run <- rbind(Sept7, Sept8, Sept9am, Sept9pm, Sept10am, Sept10pm, Sept14am, 
                 Sept14pm, Sept16, Sept17, Sept22am, Sept22pm, Sept27, Oct20 , 
                 Oct21 , Oct26, Oct27am, Oct27pm , Oct28, Oct29am, Oct29pm, Nov17, 
                 Nov18am1, Nov18am2, Nov18pm, Nov24am, NOv24pm )
                 
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

PCR_run_id <- PCR_run_id[!is.na(PCR_run_id$`Patient ID`),]

#PCR_run <- PCR_run %>% 
  #sort(as.numeric('CT', FALSE))

#Adding oneline data

URTsamples_data_updated <- read_csv("~/Viral Coinfection/ESWI Abstract/URTsamples_data_update_20210929.csv") 

URTsamples_data_update <- URTsamples_data_updated %>% 
  left_join(URTsamples, by = 'Patient ID')

#Baseline characteristics

summary(URTsamples_data_update$calc_age)

Gender_table_1 <- table(URTsamples_data_update$sex)
prop.table(Gender_table_1)

#write_csv(URTsamples_data_update, "~/Viral Coinfection/ESWI Abstract/URTsamples_data_update_20210929.csv")

#Baseline charactersitics table







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


PCR_run_id_pos_result$PCR_result <- 'Co-infection'

PCR_run_id_pos_result <- PCR_run_id_pos_result %>% 
  select(3,4)

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
  filter(PCR_result == 'Co-infection')

URTsamples_data_update_neg <- URTsamples_data_update_posneg %>% 
  filter(is.na(PCR_result == TRUE))

URTsamples_data_update_posneg$PCR_result <- URTsamples_data_update_posneg$PCR_result %>% 
  replace_na('COVID-19')

summary(URTsamples_data_update_pos$LoS_number)

summary(URTsamples_data_update_neg$LoS_number)



#Number of immunosuppressed patients in cohort - my list

URTsamples_data_update <- URTsamples_data_update %>% 
  left_join(Immsupp_med_20210730, by = c('Patient ID' = 'updated_patient_id'))

immsupp_table_cohort <- table(URTsamples_data_update$immunosupp_med_updated)
immsupp_table_cohort
prop.table(immsupp_table_cohort)

#Number of immunosuppressed patients in cohort - Matt Thorpe

immsupp_MT <- read_csv("~/ISARIC 4C/Immunocompromised_Matt_Thorpe.csv") %>% 
  select(1,3)

URTsamples_data_update <- URTsamples_data_update %>% 
  left_join(immsupp_MT, by = c('Patient ID' = 'subjid'))

immsupp_MT_table_cohort <- table(URTsamples_data_update$immunocompromise_reason)
immsupp_MT_table_cohort
prop.table(immsupp_MT_table_cohort)

#Comorbiditie numbers

comorb_list <- read_csv("~/NGS Serology/Data/Updated severity score/Newest Severity SCore/oneline_comorb_list_20210611.csv")

URTsamples_data_update <- URTsamples_data_update %>% 
  left_join(comorb_list, by = c('Patient ID' = 'subjid'))

comorb_number_table_cohort <- table(URTsamples_data_update$comorb_number)
comorb_number_table_cohort
prop.table(comorb_number_table_cohort)

#proportion of different viruses

PCR_run_results <- PCR_run_id %>% 
  select(2,4,6)

URTsamples_data_update_results <- URTsamples_data_update %>% 
  left_join(PCR_run_results, by = 'Patient ID')

URTsamples_data_update_results$`Target Name` <- URTsamples_data_update_results$`Target Name` %>%
  replace_na('Negative')

PCR_result_table <- table(URTsamples_data_update_results$`Target Name`)
PCR_result_table
prop.table(PCR_result_table)

ggplot(data = PCR_run_id, mapping = aes(x = 'Target Name')) +
  geom_bar() - failed

# CT values

PCR_run_id$CT <- as.numeric(PCR_run_id$CT)

summary(PCR_run_id$CT)

CT_boxplot <- ggplot(data = PCR_run_id, mapping = aes(y = CT))+
  geom_boxplot(colour = "midnightblue", fill = "deepskyblue4") +
  ylim(20,40) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)),
        title = element_text(colour = "midnightblue")) +
  labs(title = "CT Values")

ggsave("CT_boxplot.png")

#Comparing pos vs negative cohort

summary(URTsamples_data_update_pos$calc_age)

summary(URTsamples_data_update_neg$calc_age)

Gender_table_pos <- table(URTsamples_data_update_pos$sex)
prop.table(Gender_table_pos)

Gender_table_neg <- table(URTsamples_data_update_neg$sex)
prop.table(Gender_table_neg)

comorb_table_pos <- table(URTsamples_data_update_pos$comorb_number)
prop.table(comorb_table_pos)

comorb_table_neg <- table(URTsamples_data_update_neg$comorb_number)
prop.table(comorb_table_neg)

severity_table_pos <- table(URTsamples_data_update_pos$severity)
severity_table_pos
prop.table(severity_table_pos)

severity_table_neg <- table(URTsamples_data_update_neg$severity)
severity_table_neg
prop.table(severity_table_neg)

death_table_pos <- table(URTsamples_data_update_pos$dsterm)
death_table_pos
prop.table(death_table_pos)

death_pos_table_neg <- table(URTsamples_data_update_neg$dsterm)
death_pos_table_neg
prop.table(death_pos_table_neg)

#Compare Age pos/neg

Age_boxplot_posneg <- ggplot(data = URTsamples_data_update_posneg, mapping = aes(x = PCR_result,  y = calc_age))+
  geom_boxplot(colour = "midnightblue", fill = "deepskyblue4") +
  ylim(0,100) +
  theme(axis.text = element_text(size = rel(1)),
        axis.title = element_text(size = rel(1)),
        title = element_text(colour = "midnightblue")) +
  labs(title = "Age", x= "Infection Status", y = "Age (years)")

Age_boxplot_posneg

ggsave("Age_boxplot_posneg.png")

#Compare sex pos/neg

Sex_barplot_posneg <- ggplot(data = URTsamples_data_update_posneg, mapping = aes(x = PCR_result,fill = sex))+
  geom_bar(position = "fill")+
  theme(title = element_text(colour = "midnightblue")) +
  labs(title = "Sex", x= "Infection Status", y = "Percentage") +
  scale_fill_manual("Sex", values = c("Female" = "deepskyblue3", "Male" = "deepskyblue4")) +
  scale_y_continuous(labels = scales::percent)

Sex_barplot_posneg

ggsave("Sex_barplot_posneg.png")

#Compare comorb po/neg


comorb_barplot_posneg <- ggplot(data = URTsamples_data_update_posneg, mapping = aes(x = PCR_result,fill = comorb_number))+
  geom_bar(position = "fill") +
  theme(title = element_text(colour = "midnightblue")) +
  labs(title = "Comorbidities", x= "Infection Status", y = "Percentage") +
  scale_fill_manual("Comorbidities", values = c("0" = "deepskyblue1", "1" = "deepskyblue3", "2+" = "deepskyblue4"))+
  scale_y_continuous(labels = scales::percent)

comorb_barplot_posneg

ggsave("comorb_barplot_posneg.png")

#Compare severity pos/neg

severity_barplot_posneg <- ggplot(data = URTsamples_data_update_posneg, mapping = aes(x = PCR_result,fill = severity))+
  geom_bar(position = "fill") +
  theme(title = element_text(colour = "midnightblue"),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Disease Severity", x= "Infection Status", y = "Percentage") +
  scale_y_continuous(labels = scales::percent)+
  scale_color_brewer(palette="Dark2")

severity_barplot_posneg

ggsave("Sex_barplot_posneg.png")