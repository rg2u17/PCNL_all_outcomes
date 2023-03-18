#CLEANING SCRIPT

#Packages
library(tidyverse)
library(dplyr)
library(DataExplorer)
library(janitor)
library(targets)
library(naniar)
library(qdapTools)
library(keras)

#Load in original dataset - lots of missing values
PCNL_original<-read.csv("~/post_surgical_sepsis/data/PCNL_original.csv", na.strings = list("", " ", "NA", "N/A")) %>% janitor::clean_names() %>% as_tibble()
head(PCNL_original)

#Inspect dataset
introduce(PCNL_original)
plot_missing(PCNL_original)
plot_histogram(PCNL_original)
#plot_correlation(PCNL_original, maxcat = 10L)

#Remove redundant columns
PCNL_original1<-subset(PCNL_original, select=(-c(number_of_units_transfused,
                                                          subprocedure_residual_stones,
                                                          subprocedure_post_operative_nephrostogram,
                                                          stent_inserted_post_operatively,
                                                          subprocedure_ureteric_catheter_post_operatively,
                                                          size_of_nephrostomy,
                                                          nephrostomy_drain_in_situ,
                                                          any_unusual_complications,
                                                          procedure_abandoned
                                                          )
                                                        )
                                 )

str(PCNL_original1)
introduce(PCNL_original1)
plot_missing(PCNL_original1)



#Remove duplicate columns/post-operative variables

PCNL_original1 <-
  subset(PCNL_original1, select = (
    -c(
      charlson_comorbidities,
      ureteric_stent_insertion,
      secondary_re_look_nephroscopy,
      age_related_charlson_score,
      post_transfusion_haemoglobin,
      number_of_tracts_performed,
      pre_operative_msu,
      year,
      x,
      residual_stones,
      preprocedure_residual_stones_size_of_remaining_fragments,
      post_operative_nephrostogram,
      how_many_calyces_contain_stones
    )
  ))

###DEAL WITH MISSING DATA IN INFECTION DATASET###
#Remove rows with Age >100 + <2

PCNL_original1<-PCNL_original1[!(PCNL_original1$age<2),]
PCNL_original1<-PCNL_original1[!(PCNL_original1$age>100),]

PCNL_original1$grade_of_main_operating_surgeon <-as.factor(PCNL_original1$grade_of_main_operating_surgeon)
PCNL_original1$type_of_anaesthesia <-as.factor(PCNL_original1$type_of_anaesthesia)
PCNL_original1$number_of_stones <-as.factor(PCNL_original1$number_of_stones)
PCNL_original1$supervised_training_procedure <-as.factor(PCNL_original1$supervised_training_procedure)
PCNL_original1$pre_existing_nephrostomy_tube <-as.factor(PCNL_original1$pre_existing_nephrostomy_tube)
PCNL_original1$grade_of_performer <-as.factor(PCNL_original1$grade_of_performer)
PCNL_original1$patient_position<-as.factor(PCNL_original1$patient_position)
PCNL_original1$placement_of_tract<-as.factor(PCNL_original1$placement_of_tract)
PCNL_original1$difficult_access<-as.factor(PCNL_original1$difficult_access)
PCNL_original1$predicted_difficulty<-as.factor(PCNL_original1$predicted_difficulty)

PCNL_original1$grade_of_main_operating_surgeon <-
  PCNL_original1$grade_of_main_operating_surgeon %>% replace_na("Consultant")
PCNL_original1$type_of_anaesthesia <-
  PCNL_original1$type_of_anaesthesia %>% replace_na("General")
PCNL_original1$number_of_stones <-
  PCNL_original1$number_of_stones %>% replace_na("Single")
PCNL_original1$supervised_training_procedure <-
  PCNL_original1$supervised_training_procedure %>% replace_na("No")
PCNL_original1$pre_existing_nephrostomy_tube <-
  PCNL_original1$pre_existing_nephrostomy_tube %>% replace_na("No")
PCNL_original1$grade_of_performer <-
  PCNL_original1$grade_of_performer %>% replace_na("Consultant")
PCNL_original1$patient_position <-
  PCNL_original1$patient_position %>% replace_na("Prone")
PCNL_original1$placement_of_tract <-
  PCNL_original1$placement_of_tract %>% replace_na("Subcostal")
PCNL_original1$difficult_access <-
  PCNL_original1$difficult_access %>% replace_na("No")
PCNL_original1$predicted_difficulty <-
  PCNL_original1$predicted_difficulty %>% replace_na("None")
PCNL_original1$number_of_tracts_planned <-
  PCNL_original1$number_of_tracts_planned %>% replace_na(1)

plot_missing(PCNL_original1)


#Define variables
##Age
PCNL_original1$age<-as.numeric(PCNL_original1$age)
##GENDER
PCNL_original1$gender<-as.factor(PCNL_original1$gender)
##SIDE OF STONES
PCNL_original1$side_of_stones<-as.factor(PCNL_original1$side_of_stones)
##BMI
#PCNL_original_infection$BMI<-as.numeric(PCNL_original_infection$BMI)
#CHARLSON SCORE
PCNL_original1$charlson_score<-as.integer(PCNL_original1$charlson_score)
##PREVIOUS UTI
PCNL_original1$previous_urinary_tract_infection<-as.factor(PCNL_original1$previous_urinary_tract_infection)
##PRE-OP ABX COURSE
PCNL_original1$pre_operative_antibiotic_course<-as.factor(PCNL_original1$pre_operative_antibiotic_course)
##PRE-OP MSU RESULT
PCNL_original1$pre_operative_msu_result<-as.factor(PCNL_original1$pre_operative_msu_result)
##PRE-OP RADIOLOGY
PCNL_original1$pre_operative_radiology<-as.factor(PCNL_original1$pre_operative_radiology)
##DMSA
PCNL_original1$renogramdmsa<-as.factor(PCNL_original1$renogramdmsa)
##CATHETERISED
PCNL_original1$is_the_patient_catheterised<-as.factor(PCNL_original1$is_the_patient_catheterised)
#PRE-OP Hb
PCNL_original1$pre_op_haemoglobin<-as.numeric(PCNL_original1$pre_op_haemoglobin)
##PRE-OP GFR
PCNL_original1$pre_op_egfr<-as.factor(PCNL_original1$pre_op_egfr)
##ABX ON INDUCTION
PCNL_original1$prophylactic_antibiotics_on_induction<-as.factor(PCNL_original1$prophylactic_antibiotics_on_induction)
##GRADE OF MAIN SURGEON
PCNL_original1$grade_of_main_operating_surgeon<-as.factor(PCNL_original1$grade_of_main_operating_surgeon)
##TYPE OF ANAESTHESIA
PCNL_original1$type_of_anaesthesia<-as.factor(PCNL_original1$type_of_anaesthesia)
##IR BACKUP
PCNL_original1$interventional_radiology_backup_for_selective_renal_arte<-as.factor(PCNL_original1$interventional_radiology_backup_for_selective_renal_arte)
##STONE DIMENSIONS
PCNL_original1$stone_dimensions<-as.factor(PCNL_original1$stone_dimensions)
##NUMBER OF STONES
PCNL_original1$number_of_stones<-as.factor(PCNL_original1$number_of_stones)
##STONE COMPLEXITY
PCNL_original1$stone_complexity<-as.factor(PCNL_original1$stone_complexity)
##TRAINING PROCEDURE
PCNL_original1$supervised_training_procedure<-as.factor(PCNL_original1$supervised_training_procedure)
##PRE EXISTING PCN
PCNL_original1$pre_existing_nephrostomy_tube<-as.factor(PCNL_original1$pre_existing_nephrostomy_tube)
##PUNCTURE PERFORMED BY
PCNL_original1$puncture_tract_performed_by<-as.factor(PCNL_original1$puncture_tract_performed_by)
##GRADE OF PERFORMER
PCNL_original1$grade_of_performer<-as.factor(PCNL_original1$grade_of_performer)
##PUNCTURE SITE
PCNL_original1$puncture_site<-as.factor(PCNL_original1$puncture_site)
##IMAGE GUIDANCE FOR RENAL PUNCTURE
PCNL_original1$image_guidance_for_renal_puncture<-as.factor(PCNL_original1$image_guidance_for_renal_puncture)
##PATIENT POSITION
PCNL_original1$patient_position<-as.factor(PCNL_original1$patient_position)
##NUMBER OF TRACTS PLANNED
PCNL_original1$number_of_tracts_planned<-as.integer(PCNL_original1$number_of_tracts_planned)
##PLACEMENT OF TRACT
PCNL_original1$placement_of_tract<-factor(PCNL_original1$placement_of_tract)
##AMPLATZ SHEATH SIZE
PCNL_original1$sizeouter_diameter_of_amplatz_sheath<-as.factor(PCNL_original1$sizeouter_diameter_of_amplatz_sheath)
##DILATORS USED
PCNL_original1$dilators_used<-as.factor(PCNL_original1$dilators_used)
##DIFFICULT ACCESS
PCNL_original1$difficult_access<-as.factor(PCNL_original1$difficult_access)
##PREDICTED DIFFICULTY
PCNL_original1$predicted_difficulty<-as.factor(PCNL_original1$predicted_difficulty)
##ACCESSORY PROCEDURES
PCNL_original1$accessory_procedures<-as.factor(PCNL_original1$accessory_procedures)
##STONE EXTRACTION/FRAGMENTATION TECHNIQUE
PCNL_original1$stone_extraction_fragmentation<-as.factor(PCNL_original1$stone_extraction_fragmentation)
#stent_inserted_subsequent_to_procedure
PCNL_original1$stent_inserted_subsequent_to_procedure<-as.factor(PCNL_original1$stent_inserted_subsequent_to_procedure)
#Hounsfield units
PCNL_original1$maximum_hounsfield_units_of_the_index_stone_on_ctkub<-as.factor(PCNL_original1$maximum_hounsfield_units_of_the_index_stone_on_ctkub)
##POST-OP INFECTION OUTCOMES
PCNL_original1$post_operative_infection<-as.character(PCNL_original1$post_operative_infection)

plot_histogram(PCNL_original1)



#Pre-Operative Imaging - separate into separate columns
PCNL_original1$pre_operative_radiology<-as.character(PCNL_original1$pre_operative_radiology)
multi_column1<-mtabulate(strsplit(PCNL_original1$pre_operative_radiology, ',')) %>% janitor::clean_names()
PCNL_original1<-cbind(PCNL_original1,multi_column1)
PCNL_original1<-subset(PCNL_original1, 
                                select=(-c(pre_operative_radiology)))




#Accessory Procedures
PCNL_original1<-naniar::replace_with_na(data=PCNL_original1, 
                                                 replace=list(accessory_procedures = ""))
PCNL_original1$accessory_procedures<-as.character(PCNL_original1$accessory_procedures)
multi_column2<-mtabulate(strsplit(PCNL_original1$accessory_procedures, ',')) %>% janitor::clean_names()
PCNL_original1<-cbind(PCNL_original1,multi_column2)
PCNL_original1<-subset(PCNL_original1, 
                                select=(-c(accessory_procedures, none)))



#Stone Extraction/Fragmentation
PCNL_original1<-naniar::replace_with_na(data=PCNL_original1, 
                                                 replace=list(stone_extraction_fragmentation = ""))
PCNL_original1$stone_extraction_fragmentation<-as.character(PCNL_original1$stone_extraction_fragmentation)
multi_column3<-mtabulate(strsplit(PCNL_original1$stone_extraction_fragmentation, ',')) %>% janitor::clean_names()
PCNL_original1<-cbind(PCNL_original1,multi_column3)
PCNL_original1<-subset(PCNL_original1, 
                                select=(-c(stone_extraction_fragmentation)))



#Combine Index/Other Stone Locations then Split into single locations - 

PCNL_original1 <- unite(PCNL_original1,
                                 col="stone_location", 
                                 index_stone_location:other_stone_location, 
                                 sep=",", 
                                 remove=TRUE)
PCNL_original1$stone_location<-as.character(PCNL_original1$stone_location)
multi_column4<-mtabulate(strsplit(PCNL_original1$stone_location, ',')) %>% janitor::clean_names()
PCNL_original1<-cbind(PCNL_original1,multi_column4)
PCNL_original1<-subset(PCNL_original1, 
                                select=(-c(na)))


#combine fever + sepsis outcomes to get 3 outcomes: None, Fever, Sepsis
infection_outcome<-as.matrix(PCNL_original1$post_operative_infection)
infection_outcome<-as.data.frame(infection_outcome)
infection_outcome[infection_outcome == "Fever,SIRS - sepsis"]<-"Sepsis"
infection_outcome[infection_outcome == "SIRS - sepsis"]<-"Sepsis"
infection_outcome[infection_outcome == "None,Fever"]<-"Fever"
infection_outcome1<-infection_outcome
infection_outcome$V1<-as.factor(infection_outcome$V1)
colnames(infection_outcome)<-"multi_infection_outcome"
PCNL_original1<-cbind(PCNL_original1, infection_outcome)

#create new column for infection only (combined fever + sepsis) - this will allow comparison between binary outcome + multiple outcome
infection_outcome1[infection_outcome1=="Sepsis"]<-"Yes"
infection_outcome1[infection_outcome1=="Fever"]<-"Yes"
infection_outcome1[infection_outcome1=="None"]<-"No"
infection_outcome1$V1<-as.factor(infection_outcome1$V1)
infection_outcome1$V1<-as.factor(infection_outcome1$V1)
colnames(infection_outcome1)<-"single_infection_outcome"
PCNL_original1<-cbind(PCNL_original1, infection_outcome1)

Outcome<-subset(PCNL_original1,
                select=c(post_operative_infection))
PCNL_original1<-subset(PCNL_original1,
                                select=(-c(post_operative_infection)))
PCNL_original1<-cbind(PCNL_original1,Outcome)

#Reinspect dataset
plot_missing(PCNL_original1)

#Characterise all column
PCNL_original1$supervised_training_procedure<-as.factor(PCNL_original1$supervised_training_procedure)
PCNL_original1$pre_existing_nephrostomy_tube<-as.factor(PCNL_original1$pre_existing_nephrostomy_tube)
PCNL_original1$patient_position<-as.factor(PCNL_original1$patient_position)
PCNL_original1$grade_of_performer<-as.factor(PCNL_original1$grade_of_performer)
PCNL_original1$grade_of_main_operating_surgeon<-as.factor(PCNL_original1$grade_of_main_operating_surgeon)
PCNL_original1$type_of_anaesthesia<-as.factor(PCNL_original1$type_of_anaesthesia)
PCNL_original1$number_of_stones<-as.factor(PCNL_original1$number_of_stones)
PCNL_original1$placement_of_tract<-as.factor(PCNL_original1$placement_of_tract)
PCNL_original1$difficult_access<-as.factor(PCNL_original1$difficult_access)
PCNL_original1$predicted_difficulty<-as.factor(PCNL_original1$predicted_difficulty)
PCNL_original1$diagnostic_non_contrast_ct<-as.factor(PCNL_original1$diagnostic_non_contrast_ct)
PCNL_original1$ivu<-as.factor(PCNL_original1$ivu)
PCNL_original1$kub<-as.factor(PCNL_original1$kub)
PCNL_original1$planning_ctu<-as.factor(PCNL_original1$planning_ctu)
PCNL_original1$uss<-as.factor(PCNL_original1$uss)
PCNL_original1$antegrade_stent_insertion<-as.factor(PCNL_original1$antegrade_stent_insertion)
PCNL_original1$flexible_renoscopy<-as.factor(PCNL_original1$flexible_renoscopy)
PCNL_original1$flexible_ureteroscopy<-as.factor(PCNL_original1$flexible_ureteroscopy)
PCNL_original1$retrograde_stent_insertion<-as.factor(PCNL_original1$retrograde_stent_insertion)
PCNL_original1$ureteric_catheter_antegradely<-as.factor(PCNL_original1$ureteric_catheter_antegradely)
PCNL_original1$ureteric_catheter_retrogradely<-as.factor(PCNL_original1$ureteric_catheter_retrogradely)
PCNL_original1$laser<-as.factor(PCNL_original1$laser)
PCNL_original1$lift_out<-as.factor(PCNL_original1$lift_out)
PCNL_original1$lithoclast<-as.factor(PCNL_original1$lithoclast)
PCNL_original1$ultrasonic_lithotripter<-as.factor(PCNL_original1$ultrasonic_lithotripter)

PCNL_original1$calyceal_diverticular<-ifelse(PCNL_original1$calyceal_diverticular=="0","0","1")
PCNL_original1$calyceal_diverticular<-as.factor(PCNL_original1$calyceal_diverticular) 
PCNL_original1$calyceal_l<-ifelse(PCNL_original1$calyceal_l=="0","0","1")
PCNL_original1$calyceal_l<-as.factor(PCNL_original1$calyceal_l)
PCNL_original1$calyceal_m<-ifelse(PCNL_original1$calyceal_m=="0","0","1")
PCNL_original1$calyceal_m<-as.factor(PCNL_original1$calyceal_m)
PCNL_original1$calyceal_u<-ifelse(PCNL_original1$calyceal_u=="0","0","1")
PCNL_original1$calyceal_u<-as.factor(PCNL_original1$calyceal_u)
PCNL_original1$complete_staghorn<-ifelse(PCNL_original1$complete_staghorn=="0","0","1")
PCNL_original1$complete_staghorn<-as.factor(PCNL_original1$complete_staghorn)
PCNL_original1$partial_staghorn<-ifelse(PCNL_original1$partial_staghorn=="0","0","1")
PCNL_original1$partial_staghorn<-as.factor(PCNL_original1$partial_staghorn)
PCNL_original1$pelvic<-ifelse(PCNL_original1$pelvic=="0","0","1")
PCNL_original1$pelvic<-as.factor(PCNL_original1$pelvic)
PCNL_original1$ureter_other<-ifelse(PCNL_original1$ureter_other=="0","0","1")
PCNL_original1$ureter_other<-as.factor(PCNL_original1$pelvic)
PCNL_original1$upper_ureteric<-ifelse(PCNL_original1$upper_ureteric=="0","0","1")
PCNL_original1$upper_ureteric<-as.factor(PCNL_original1$upper_ureteric)


# Sort Outcomes
#multi_infection_outcome
#post_operative_infection

#single_infection_outcome 
#blood_transfusion
#itu_hdu_admission
#complete_clearance_on_fluoroscopy
#visceral_injury
#clearance_on_post_operative_radiological_imaging_during_a
#patient_statusdischarge
#postop_complications
#clavien_dindo_grade_of_complications
#post_operative_stay
#stone_free_at_follow_up
#adjuvant_treatment
#intraop_complications

PCNL_original1$blood_transfusion<-as.factor(PCNL_original1$blood_transfusion)
PCNL_original1$single_infection_outcome<-as.factor(PCNL_original1$single_infection_outcome)
PCNL_original1$itu_hdu_admission<-as.factor(PCNL_original1$itu_hdu_admission)
PCNL_original1$visceral_injury<-as.factor(PCNL_original1$visceral_injury)
PCNL_original1$clearance_on_post_operative_radiological_imaging_during_a <-
  as.factor(PCNL_original1$clearance_on_post_operative_radiological_imaging_during_a)
PCNL_original1$patient_statusdischarge<-as.factor(PCNL_original1$patient_statusdischarge)
PCNL_original1$postop_complications<-as.factor(PCNL_original1$postop_complications)
PCNL_original1$clavien_dindo_grade_of_complications<-as.factor(PCNL_original1$clavien_dindo_grade_of_complications)
PCNL_original1$post_operative_stay<-as.numeric(PCNL_original1$post_operative_stay)
PCNL_original1$stone_free_at_follow_up<-as.factor(PCNL_original1$stone_free_at_follow_up)
PCNL_original1$adjuvant_treatment<-as.factor(PCNL_original1$adjuvant_treatment)
PCNL_original1$intraop_complications<-as.factor(PCNL_original1$intraop_complications)

## Transfusion
PCNL_original_transfusion <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    itu_hdu_admission,
    complete_clearance_on_fluoroscopy,
    visceral_injury,
    clearance_on_post_operative_radiological_imaging_during_a,
    patient_statusdischarge,
    postop_complications,
    clavien_dindo_grade_of_complications,
    post_operative_stay,
    stone_free_at_follow_up,
    adjuvant_treatment,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_transfusion <-
  PCNL_original_transfusion[!is.na(PCNL_original_transfusion$blood_transfusion), ]
summary(PCNL_original_transfusion$blood_transfusion)

## Single Infection outcome
PCNL_original_infection <- subset(
  PCNL_original1,
  select = -c(
    blood_transfusion,
    itu_hdu_admission,
    complete_clearance_on_fluoroscopy,
    visceral_injury,
    clearance_on_post_operative_radiological_imaging_during_a,
    patient_statusdischarge,
    postop_complications,
    clavien_dindo_grade_of_complications,
    post_operative_stay,
    stone_free_at_follow_up,
    adjuvant_treatment,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_infection<-PCNL_original_infection[!is.na(PCNL_original_infection$single_infection_outcome),]
summary(PCNL_original_infection$single_infection_outcome)

## ITU / HDU Admission

PCNL_original_itu_hdu <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    blood_transfusion,
    complete_clearance_on_fluoroscopy,
    visceral_injury,
    clearance_on_post_operative_radiological_imaging_during_a,
    patient_statusdischarge,
    postop_complications,
    clavien_dindo_grade_of_complications,
    post_operative_stay,
    stone_free_at_follow_up,
    adjuvant_treatment,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_itu_hdu<-PCNL_original_itu_hdu[!is.na(PCNL_original_itu_hdu$itu_hdu_admission),]
summary(PCNL_original_itu_hdu$itu_hdu_admission)

## Complete_clearance_on_fluoroscopy

PCNL_original_clearance_on_fluoro <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    blood_transfusion,
    itu_hdu_admission,
    visceral_injury,
    clearance_on_post_operative_radiological_imaging_during_a,
    patient_statusdischarge,
    postop_complications,
    clavien_dindo_grade_of_complications,
    post_operative_stay,
    stone_free_at_follow_up,
    adjuvant_treatment,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_clearance_on_fluoro<-PCNL_original_clearance_on_fluoro[!is.na(PCNL_original_clearance_on_fluoro$complete_clearance_on_fluoroscopy),]
summary(PCNL_original_clearance_on_fluoro$complete_clearance_on_fluoroscopy)

## visceral_injury

PCNL_original_visc_inj <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    blood_transfusion,
    itu_hdu_admission,
    complete_clearance_on_fluoroscopy,
    clearance_on_post_operative_radiological_imaging_during_a,
    patient_statusdischarge,
    postop_complications,
    clavien_dindo_grade_of_complications,
    post_operative_stay,
    stone_free_at_follow_up,
    adjuvant_treatment,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_visc_inj<-PCNL_original_visc_inj[!is.na(PCNL_original_visc_inj$visceral_injury),]
summary(PCNL_original_visc_inj$visceral_injury)

#clearance_on_post_operative_radiological_imaging_during_a

PCNL_original_clearance_during_admission <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    blood_transfusion,
    itu_hdu_admission,
    complete_clearance_on_fluoroscopy,
    visceral_injury,
    patient_statusdischarge,
    postop_complications,
    clavien_dindo_grade_of_complications,
    post_operative_stay,
    stone_free_at_follow_up,
    adjuvant_treatment,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_clearance_during_admission <-
  PCNL_original_clearance_during_admission[!is.na(
    PCNL_original_clearance_during_admission$clearance_on_post_operative_radiological_imaging_during_a
  ), ]
summary(
  PCNL_original_clearance_during_admission$clearance_on_post_operative_radiological_imaging_during_a
)

#patient_statusdischarge
PCNL_original_death <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    blood_transfusion,
    itu_hdu_admission,
    complete_clearance_on_fluoroscopy,
    visceral_injury,
    clearance_on_post_operative_radiological_imaging_during_a,
    postop_complications,
    clavien_dindo_grade_of_complications,
    post_operative_stay,
    stone_free_at_follow_up,
    adjuvant_treatment,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_death<-PCNL_original_death[!is.na(PCNL_original_death$patient_statusdischarge),]
summary(PCNL_original_death$patient_statusdischarge)


#postop_complications
PCNL_original_post_op_comp <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    blood_transfusion,
    itu_hdu_admission,
    complete_clearance_on_fluoroscopy,
    visceral_injury,
    clearance_on_post_operative_radiological_imaging_during_a,
    patient_statusdischarge,
    clavien_dindo_grade_of_complications,
    post_operative_stay,
    stone_free_at_follow_up,
    adjuvant_treatment,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_post_op_comp<-PCNL_original_post_op_comp[!is.na(PCNL_original_post_op_comp$postop_complications),]
summary(PCNL_original_post_op_comp$postop_complications)


#clavien_dindo_grade_of_complications
PCNL_original_clav <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    blood_transfusion,
    itu_hdu_admission,
    complete_clearance_on_fluoroscopy,
    visceral_injury,
    clearance_on_post_operative_radiological_imaging_during_a,
    patient_statusdischarge,
    postop_complications,
    post_operative_stay,
    stone_free_at_follow_up,
    adjuvant_treatment,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_clav<-PCNL_original_clav[!is.na(PCNL_original_clav$clavien_dindo_grade_of_complications),]
summary(PCNL_original_clav$clavien_dindo_grade_of_complications)

#post_operative_stay
PCNL_original_stay <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    blood_transfusion,
    itu_hdu_admission,
    complete_clearance_on_fluoroscopy,
    visceral_injury,
    clearance_on_post_operative_radiological_imaging_during_a,
    patient_statusdischarge,
    postop_complications,
    clavien_dindo_grade_of_complications,
    stone_free_at_follow_up,
    adjuvant_treatment,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_stay<-PCNL_original_stay[!is.na(PCNL_original_stay$post_operative_stay),]
summary(PCNL_original_stay$post_operative_stay)


#stone_free_at_follow_up
PCNL_original_sf_at_fu <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    blood_transfusion,
    itu_hdu_admission,
    complete_clearance_on_fluoroscopy,
    visceral_injury,
    clearance_on_post_operative_radiological_imaging_during_a,
    patient_statusdischarge,
    postop_complications,
    clavien_dindo_grade_of_complications,
    post_operative_stay,
    adjuvant_treatment,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_sf_at_fu<-PCNL_original_sf_at_fu[!is.na(PCNL_original_sf_at_fu$stone_free_at_follow_up),]
summary(PCNL_original_sf_at_fu$stone_free_at_follow_up)


#adjuvant_treatment
PCNL_original_adj_rx <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    blood_transfusion,
    itu_hdu_admission,
    complete_clearance_on_fluoroscopy,
    visceral_injury,
    clearance_on_post_operative_radiological_imaging_during_a,
    patient_statusdischarge,
    postop_complications,
    clavien_dindo_grade_of_complications,
    post_operative_stay,
    stone_free_at_follow_up,
    intraop_complications,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_adj_rx<-PCNL_original_adj_rx[!is.na(PCNL_original_adj_rx$adjuvant_treatment),]
summary(PCNL_original_adj_rx$adjuvant_treatment)


#intraop_complications
PCNL_original_intra_op_comp <- subset(
  PCNL_original1,
  select = -c(
    single_infection_outcome,
    blood_transfusion,
    itu_hdu_admission,
    complete_clearance_on_fluoroscopy,
    visceral_injury,
    clearance_on_post_operative_radiological_imaging_during_a,
    patient_statusdischarge,
    postop_complications,
    clavien_dindo_grade_of_complications,
    post_operative_stay,
    stone_free_at_follow_up,
    adjuvant_treatment,
    stone_location,
    image_guidance_for_renal_puncture == "CT",
    multi_infection_outcome,
    post_operative_infection
  )
) %>% as_tibble()
PCNL_original_intra_op_comp<-PCNL_original_intra_op_comp[!is.na(PCNL_original_intra_op_comp$intraop_complications),]
summary(PCNL_original_intra_op_comp$intraop_complications)


