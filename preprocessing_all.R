library(tidyverse)
library(dplyr)
library(DataExplorer)
library(janitor)
library(targets)
library(naniar)
library(qdapTools)
library(DataExplorer)
library(tidyverse)
library(tidymodels)
library(mice)
library(arm)
library(keras)
library(tensorflow)
library(MLeval)
library(pROC)
library(caret)
library(tidymodels)
library(gt)
library(gtExtras)
library(tensorflow)
library(ROSE)

#single_infection_outcome 
PCNL_single_infection <- subset(
  PCNL_original_infection,
  select = -c(
    grade_of_main_operating_surgeon,
    type_of_anaesthesia,
    number_of_stones,
    supervised_training_procedure,
    grade_of_performer,
    patient_position,
    placement_of_tract,
    difficult_access,
    predicted_difficulty,
    calyceal_u,
    pre_existing_nephrostomy_tube,
    number_of_tracts_planned
  )
) %>% as_tibble() %>% janitor:: clean_names()

plot_missing(PCNL_single_infection)


##### Omit NA Dataset
PCNL_single_infection_omit_na <- na.omit(PCNL_single_infection)
str(PCNL_single_infection_omit_na)

PCNL_single_infection_sample<-sample(1:nrow(PCNL_single_infection), size=nrow(PCNL_single_infection) *0.7)
PCNL_single_infection_train<-PCNL_single_infection[PCNL_single_infection_sample,] %>% as_tibble()
PCNL_single_infection_test<-PCNL_single_infection[-PCNL_single_infection_sample,] %>% as_tibble()

PCNL_single_infection_omit_na_sample<-sample(1:nrow(PCNL_single_infection_omit_na), size=nrow(PCNL_single_infection_omit_na) *0.7)
PCNL_single_infection_omit_na_train<-PCNL_single_infection_omit_na[PCNL_single_infection_omit_na_sample,] %>% as_tibble()
PCNL_single_infection_omit_na_test<-PCNL_single_infection_omit_na[-PCNL_single_infection_omit_na_sample,] %>% as_tibble()
summary(PCNL_single_infection_omit_na_train$single_infection_outcome)

##### Oversampled Dataset
PCNL_single_infection_omit_na_train_oversample <-
  ROSE::ovun.sample(single_infection_outcome ~ ., 
                    data = PCNL_single_infection_omit_na_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_single_infection_omit_na_train_oversample$single_infection_outcome)

##### Imputed Dataset
PCNL_single_infection_imp_pre <- PCNL_single_infection %>% drop_na(single_infection_outcome)
PCNL_single_infection_imp1<-mice(PCNL_single_infection_imp_pre, m=1)
summary(PCNL_single_infection_imp1)

PCNL_single_infection_imp<-mice::complete(PCNL_single_infection_imp1,1) %>% as_tibble()

PCNL_single_infection_imp_sample<-sample(1:nrow(PCNL_single_infection_imp), size=nrow(PCNL_single_infection_imp) *0.7)
PCNL_single_infection_imp_train<-PCNL_single_infection_imp[PCNL_single_infection_imp_sample,] %>% as_tibble()
PCNL_single_infection_imp_test<-PCNL_single_infection_imp[-PCNL_single_infection_imp_sample,] %>% as_tibble()
PCNL_single_infection_imp_train$single_infection_outcome<-as.factor(PCNL_single_infection_imp_train$single_infection_outcome)
PCNL_single_infection_imp_test$single_infection_outcome<-as.factor(PCNL_single_infection_imp_test$single_infection_outcome)
summary(PCNL_single_infection_imp_train$single_infection_outcome)


##### Oversampling of Imputed Dataset
PCNL_single_infection_imp_train_oversample <-
  ROSE::ovun.sample(single_infection_outcome ~ ., 
                    data = PCNL_single_infection_imp_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_single_infection_imp_train_oversample$single_infection_outcome)


#blood_transfusion
PCNL_original_transfusion <- subset(
  PCNL_original_transfusion,
  select = -c(
    grade_of_main_operating_surgeon,
    type_of_anaesthesia,
    number_of_stones,
    supervised_training_procedure,
    grade_of_performer,
    patient_position,
    placement_of_tract,
    difficult_access,
    predicted_difficulty,
    calyceal_u,
    pre_existing_nephrostomy_tube,
    number_of_tracts_planned
  )
) %>% as_tibble() %>% janitor:: clean_names()

plot_missing(PCNL_original_transfusion)


##### Omit NA Dataset
PCNL_transfusion_omit_na <- na.omit(PCNL_original_transfusion)
str(PCNL_transfusion_omit_na)

PCNL_transfusion_sample<-sample(1:nrow(PCNL_original_transfusion), size=nrow(PCNL_original_transfusion) *0.7)
PCNL_transfusion_train<-PCNL_original_transfusion[PCNL_transfusion_sample,] %>% as_tibble()
PCNL_transfusion_test<-PCNL_original_transfusion[-PCNL_transfusion_sample,] %>% as_tibble()

PCNL_transfusion_omit_na_sample<-sample(1:nrow(PCNL_transfusion_omit_na), size=nrow(PCNL_transfusion_omit_na) *0.7)
PCNL_transfusion_omit_na_train<-PCNL_transfusion_omit_na[PCNL_transfusion_omit_na_sample,] %>% as_tibble()
PCNL_transfusion_omit_na_test<-PCNL_transfusion_omit_na[-PCNL_transfusion_omit_na_sample,] %>% as_tibble()
summary(PCNL_transfusion_omit_na_train$blood_transfusion)

##### Oversampled Dataset
PCNL_transfusion_omit_na_train_oversample <-
  ROSE::ovun.sample(blood_transfusion ~ ., 
                    data = PCNL_transfusion_omit_na_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_transfusion_omit_na_train_oversample$blood_transfusion)

##### Imputed Dataset
PCNL_transfusion_imp_pre <- PCNL_original_transfusion %>% drop_na(blood_transfusion)
PCNL_transfusion_imp1<-mice(PCNL_transfusion_imp_pre, m=1)
summary(PCNL_transfusion_imp1)

PCNL_transfusion_imp<-mice::complete(PCNL_transfusion_imp1,1) %>% as_tibble()

PCNL_transfusion_imp_sample<-sample(1:nrow(PCNL_transfusion_imp), size=nrow(PCNL_transfusion_imp) *0.7)
PCNL_transfusion_imp_train<-PCNL_transfusion_imp[PCNL_transfusion_imp_sample,] %>% as_tibble()
PCNL_transfusion_imp_test<-PCNL_transfusion_imp[-PCNL_transfusion_imp_sample,] %>% as_tibble()

summary(PCNL_transfusion_imp_train$blood_transfusion)


##### Oversampling of Imputed Dataset
PCNL_transfusion_imp_train_oversample <-
  ROSE::ovun.sample(blood_transfusion ~ ., 
                    data = PCNL_transfusion_imp_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_transfusion_imp_train_oversample$blood_transfusion)

#itu_hdu_admission

PCNL_original_itu_hdu <- subset(
  PCNL_original_itu_hdu,
  select = -c(
    grade_of_main_operating_surgeon,
    type_of_anaesthesia,
    number_of_stones,
    supervised_training_procedure,
    grade_of_performer,
    patient_position,
    placement_of_tract,
    difficult_access,
    predicted_difficulty,
    calyceal_u,
    pre_existing_nephrostomy_tube,
    number_of_tracts_planned
  )
) %>% as_tibble() %>% janitor:: clean_names()

plot_missing(PCNL_original_itu_hdu)


##### Omit NA Dataset
PCNL_itu_hdu_omit_na <- na.omit(PCNL_original_itu_hdu)
str(PCNL_itu_hdu_omit_na)

PCNL_itu_hdu_sample<-sample(1:nrow(PCNL_original_itu_hdu), size=nrow(PCNL_original_itu_hdu) *0.7)
PCNL_itu_hdu_train<-PCNL_original_itu_hdu[PCNL_itu_hdu_sample,] %>% as_tibble()
PCNL_itu_hdu_test<-PCNL_original_itu_hdu[-PCNL_itu_hdu_sample,] %>% as_tibble()

PCNL_itu_hdu_omit_na_sample<-sample(1:nrow(PCNL_itu_hdu_omit_na), size=nrow(PCNL_itu_hdu_omit_na) *0.7)
PCNL_itu_hdu_omit_na_train<-PCNL_itu_hdu_omit_na[PCNL_itu_hdu_omit_na_sample,] %>% as_tibble()
PCNL_itu_hdu_omit_na_test<-PCNL_itu_hdu_omit_na[-PCNL_itu_hdu_omit_na_sample,] %>% as_tibble()
summary(PCNL_itu_hdu_omit_na_train$itu_hdu_admission)

##### Oversampled Dataset
PCNL_itu_hdu_omit_na_train_oversample <-
  ROSE::ovun.sample(itu_hdu_admission ~ ., 
                    data = PCNL_itu_hdu_omit_na_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_itu_hdu_omit_na_train_oversample$itu_hdu_admission)

##### Imputed Dataset
PCNL_itu_hdu_imp_pre <- PCNL_original_itu_hdu %>% drop_na(itu_hdu_admission)
PCNL_itu_hdu_imp1<-mice(PCNL_itu_hdu_imp_pre, m=1)
summary(PCNL_itu_hdu_imp1)

PCNL_itu_hdu_imp<-mice::complete(PCNL_itu_hdu_imp1,1) %>% as_tibble()

PCNL_itu_hdu_imp_sample<-sample(1:nrow(PCNL_itu_hdu_imp), size=nrow(PCNL_itu_hdu_imp) *0.7)
PCNL_itu_hdu_imp_train<-PCNL_itu_hdu_imp[PCNL_itu_hdu_imp_sample,] %>% as_tibble()
PCNL_itu_hdu_imp_test<-PCNL_itu_hdu_imp[-PCNL_itu_hdu_imp_sample,] %>% as_tibble()

summary(PCNL_itu_hdu_imp_train$itu_hdu_admission)


##### Oversampling of Imputed Dataset
PCNL_itu_hdu_imp_train_oversample <-
  ROSE::ovun.sample(itu_hdu_admission ~ ., 
                    data = PCNL_itu_hdu_imp_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_itu_hdu_imp_train_oversample$itu_hdu_admission)

#complete_clearance_on_fluoroscopy

PCNL_original_clearance_on_fluoro <- subset(
  PCNL_original_clearance_on_fluoro,
  select = -c(
    grade_of_main_operating_surgeon,
    type_of_anaesthesia,
    number_of_stones,
    supervised_training_procedure,
    grade_of_performer,
    patient_position,
    placement_of_tract,
    difficult_access,
    predicted_difficulty,
    calyceal_u,
    pre_existing_nephrostomy_tube,
    number_of_tracts_planned
  )
) %>% as_tibble() %>% janitor:: clean_names()

plot_missing(PCNL_original_clearance_on_fluoro)


##### Omit NA Dataset
PCNL_clearance_on_fluoro_omit_na <- na.omit(PCNL_original_clearance_on_fluoro)
str(PCNL_clearance_on_fluoro_omit_na)

PCNL_clearance_on_fluoro_sample<-sample(1:nrow(PCNL_original_clearance_on_fluoro), size=nrow(PCNL_original_clearance_on_fluoro) *0.7)
PCNL_clearance_on_fluoro_train<-PCNL_original_clearance_on_fluoro[PCNL_clearance_on_fluoro_sample,] %>% as_tibble()
PCNL_clearance_on_fluoro_test<-PCNL_original_clearance_on_fluoro[-PCNL_clearance_on_fluoro_sample,] %>% as_tibble()

PCNL_clearance_on_fluoro_omit_na_sample<-sample(1:nrow(PCNL_clearance_on_fluoro_omit_na), size=nrow(PCNL_clearance_on_fluoro_omit_na) *0.7)
PCNL_clearance_on_fluoro_omit_na_train<-PCNL_clearance_on_fluoro_omit_na[PCNL_clearance_on_fluoro_omit_na_sample,] %>% as_tibble()
PCNL_clearance_on_fluoro_omit_na_test<-PCNL_clearance_on_fluoro_omit_na[-PCNL_clearance_on_fluoro_omit_na_sample,] %>% as_tibble()
summary(PCNL_clearance_on_fluoro_omit_na_train$complete_clearance_on_fluoroscopy)

##### Oversampled Dataset
PCNL_clearance_on_fluoro_omit_na_train_oversample <-
  ROSE::ovun.sample(complete_clearance_on_fluoroscopy ~ ., 
                    data = PCNL_clearance_on_fluoro_omit_na_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_clearance_on_fluoro_omit_na_train_oversample$complete_clearance_on_fluoroscopy)

##### Imputed Dataset
PCNL_clearance_on_fluoro_imp_pre <- PCNL_original_clearance_on_fluoro %>% drop_na(complete_clearance_on_fluoroscopy)
PCNL_clearance_on_fluoro_imp1<-mice(PCNL_clearance_on_fluoro_imp_pre, m=1)
summary(PCNL_clearance_on_fluoro_imp1)

PCNL_clearance_on_fluoro_imp<-mice::complete(PCNL_clearance_on_fluoro_imp1,1) %>% as_tibble()

PCNL_clearance_on_fluoro_imp_sample<-sample(1:nrow(PCNL_clearance_on_fluoro_imp), size=nrow(PCNL_clearance_on_fluoro_imp) *0.7)
PCNL_clearance_on_fluoro_imp_train<-PCNL_clearance_on_fluoro_imp[PCNL_clearance_on_fluoro_imp_sample,] %>% as_tibble()
PCNL_clearance_on_fluoro_imp_test<-PCNL_clearance_on_fluoro_imp[-PCNL_clearance_on_fluoro_imp_sample,] %>% as_tibble()

summary(PCNL_clearance_on_fluoro_imp_train$complete_clearance_on_fluoroscopy)


##### Oversampling of Imputed Dataset
PCNL_clearance_on_fluoro_imp_train_oversample <-
  ROSE::ovun.sample(complete_clearance_on_fluoroscopy ~ ., 
                    data = PCNL_clearance_on_fluoro_imp_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_clearance_on_fluoro_imp_train_oversample$complete_clearance_on_fluoroscopy)

#visceral_injury
PCNL_original_visc_inj <- subset(
  PCNL_original_visc_inj,
  select = -c(
    grade_of_main_operating_surgeon,
    type_of_anaesthesia,
    number_of_stones,
    supervised_training_procedure,
    grade_of_performer,
    patient_position,
    placement_of_tract,
    difficult_access,
    predicted_difficulty,
    calyceal_u,
    pre_existing_nephrostomy_tube,
    number_of_tracts_planned
  )
) %>% as_tibble() %>% janitor:: clean_names()

plot_missing(PCNL_original_visc_inj)


##### Omit NA Dataset
PCNL_visc_inj_omit_na <- na.omit(PCNL_original_visc_inj)
str(PCNL_visc_inj_omit_na)

PCNL_visc_inj_sample<-sample(1:nrow(PCNL_original_visc_inj), size=nrow(PCNL_original_visc_inj) *0.7)
PCNL_visc_inj_train<-PCNL_original_visc_inj[PCNL_visc_inj_sample,] %>% as_tibble()
PCNL_visc_inj_test<-PCNL_original_visc_inj[-PCNL_visc_inj_sample,] %>% as_tibble()

PCNL_visc_inj_omit_na_sample<-sample(1:nrow(PCNL_visc_inj_omit_na), size=nrow(PCNL_visc_inj_omit_na) *0.7)
PCNL_visc_inj_omit_na_train<-PCNL_visc_inj_omit_na[PCNL_visc_inj_omit_na_sample,] %>% as_tibble()
PCNL_visc_inj_omit_na_test<-PCNL_visc_inj_omit_na[-PCNL_visc_inj_omit_na_sample,] %>% as_tibble()
summary(PCNL_visc_inj_omit_na_train$visceral_injury)

##### Oversampled Dataset
PCNL_visc_inj_omit_na_train_oversample <-
  ROSE::ovun.sample(visceral_injury ~ ., 
                    data = PCNL_visc_inj_omit_na_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_visc_inj_omit_na_train_oversample$visceral_injury)

##### Imputed Dataset
PCNL_visc_inj_imp_pre <- PCNL_original_visc_inj %>% drop_na(visceral_injury)
PCNL_visc_inj_imp1<-mice(PCNL_visc_inj_imp_pre, m=1)
summary(PCNL_visc_inj_imp1)

PCNL_visc_inj_imp<-mice::complete(PCNL_visc_inj_imp1,1) %>% as_tibble()

PCNL_visc_inj_imp_sample<-sample(1:nrow(PCNL_visc_inj_imp), size=nrow(PCNL_visc_inj_imp) *0.7)
PCNL_visc_inj_imp_train<-PCNL_visc_inj_imp[PCNL_visc_inj_imp_sample,] %>% as_tibble()
PCNL_visc_inj_imp_test<-PCNL_visc_inj_imp[-PCNL_visc_inj_imp_sample,] %>% as_tibble()

summary(PCNL_visc_inj_imp_train$visceral_injury)


##### Oversampling of Imputed Dataset
PCNL_visc_inj_imp_train_oversample <-
  ROSE::ovun.sample(visceral_injury ~ ., 
                    data = PCNL_visc_inj_imp_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_visc_inj_imp_train_oversample$visceral_injury)


#clearance_on_post_operative_radiological_imaging_during_a
PCNL_original_clearance_during_admission <- subset(
  PCNL_original_clearance_during_admission,
  select = -c(
    grade_of_main_operating_surgeon,
    type_of_anaesthesia,
    number_of_stones,
    supervised_training_procedure,
    grade_of_performer,
    patient_position,
    placement_of_tract,
    difficult_access,
    predicted_difficulty,
    calyceal_u,
    pre_existing_nephrostomy_tube,
    number_of_tracts_planned
  )
) %>% as_tibble() %>% janitor:: clean_names() 

plot_missing(PCNL_original_clearance_during_admission)
summary(PCNL_original_clearance_during_admission$clearance_on_post_operative_radiological_imaging_during_a)

clearance_list <-
  split(
    PCNL_original_clearance_during_admission,
    PCNL_original_clearance_during_admission$clearance_on_post_operative_radiological_imaging_during_a
  )

PCNL_original_clearance_during_admission <- rbind(clearance_list$Yes,
                                                  clearance_list$No)
PCNL_original_clearance_during_admission$clearance_on_post_operative_radiological_imaging_during_a <- factor(PCNL_original_clearance_during_admission$clearance_on_post_operative_radiological_imaging_during_a,
                                                                                                             levels = c("No",
                                                                                                                        "Yes"))

##### Omit NA Dataset
PCNL_clearance_during_admission_omit_na <- na.omit(PCNL_original_clearance_during_admission)
str(PCNL_clearance_during_admission_omit_na)

PCNL_clearance_during_admission_sample<-sample(1:nrow(PCNL_original_clearance_during_admission), size=nrow(PCNL_original_clearance_during_admission) *0.7)
PCNL_clearance_during_admission_train<-PCNL_original_clearance_during_admission[PCNL_clearance_during_admission_sample,] %>% as_tibble()
PCNL_clearance_during_admission_test<-PCNL_original_clearance_during_admission[-PCNL_clearance_during_admission_sample,] %>% as_tibble()

PCNL_clearance_during_admission_omit_na_sample<-sample(1:nrow(PCNL_clearance_during_admission_omit_na), size=nrow(PCNL_clearance_during_admission_omit_na) *0.7)
PCNL_clearance_during_admission_omit_na_train<-PCNL_clearance_during_admission_omit_na[PCNL_clearance_during_admission_omit_na_sample,] %>% as_tibble()
PCNL_clearance_during_admission_omit_na_test<-PCNL_clearance_during_admission_omit_na[-PCNL_clearance_during_admission_omit_na_sample,] %>% as_tibble()
summary(PCNL_clearance_during_admission_omit_na_train$clearance_on_post_operative_radiological_imaging_during_a)

##### Oversampled Dataset
PCNL_clearance_during_admission_omit_na_train_oversample <-
  ROSE::ovun.sample(clearance_on_post_operative_radiological_imaging_during_a ~ ., 
                    data = PCNL_clearance_during_admission_omit_na_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_clearance_during_admission_omit_na_train_oversample$clearance_on_post_operative_radiological_imaging_during_a)

##### Imputed Dataset
PCNL_clearance_during_admission_imp_pre <- PCNL_original_clearance_during_admission %>% drop_na(clearance_on_post_operative_radiological_imaging_during_a)
PCNL_clearance_during_admission_imp1<-mice(PCNL_clearance_during_admission_imp_pre, m=1)
summary(PCNL_clearance_during_admission_imp1)

PCNL_clearance_during_admission_imp<-mice::complete(PCNL_clearance_during_admission_imp1,1) %>% as_tibble()

PCNL_clearance_during_admission_imp_sample<-sample(1:nrow(PCNL_clearance_during_admission_imp), size=nrow(PCNL_clearance_during_admission_imp) *0.7)
PCNL_clearance_during_admission_imp_train<-PCNL_clearance_during_admission_imp[PCNL_clearance_during_admission_imp_sample,] %>% as_tibble()
PCNL_clearance_during_admission_imp_test<-PCNL_clearance_during_admission_imp[-PCNL_clearance_during_admission_imp_sample,] %>% as_tibble()

summary(PCNL_clearance_during_admission_imp_train$clearance_on_post_operative_radiological_imaging_during_a)


##### Oversampling of Imputed Dataset
PCNL_clearance_during_admission_imp_train_oversample <-
  ROSE::ovun.sample(clearance_on_post_operative_radiological_imaging_during_a ~ ., 
                    data = PCNL_clearance_during_admission_imp_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_clearance_during_admission_imp_train_oversample$clearance_on_post_operative_radiological_imaging_during_a)


#patient_statusdischarge
PCNL_original_death <- subset(
  PCNL_original_death,
  select = -c(
    grade_of_main_operating_surgeon,
    type_of_anaesthesia,
    number_of_stones,
    supervised_training_procedure,
    grade_of_performer,
    patient_position,
    placement_of_tract,
    difficult_access,
    predicted_difficulty,
    calyceal_u,
    pre_existing_nephrostomy_tube,
    number_of_tracts_planned
  )
) %>% as_tibble() %>% janitor:: clean_names()

plot_missing(PCNL_original_death)

##### Omit NA Dataset
PCNL_death_omit_na <- na.omit(PCNL_original_death)
str(PCNL_death_omit_na)

PCNL_original_death_sample<-sample(1:nrow(PCNL_original_death), size=nrow(PCNL_original_death) *0.7)
PCNL_original_death_train<-PCNL_original_death[PCNL_original_death_sample,] %>% as_tibble()
PCNL_original_death_test<-PCNL_original_death[-PCNL_original_death_sample,] %>% as_tibble()

PCNL_death_omit_na_sample<-sample(1:nrow(PCNL_death_omit_na), size=nrow(PCNL_death_omit_na) *0.7)
PCNL_death_omit_na_train<-PCNL_death_omit_na[PCNL_death_omit_na_sample,] %>% as_tibble()
PCNL_death_omit_na_test<-PCNL_death_omit_na[-PCNL_death_omit_na_sample,] %>% as_tibble()
summary(PCNL_death_omit_na_train$patient_statusdischarge)

##### Oversampled Dataset
PCNL_death_omit_na_train_oversample <-
  ROSE::ovun.sample(patient_statusdischarge ~ ., 
                    data = PCNL_death_omit_na_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_death_omit_na_train_oversample$patient_statusdischarge)

##### Imputed Dataset
PCNL_death_imp_pre <- PCNL_original_death %>% drop_na(patient_statusdischarge)
PCNL_death_imp1<-mice(PCNL_death_imp_pre, m=1)
summary(PCNL_death_imp1)

PCNL_death_imp<-mice::complete(PCNL_death_imp1,1) %>% as_tibble()

PCNL_death_imp_sample<-sample(1:nrow(PCNL_death_imp), size=nrow(PCNL_death_imp) *0.7)
PCNL_death_imp_train<-PCNL_death_imp[PCNL_death_imp_sample,] %>% as_tibble()
PCNL_death_imp_test<-PCNL_death_imp[-PCNL_death_imp_sample,] %>% as_tibble()

summary(PCNL_death_imp_train$patient_statusdischarge)


##### Oversampling of Imputed Dataset
PCNL_death_imp_train_oversample <-
  ROSE::ovun.sample(patient_statusdischarge ~ ., 
                    data = PCNL_death_imp_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_death_imp_train_oversample$patient_statusdischarge)
PCNL_death_imp_train_oversample$patient_statusdischarge <- as.factor(PCNL_death_imp_train_oversample$patient_statusdischarge)

#postop_complications
PCNL_original_post_op_comp <- subset(
  PCNL_original_post_op_comp,
  select = -c(
    grade_of_main_operating_surgeon,
    type_of_anaesthesia,
    number_of_stones,
    supervised_training_procedure,
    grade_of_performer,
    patient_position,
    placement_of_tract,
    difficult_access,
    predicted_difficulty,
    calyceal_u,
    pre_existing_nephrostomy_tube,
    number_of_tracts_planned
  )
) %>% as_tibble() %>% janitor:: clean_names()

plot_missing(PCNL_original_post_op_comp)


##### Omit NA Dataset
PCNL_post_op_comp_omit_na <- na.omit(PCNL_original_post_op_comp)
str(PCNL_post_op_comp_omit_na)

PCNL_original_post_op_comp_sample<-sample(1:nrow(PCNL_original_post_op_comp), size=nrow(PCNL_original_post_op_comp) *0.7)
PCNL_original_post_op_comp_train<-PCNL_original_post_op_comp[PCNL_original_post_op_comp_sample,] %>% as_tibble()
PCNL_original_post_op_comp_test<-PCNL_original_post_op_comp[-PCNL_original_post_op_comp_sample,] %>% as_tibble()

PCNL_post_op_comp_omit_na_sample<-sample(1:nrow(PCNL_post_op_comp_omit_na), size=nrow(PCNL_post_op_comp_omit_na) *0.7)
PCNL_post_op_comp_omit_na_train<-PCNL_post_op_comp_omit_na[PCNL_post_op_comp_omit_na_sample,] %>% as_tibble()
PCNL_post_op_comp_omit_na_test<-PCNL_post_op_comp_omit_na[-PCNL_post_op_comp_omit_na_sample,] %>% as_tibble()
summary(PCNL_post_op_comp_omit_na_train$postop_complications)

##### Oversampled Dataset
PCNL_post_op_comp_omit_na_train_oversample <-
  ROSE::ovun.sample(postop_complications ~ ., 
                    data = PCNL_post_op_comp_omit_na_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_post_op_comp_omit_na_train_oversample$postop_complications)

##### Imputed Dataset
PCNL_post_op_comp_imp_pre <- PCNL_original_post_op_comp %>% drop_na(postop_complications)
PCNL_post_op_comp_imp1<-mice(PCNL_post_op_comp_imp_pre, m=1)
summary(PCNL_post_op_comp_imp1)

PCNL_post_op_comp_imp<-mice::complete(PCNL_post_op_comp_imp1,1) %>% as_tibble()

PCNL_post_op_comp_imp_sample<-sample(1:nrow(PCNL_post_op_comp_imp), size=nrow(PCNL_post_op_comp_imp) *0.7)
PCNL_post_op_comp_imp_train<-PCNL_post_op_comp_imp[PCNL_post_op_comp_imp_sample,] %>% as_tibble()
PCNL_post_op_comp_imp_test<-PCNL_post_op_comp_imp[-PCNL_post_op_comp_imp_sample,] %>% as_tibble()

summary(PCNL_post_op_comp_imp_train$postop_complications)


##### Oversampling of Imputed Dataset
PCNL_post_op_comp_imp_train_oversample <-
  ROSE::ovun.sample(postop_complications ~ ., 
                    data = PCNL_post_op_comp_imp_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_post_op_comp_imp_train_oversample$postop_complications)

#stone_free_at_follow_up
PCNL_original_sf_at_fu <- subset(
  PCNL_original_sf_at_fu,
  select = -c(
    grade_of_main_operating_surgeon,
    type_of_anaesthesia,
    number_of_stones,
    supervised_training_procedure,
    grade_of_performer,
    patient_position,
    placement_of_tract,
    difficult_access,
    predicted_difficulty,
    calyceal_u,
    pre_existing_nephrostomy_tube,
    number_of_tracts_planned
  )
) %>% as_tibble() %>% janitor:: clean_names()

plot_missing(PCNL_original_sf_at_fu)


##### Omit NA Dataset
PCNL_sf_at_fu_omit_na <- na.omit(PCNL_original_sf_at_fu)
str(PCNL_sf_at_fu_omit_na)

PCNL_original_sf_at_fu_sample<-sample(1:nrow(PCNL_original_sf_at_fu), size=nrow(PCNL_original_sf_at_fu) *0.7)
PCNL_original_sf_at_fu_train<-PCNL_original_sf_at_fu[PCNL_original_sf_at_fu_sample,] %>% as_tibble()
PCNL_original_sf_at_fu_test<-PCNL_original_sf_at_fu[-PCNL_original_sf_at_fu_sample,] %>% as_tibble()

PCNL_sf_at_fu_omit_na_sample<-sample(1:nrow(PCNL_sf_at_fu_omit_na), size=nrow(PCNL_sf_at_fu_omit_na) *0.7)
PCNL_sf_at_fu_omit_na_train<-PCNL_sf_at_fu_omit_na[PCNL_sf_at_fu_omit_na_sample,] %>% as_tibble()
PCNL_sf_at_fu_omit_na_test<-PCNL_sf_at_fu_omit_na[-PCNL_sf_at_fu_omit_na_sample,] %>% as_tibble()
summary(PCNL_sf_at_fu_omit_na_train$stone_free_at_follow_up)

##### Oversampled Dataset
PCNL_sf_at_fu_omit_na_train_oversample <-
  ROSE::ovun.sample(stone_free_at_follow_up ~ ., 
                    data = PCNL_sf_at_fu_omit_na_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_sf_at_fu_omit_na_train_oversample$stone_free_at_follow_up)

##### Imputed Dataset
PCNL_sf_at_fu_imp_pre <- PCNL_original_sf_at_fu %>% drop_na(stone_free_at_follow_up)
PCNL_sf_at_fu_imp1<-mice(PCNL_sf_at_fu_imp_pre, m=1)
summary(PCNL_sf_at_fu_imp1)

PCNL_sf_at_fu_imp<-mice::complete(PCNL_sf_at_fu_imp1,1) %>% as_tibble()

PCNL_sf_at_fu_imp_sample<-sample(1:nrow(PCNL_sf_at_fu_imp), size=nrow(PCNL_sf_at_fu_imp) *0.7)
PCNL_sf_at_fu_imp_train<-PCNL_sf_at_fu_imp[PCNL_sf_at_fu_imp_sample,] %>% as_tibble()
PCNL_sf_at_fu_imp_test<-PCNL_sf_at_fu_imp[-PCNL_sf_at_fu_imp_sample,] %>% as_tibble()

summary(PCNL_sf_at_fu_imp_train$stone_free_at_follow_up)


##### Oversampling of Imputed Dataset
PCNL_sf_at_fu_imp_train_oversample <-
  ROSE::ovun.sample(stone_free_at_follow_up ~ ., 
                    data = PCNL_sf_at_fu_imp_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_sf_at_fu_imp_train_oversample$stone_free_at_follow_up)


#adjuvant_treatment
PCNL_original_adj_rx <- subset(
  PCNL_original_adj_rx,
  select = -c(
    grade_of_main_operating_surgeon,
    type_of_anaesthesia,
    number_of_stones,
    supervised_training_procedure,
    grade_of_performer,
    patient_position,
    placement_of_tract,
    difficult_access,
    predicted_difficulty,
    calyceal_u,
    pre_existing_nephrostomy_tube,
    number_of_tracts_planned
  )
) %>% as_tibble() %>% janitor:: clean_names()

plot_missing(PCNL_original_adj_rx)


##### Omit NA Dataset
PCNL_adj_rx_omit_na <- na.omit(PCNL_original_adj_rx)
str(PCNL_adj_rx_omit_na)

PCNL_original_adj_rx_sample<-sample(1:nrow(PCNL_original_adj_rx), size=nrow(PCNL_original_adj_rx) *0.7)
PCNL_original_adj_rx_train<-PCNL_original_adj_rx[PCNL_original_adj_rx_sample,] %>% as_tibble()
PCNL_original_adj_rx_test<-PCNL_original_adj_rx[-PCNL_original_adj_rx_sample,] %>% as_tibble()

PCNL_adj_rx_omit_na_sample<-sample(1:nrow(PCNL_adj_rx_omit_na), size=nrow(PCNL_adj_rx_omit_na) *0.7)
PCNL_adj_rx_omit_na_train<-PCNL_adj_rx_omit_na[PCNL_adj_rx_omit_na_sample,] %>% as_tibble()
PCNL_adj_rx_omit_na_test<-PCNL_adj_rx_omit_na[-PCNL_adj_rx_omit_na_sample,] %>% as_tibble()
summary(PCNL_adj_rx_omit_na_train$adjuvant_treatment)

##### Oversampled Dataset
PCNL_adj_rx_omit_na_train_oversample <-
  ROSE::ovun.sample(adjuvant_treatment ~ ., 
                    data = PCNL_adj_rx_omit_na_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_adj_rx_omit_na_train_oversample$adjuvant_treatment)

##### Imputed Dataset
PCNL_adj_rx_imp_pre <- PCNL_original_adj_rx %>% drop_na(adjuvant_treatment)
PCNL_adj_rx_imp1<-mice(PCNL_adj_rx_imp_pre, m=1)
summary(PCNL_sf_at_fu_imp1)

PCNL_adj_rx_imp<-mice::complete(PCNL_adj_rx_imp1,1) %>% as_tibble()

PCNL_adj_rx_imp_sample<-sample(1:nrow(PCNL_adj_rx_imp), size=nrow(PCNL_adj_rx_imp) *0.7)
PCNL_adj_rx_imp_train<-PCNL_adj_rx_imp[PCNL_adj_rx_imp_sample,] %>% as_tibble()
PCNL_adj_rx_imp_test<-PCNL_adj_rx_imp[-PCNL_adj_rx_imp_sample,] %>% as_tibble()

summary(PCNL_adj_rx_imp_train$adjuvant_treatment)


##### Oversampling of Imputed Dataset
PCNL_adj_rx_imp_train_oversample <-
  ROSE::ovun.sample(adjuvant_treatment ~ ., 
                    data = PCNL_adj_rx_imp_train, 
                    seed = 1234,
                    method = "over")$data
summary(PCNL_adj_rx_imp_train_oversample$adjuvant_treatment)



