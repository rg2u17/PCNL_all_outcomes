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
) %>% as_tibble() %>% janitor::clean_names()

plot_missing(PCNL_single_infection)


##### Omit NA Dataset
PCNL_single_infection_omit_na <- na.omit(PCNL_single_infection)
str(PCNL_single_infection_omit_na)

PCNL_single_infection_sample <-
  sample(1:nrow(PCNL_single_infection),
         size = nrow(PCNL_single_infection) * 0.7)
PCNL_single_infection_train <-
  PCNL_single_infection[PCNL_single_infection_sample,] %>% as_tibble()
PCNL_single_infection_test <-
  PCNL_single_infection[-PCNL_single_infection_sample,] %>% as_tibble()

PCNL_single_infection_omit_na_sample <-
  sample(1:nrow(PCNL_single_infection_omit_na),
         size = nrow(PCNL_single_infection_omit_na) * 0.7)
PCNL_single_infection_omit_na_train <-
  PCNL_single_infection_omit_na[PCNL_single_infection_omit_na_sample,] %>% as_tibble()
PCNL_single_infection_omit_na_test <-
  PCNL_single_infection_omit_na[-PCNL_single_infection_omit_na_sample,] %>% as_tibble()
summary(PCNL_single_infection_omit_na_train$single_infection_outcome)

PCNL_single_infection_omit_na_train_outcome <-
  subset(PCNL_single_infection_omit_na_train,
         select = single_infection_outcome)
PCNL_single_infection_omit_na_train_outcome$single_infection_outcome <-
  as.numeric(PCNL_single_infection_omit_na_train_outcome$single_infection_outcome)
PCNL_single_infection_omit_na_train_outcome$single_infection_outcome <-
  ifelse(PCNL_single_infection_omit_na_train_outcome$single_infection_outcome == 2,
         1,
         0)
PCNL_single_infection_omit_na_train_outcome <-
  as.matrix(PCNL_single_infection_omit_na_train_outcome)
PCNL_single_infection_omit_na_test_outcome <-
  subset(PCNL_single_infection_omit_na_test,
         select = single_infection_outcome)
PCNL_single_infection_omit_na_test_outcome$single_infection_outcome <-
  as.numeric(PCNL_single_infection_omit_na_test_outcome$single_infection_outcome)
PCNL_single_infection_omit_na_test_outcome$single_infection_outcome <-
  ifelse(PCNL_single_infection_omit_na_test_outcome$single_infection_outcome == 2,
         1,
         0)
PCNL_single_infection_omit_na_test_outcome <-
  as.matrix(PCNL_single_infection_omit_na_test_outcome)

PCNL_single_infection_omit_na_train_predictors <-
  subset(PCNL_single_infection_omit_na_train,
         select = -single_infection_outcome)
PCNL_single_infection_omit_na_test_predictors <-
  subset(PCNL_single_infection_omit_na_test,
         select = -single_infection_outcome)

PCNL_single_infection_omit_na_train_predictors2 <-
  PCNL_single_infection_omit_na_train_predictors
PCNL_single_infection_omit_na_train_predictors2$stone_complexity <-
  as.integer(PCNL_single_infection_omit_na_train_predictors2$stone_complexity)
PCNL_single_infection_omit_na_train_predictors2$stone_complexity <-
  as.factor(PCNL_single_infection_omit_na_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_single_infection_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_single_infection_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_single_infection_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_single_infection_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_single_infection_omit_na_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_single_infection_omit_na_train_predictors2$pre_operative_msu_result)
PCNL_single_infection_omit_na_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_single_infection_omit_na_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_single_infection_omit_na_train_predictors2$stone_dimensions <-
  as.integer(PCNL_single_infection_omit_na_train_predictors2$stone_dimensions)
PCNL_single_infection_omit_na_train_predictors2$stone_dimensions <-
  as.factor(PCNL_single_infection_omit_na_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_single_infection_omit_na_train_predictors2$puncture_site <-
  as.integer(PCNL_single_infection_omit_na_train_predictors2$puncture_site)
PCNL_single_infection_omit_na_train_predictors2$puncture_site <-
  as.factor(PCNL_single_infection_omit_na_train_predictors2$puncture_site) %>% to_categorical()
PCNL_single_infection_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_single_infection_omit_na_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_single_infection_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_single_infection_omit_na_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_single_infection_omit_na_train_predictors2$renogramdmsa <-
  ifelse(PCNL_single_infection_omit_na_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_single_infection_omit_na_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_train_predictors2$calyceal_l <-
  ifelse(PCNL_single_infection_omit_na_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_train_predictors2$calyceal_m <-
  ifelse(PCNL_single_infection_omit_na_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_train_predictors2$complete_staghorn <-
  ifelse(PCNL_single_infection_omit_na_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_train_predictors2$partial_staghorn <-
  ifelse(PCNL_single_infection_omit_na_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_train_predictors2$pelvic <-
  ifelse(PCNL_single_infection_omit_na_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_train_predictors2$upper_ureteric <-
  ifelse(PCNL_single_infection_omit_na_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_train_predictors2$ureter_other <-
  ifelse(PCNL_single_infection_omit_na_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



single_infection_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_single_infection_omit_na_train_predictors2$stone_complexity)
colnames(single_infection_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
single_infection_na_omit_train_predictors_stone_complexity <-
  subset(single_infection_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_single_infection_omit_na_train_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_train_predictors2,
    single_infection_na_omit_train_predictors_stone_complexity
  )

single_infection_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_single_infection_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(single_infection_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
single_infection_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    single_infection_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_single_infection_omit_na_train_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_train_predictors2,
    single_infection_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

single_infection_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_single_infection_omit_na_train_predictors2$pre_operative_msu_result)
colnames(single_infection_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
single_infection_na_omit_train_predictors_pre_operative_msu_result <-
  subset(single_infection_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_single_infection_omit_na_train_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_train_predictors2,
    single_infection_na_omit_train_predictors_pre_operative_msu_result
  )

single_infection_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_single_infection_omit_na_train_predictors2$puncture_site)
colnames(single_infection_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
single_infection_na_omit_train_predictors_puncture_site <-
  subset(single_infection_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_single_infection_omit_na_train_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_train_predictors2,
    single_infection_na_omit_train_predictors_puncture_site
  )

single_infection_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_single_infection_omit_na_train_predictors2$stone_dimensions)
colnames(single_infection_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
single_infection_na_omit_train_predictors_stone_dimensions <-
  subset(single_infection_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_single_infection_omit_na_train_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_train_predictors2,
    single_infection_na_omit_train_predictors_stone_dimensions
  )

single_infection_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_single_infection_omit_na_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(single_infection_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
single_infection_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    single_infection_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_single_infection_omit_na_train_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_train_predictors2,
    single_infection_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_single_infection_omit_na_train_predictors2 <-
  subset(
    PCNL_single_infection_omit_na_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_single_infection_omit_na_train_predictors2 <-
  as.matrix(PCNL_single_infection_omit_na_train_predictors2)


PCNL_single_infection_omit_na_test_predictors2 <-
  PCNL_single_infection_omit_na_test_predictors
PCNL_single_infection_omit_na_test_predictors2$stone_complexity <-
  as.integer(PCNL_single_infection_omit_na_test_predictors2$stone_complexity)
PCNL_single_infection_omit_na_test_predictors2$stone_complexity <-
  as.factor(PCNL_single_infection_omit_na_test_predictors2$stone_complexity) %>% to_categorical()
PCNL_single_infection_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_single_infection_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_single_infection_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_single_infection_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_single_infection_omit_na_test_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_single_infection_omit_na_test_predictors2$pre_operative_msu_result)
PCNL_single_infection_omit_na_test_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_single_infection_omit_na_test_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_single_infection_omit_na_test_predictors2$stone_dimensions <-
  as.integer(PCNL_single_infection_omit_na_test_predictors2$stone_dimensions)
PCNL_single_infection_omit_na_test_predictors2$stone_dimensions <-
  as.factor(PCNL_single_infection_omit_na_test_predictors2$stone_dimensions) %>% to_categorical()
PCNL_single_infection_omit_na_test_predictors2$puncture_site <-
  as.integer(PCNL_single_infection_omit_na_test_predictors2$puncture_site)
PCNL_single_infection_omit_na_test_predictors2$puncture_site <-
  as.factor(PCNL_single_infection_omit_na_test_predictors2$puncture_site) %>% to_categorical()
PCNL_single_infection_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_single_infection_omit_na_test_predictors2$image_guidance_for_renal_puncture
  )
PCNL_single_infection_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_single_infection_omit_na_test_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_single_infection_omit_na_test_predictors2$renogramdmsa <-
  ifelse(PCNL_single_infection_omit_na_test_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_test_predictors2$calyceal_diverticular <-
  ifelse(PCNL_single_infection_omit_na_test_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_test_predictors2$calyceal_l <-
  ifelse(PCNL_single_infection_omit_na_test_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_test_predictors2$calyceal_m <-
  ifelse(PCNL_single_infection_omit_na_test_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_test_predictors2$complete_staghorn <-
  ifelse(PCNL_single_infection_omit_na_test_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_test_predictors2$partial_staghorn <-
  ifelse(PCNL_single_infection_omit_na_test_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_test_predictors2$pelvic <-
  ifelse(PCNL_single_infection_omit_na_test_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_test_predictors2$upper_ureteric <-
  ifelse(PCNL_single_infection_omit_na_test_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_omit_na_test_predictors2$ureter_other <-
  ifelse(PCNL_single_infection_omit_na_test_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



single_infection_na_omit_test_predictors_stone_complexity <-
  as_tibble(PCNL_single_infection_omit_na_test_predictors2$stone_complexity)
colnames(single_infection_na_omit_test_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
single_infection_na_omit_test_predictors_stone_complexity <-
  subset(single_infection_na_omit_test_predictors_stone_complexity,
         select = -GSS0)
PCNL_single_infection_omit_na_test_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_test_predictors2,
    single_infection_na_omit_test_predictors_stone_complexity
  )

single_infection_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_single_infection_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(single_infection_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
single_infection_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    single_infection_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_single_infection_omit_na_test_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_test_predictors2,
    single_infection_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
  )

single_infection_na_omit_test_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_single_infection_omit_na_test_predictors2$pre_operative_msu_result)
colnames(single_infection_na_omit_test_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
single_infection_na_omit_test_predictors_pre_operative_msu_result <-
  subset(single_infection_na_omit_test_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_single_infection_omit_na_test_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_test_predictors2,
    single_infection_na_omit_test_predictors_pre_operative_msu_result
  )

single_infection_na_omit_test_predictors_puncture_site <-
  as_tibble(PCNL_single_infection_omit_na_test_predictors2$puncture_site)
colnames(single_infection_na_omit_test_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
single_infection_na_omit_test_predictors_puncture_site <-
  subset(single_infection_na_omit_test_predictors_puncture_site,
         select = -puncture0)
PCNL_single_infection_omit_na_test_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_test_predictors2,
    single_infection_na_omit_test_predictors_puncture_site
  )

single_infection_na_omit_test_predictors_stone_dimensions <-
  as_tibble(PCNL_single_infection_omit_na_test_predictors2$stone_dimensions)
colnames(single_infection_na_omit_test_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
single_infection_na_omit_test_predictors_stone_dimensions <-
  subset(single_infection_na_omit_test_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_single_infection_omit_na_test_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_test_predictors2,
    single_infection_na_omit_test_predictors_stone_dimensions
  )

single_infection_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_single_infection_omit_na_test_predictors2$image_guidance_for_renal_puncture
  )
colnames(single_infection_na_omit_test_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
single_infection_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  subset(
    single_infection_na_omit_test_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_single_infection_omit_na_test_predictors2 <-
  cbind(
    PCNL_single_infection_omit_na_test_predictors2,
    single_infection_na_omit_test_predictors_image_guidance_for_renal_puncture
  )

PCNL_single_infection_omit_na_test_predictors2 <-
  subset(
    PCNL_single_infection_omit_na_test_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_single_infection_omit_na_test_predictors2 <-
  as.matrix(PCNL_single_infection_omit_na_test_predictors2)

##### Oversampled Dataset
PCNL_single_infection_omit_na_train_oversample <-
  ROSE::ovun.sample(
    single_infection_outcome ~ .,
    data = PCNL_single_infection_omit_na_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_single_infection_omit_na_train_oversample$single_infection_outcome)

PCNL_single_infection_oversample_train_outcome <-
  subset(PCNL_single_infection_omit_na_train_oversample,
         select = single_infection_outcome)
PCNL_single_infection_oversample_train_outcome$single_infection_outcome <-
  as.numeric(PCNL_single_infection_oversample_train_outcome$single_infection_outcome)
PCNL_single_infection_oversample_train_outcome$single_infection_outcome <-
  ifelse(PCNL_single_infection_oversample_train_outcome$single_infection_outcome == 2,
         1,
         0)
PCNL_single_infection_oversample_train_outcome <-
  as.matrix(PCNL_single_infection_oversample_train_outcome)



PCNL_single_infection_oversample_train_predictors <-
  subset(PCNL_single_infection_omit_na_train_oversample,
         select = -single_infection_outcome)
PCNL_single_infection_oversample_train_predictors2 <-
  PCNL_single_infection_oversample_train_predictors
PCNL_single_infection_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_single_infection_oversample_train_predictors2$stone_complexity)
PCNL_single_infection_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_single_infection_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_single_infection_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_single_infection_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_single_infection_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_single_infection_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_single_infection_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_single_infection_oversample_train_predictors2$pre_operative_msu_result)
PCNL_single_infection_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_single_infection_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_single_infection_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_single_infection_oversample_train_predictors2$stone_dimensions)
PCNL_single_infection_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_single_infection_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_single_infection_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_single_infection_oversample_train_predictors2$puncture_site)
PCNL_single_infection_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_single_infection_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_single_infection_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_single_infection_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_single_infection_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_single_infection_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_single_infection_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_single_infection_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_single_infection_oversample_train_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_single_infection_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_single_infection_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_single_infection_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_single_infection_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_single_infection_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_single_infection_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_single_infection_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_single_infection_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



single_infection_oversample_train_predictors_stone_complexity <-
  as_tibble(PCNL_single_infection_oversample_train_predictors2$stone_complexity)
colnames(single_infection_oversample_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
single_infection_oversample_train_predictors_stone_complexity <-
  subset(single_infection_oversample_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_single_infection_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_oversample_train_predictors2,
    single_infection_oversample_train_predictors_stone_complexity
  )

single_infection_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_single_infection_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(single_infection_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
single_infection_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    single_infection_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_single_infection_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_oversample_train_predictors2,
    single_infection_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

single_infection_oversample_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_single_infection_oversample_train_predictors2$pre_operative_msu_result)
colnames(single_infection_oversample_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
single_infection_oversample_train_predictors_pre_operative_msu_result <-
  subset(single_infection_oversample_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_single_infection_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_oversample_train_predictors2,
    single_infection_oversample_train_predictors_pre_operative_msu_result
  )

single_infection_oversample_train_predictors_puncture_site <-
  as_tibble(PCNL_single_infection_oversample_train_predictors2$puncture_site)
colnames(single_infection_oversample_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
single_infection_oversample_train_predictors_puncture_site <-
  subset(single_infection_oversample_train_predictors_puncture_site,
         select = -puncture0)
PCNL_single_infection_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_oversample_train_predictors2,
    single_infection_oversample_train_predictors_puncture_site
  )

single_infection_oversample_train_predictors_stone_dimensions <-
  as_tibble(PCNL_single_infection_oversample_train_predictors2$stone_dimensions)
colnames(single_infection_oversample_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
single_infection_oversample_train_predictors_stone_dimensions <-
  subset(single_infection_oversample_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_single_infection_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_oversample_train_predictors2,
    single_infection_oversample_train_predictors_stone_dimensions
  )

single_infection_oversample_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_single_infection_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(single_infection_oversample_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
single_infection_oversample_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    single_infection_oversample_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_single_infection_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_oversample_train_predictors2,
    single_infection_oversample_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_single_infection_oversample_train_predictors2 <-
  subset(
    PCNL_single_infection_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_single_infection_oversample_train_predictors2 <-
  as.matrix(PCNL_single_infection_oversample_train_predictors2)

##### Imputed Dataset
PCNL_single_infection_imp_pre <-
  PCNL_single_infection %>% drop_na(single_infection_outcome)
PCNL_single_infection_imp1 <-
  mice(PCNL_single_infection_imp_pre, m = 1)
summary(PCNL_single_infection_imp1)

PCNL_single_infection_imp <-
  mice::complete(PCNL_single_infection_imp1, 1) %>% as_tibble()

PCNL_single_infection_imp_sample <-
  sample(1:nrow(PCNL_single_infection_imp),
         size = nrow(PCNL_single_infection_imp) * 0.7)
PCNL_single_infection_imp_train <-
  PCNL_single_infection_imp[PCNL_single_infection_imp_sample,] %>% as_tibble()
PCNL_single_infection_imp_test <-
  PCNL_single_infection_imp[-PCNL_single_infection_imp_sample,] %>% as_tibble()

PCNL_single_infection_imp_train$single_infection_outcome <-
  as.factor(PCNL_single_infection_imp_train$single_infection_outcome)
PCNL_single_infection_imp_test$single_infection_outcome <-
  as.factor(PCNL_single_infection_imp_test$single_infection_outcome)
summary(PCNL_single_infection_imp_train$single_infection_outcome)

PCNL_single_infection_imp_train_outcome <-
  subset(PCNL_single_infection_imp_train,
         select = single_infection_outcome)
PCNL_single_infection_imp_train_outcome$single_infection_outcome <-
  as.numeric(PCNL_single_infection_imp_train_outcome$single_infection_outcome)
PCNL_single_infection_imp_train_outcome$single_infection_outcome <-
  ifelse(PCNL_single_infection_imp_train_outcome$single_infection_outcome == 2,
         1,
         0)
PCNL_single_infection_imp_train_outcome <-
  as.matrix(PCNL_single_infection_imp_train_outcome)

PCNL_single_infection_imp_train_predictors <-
  subset(PCNL_single_infection_imp_train,
         select = -single_infection_outcome)
PCNL_single_infection_imp_train_predictors2 <-
  PCNL_single_infection_imp_train_predictors
PCNL_single_infection_imp_train_predictors2$stone_complexity <-
  as.integer(PCNL_single_infection_imp_train_predictors2$stone_complexity)
PCNL_single_infection_imp_train_predictors2$stone_complexity <-
  as.factor(PCNL_single_infection_imp_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_single_infection_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_single_infection_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_single_infection_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_single_infection_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_single_infection_imp_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_single_infection_imp_train_predictors2$pre_operative_msu_result)
PCNL_single_infection_imp_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_single_infection_imp_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_single_infection_imp_train_predictors2$stone_dimensions <-
  as.integer(PCNL_single_infection_imp_train_predictors2$stone_dimensions)
PCNL_single_infection_imp_train_predictors2$stone_dimensions <-
  as.factor(PCNL_single_infection_imp_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_single_infection_imp_train_predictors2$puncture_site <-
  as.integer(PCNL_single_infection_imp_train_predictors2$puncture_site)
PCNL_single_infection_imp_train_predictors2$puncture_site <-
  as.factor(PCNL_single_infection_imp_train_predictors2$puncture_site) %>% to_categorical()
PCNL_single_infection_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_single_infection_imp_train_predictors2$image_guidance_for_renal_puncture)
PCNL_single_infection_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_single_infection_imp_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_single_infection_imp_train_predictors2$renogramdmsa <-
  ifelse(PCNL_single_infection_imp_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_single_infection_imp_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_train_predictors2$calyceal_l <-
  ifelse(PCNL_single_infection_imp_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_train_predictors2$calyceal_m <-
  ifelse(PCNL_single_infection_imp_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_train_predictors2$complete_staghorn <-
  ifelse(PCNL_single_infection_imp_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_train_predictors2$partial_staghorn <-
  ifelse(PCNL_single_infection_imp_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_train_predictors2$pelvic <-
  ifelse(PCNL_single_infection_imp_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_train_predictors2$upper_ureteric <-
  ifelse(PCNL_single_infection_imp_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_train_predictors2$ureter_other <-
  ifelse(PCNL_single_infection_imp_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



single_infection_imp_train_predictors_stone_complexity <-
  as_tibble(PCNL_single_infection_imp_train_predictors2$stone_complexity)
colnames(single_infection_imp_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
single_infection_imp_train_predictors_stone_complexity <-
  subset(single_infection_imp_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_single_infection_imp_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_train_predictors2,
    single_infection_imp_train_predictors_stone_complexity
  )

single_infection_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_single_infection_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(single_infection_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
single_infection_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    single_infection_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_single_infection_imp_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_train_predictors2,
    single_infection_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

single_infection_imp_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_single_infection_imp_train_predictors2$pre_operative_msu_result)
colnames(single_infection_imp_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
single_infection_imp_train_predictors_pre_operative_msu_result <-
  subset(single_infection_imp_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_single_infection_imp_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_train_predictors2,
    single_infection_imp_train_predictors_pre_operative_msu_result
  )

single_infection_imp_train_predictors_puncture_site <-
  as_tibble(PCNL_single_infection_imp_train_predictors2$puncture_site)
colnames(single_infection_imp_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
single_infection_imp_train_predictors_puncture_site <-
  subset(single_infection_imp_train_predictors_puncture_site,
         select = -puncture0)
PCNL_single_infection_imp_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_train_predictors2,
    single_infection_imp_train_predictors_puncture_site
  )

single_infection_imp_train_predictors_stone_dimensions <-
  as_tibble(PCNL_single_infection_imp_train_predictors2$stone_dimensions)
colnames(single_infection_imp_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
single_infection_imp_train_predictors_stone_dimensions <-
  subset(single_infection_imp_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_single_infection_imp_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_train_predictors2,
    single_infection_imp_train_predictors_stone_dimensions
  )

single_infection_imp_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_single_infection_imp_train_predictors2$image_guidance_for_renal_puncture)
colnames(single_infection_imp_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
single_infection_imp_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    single_infection_imp_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_single_infection_imp_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_train_predictors2,
    single_infection_imp_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_single_infection_imp_train_predictors2 <-
  subset(
    PCNL_single_infection_imp_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_single_infection_imp_train_predictors2 <-
  as.matrix(PCNL_single_infection_imp_train_predictors2)

##### Oversampling of Imputed Dataset
PCNL_single_infection_imp_oversample_train <-
  ROSE::ovun.sample(
    single_infection_outcome ~ .,
    data = PCNL_single_infection_imp_train,
    seed = 1234,
    method = "over"
  )$data

PCNL_single_infection_imp_oversample_train_outcome <-
  subset(PCNL_single_infection_imp_oversample_train,
         select = single_infection_outcome)
PCNL_single_infection_imp_oversample_train_outcome$single_infection_outcome <-
  as.numeric(PCNL_single_infection_imp_oversample_train_outcome$single_infection_outcome)
PCNL_single_infection_imp_oversample_train_outcome$single_infection_outcome <-
  ifelse(PCNL_single_infection_imp_oversample_train_outcome$single_infection_outcome == 2,
         1,
         0)
PCNL_single_infection_imp_oversample_train_outcome <-
  as.matrix(PCNL_single_infection_imp_oversample_train_outcome)
PCNL_single_infection_imp_oversample_train_predictors <-
  subset(PCNL_single_infection_imp_oversample_train,
         select = -single_infection_outcome)

PCNL_single_infection_imp_oversample_train_predictors2 <-
  PCNL_single_infection_imp_oversample_train_predictors
PCNL_single_infection_imp_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_single_infection_imp_oversample_train_predictors2$stone_complexity)
PCNL_single_infection_imp_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_single_infection_imp_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_single_infection_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_single_infection_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_single_infection_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_single_infection_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_single_infection_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(
    PCNL_single_infection_imp_oversample_train_predictors2$pre_operative_msu_result
  )
PCNL_single_infection_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(
    PCNL_single_infection_imp_oversample_train_predictors2$pre_operative_msu_result
  ) %>% to_categorical()
PCNL_single_infection_imp_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_single_infection_imp_oversample_train_predictors2$stone_dimensions)
PCNL_single_infection_imp_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_single_infection_imp_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_single_infection_imp_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_single_infection_imp_oversample_train_predictors2$puncture_site)
PCNL_single_infection_imp_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_single_infection_imp_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_single_infection_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_single_infection_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_single_infection_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_single_infection_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_single_infection_imp_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_single_infection_imp_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_single_infection_imp_oversample_train_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_single_infection_imp_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_single_infection_imp_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_single_infection_imp_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_oversample_train_predictors2$complete_staghorn <-
  ifelse(
    PCNL_single_infection_imp_oversample_train_predictors2$complete_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_single_infection_imp_oversample_train_predictors2$partial_staghorn <-
  ifelse(
    PCNL_single_infection_imp_oversample_train_predictors2$partial_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_single_infection_imp_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_single_infection_imp_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_single_infection_imp_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_single_infection_imp_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_single_infection_imp_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



single_infection_imp_oversample_train_predictors_stone_complexity <-
  as_tibble(PCNL_single_infection_imp_oversample_train_predictors2$stone_complexity)
colnames(single_infection_imp_oversample_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
single_infection_imp_oversample_train_predictors_stone_complexity <-
  subset(single_infection_imp_oversample_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_single_infection_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_oversample_train_predictors2,
    single_infection_imp_oversample_train_predictors_stone_complexity
  )

single_infection_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_single_infection_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  single_infection_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
) <- c("size0",
       "size1",
       "size2",
       "size3",
       "size4",
       "size5",
       "size6",
       "size7")
single_infection_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    single_infection_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_single_infection_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_oversample_train_predictors2,
    single_infection_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

single_infection_imp_oversample_train_predictors_pre_operative_msu_result <-
  as_tibble(
    PCNL_single_infection_imp_oversample_train_predictors2$pre_operative_msu_result
  )
colnames(single_infection_imp_oversample_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
single_infection_imp_oversample_train_predictors_pre_operative_msu_result <-
  subset(
    single_infection_imp_oversample_train_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_single_infection_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_oversample_train_predictors2,
    single_infection_imp_oversample_train_predictors_pre_operative_msu_result
  )

single_infection_imp_oversample_train_predictors_puncture_site <-
  as_tibble(PCNL_single_infection_imp_oversample_train_predictors2$puncture_site)
colnames(single_infection_imp_oversample_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
single_infection_imp_oversample_train_predictors_puncture_site <-
  subset(
    single_infection_imp_oversample_train_predictors_puncture_site,
    select = -puncture0
  )
PCNL_single_infection_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_oversample_train_predictors2,
    single_infection_imp_oversample_train_predictors_puncture_site
  )

single_infection_imp_oversample_train_predictors_stone_dimensions <-
  as_tibble(PCNL_single_infection_imp_oversample_train_predictors2$stone_dimensions)
colnames(single_infection_imp_oversample_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
single_infection_imp_oversample_train_predictors_stone_dimensions <-
  subset(
    single_infection_imp_oversample_train_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_single_infection_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_oversample_train_predictors2,
    single_infection_imp_oversample_train_predictors_stone_dimensions
  )

single_infection_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_single_infection_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(
  single_infection_imp_oversample_train_predictors_image_guidance_for_renal_puncture
) <- c("image0",
       "image1",
       "image2",
       "image3",
       "image4")
single_infection_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    single_infection_imp_oversample_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_single_infection_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_single_infection_imp_oversample_train_predictors2,
    single_infection_imp_oversample_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_single_infection_imp_oversample_train_predictors2 <-
  subset(
    PCNL_single_infection_imp_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_single_infection_imp_oversample_train_predictors2 <-
  as.matrix(PCNL_single_infection_imp_oversample_train_predictors2)


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
) %>% as_tibble() %>% janitor::clean_names()

plot_missing(PCNL_original_transfusion)


##### Omit NA Dataset
PCNL_transfusion_omit_na <- na.omit(PCNL_original_transfusion)
str(PCNL_transfusion_omit_na)

PCNL_transfusion_sample <-
  sample(1:nrow(PCNL_original_transfusion),
         size = nrow(PCNL_original_transfusion) * 0.7)
PCNL_transfusion_train <-
  PCNL_original_transfusion[PCNL_transfusion_sample,] %>% as_tibble()
PCNL_transfusion_test <-
  PCNL_original_transfusion[-PCNL_transfusion_sample,] %>% as_tibble()

PCNL_transfusion_omit_na_sample <-
  sample(1:nrow(PCNL_transfusion_omit_na),
         size = nrow(PCNL_transfusion_omit_na) * 0.7)
PCNL_transfusion_omit_na_train <-
  PCNL_transfusion_omit_na[PCNL_transfusion_omit_na_sample,] %>% as_tibble()
PCNL_transfusion_omit_na_test <-
  PCNL_transfusion_omit_na[-PCNL_transfusion_omit_na_sample,] %>% as_tibble()
summary(PCNL_transfusion_omit_na_train$blood_transfusion)

PCNL_transfusion_omit_na_train_outcome <-
  subset(PCNL_transfusion_omit_na_train,
         select = blood_transfusion)
PCNL_transfusion_omit_na_train_outcome$blood_transfusion <-
  as.numeric(PCNL_transfusion_omit_na_train_outcome$blood_transfusion)
PCNL_transfusion_omit_na_train_outcome$blood_transfusion <-
  ifelse(PCNL_transfusion_omit_na_train_outcome$blood_transfusion == 2,
         1,
         0)
PCNL_transfusion_omit_na_train_outcome <-
  as.matrix(PCNL_transfusion_omit_na_train_outcome)
PCNL_transfusion_omit_na_test_outcome <-
  subset(PCNL_transfusion_omit_na_test,
         select = blood_transfusion)
PCNL_transfusion_omit_na_test_outcome$blood_transfusion <-
  as.numeric(PCNL_transfusion_omit_na_test_outcome$blood_transfusion)
PCNL_transfusion_omit_na_test_outcome$blood_transfusion <-
  ifelse(PCNL_transfusion_omit_na_test_outcome$blood_transfusion == 2,
         1,
         0)
PCNL_transfusion_omit_na_test_outcome <-
  as.matrix(PCNL_transfusion_omit_na_test_outcome)
PCNL_transfusion_omit_na_train_predictors <-
  subset(PCNL_transfusion_omit_na_train,
         select = -blood_transfusion)
PCNL_transfusion_omit_na_test_predictors <-
  subset(PCNL_transfusion_omit_na_test,
         select = -blood_transfusion)

PCNL_transfusion_omit_na_train_predictors2 <-
  PCNL_transfusion_omit_na_train_predictors
PCNL_transfusion_omit_na_train_predictors2$stone_complexity <-
  as.integer(PCNL_transfusion_omit_na_train_predictors2$stone_complexity)
PCNL_transfusion_omit_na_train_predictors2$stone_complexity <-
  as.factor(PCNL_transfusion_omit_na_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_transfusion_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_transfusion_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_transfusion_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_transfusion_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_transfusion_omit_na_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_transfusion_omit_na_train_predictors2$pre_operative_msu_result)
PCNL_transfusion_omit_na_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_transfusion_omit_na_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_transfusion_omit_na_train_predictors2$stone_dimensions <-
  as.integer(PCNL_transfusion_omit_na_train_predictors2$stone_dimensions)
PCNL_transfusion_omit_na_train_predictors2$stone_dimensions <-
  as.factor(PCNL_transfusion_omit_na_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_transfusion_omit_na_train_predictors2$puncture_site <-
  as.integer(PCNL_transfusion_omit_na_train_predictors2$puncture_site)
PCNL_transfusion_omit_na_train_predictors2$puncture_site <-
  as.factor(PCNL_transfusion_omit_na_train_predictors2$puncture_site) %>% to_categorical()
PCNL_transfusion_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_transfusion_omit_na_train_predictors2$image_guidance_for_renal_puncture)
PCNL_transfusion_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_transfusion_omit_na_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_transfusion_omit_na_train_predictors2$renogramdmsa <-
  ifelse(PCNL_transfusion_omit_na_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_transfusion_omit_na_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_train_predictors2$calyceal_l <-
  ifelse(PCNL_transfusion_omit_na_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_train_predictors2$calyceal_m <-
  ifelse(PCNL_transfusion_omit_na_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_train_predictors2$complete_staghorn <-
  ifelse(PCNL_transfusion_omit_na_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_train_predictors2$partial_staghorn <-
  ifelse(PCNL_transfusion_omit_na_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_train_predictors2$pelvic <-
  ifelse(PCNL_transfusion_omit_na_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_train_predictors2$upper_ureteric <-
  ifelse(PCNL_transfusion_omit_na_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_train_predictors2$ureter_other <-
  ifelse(PCNL_transfusion_omit_na_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



transfusion_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_transfusion_omit_na_train_predictors2$stone_complexity)
colnames(transfusion_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
transfusion_na_omit_train_predictors_stone_complexity <-
  subset(transfusion_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_transfusion_omit_na_train_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_train_predictors2,
    transfusion_na_omit_train_predictors_stone_complexity
  )

transfusion_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_transfusion_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(transfusion_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
transfusion_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    transfusion_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_transfusion_omit_na_train_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_train_predictors2,
    transfusion_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

transfusion_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_transfusion_omit_na_train_predictors2$pre_operative_msu_result)
colnames(transfusion_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
transfusion_na_omit_train_predictors_pre_operative_msu_result <-
  subset(transfusion_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_transfusion_omit_na_train_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_train_predictors2,
    transfusion_na_omit_train_predictors_pre_operative_msu_result
  )

transfusion_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_transfusion_omit_na_train_predictors2$puncture_site)
colnames(transfusion_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
transfusion_na_omit_train_predictors_puncture_site <-
  subset(transfusion_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_transfusion_omit_na_train_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_train_predictors2,
    transfusion_na_omit_train_predictors_puncture_site
  )

transfusion_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_transfusion_omit_na_train_predictors2$stone_dimensions)
colnames(transfusion_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
transfusion_na_omit_train_predictors_stone_dimensions <-
  subset(transfusion_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_transfusion_omit_na_train_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_train_predictors2,
    transfusion_na_omit_train_predictors_stone_dimensions
  )

transfusion_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_transfusion_omit_na_train_predictors2$image_guidance_for_renal_puncture)
colnames(transfusion_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
transfusion_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    transfusion_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_transfusion_omit_na_train_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_train_predictors2,
    transfusion_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_transfusion_omit_na_train_predictors2 <-
  subset(
    PCNL_transfusion_omit_na_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_transfusion_omit_na_train_predictors2 <-
  as.matrix(PCNL_transfusion_omit_na_train_predictors2)


PCNL_transfusion_omit_na_test_predictors2 <-
  PCNL_transfusion_omit_na_test_predictors
PCNL_transfusion_omit_na_test_predictors2$stone_complexity <-
  as.integer(PCNL_transfusion_omit_na_test_predictors2$stone_complexity)
PCNL_transfusion_omit_na_test_predictors2$stone_complexity <-
  as.factor(PCNL_transfusion_omit_na_test_predictors2$stone_complexity) %>% to_categorical()
PCNL_transfusion_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_transfusion_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_transfusion_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_transfusion_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_transfusion_omit_na_test_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_transfusion_omit_na_test_predictors2$pre_operative_msu_result)
PCNL_transfusion_omit_na_test_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_transfusion_omit_na_test_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_transfusion_omit_na_test_predictors2$stone_dimensions <-
  as.integer(PCNL_transfusion_omit_na_test_predictors2$stone_dimensions)
PCNL_transfusion_omit_na_test_predictors2$stone_dimensions <-
  as.factor(PCNL_transfusion_omit_na_test_predictors2$stone_dimensions) %>% to_categorical()
PCNL_transfusion_omit_na_test_predictors2$puncture_site <-
  as.integer(PCNL_transfusion_omit_na_test_predictors2$puncture_site)
PCNL_transfusion_omit_na_test_predictors2$puncture_site <-
  as.factor(PCNL_transfusion_omit_na_test_predictors2$puncture_site) %>% to_categorical()
PCNL_transfusion_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_transfusion_omit_na_test_predictors2$image_guidance_for_renal_puncture)
PCNL_transfusion_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_transfusion_omit_na_test_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_transfusion_omit_na_test_predictors2$renogramdmsa <-
  ifelse(PCNL_transfusion_omit_na_test_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_test_predictors2$calyceal_diverticular <-
  ifelse(PCNL_transfusion_omit_na_test_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_test_predictors2$calyceal_l <-
  ifelse(PCNL_transfusion_omit_na_test_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_test_predictors2$calyceal_m <-
  ifelse(PCNL_transfusion_omit_na_test_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_test_predictors2$complete_staghorn <-
  ifelse(PCNL_transfusion_omit_na_test_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_test_predictors2$partial_staghorn <-
  ifelse(PCNL_transfusion_omit_na_test_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_test_predictors2$pelvic <-
  ifelse(PCNL_transfusion_omit_na_test_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_test_predictors2$upper_ureteric <-
  ifelse(PCNL_transfusion_omit_na_test_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_omit_na_test_predictors2$ureter_other <-
  ifelse(PCNL_transfusion_omit_na_test_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



transfusion_na_omit_test_predictors_stone_complexity <-
  as_tibble(PCNL_transfusion_omit_na_test_predictors2$stone_complexity)
colnames(transfusion_na_omit_test_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
transfusion_na_omit_test_predictors_stone_complexity <-
  subset(transfusion_na_omit_test_predictors_stone_complexity,
         select = -GSS0)
PCNL_transfusion_omit_na_test_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_test_predictors2,
    transfusion_na_omit_test_predictors_stone_complexity
  )

transfusion_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_transfusion_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(transfusion_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
transfusion_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    transfusion_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_transfusion_omit_na_test_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_test_predictors2,
    transfusion_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
  )

transfusion_na_omit_test_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_transfusion_omit_na_test_predictors2$pre_operative_msu_result)
colnames(transfusion_na_omit_test_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
transfusion_na_omit_test_predictors_pre_operative_msu_result <-
  subset(transfusion_na_omit_test_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_transfusion_omit_na_test_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_test_predictors2,
    transfusion_na_omit_test_predictors_pre_operative_msu_result
  )

transfusion_na_omit_test_predictors_puncture_site <-
  as_tibble(PCNL_transfusion_omit_na_test_predictors2$puncture_site)
colnames(transfusion_na_omit_test_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
transfusion_na_omit_test_predictors_puncture_site <-
  subset(transfusion_na_omit_test_predictors_puncture_site,
         select = -puncture0)
PCNL_transfusion_omit_na_test_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_test_predictors2,
    transfusion_na_omit_test_predictors_puncture_site
  )

transfusion_na_omit_test_predictors_stone_dimensions <-
  as_tibble(PCNL_transfusion_omit_na_test_predictors2$stone_dimensions)
colnames(transfusion_na_omit_test_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
transfusion_na_omit_test_predictors_stone_dimensions <-
  subset(transfusion_na_omit_test_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_transfusion_omit_na_test_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_test_predictors2,
    transfusion_na_omit_test_predictors_stone_dimensions
  )

transfusion_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_transfusion_omit_na_test_predictors2$image_guidance_for_renal_puncture)
colnames(transfusion_na_omit_test_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
transfusion_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  subset(
    transfusion_na_omit_test_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_transfusion_omit_na_test_predictors2 <-
  cbind(
    PCNL_transfusion_omit_na_test_predictors2,
    transfusion_na_omit_test_predictors_image_guidance_for_renal_puncture
  )

PCNL_transfusion_omit_na_test_predictors2 <-
  subset(
    PCNL_transfusion_omit_na_test_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_transfusion_omit_na_test_predictors2 <-
  as.matrix(PCNL_transfusion_omit_na_test_predictors2)

##### Oversampled Dataset
PCNL_transfusion_omit_na_train_oversample <-
  ROSE::ovun.sample(
    blood_transfusion ~ .,
    data = PCNL_transfusion_omit_na_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_transfusion_omit_na_train_oversample$blood_transfusion)

PCNL_transfusion_oversample_train_outcome <-
  subset(PCNL_transfusion_omit_na_train_oversample,
         select = blood_transfusion)
PCNL_transfusion_oversample_train_outcome$blood_transfusion <-
  as.numeric(PCNL_transfusion_oversample_train_outcome$blood_transfusion)
PCNL_transfusion_oversample_train_outcome$blood_transfusion <-
  ifelse(PCNL_transfusion_oversample_train_outcome$blood_transfusion == 2,
         1,
         0)
PCNL_transfusion_oversample_train_outcome <-
  as.matrix(PCNL_transfusion_oversample_train_outcome)
PCNL_transfusion_oversample_train_predictors <-
  subset(PCNL_transfusion_omit_na_train_oversample,
         select = -blood_transfusion)

PCNL_transfusion_oversample_train_predictors2 <-
  PCNL_transfusion_oversample_train_predictors
PCNL_transfusion_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_transfusion_oversample_train_predictors2$stone_complexity)
PCNL_transfusion_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_transfusion_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_transfusion_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_transfusion_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_transfusion_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_transfusion_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_transfusion_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_transfusion_oversample_train_predictors2$pre_operative_msu_result)
PCNL_transfusion_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_transfusion_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_transfusion_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_transfusion_oversample_train_predictors2$stone_dimensions)
PCNL_transfusion_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_transfusion_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_transfusion_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_transfusion_oversample_train_predictors2$puncture_site)
PCNL_transfusion_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_transfusion_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_transfusion_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_transfusion_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_transfusion_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_transfusion_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_transfusion_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_transfusion_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_transfusion_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_transfusion_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_transfusion_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_transfusion_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_transfusion_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_transfusion_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_transfusion_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_transfusion_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



transfusion_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_transfusion_oversample_train_predictors2$stone_complexity)
colnames(transfusion_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
transfusion_na_omit_train_predictors_stone_complexity <-
  subset(transfusion_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_transfusion_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_oversample_train_predictors2,
    transfusion_na_omit_train_predictors_stone_complexity
  )

transfusion_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_transfusion_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(transfusion_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
transfusion_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    transfusion_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_transfusion_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_oversample_train_predictors2,
    transfusion_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

transfusion_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_transfusion_oversample_train_predictors2$pre_operative_msu_result)
colnames(transfusion_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
transfusion_na_omit_train_predictors_pre_operative_msu_result <-
  subset(transfusion_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_transfusion_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_oversample_train_predictors2,
    transfusion_na_omit_train_predictors_pre_operative_msu_result
  )

transfusion_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_transfusion_oversample_train_predictors2$puncture_site)
colnames(transfusion_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
transfusion_na_omit_train_predictors_puncture_site <-
  subset(transfusion_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_transfusion_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_oversample_train_predictors2,
    transfusion_na_omit_train_predictors_puncture_site
  )

transfusion_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_transfusion_oversample_train_predictors2$stone_dimensions)
colnames(transfusion_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
transfusion_na_omit_train_predictors_stone_dimensions <-
  subset(transfusion_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_transfusion_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_oversample_train_predictors2,
    transfusion_na_omit_train_predictors_stone_dimensions
  )

transfusion_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_transfusion_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(transfusion_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
transfusion_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    transfusion_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_transfusion_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_oversample_train_predictors2,
    transfusion_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_transfusion_oversample_train_predictors2 <-
  subset(
    PCNL_transfusion_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_transfusion_oversample_train_predictors2 <-
  as.matrix(PCNL_transfusion_oversample_train_predictors2)

##### Imputed Dataset
PCNL_transfusion_imp_pre <-
  PCNL_transfusion_omit_na %>% drop_na(blood_transfusion)
PCNL_transfusion_imp1 <- mice(PCNL_transfusion_imp_pre, m = 1)
summary(PCNL_transfusion_imp1)

PCNL_transfusion_imp <-
  mice::complete(PCNL_transfusion_imp1, 1) %>% as_tibble()

PCNL_transfusion_imp_sample <-
  sample(1:nrow(PCNL_transfusion_imp),
         size = nrow(PCNL_transfusion_imp) * 0.7)
PCNL_transfusion_imp_train <-
  PCNL_transfusion_imp[PCNL_transfusion_imp_sample,] %>% as_tibble()
PCNL_transfusion_imp_test <-
  PCNL_transfusion_imp[-PCNL_transfusion_imp_sample,] %>% as_tibble()
PCNL_transfusion_imp_train$blood_transfusion <-
  as.factor(PCNL_transfusion_imp_train$blood_transfusion)
PCNL_transfusion_imp_test$blood_transfusion <-
  as.factor(PCNL_transfusion_imp_test$blood_transfusion)
summary(PCNL_transfusion_imp_train$blood_transfusion)

PCNL_transfusion_imp_train_outcome <-
  subset(PCNL_transfusion_imp_train,
         select = blood_transfusion)
PCNL_transfusion_imp_train_outcome$blood_transfusion <-
  as.numeric(PCNL_transfusion_imp_train_outcome$blood_transfusion)
PCNL_transfusion_imp_train_outcome$blood_transfusion <-
  ifelse(PCNL_transfusion_imp_train_outcome$blood_transfusion == 2,
         1,
         0)
PCNL_transfusion_imp_train_outcome <-
  as.matrix(PCNL_transfusion_imp_train_outcome)
PCNL_transfusion_imp_train_predictors <-
  subset(PCNL_transfusion_imp_train,
         select = -blood_transfusion)

PCNL_transfusion_imp_train_predictors2 <-
  PCNL_transfusion_imp_train_predictors
PCNL_transfusion_imp_train_predictors2$stone_complexity <-
  as.integer(PCNL_transfusion_imp_train_predictors2$stone_complexity)
PCNL_transfusion_imp_train_predictors2$stone_complexity <-
  as.factor(PCNL_transfusion_imp_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_transfusion_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_transfusion_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_transfusion_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_transfusion_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_transfusion_imp_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_transfusion_imp_train_predictors2$pre_operative_msu_result)
PCNL_transfusion_imp_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_transfusion_imp_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_transfusion_imp_train_predictors2$stone_dimensions <-
  as.integer(PCNL_transfusion_imp_train_predictors2$stone_dimensions)
PCNL_transfusion_imp_train_predictors2$stone_dimensions <-
  as.factor(PCNL_transfusion_imp_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_transfusion_imp_train_predictors2$puncture_site <-
  as.integer(PCNL_transfusion_imp_train_predictors2$puncture_site)
PCNL_transfusion_imp_train_predictors2$puncture_site <-
  as.factor(PCNL_transfusion_imp_train_predictors2$puncture_site) %>% to_categorical()
PCNL_transfusion_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_transfusion_imp_train_predictors2$image_guidance_for_renal_puncture)
PCNL_transfusion_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_transfusion_imp_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_transfusion_imp_train_predictors2$renogramdmsa <-
  ifelse(PCNL_transfusion_imp_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_transfusion_imp_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_train_predictors2$calyceal_l <-
  ifelse(PCNL_transfusion_imp_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_train_predictors2$calyceal_m <-
  ifelse(PCNL_transfusion_imp_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_train_predictors2$complete_staghorn <-
  ifelse(PCNL_transfusion_imp_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_train_predictors2$partial_staghorn <-
  ifelse(PCNL_transfusion_imp_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_train_predictors2$pelvic <-
  ifelse(PCNL_transfusion_imp_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_train_predictors2$upper_ureteric <-
  ifelse(PCNL_transfusion_imp_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_train_predictors2$ureter_other <-
  ifelse(PCNL_transfusion_imp_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



transfusion_imp_train_predictors_stone_complexity <-
  as_tibble(PCNL_transfusion_imp_train_predictors2$stone_complexity)
colnames(transfusion_imp_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
transfusion_imp_train_predictors_stone_complexity <-
  subset(transfusion_imp_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_transfusion_imp_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_train_predictors2,
    transfusion_imp_train_predictors_stone_complexity
  )

transfusion_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_transfusion_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(transfusion_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
transfusion_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    transfusion_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_transfusion_imp_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_train_predictors2,
    transfusion_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

transfusion_imp_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_transfusion_imp_train_predictors2$pre_operative_msu_result)
colnames(transfusion_imp_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
transfusion_imp_train_predictors_pre_operative_msu_result <-
  subset(transfusion_imp_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_transfusion_imp_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_train_predictors2,
    transfusion_imp_train_predictors_pre_operative_msu_result
  )

transfusion_imp_train_predictors_puncture_site <-
  as_tibble(PCNL_transfusion_imp_train_predictors2$puncture_site)
colnames(transfusion_imp_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
transfusion_imp_train_predictors_puncture_site <-
  subset(transfusion_imp_train_predictors_puncture_site,
         select = -puncture0)
PCNL_transfusion_imp_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_train_predictors2,
    transfusion_imp_train_predictors_puncture_site
  )

transfusion_imp_train_predictors_stone_dimensions <-
  as_tibble(PCNL_transfusion_imp_train_predictors2$stone_dimensions)
colnames(transfusion_imp_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
transfusion_imp_train_predictors_stone_dimensions <-
  subset(transfusion_imp_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_transfusion_imp_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_train_predictors2,
    transfusion_imp_train_predictors_stone_dimensions
  )

transfusion_imp_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_transfusion_imp_train_predictors2$image_guidance_for_renal_puncture)
colnames(transfusion_imp_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
transfusion_imp_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    transfusion_imp_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_transfusion_imp_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_train_predictors2,
    transfusion_imp_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_transfusion_imp_train_predictors2 <-
  subset(
    PCNL_transfusion_imp_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_transfusion_imp_train_predictors2 <-
  as.matrix(PCNL_transfusion_imp_train_predictors2)


##### Oversampling of Imputed Dataset
PCNL_transfusion_imp_oversample_train <-
  ROSE::ovun.sample(
    blood_transfusion ~ .,
    data = PCNL_transfusion_imp_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_transfusion_imp_oversample_train$blood_transfusion)

PCNL_transfusion_imp_oversample_train_outcome <-
  subset(PCNL_transfusion_imp_oversample_train,
         select = blood_transfusion)
PCNL_transfusion_imp_oversample_train_outcome$blood_transfusion <-
  as.numeric(PCNL_transfusion_imp_oversample_train_outcome$blood_transfusion)
PCNL_transfusion_imp_oversample_train_outcome$blood_transfusion <-
  ifelse(PCNL_transfusion_imp_oversample_train_outcome$blood_transfusion == 2,
         1,
         0)
PCNL_transfusion_imp_oversample_train_outcome <-
  as.matrix(PCNL_transfusion_imp_oversample_train_outcome)

PCNL_transfusion_imp_oversample_train_predictors <-
  subset(PCNL_transfusion_imp_oversample_train,
         select = -blood_transfusion)

PCNL_transfusion_imp_oversample_train_predictors2 <-
  PCNL_transfusion_imp_oversample_train_predictors
PCNL_transfusion_imp_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_transfusion_imp_oversample_train_predictors2$stone_complexity)
PCNL_transfusion_imp_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_transfusion_imp_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_transfusion_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_transfusion_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_transfusion_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_transfusion_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_transfusion_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_transfusion_imp_oversample_train_predictors2$pre_operative_msu_result)
PCNL_transfusion_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_transfusion_imp_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_transfusion_imp_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_transfusion_imp_oversample_train_predictors2$stone_dimensions)
PCNL_transfusion_imp_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_transfusion_imp_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_transfusion_imp_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_transfusion_imp_oversample_train_predictors2$puncture_site)
PCNL_transfusion_imp_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_transfusion_imp_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_transfusion_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_transfusion_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_transfusion_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_transfusion_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_transfusion_imp_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_transfusion_imp_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_transfusion_imp_oversample_train_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_transfusion_imp_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_transfusion_imp_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_transfusion_imp_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_transfusion_imp_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_transfusion_imp_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_transfusion_imp_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_transfusion_imp_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_transfusion_imp_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_transfusion_imp_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



transfusion_imp_oversample_train_predictors_stone_complexity <-
  as_tibble(PCNL_transfusion_imp_oversample_train_predictors2$stone_complexity)
colnames(transfusion_imp_oversample_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
transfusion_imp_oversample_train_predictors_stone_complexity <-
  subset(transfusion_imp_oversample_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_transfusion_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_oversample_train_predictors2,
    transfusion_imp_oversample_train_predictors_stone_complexity
  )

transfusion_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_transfusion_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  transfusion_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
) <- c("size0",
       "size1",
       "size2",
       "size3",
       "size4",
       "size5",
       "size6",
       "size7")
transfusion_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    transfusion_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_transfusion_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_oversample_train_predictors2,
    transfusion_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

transfusion_imp_oversample_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_transfusion_imp_oversample_train_predictors2$pre_operative_msu_result)
colnames(transfusion_imp_oversample_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
transfusion_imp_oversample_train_predictors_pre_operative_msu_result <-
  subset(
    transfusion_imp_oversample_train_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_transfusion_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_oversample_train_predictors2,
    transfusion_imp_oversample_train_predictors_pre_operative_msu_result
  )

transfusion_imp_oversample_train_predictors_puncture_site <-
  as_tibble(PCNL_transfusion_imp_oversample_train_predictors2$puncture_site)
colnames(transfusion_imp_oversample_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
transfusion_imp_oversample_train_predictors_puncture_site <-
  subset(transfusion_imp_oversample_train_predictors_puncture_site,
         select = -puncture0)
PCNL_transfusion_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_oversample_train_predictors2,
    transfusion_imp_oversample_train_predictors_puncture_site
  )

transfusion_imp_oversample_train_predictors_stone_dimensions <-
  as_tibble(PCNL_transfusion_imp_oversample_train_predictors2$stone_dimensions)
colnames(transfusion_imp_oversample_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
transfusion_imp_oversample_train_predictors_stone_dimensions <-
  subset(
    transfusion_imp_oversample_train_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_transfusion_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_oversample_train_predictors2,
    transfusion_imp_oversample_train_predictors_stone_dimensions
  )

transfusion_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_transfusion_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(transfusion_imp_oversample_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
transfusion_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    transfusion_imp_oversample_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_transfusion_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_transfusion_imp_oversample_train_predictors2,
    transfusion_imp_oversample_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_transfusion_imp_oversample_train_predictors2 <-
  subset(
    PCNL_transfusion_imp_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_transfusion_imp_oversample_train_predictors2 <-
  as.matrix(PCNL_transfusion_imp_oversample_train_predictors2)

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
) %>% as_tibble() %>% janitor::clean_names()

plot_missing(PCNL_original_itu_hdu)


##### Omit NA Dataset
PCNL_itu_hdu_omit_na <- na.omit(PCNL_original_itu_hdu)
str(PCNL_itu_hdu_omit_na)

PCNL_itu_hdu_sample <-
  sample(1:nrow(PCNL_original_itu_hdu),
         size = nrow(PCNL_original_itu_hdu) * 0.7)
PCNL_itu_hdu_train <-
  PCNL_original_itu_hdu[PCNL_itu_hdu_sample, ] %>% as_tibble()
PCNL_itu_hdu_test <-
  PCNL_original_itu_hdu[-PCNL_itu_hdu_sample, ] %>% as_tibble()

PCNL_itu_hdu_omit_na_sample <-
  sample(1:nrow(PCNL_itu_hdu_omit_na),
         size = nrow(PCNL_itu_hdu_omit_na) * 0.7)
PCNL_itu_hdu_omit_na_train <-
  PCNL_itu_hdu_omit_na[PCNL_itu_hdu_omit_na_sample, ] %>% as_tibble()
PCNL_itu_hdu_omit_na_test <-
  PCNL_itu_hdu_omit_na[-PCNL_itu_hdu_omit_na_sample, ] %>% as_tibble()
summary(PCNL_itu_hdu_omit_na_train$itu_hdu_admission)

PCNL_itu_hdu_omit_na_train_outcome <-
  subset(PCNL_itu_hdu_omit_na_train,
         select = itu_hdu_admission)
PCNL_itu_hdu_omit_na_train_outcome$itu_hdu_admission <-
  as.numeric(PCNL_itu_hdu_omit_na_train_outcome$itu_hdu_admission)
PCNL_itu_hdu_omit_na_train_outcome$itu_hdu_admission <-
  ifelse(PCNL_itu_hdu_omit_na_train_outcome$itu_hdu_admission == 2,
         1,
         0)
PCNL_itu_hdu_omit_na_train_outcome <-
  as.matrix(PCNL_itu_hdu_omit_na_train_outcome)
PCNL_itu_hdu_omit_na_test_outcome <-
  subset(PCNL_itu_hdu_omit_na_test,
         select = itu_hdu_admission)
PCNL_itu_hdu_omit_na_test_outcome$itu_hdu_admission <-
  as.numeric(PCNL_itu_hdu_omit_na_test_outcome$itu_hdu_admission)
PCNL_itu_hdu_omit_na_test_outcome$itu_hdu_admission <-
  ifelse(PCNL_itu_hdu_omit_na_test_outcome$itu_hdu_admission == 2, 1, 0)
PCNL_itu_hdu_omit_na_test_outcome <-
  as.matrix(PCNL_itu_hdu_omit_na_test_outcome)
PCNL_itu_hdu_omit_na_train_predictors <-
  subset(PCNL_itu_hdu_omit_na_train,
         select = -itu_hdu_admission)
PCNL_itu_hdu_omit_na_test_predictors <-
  subset(PCNL_itu_hdu_omit_na_test,
         select = -itu_hdu_admission)

PCNL_itu_hdu_omit_na_train_predictors2 <-
  PCNL_itu_hdu_omit_na_train_predictors
PCNL_itu_hdu_omit_na_train_predictors2$stone_complexity <-
  as.integer(PCNL_itu_hdu_omit_na_train_predictors2$stone_complexity)
PCNL_itu_hdu_omit_na_train_predictors2$stone_complexity <-
  as.factor(PCNL_itu_hdu_omit_na_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_itu_hdu_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_itu_hdu_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_itu_hdu_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_itu_hdu_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_itu_hdu_omit_na_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_itu_hdu_omit_na_train_predictors2$pre_operative_msu_result)
PCNL_itu_hdu_omit_na_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_itu_hdu_omit_na_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_itu_hdu_omit_na_train_predictors2$stone_dimensions <-
  as.integer(PCNL_itu_hdu_omit_na_train_predictors2$stone_dimensions)
PCNL_itu_hdu_omit_na_train_predictors2$stone_dimensions <-
  as.factor(PCNL_itu_hdu_omit_na_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_itu_hdu_omit_na_train_predictors2$puncture_site <-
  as.integer(PCNL_itu_hdu_omit_na_train_predictors2$puncture_site)
PCNL_itu_hdu_omit_na_train_predictors2$puncture_site <-
  as.factor(PCNL_itu_hdu_omit_na_train_predictors2$puncture_site) %>% to_categorical()
PCNL_itu_hdu_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_itu_hdu_omit_na_train_predictors2$image_guidance_for_renal_puncture)
PCNL_itu_hdu_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_itu_hdu_omit_na_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_itu_hdu_omit_na_train_predictors2$renogramdmsa <-
  ifelse(PCNL_itu_hdu_omit_na_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_itu_hdu_omit_na_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_train_predictors2$calyceal_l <-
  ifelse(PCNL_itu_hdu_omit_na_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_train_predictors2$calyceal_m <-
  ifelse(PCNL_itu_hdu_omit_na_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_train_predictors2$complete_staghorn <-
  ifelse(PCNL_itu_hdu_omit_na_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_train_predictors2$partial_staghorn <-
  ifelse(PCNL_itu_hdu_omit_na_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_train_predictors2$pelvic <-
  ifelse(PCNL_itu_hdu_omit_na_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_train_predictors2$upper_ureteric <-
  ifelse(PCNL_itu_hdu_omit_na_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_train_predictors2$ureter_other <-
  ifelse(PCNL_itu_hdu_omit_na_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



itu_hdu_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_itu_hdu_omit_na_train_predictors2$stone_complexity)
colnames(itu_hdu_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
itu_hdu_na_omit_train_predictors_stone_complexity <-
  subset(itu_hdu_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_itu_hdu_omit_na_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_train_predictors2,
    itu_hdu_na_omit_train_predictors_stone_complexity
  )

itu_hdu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_itu_hdu_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(itu_hdu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
itu_hdu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    itu_hdu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_itu_hdu_omit_na_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_train_predictors2,
    itu_hdu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

itu_hdu_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_itu_hdu_omit_na_train_predictors2$pre_operative_msu_result)
colnames(itu_hdu_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
itu_hdu_na_omit_train_predictors_pre_operative_msu_result <-
  subset(itu_hdu_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_itu_hdu_omit_na_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_train_predictors2,
    itu_hdu_na_omit_train_predictors_pre_operative_msu_result
  )

itu_hdu_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_itu_hdu_omit_na_train_predictors2$puncture_site)
colnames(itu_hdu_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
itu_hdu_na_omit_train_predictors_puncture_site <-
  subset(itu_hdu_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_itu_hdu_omit_na_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_train_predictors2,
    itu_hdu_na_omit_train_predictors_puncture_site
  )

itu_hdu_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_itu_hdu_omit_na_train_predictors2$stone_dimensions)
colnames(itu_hdu_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
itu_hdu_na_omit_train_predictors_stone_dimensions <-
  subset(itu_hdu_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_itu_hdu_omit_na_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_train_predictors2,
    itu_hdu_na_omit_train_predictors_stone_dimensions
  )

itu_hdu_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_itu_hdu_omit_na_train_predictors2$image_guidance_for_renal_puncture)
colnames(itu_hdu_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
itu_hdu_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    itu_hdu_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_itu_hdu_omit_na_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_train_predictors2,
    itu_hdu_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_itu_hdu_omit_na_train_predictors2 <-
  subset(
    PCNL_itu_hdu_omit_na_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_itu_hdu_omit_na_train_predictors2 <-
  as.matrix(PCNL_itu_hdu_omit_na_train_predictors2)


PCNL_itu_hdu_omit_na_test_predictors2 <-
  PCNL_itu_hdu_omit_na_test_predictors
PCNL_itu_hdu_omit_na_test_predictors2$stone_complexity <-
  as.integer(PCNL_itu_hdu_omit_na_test_predictors2$stone_complexity)
PCNL_itu_hdu_omit_na_test_predictors2$stone_complexity <-
  as.factor(PCNL_itu_hdu_omit_na_test_predictors2$stone_complexity) %>% to_categorical()
PCNL_itu_hdu_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_itu_hdu_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_itu_hdu_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_itu_hdu_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_itu_hdu_omit_na_test_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_itu_hdu_omit_na_test_predictors2$pre_operative_msu_result)
PCNL_itu_hdu_omit_na_test_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_itu_hdu_omit_na_test_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_itu_hdu_omit_na_test_predictors2$stone_dimensions <-
  as.integer(PCNL_itu_hdu_omit_na_test_predictors2$stone_dimensions)
PCNL_itu_hdu_omit_na_test_predictors2$stone_dimensions <-
  as.factor(PCNL_itu_hdu_omit_na_test_predictors2$stone_dimensions) %>% to_categorical()
PCNL_itu_hdu_omit_na_test_predictors2$puncture_site <-
  as.integer(PCNL_itu_hdu_omit_na_test_predictors2$puncture_site)
PCNL_itu_hdu_omit_na_test_predictors2$puncture_site <-
  as.factor(PCNL_itu_hdu_omit_na_test_predictors2$puncture_site) %>% to_categorical()
PCNL_itu_hdu_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_itu_hdu_omit_na_test_predictors2$image_guidance_for_renal_puncture)
PCNL_itu_hdu_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_itu_hdu_omit_na_test_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_itu_hdu_omit_na_test_predictors2$renogramdmsa <-
  ifelse(PCNL_itu_hdu_omit_na_test_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_test_predictors2$calyceal_diverticular <-
  ifelse(PCNL_itu_hdu_omit_na_test_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_test_predictors2$calyceal_l <-
  ifelse(PCNL_itu_hdu_omit_na_test_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_test_predictors2$calyceal_m <-
  ifelse(PCNL_itu_hdu_omit_na_test_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_test_predictors2$complete_staghorn <-
  ifelse(PCNL_itu_hdu_omit_na_test_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_test_predictors2$partial_staghorn <-
  ifelse(PCNL_itu_hdu_omit_na_test_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_test_predictors2$pelvic <-
  ifelse(PCNL_itu_hdu_omit_na_test_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_test_predictors2$upper_ureteric <-
  ifelse(PCNL_itu_hdu_omit_na_test_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_omit_na_test_predictors2$ureter_other <-
  ifelse(PCNL_itu_hdu_omit_na_test_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



itu_hdu_na_omit_test_predictors_stone_complexity <-
  as_tibble(PCNL_itu_hdu_omit_na_test_predictors2$stone_complexity)
colnames(itu_hdu_na_omit_test_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
itu_hdu_na_omit_test_predictors_stone_complexity <-
  subset(itu_hdu_na_omit_test_predictors_stone_complexity,
         select = -GSS0)
PCNL_itu_hdu_omit_na_test_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_test_predictors2,
    itu_hdu_na_omit_test_predictors_stone_complexity
  )

itu_hdu_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_itu_hdu_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(itu_hdu_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
itu_hdu_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    itu_hdu_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_itu_hdu_omit_na_test_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_test_predictors2,
    itu_hdu_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
  )

itu_hdu_na_omit_test_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_itu_hdu_omit_na_test_predictors2$pre_operative_msu_result)
colnames(itu_hdu_na_omit_test_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
itu_hdu_na_omit_test_predictors_pre_operative_msu_result <-
  subset(itu_hdu_na_omit_test_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_itu_hdu_omit_na_test_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_test_predictors2,
    itu_hdu_na_omit_test_predictors_pre_operative_msu_result
  )

itu_hdu_na_omit_test_predictors_puncture_site <-
  as_tibble(PCNL_itu_hdu_omit_na_test_predictors2$puncture_site)
colnames(itu_hdu_na_omit_test_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
itu_hdu_na_omit_test_predictors_puncture_site <-
  subset(itu_hdu_na_omit_test_predictors_puncture_site,
         select = -puncture0)
PCNL_itu_hdu_omit_na_test_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_test_predictors2,
    itu_hdu_na_omit_test_predictors_puncture_site
  )

itu_hdu_na_omit_test_predictors_stone_dimensions <-
  as_tibble(PCNL_itu_hdu_omit_na_test_predictors2$stone_dimensions)
colnames(itu_hdu_na_omit_test_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
itu_hdu_na_omit_test_predictors_stone_dimensions <-
  subset(itu_hdu_na_omit_test_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_itu_hdu_omit_na_test_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_test_predictors2,
    itu_hdu_na_omit_test_predictors_stone_dimensions
  )

itu_hdu_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_itu_hdu_omit_na_test_predictors2$image_guidance_for_renal_puncture)
colnames(itu_hdu_na_omit_test_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
itu_hdu_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  subset(
    itu_hdu_na_omit_test_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_itu_hdu_omit_na_test_predictors2 <-
  cbind(
    PCNL_itu_hdu_omit_na_test_predictors2,
    itu_hdu_na_omit_test_predictors_image_guidance_for_renal_puncture
  )

PCNL_itu_hdu_omit_na_test_predictors2 <-
  subset(
    PCNL_itu_hdu_omit_na_test_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_itu_hdu_omit_na_test_predictors2 <-
  as.matrix(PCNL_itu_hdu_omit_na_test_predictors2)

##### Oversampled Dataset
PCNL_itu_hdu_omit_na_train_oversample <-
  ROSE::ovun.sample(
    itu_hdu_admission ~ .,
    data = PCNL_itu_hdu_omit_na_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_itu_hdu_omit_na_train_oversample$itu_hdu_admission)

PCNL_itu_hdu_oversample_train_outcome <-
  subset(PCNL_itu_hdu_omit_na_train_oversample,
         select = itu_hdu_admission)
PCNL_itu_hdu_oversample_train_outcome$itu_hdu_admission <-
  as.numeric(PCNL_itu_hdu_oversample_train_outcome$itu_hdu_admission)
PCNL_itu_hdu_oversample_train_outcome$itu_hdu_admission <-
  ifelse(PCNL_itu_hdu_oversample_train_outcome$itu_hdu_admission == 2,
         1,
         0)
PCNL_itu_hdu_oversample_train_outcome <-
  as.matrix(PCNL_itu_hdu_oversample_train_outcome)

PCNL_itu_hdu_oversample_train_predictors <-
  subset(PCNL_itu_hdu_omit_na_train_oversample,
         select = -itu_hdu_admission)

PCNL_itu_hdu_oversample_train_predictors2 <-
  PCNL_itu_hdu_oversample_train_predictors
PCNL_itu_hdu_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_itu_hdu_oversample_train_predictors2$stone_complexity)
PCNL_itu_hdu_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_itu_hdu_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_itu_hdu_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_itu_hdu_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_itu_hdu_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_itu_hdu_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_itu_hdu_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_itu_hdu_oversample_train_predictors2$pre_operative_msu_result)
PCNL_itu_hdu_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_itu_hdu_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_itu_hdu_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_itu_hdu_oversample_train_predictors2$stone_dimensions)
PCNL_itu_hdu_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_itu_hdu_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_itu_hdu_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_itu_hdu_oversample_train_predictors2$puncture_site)
PCNL_itu_hdu_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_itu_hdu_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_itu_hdu_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_itu_hdu_oversample_train_predictors2$image_guidance_for_renal_puncture)
PCNL_itu_hdu_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_itu_hdu_oversample_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_itu_hdu_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_itu_hdu_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_itu_hdu_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_itu_hdu_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_itu_hdu_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_itu_hdu_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_itu_hdu_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_itu_hdu_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_itu_hdu_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_itu_hdu_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



itu_hdu_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_itu_hdu_oversample_train_predictors2$stone_complexity)
colnames(itu_hdu_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
itu_hdu_na_omit_train_predictors_stone_complexity <-
  subset(itu_hdu_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_itu_hdu_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_oversample_train_predictors2,
    itu_hdu_na_omit_train_predictors_stone_complexity
  )

itu_hdu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_itu_hdu_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(itu_hdu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
itu_hdu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    itu_hdu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_itu_hdu_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_oversample_train_predictors2,
    itu_hdu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

itu_hdu_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_itu_hdu_oversample_train_predictors2$pre_operative_msu_result)
colnames(itu_hdu_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
itu_hdu_na_omit_train_predictors_pre_operative_msu_result <-
  subset(itu_hdu_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_itu_hdu_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_oversample_train_predictors2,
    itu_hdu_na_omit_train_predictors_pre_operative_msu_result
  )

itu_hdu_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_itu_hdu_oversample_train_predictors2$puncture_site)
colnames(itu_hdu_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
itu_hdu_na_omit_train_predictors_puncture_site <-
  subset(itu_hdu_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_itu_hdu_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_oversample_train_predictors2,
    itu_hdu_na_omit_train_predictors_puncture_site
  )

itu_hdu_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_itu_hdu_oversample_train_predictors2$stone_dimensions)
colnames(itu_hdu_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
itu_hdu_na_omit_train_predictors_stone_dimensions <-
  subset(itu_hdu_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_itu_hdu_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_oversample_train_predictors2,
    itu_hdu_na_omit_train_predictors_stone_dimensions
  )

itu_hdu_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_itu_hdu_oversample_train_predictors2$image_guidance_for_renal_puncture)
colnames(itu_hdu_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
itu_hdu_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    itu_hdu_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_itu_hdu_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_oversample_train_predictors2,
    itu_hdu_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_itu_hdu_oversample_train_predictors2 <-
  subset(
    PCNL_itu_hdu_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_itu_hdu_oversample_train_predictors2 <-
  as.matrix(PCNL_itu_hdu_oversample_train_predictors2)

##### Imputed Dataset
PCNL_itu_hdu_imp_pre <-
  PCNL_itu_hdu_omit_na %>% drop_na(itu_hdu_admission)
PCNL_itu_hdu_imp1 <- mice(PCNL_itu_hdu_imp_pre, m = 1)
summary(PCNL_itu_hdu_imp1)

PCNL_itu_hdu_imp <-
  mice::complete(PCNL_itu_hdu_imp1, 1) %>% as_tibble()

PCNL_itu_hdu_imp_sample <-
  sample(1:nrow(PCNL_itu_hdu_imp), size = nrow(PCNL_itu_hdu_imp) * 0.7)
PCNL_itu_hdu_imp_train <-
  PCNL_itu_hdu_imp[PCNL_itu_hdu_imp_sample, ] %>% as_tibble()
PCNL_itu_hdu_imp_test <-
  PCNL_itu_hdu_imp[-PCNL_itu_hdu_imp_sample, ] %>% as_tibble()
PCNL_itu_hdu_imp_train$itu_hdu_admission <-
  as.factor(PCNL_itu_hdu_imp_train$itu_hdu_admission)
PCNL_itu_hdu_imp_test$itu_hdu_admission <-
  as.factor(PCNL_itu_hdu_imp_test$itu_hdu_admission)
summary(PCNL_itu_hdu_imp_train$itu_hdu_admission)

PCNL_itu_hdu_imp_train_outcome <-
  subset(PCNL_itu_hdu_imp_train,
         select = itu_hdu_admission)
PCNL_itu_hdu_imp_train_outcome$itu_hdu_admission <-
  as.numeric(PCNL_itu_hdu_imp_train_outcome$itu_hdu_admission)
PCNL_itu_hdu_imp_train_outcome$itu_hdu_admission <-
  ifelse(PCNL_itu_hdu_imp_train_outcome$itu_hdu_admission == 2,
         1,
         0)
PCNL_itu_hdu_imp_train_outcome <-
  as.matrix(PCNL_itu_hdu_imp_train_outcome)

PCNL_itu_hdu_imp_train_predictors <-
  subset(PCNL_itu_hdu_imp_train,
         select = -itu_hdu_admission)

PCNL_itu_hdu_imp_train_predictors2 <-
  PCNL_itu_hdu_imp_train_predictors
PCNL_itu_hdu_imp_train_predictors2$stone_complexity <-
  as.integer(PCNL_itu_hdu_imp_train_predictors2$stone_complexity)
PCNL_itu_hdu_imp_train_predictors2$stone_complexity <-
  as.factor(PCNL_itu_hdu_imp_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_itu_hdu_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_itu_hdu_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_itu_hdu_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_itu_hdu_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_itu_hdu_imp_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_itu_hdu_imp_train_predictors2$pre_operative_msu_result)
PCNL_itu_hdu_imp_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_itu_hdu_imp_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_itu_hdu_imp_train_predictors2$stone_dimensions <-
  as.integer(PCNL_itu_hdu_imp_train_predictors2$stone_dimensions)
PCNL_itu_hdu_imp_train_predictors2$stone_dimensions <-
  as.factor(PCNL_itu_hdu_imp_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_itu_hdu_imp_train_predictors2$puncture_site <-
  as.integer(PCNL_itu_hdu_imp_train_predictors2$puncture_site)
PCNL_itu_hdu_imp_train_predictors2$puncture_site <-
  as.factor(PCNL_itu_hdu_imp_train_predictors2$puncture_site) %>% to_categorical()
PCNL_itu_hdu_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_itu_hdu_imp_train_predictors2$image_guidance_for_renal_puncture)
PCNL_itu_hdu_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_itu_hdu_imp_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_itu_hdu_imp_train_predictors2$renogramdmsa <-
  ifelse(PCNL_itu_hdu_imp_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_itu_hdu_imp_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_train_predictors2$calyceal_l <-
  ifelse(PCNL_itu_hdu_imp_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_train_predictors2$calyceal_m <-
  ifelse(PCNL_itu_hdu_imp_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_train_predictors2$complete_staghorn <-
  ifelse(PCNL_itu_hdu_imp_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_train_predictors2$partial_staghorn <-
  ifelse(PCNL_itu_hdu_imp_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_train_predictors2$pelvic <-
  ifelse(PCNL_itu_hdu_imp_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_train_predictors2$upper_ureteric <-
  ifelse(PCNL_itu_hdu_imp_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_train_predictors2$ureter_other <-
  ifelse(PCNL_itu_hdu_imp_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



itu_hdu_imp_train_predictors_stone_complexity <-
  as_tibble(PCNL_itu_hdu_imp_train_predictors2$stone_complexity)
colnames(itu_hdu_imp_train_predictors_stone_complexity) <- c("GSS0",
                                                             "GSSI",
                                                             "GSSII",
                                                             "GSSIII",
                                                             "GSSIV")
itu_hdu_imp_train_predictors_stone_complexity <-
  subset(itu_hdu_imp_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_itu_hdu_imp_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_imp_train_predictors2,
    itu_hdu_imp_train_predictors_stone_complexity
  )

itu_hdu_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_itu_hdu_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(itu_hdu_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
itu_hdu_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(itu_hdu_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath,
         select = -size0)
PCNL_itu_hdu_imp_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_imp_train_predictors2,
    itu_hdu_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

itu_hdu_imp_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_itu_hdu_imp_train_predictors2$pre_operative_msu_result)
colnames(itu_hdu_imp_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
itu_hdu_imp_train_predictors_pre_operative_msu_result <-
  subset(itu_hdu_imp_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_itu_hdu_imp_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_imp_train_predictors2,
    itu_hdu_imp_train_predictors_pre_operative_msu_result
  )

itu_hdu_imp_train_predictors_puncture_site <-
  as_tibble(PCNL_itu_hdu_imp_train_predictors2$puncture_site)
colnames(itu_hdu_imp_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
itu_hdu_imp_train_predictors_puncture_site <-
  subset(itu_hdu_imp_train_predictors_puncture_site,
         select = -puncture0)
PCNL_itu_hdu_imp_train_predictors2 <-
  cbind(PCNL_itu_hdu_imp_train_predictors2,
        itu_hdu_imp_train_predictors_puncture_site)

itu_hdu_imp_train_predictors_stone_dimensions <-
  as_tibble(PCNL_itu_hdu_imp_train_predictors2$stone_dimensions)
colnames(itu_hdu_imp_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
itu_hdu_imp_train_predictors_stone_dimensions <-
  subset(itu_hdu_imp_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_itu_hdu_imp_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_imp_train_predictors2,
    itu_hdu_imp_train_predictors_stone_dimensions
  )

itu_hdu_imp_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_itu_hdu_imp_train_predictors2$image_guidance_for_renal_puncture)
colnames(itu_hdu_imp_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
itu_hdu_imp_train_predictors_image_guidance_for_renal_puncture <-
  subset(itu_hdu_imp_train_predictors_image_guidance_for_renal_puncture,
         select = -image0)
PCNL_itu_hdu_imp_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_imp_train_predictors2,
    itu_hdu_imp_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_itu_hdu_imp_train_predictors2 <-
  subset(
    PCNL_itu_hdu_imp_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_itu_hdu_imp_train_predictors2 <-
  as.matrix(PCNL_itu_hdu_imp_train_predictors2)


##### Oversampling of Imputed Dataset
PCNL_itu_hdu_imp_train_oversample <-
  ROSE::ovun.sample(
    itu_hdu_admission ~ .,
    data = PCNL_itu_hdu_imp_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_itu_hdu_imp_train_oversample$itu_hdu_admission)

PCNL_itu_hdu_imp_oversample_train_outcome <-
  subset(PCNL_itu_hdu_imp_train_oversample,
         select = itu_hdu_admission)
PCNL_itu_hdu_imp_oversample_train_outcome$itu_hdu_admission <-
  as.numeric(PCNL_itu_hdu_imp_oversample_train_outcome$itu_hdu_admission)
PCNL_itu_hdu_imp_oversample_train_outcome$itu_hdu_admission <-
  ifelse(PCNL_itu_hdu_imp_oversample_train_outcome$itu_hdu_admission == 2,
         1,
         0)
PCNL_itu_hdu_imp_oversample_train_outcome <-
  as.matrix(PCNL_itu_hdu_imp_oversample_train_outcome)

PCNL_itu_hdu_imp_oversample_train_predictors <-
  subset(PCNL_itu_hdu_imp_train_oversample,
         select = -itu_hdu_admission)

PCNL_itu_hdu_imp_oversample_train_predictors2 <-
  PCNL_itu_hdu_imp_oversample_train_predictors
PCNL_itu_hdu_imp_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_itu_hdu_imp_oversample_train_predictors2$stone_complexity)
PCNL_itu_hdu_imp_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_itu_hdu_imp_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_itu_hdu_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_itu_hdu_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_itu_hdu_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_itu_hdu_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_itu_hdu_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_itu_hdu_imp_oversample_train_predictors2$pre_operative_msu_result)
PCNL_itu_hdu_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_itu_hdu_imp_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_itu_hdu_imp_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_itu_hdu_imp_oversample_train_predictors2$stone_dimensions)
PCNL_itu_hdu_imp_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_itu_hdu_imp_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_itu_hdu_imp_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_itu_hdu_imp_oversample_train_predictors2$puncture_site)
PCNL_itu_hdu_imp_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_itu_hdu_imp_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_itu_hdu_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_itu_hdu_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_itu_hdu_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_itu_hdu_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_itu_hdu_imp_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_itu_hdu_imp_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_itu_hdu_imp_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_itu_hdu_imp_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_itu_hdu_imp_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_itu_hdu_imp_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_itu_hdu_imp_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_itu_hdu_imp_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_itu_hdu_imp_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_itu_hdu_imp_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_itu_hdu_imp_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



itu_hdu_imp_oversample_train_predictors_stone_complexity <-
  as_tibble(PCNL_itu_hdu_imp_oversample_train_predictors2$stone_complexity)
colnames(itu_hdu_imp_oversample_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
itu_hdu_imp_oversample_train_predictors_stone_complexity <-
  subset(itu_hdu_imp_oversample_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_itu_hdu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_imp_oversample_train_predictors2,
    itu_hdu_imp_oversample_train_predictors_stone_complexity
  )

itu_hdu_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_itu_hdu_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(itu_hdu_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
itu_hdu_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    itu_hdu_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_itu_hdu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_imp_oversample_train_predictors2,
    itu_hdu_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

itu_hdu_imp_oversample_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_itu_hdu_imp_oversample_train_predictors2$pre_operative_msu_result)
colnames(itu_hdu_imp_oversample_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
itu_hdu_imp_oversample_train_predictors_pre_operative_msu_result <-
  subset(itu_hdu_imp_oversample_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_itu_hdu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_imp_oversample_train_predictors2,
    itu_hdu_imp_oversample_train_predictors_pre_operative_msu_result
  )

itu_hdu_imp_oversample_train_predictors_puncture_site <-
  as_tibble(PCNL_itu_hdu_imp_oversample_train_predictors2$puncture_site)
colnames(itu_hdu_imp_oversample_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
itu_hdu_imp_oversample_train_predictors_puncture_site <-
  subset(itu_hdu_imp_oversample_train_predictors_puncture_site,
         select = -puncture0)
PCNL_itu_hdu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_imp_oversample_train_predictors2,
    itu_hdu_imp_oversample_train_predictors_puncture_site
  )

itu_hdu_imp_oversample_train_predictors_stone_dimensions <-
  as_tibble(PCNL_itu_hdu_imp_oversample_train_predictors2$stone_dimensions)
colnames(itu_hdu_imp_oversample_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
itu_hdu_imp_oversample_train_predictors_stone_dimensions <-
  subset(itu_hdu_imp_oversample_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_itu_hdu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_imp_oversample_train_predictors2,
    itu_hdu_imp_oversample_train_predictors_stone_dimensions
  )

itu_hdu_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_itu_hdu_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(itu_hdu_imp_oversample_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
itu_hdu_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    itu_hdu_imp_oversample_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_itu_hdu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_itu_hdu_imp_oversample_train_predictors2,
    itu_hdu_imp_oversample_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_itu_hdu_imp_oversample_train_predictors2 <-
  subset(
    PCNL_itu_hdu_imp_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_itu_hdu_imp_oversample_train_predictors2 <-
  as.matrix(PCNL_itu_hdu_imp_oversample_train_predictors2)

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
) %>% as_tibble() %>% janitor::clean_names()

plot_missing(PCNL_original_clearance_on_fluoro)
PCNL_original_clearance_on_fluoro$complete_clearance_on_fluoroscopy <-
  as.factor(PCNL_original_clearance_on_fluoro$complete_clearance_on_fluoroscopy)


##### Omit NA Dataset
PCNL_clearance_on_fluoro_omit_na <-
  na.omit(PCNL_original_clearance_on_fluoro)
str(PCNL_clearance_on_fluoro_omit_na)

PCNL_clearance_on_fluoro_sample <-
  sample(
    1:nrow(PCNL_original_clearance_on_fluoro),
    size = nrow(PCNL_original_clearance_on_fluoro) * 0.7
  )
PCNL_clearance_on_fluoro_train <-
  PCNL_original_clearance_on_fluoro[PCNL_clearance_on_fluoro_sample, ] %>% as_tibble()
PCNL_clearance_on_fluoro_test <-
  PCNL_original_clearance_on_fluoro[-PCNL_clearance_on_fluoro_sample, ] %>% as_tibble()

PCNL_clearance_on_fluoro_omit_na_sample <-
  sample(
    1:nrow(PCNL_clearance_on_fluoro_omit_na),
    size = nrow(PCNL_clearance_on_fluoro_omit_na) * 0.7
  )
PCNL_clearance_on_fluoro_omit_na_train <-
  PCNL_clearance_on_fluoro_omit_na[PCNL_clearance_on_fluoro_omit_na_sample, ] %>% as_tibble()
PCNL_clearance_on_fluoro_omit_na_test <-
  PCNL_clearance_on_fluoro_omit_na[-PCNL_clearance_on_fluoro_omit_na_sample, ] %>% as_tibble()
summary(PCNL_clearance_on_fluoro_omit_na_train$complete_clearance_on_fluoroscopy)

PCNL_clearance_on_fluoro_omit_na_train_outcome <-
  subset(PCNL_clearance_on_fluoro_omit_na_train,
         select = complete_clearance_on_fluoroscopy)
PCNL_clearance_on_fluoro_omit_na_train_outcome$complete_clearance_on_fluoroscopy <-
  as.numeric(
    PCNL_clearance_on_fluoro_omit_na_train_outcome$complete_clearance_on_fluoroscopy
  )
PCNL_clearance_on_fluoro_omit_na_train_outcome$complete_clearance_on_fluoroscopy <-
  ifelse(
    PCNL_clearance_on_fluoro_omit_na_train_outcome$complete_clearance_on_fluoroscopy == 2,
    1,
    0
  )
PCNL_clearance_on_fluoro_omit_na_train_outcome <-
  as.matrix(PCNL_clearance_on_fluoro_omit_na_train_outcome)
PCNL_clearance_on_fluoro_omit_na_test_outcome <-
  subset(PCNL_clearance_on_fluoro_omit_na_test,
         select = complete_clearance_on_fluoroscopy)
PCNL_clearance_on_fluoro_omit_na_test_outcome$complete_clearance_on_fluoroscopy <-
  as.numeric(
    PCNL_clearance_on_fluoro_omit_na_test_outcome$complete_clearance_on_fluoroscopy
  )
PCNL_clearance_on_fluoro_omit_na_test_outcome$complete_clearance_on_fluoroscopy <-
  ifelse(
    PCNL_clearance_on_fluoro_omit_na_test_outcome$complete_clearance_on_fluoroscopy == 2,
    1,
    0
  )
PCNL_clearance_on_fluoro_omit_na_test_outcome <-
  as.matrix(PCNL_clearance_on_fluoro_omit_na_test_outcome)
PCNL_clearance_on_fluoro_omit_na_train_predictors <-
  subset(
    PCNL_clearance_on_fluoro_omit_na_train,
    select = -complete_clearance_on_fluoroscopy
  )
PCNL_clearance_on_fluoro_omit_na_test_predictors <-
  subset(PCNL_clearance_on_fluoro_omit_na_test,
         select = -complete_clearance_on_fluoroscopy)

PCNL_clearance_on_fluoro_omit_na_train_predictors2 <-
  PCNL_clearance_on_fluoro_omit_na_train_predictors
PCNL_clearance_on_fluoro_omit_na_train_predictors2$stone_complexity <-
  as.integer(PCNL_clearance_on_fluoro_omit_na_train_predictors2$stone_complexity)
PCNL_clearance_on_fluoro_omit_na_train_predictors2$stone_complexity <-
  as.factor(PCNL_clearance_on_fluoro_omit_na_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_clearance_on_fluoro_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_clearance_on_fluoro_omit_na_train_predictors2$pre_operative_msu_result)
PCNL_clearance_on_fluoro_omit_na_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_clearance_on_fluoro_omit_na_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$stone_dimensions <-
  as.integer(PCNL_clearance_on_fluoro_omit_na_train_predictors2$stone_dimensions)
PCNL_clearance_on_fluoro_omit_na_train_predictors2$stone_dimensions <-
  as.factor(PCNL_clearance_on_fluoro_omit_na_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$puncture_site <-
  as.integer(PCNL_clearance_on_fluoro_omit_na_train_predictors2$puncture_site)
PCNL_clearance_on_fluoro_omit_na_train_predictors2$puncture_site <-
  as.factor(PCNL_clearance_on_fluoro_omit_na_train_predictors2$puncture_site) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_clearance_on_fluoro_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$renogramdmsa <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$calyceal_l <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$calyceal_m <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$complete_staghorn <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$partial_staghorn <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$pelvic <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$upper_ureteric <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_train_predictors2$ureter_other <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



clearance_on_fluoro_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_clearance_on_fluoro_omit_na_train_predictors2$stone_complexity)
colnames(clearance_on_fluoro_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
clearance_on_fluoro_na_omit_train_predictors_stone_complexity <-
  subset(clearance_on_fluoro_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_clearance_on_fluoro_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_stone_complexity
  )

clearance_on_fluoro_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  clearance_on_fluoro_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
clearance_on_fluoro_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    clearance_on_fluoro_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_clearance_on_fluoro_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

clearance_on_fluoro_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_clearance_on_fluoro_omit_na_train_predictors2$pre_operative_msu_result)
colnames(clearance_on_fluoro_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
clearance_on_fluoro_na_omit_train_predictors_pre_operative_msu_result <-
  subset(
    clearance_on_fluoro_na_omit_train_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_clearance_on_fluoro_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_pre_operative_msu_result
  )

clearance_on_fluoro_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_clearance_on_fluoro_omit_na_train_predictors2$puncture_site)
colnames(clearance_on_fluoro_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
clearance_on_fluoro_na_omit_train_predictors_puncture_site <-
  subset(clearance_on_fluoro_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_clearance_on_fluoro_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_puncture_site
  )

clearance_on_fluoro_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_clearance_on_fluoro_omit_na_train_predictors2$stone_dimensions)
colnames(clearance_on_fluoro_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
clearance_on_fluoro_na_omit_train_predictors_stone_dimensions <-
  subset(
    clearance_on_fluoro_na_omit_train_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_clearance_on_fluoro_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_stone_dimensions
  )

clearance_on_fluoro_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(clearance_on_fluoro_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
clearance_on_fluoro_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    clearance_on_fluoro_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_clearance_on_fluoro_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_clearance_on_fluoro_omit_na_train_predictors2 <-
  subset(
    PCNL_clearance_on_fluoro_omit_na_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_clearance_on_fluoro_omit_na_train_predictors2 <-
  as.matrix(PCNL_clearance_on_fluoro_omit_na_train_predictors2)


PCNL_clearance_on_fluoro_omit_na_test_predictors2 <-
  PCNL_clearance_on_fluoro_omit_na_test_predictors
PCNL_clearance_on_fluoro_omit_na_test_predictors2$stone_complexity <-
  as.integer(PCNL_clearance_on_fluoro_omit_na_test_predictors2$stone_complexity)
PCNL_clearance_on_fluoro_omit_na_test_predictors2$stone_complexity <-
  as.factor(PCNL_clearance_on_fluoro_omit_na_test_predictors2$stone_complexity) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_clearance_on_fluoro_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_clearance_on_fluoro_omit_na_test_predictors2$pre_operative_msu_result)
PCNL_clearance_on_fluoro_omit_na_test_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_clearance_on_fluoro_omit_na_test_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$stone_dimensions <-
  as.integer(PCNL_clearance_on_fluoro_omit_na_test_predictors2$stone_dimensions)
PCNL_clearance_on_fluoro_omit_na_test_predictors2$stone_dimensions <-
  as.factor(PCNL_clearance_on_fluoro_omit_na_test_predictors2$stone_dimensions) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$puncture_site <-
  as.integer(PCNL_clearance_on_fluoro_omit_na_test_predictors2$puncture_site)
PCNL_clearance_on_fluoro_omit_na_test_predictors2$puncture_site <-
  as.factor(PCNL_clearance_on_fluoro_omit_na_test_predictors2$puncture_site) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2$image_guidance_for_renal_puncture
  )
PCNL_clearance_on_fluoro_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$renogramdmsa <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_test_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$calyceal_l <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_test_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$calyceal_m <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_test_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$complete_staghorn <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_test_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$partial_staghorn <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_test_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$pelvic <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_test_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$upper_ureteric <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_test_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_omit_na_test_predictors2$ureter_other <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_test_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



clearance_on_fluoro_na_omit_test_predictors_stone_complexity <-
  as_tibble(PCNL_clearance_on_fluoro_omit_na_test_predictors2$stone_complexity)
colnames(clearance_on_fluoro_na_omit_test_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
clearance_on_fluoro_na_omit_test_predictors_stone_complexity <-
  subset(clearance_on_fluoro_na_omit_test_predictors_stone_complexity,
         select = -GSS0)
PCNL_clearance_on_fluoro_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2,
    clearance_on_fluoro_na_omit_test_predictors_stone_complexity
  )

clearance_on_fluoro_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  clearance_on_fluoro_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
clearance_on_fluoro_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    clearance_on_fluoro_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_clearance_on_fluoro_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2,
    clearance_on_fluoro_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
  )

clearance_on_fluoro_na_omit_test_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_clearance_on_fluoro_omit_na_test_predictors2$pre_operative_msu_result)
colnames(clearance_on_fluoro_na_omit_test_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
clearance_on_fluoro_na_omit_test_predictors_pre_operative_msu_result <-
  subset(
    clearance_on_fluoro_na_omit_test_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_clearance_on_fluoro_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2,
    clearance_on_fluoro_na_omit_test_predictors_pre_operative_msu_result
  )

clearance_on_fluoro_na_omit_test_predictors_puncture_site <-
  as_tibble(PCNL_clearance_on_fluoro_omit_na_test_predictors2$puncture_site)
colnames(clearance_on_fluoro_na_omit_test_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
clearance_on_fluoro_na_omit_test_predictors_puncture_site <-
  subset(clearance_on_fluoro_na_omit_test_predictors_puncture_site,
         select = -puncture0)
PCNL_clearance_on_fluoro_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2,
    clearance_on_fluoro_na_omit_test_predictors_puncture_site
  )

clearance_on_fluoro_na_omit_test_predictors_stone_dimensions <-
  as_tibble(PCNL_clearance_on_fluoro_omit_na_test_predictors2$stone_dimensions)
colnames(clearance_on_fluoro_na_omit_test_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
clearance_on_fluoro_na_omit_test_predictors_stone_dimensions <-
  subset(
    clearance_on_fluoro_na_omit_test_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_clearance_on_fluoro_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2,
    clearance_on_fluoro_na_omit_test_predictors_stone_dimensions
  )

clearance_on_fluoro_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2$image_guidance_for_renal_puncture
  )
colnames(clearance_on_fluoro_na_omit_test_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
clearance_on_fluoro_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  subset(
    clearance_on_fluoro_na_omit_test_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_clearance_on_fluoro_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2,
    clearance_on_fluoro_na_omit_test_predictors_image_guidance_for_renal_puncture
  )

PCNL_clearance_on_fluoro_omit_na_test_predictors2 <-
  subset(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_clearance_on_fluoro_omit_na_test_predictors2 <-
  as.matrix(PCNL_clearance_on_fluoro_omit_na_test_predictors2)

##### Oversampled Dataset
PCNL_clearance_on_fluoro_omit_na_train_oversample <-
  ROSE::ovun.sample(
    complete_clearance_on_fluoroscopy ~ .,
    data = PCNL_clearance_on_fluoro_omit_na_train,
    seed = 1234,
    method = "over"
  )$data
summary(
  PCNL_clearance_on_fluoro_omit_na_train_oversample$complete_clearance_on_fluoroscopy
)

PCNL_clearance_on_fluoro_oversample_train_outcome <-
  subset(PCNL_clearance_on_fluoro_omit_na_train_oversample,
         select = complete_clearance_on_fluoroscopy)
PCNL_clearance_on_fluoro_oversample_train_outcome$complete_clearance_on_fluoroscopy <-
  as.numeric(
    PCNL_clearance_on_fluoro_oversample_train_outcome$complete_clearance_on_fluoroscopy
  )
PCNL_clearance_on_fluoro_oversample_train_outcome$complete_clearance_on_fluoroscopy <-
  ifelse(
    PCNL_clearance_on_fluoro_oversample_train_outcome$complete_clearance_on_fluoroscopy == 2,
    1,
    0
  )
PCNL_clearance_on_fluoro_oversample_train_outcome <-
  as.matrix(PCNL_clearance_on_fluoro_oversample_train_outcome)

PCNL_clearance_on_fluoro_oversample_train_predictors <-
  subset(
    PCNL_clearance_on_fluoro_omit_na_train_oversample,
    select = -complete_clearance_on_fluoroscopy
  )

PCNL_clearance_on_fluoro_oversample_train_predictors2 <-
  PCNL_clearance_on_fluoro_oversample_train_predictors
PCNL_clearance_on_fluoro_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_clearance_on_fluoro_oversample_train_predictors2$stone_complexity)
PCNL_clearance_on_fluoro_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_clearance_on_fluoro_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_clearance_on_fluoro_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_clearance_on_fluoro_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_clearance_on_fluoro_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_clearance_on_fluoro_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_clearance_on_fluoro_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_clearance_on_fluoro_oversample_train_predictors2$pre_operative_msu_result)
PCNL_clearance_on_fluoro_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_clearance_on_fluoro_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_clearance_on_fluoro_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_clearance_on_fluoro_oversample_train_predictors2$stone_dimensions)
PCNL_clearance_on_fluoro_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_clearance_on_fluoro_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_clearance_on_fluoro_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_clearance_on_fluoro_oversample_train_predictors2$puncture_site)
PCNL_clearance_on_fluoro_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_clearance_on_fluoro_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_clearance_on_fluoro_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_clearance_on_fluoro_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_clearance_on_fluoro_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_clearance_on_fluoro_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_clearance_on_fluoro_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_clearance_on_fluoro_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_clearance_on_fluoro_oversample_train_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_on_fluoro_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_clearance_on_fluoro_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_clearance_on_fluoro_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_oversample_train_predictors2$complete_staghorn <-
  ifelse(
    PCNL_clearance_on_fluoro_oversample_train_predictors2$complete_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_on_fluoro_oversample_train_predictors2$partial_staghorn <-
  ifelse(
    PCNL_clearance_on_fluoro_oversample_train_predictors2$partial_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_on_fluoro_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_clearance_on_fluoro_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_clearance_on_fluoro_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_clearance_on_fluoro_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



clearance_on_fluoro_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_clearance_on_fluoro_oversample_train_predictors2$stone_complexity)
colnames(clearance_on_fluoro_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
clearance_on_fluoro_na_omit_train_predictors_stone_complexity <-
  subset(clearance_on_fluoro_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_clearance_on_fluoro_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_oversample_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_stone_complexity
  )

clearance_on_fluoro_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_clearance_on_fluoro_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  clearance_on_fluoro_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
clearance_on_fluoro_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    clearance_on_fluoro_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_clearance_on_fluoro_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_oversample_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

clearance_on_fluoro_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_clearance_on_fluoro_oversample_train_predictors2$pre_operative_msu_result)
colnames(clearance_on_fluoro_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
clearance_on_fluoro_na_omit_train_predictors_pre_operative_msu_result <-
  subset(
    clearance_on_fluoro_na_omit_train_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_clearance_on_fluoro_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_oversample_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_pre_operative_msu_result
  )

clearance_on_fluoro_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_clearance_on_fluoro_oversample_train_predictors2$puncture_site)
colnames(clearance_on_fluoro_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
clearance_on_fluoro_na_omit_train_predictors_puncture_site <-
  subset(clearance_on_fluoro_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_clearance_on_fluoro_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_oversample_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_puncture_site
  )

clearance_on_fluoro_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_clearance_on_fluoro_oversample_train_predictors2$stone_dimensions)
colnames(clearance_on_fluoro_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
clearance_on_fluoro_na_omit_train_predictors_stone_dimensions <-
  subset(
    clearance_on_fluoro_na_omit_train_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_clearance_on_fluoro_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_oversample_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_stone_dimensions
  )

clearance_on_fluoro_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_clearance_on_fluoro_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(clearance_on_fluoro_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
clearance_on_fluoro_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    clearance_on_fluoro_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_clearance_on_fluoro_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_oversample_train_predictors2,
    clearance_on_fluoro_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_clearance_on_fluoro_oversample_train_predictors2 <-
  subset(
    PCNL_clearance_on_fluoro_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_clearance_on_fluoro_oversample_train_predictors2 <-
  as.matrix(PCNL_clearance_on_fluoro_oversample_train_predictors2)

##### Imputed Dataset
PCNL_clearance_on_fluoro_imp_pre <-
  PCNL_clearance_on_fluoro_omit_na %>% drop_na(complete_clearance_on_fluoroscopy)
PCNL_clearance_on_fluoro_imp1 <-
  mice(PCNL_clearance_on_fluoro_imp_pre, m = 1)
summary(PCNL_clearance_on_fluoro_imp1)

PCNL_clearance_on_fluoro_imp <-
  mice::complete(PCNL_clearance_on_fluoro_imp1, 1) %>% as_tibble()

PCNL_clearance_on_fluoro_imp_sample <-
  sample(1:nrow(PCNL_clearance_on_fluoro_imp),
         size = nrow(PCNL_clearance_on_fluoro_imp) * 0.7)
PCNL_clearance_on_fluoro_imp_train <-
  PCNL_clearance_on_fluoro_imp[PCNL_clearance_on_fluoro_imp_sample, ] %>% as_tibble()
PCNL_clearance_on_fluoro_imp_test <-
  PCNL_clearance_on_fluoro_imp[-PCNL_clearance_on_fluoro_imp_sample, ] %>% as_tibble()
PCNL_clearance_on_fluoro_imp_train$complete_clearance_on_fluoroscopy <-
  as.factor(PCNL_clearance_on_fluoro_imp_train$complete_clearance_on_fluoroscopy)
PCNL_clearance_on_fluoro_imp_test$complete_clearance_on_fluoroscopy <-
  as.factor(PCNL_clearance_on_fluoro_imp_test$complete_clearance_on_fluoroscopy)
summary(PCNL_clearance_on_fluoro_imp_train$complete_clearance_on_fluoroscopy)

PCNL_clearance_on_fluoro_imp_train_outcome <-
  subset(PCNL_clearance_on_fluoro_imp_train,
         select = complete_clearance_on_fluoroscopy)
PCNL_clearance_on_fluoro_imp_train_outcome$complete_clearance_on_fluoroscopy <-
  as.numeric(
    PCNL_clearance_on_fluoro_imp_train_outcome$complete_clearance_on_fluoroscopy
  )
PCNL_clearance_on_fluoro_imp_train_outcome$complete_clearance_on_fluoroscopy <-
  ifelse(
    PCNL_clearance_on_fluoro_imp_train_outcome$complete_clearance_on_fluoroscopy == 2,
    1,
    0
  )
PCNL_clearance_on_fluoro_imp_train_outcome <-
  as.matrix(PCNL_clearance_on_fluoro_imp_train_outcome)

PCNL_clearance_on_fluoro_imp_train_predictors <-
  subset(PCNL_clearance_on_fluoro_imp_train,
         select = -complete_clearance_on_fluoroscopy)

PCNL_clearance_on_fluoro_imp_train_predictors2 <-
  PCNL_clearance_on_fluoro_imp_train_predictors
PCNL_clearance_on_fluoro_imp_train_predictors2$stone_complexity <-
  as.integer(PCNL_clearance_on_fluoro_imp_train_predictors2$stone_complexity)
PCNL_clearance_on_fluoro_imp_train_predictors2$stone_complexity <-
  as.factor(PCNL_clearance_on_fluoro_imp_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_clearance_on_fluoro_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_clearance_on_fluoro_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_clearance_on_fluoro_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_clearance_on_fluoro_imp_train_predictors2$pre_operative_msu_result)
PCNL_clearance_on_fluoro_imp_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_clearance_on_fluoro_imp_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_train_predictors2$stone_dimensions <-
  as.integer(PCNL_clearance_on_fluoro_imp_train_predictors2$stone_dimensions)
PCNL_clearance_on_fluoro_imp_train_predictors2$stone_dimensions <-
  as.factor(PCNL_clearance_on_fluoro_imp_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_train_predictors2$puncture_site <-
  as.integer(PCNL_clearance_on_fluoro_imp_train_predictors2$puncture_site)
PCNL_clearance_on_fluoro_imp_train_predictors2$puncture_site <-
  as.factor(PCNL_clearance_on_fluoro_imp_train_predictors2$puncture_site) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_clearance_on_fluoro_imp_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_clearance_on_fluoro_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_clearance_on_fluoro_imp_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_train_predictors2$renogramdmsa <-
  ifelse(PCNL_clearance_on_fluoro_imp_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_clearance_on_fluoro_imp_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_train_predictors2$calyceal_l <-
  ifelse(PCNL_clearance_on_fluoro_imp_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_train_predictors2$calyceal_m <-
  ifelse(PCNL_clearance_on_fluoro_imp_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_train_predictors2$complete_staghorn <-
  ifelse(PCNL_clearance_on_fluoro_imp_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_train_predictors2$partial_staghorn <-
  ifelse(PCNL_clearance_on_fluoro_imp_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_train_predictors2$pelvic <-
  ifelse(PCNL_clearance_on_fluoro_imp_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_train_predictors2$upper_ureteric <-
  ifelse(PCNL_clearance_on_fluoro_imp_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_train_predictors2$ureter_other <-
  ifelse(PCNL_clearance_on_fluoro_imp_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



clearance_on_fluoro_imp_train_predictors_stone_complexity <-
  as_tibble(PCNL_clearance_on_fluoro_imp_train_predictors2$stone_complexity)
colnames(clearance_on_fluoro_imp_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
clearance_on_fluoro_imp_train_predictors_stone_complexity <-
  subset(clearance_on_fluoro_imp_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_clearance_on_fluoro_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_train_predictors2,
    clearance_on_fluoro_imp_train_predictors_stone_complexity
  )

clearance_on_fluoro_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_clearance_on_fluoro_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(clearance_on_fluoro_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
clearance_on_fluoro_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    clearance_on_fluoro_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_clearance_on_fluoro_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_train_predictors2,
    clearance_on_fluoro_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

clearance_on_fluoro_imp_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_clearance_on_fluoro_imp_train_predictors2$pre_operative_msu_result)
colnames(clearance_on_fluoro_imp_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
clearance_on_fluoro_imp_train_predictors_pre_operative_msu_result <-
  subset(clearance_on_fluoro_imp_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_clearance_on_fluoro_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_train_predictors2,
    clearance_on_fluoro_imp_train_predictors_pre_operative_msu_result
  )

clearance_on_fluoro_imp_train_predictors_puncture_site <-
  as_tibble(PCNL_clearance_on_fluoro_imp_train_predictors2$puncture_site)
colnames(clearance_on_fluoro_imp_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
clearance_on_fluoro_imp_train_predictors_puncture_site <-
  subset(clearance_on_fluoro_imp_train_predictors_puncture_site,
         select = -puncture0)
PCNL_clearance_on_fluoro_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_train_predictors2,
    clearance_on_fluoro_imp_train_predictors_puncture_site
  )

clearance_on_fluoro_imp_train_predictors_stone_dimensions <-
  as_tibble(PCNL_clearance_on_fluoro_imp_train_predictors2$stone_dimensions)
colnames(clearance_on_fluoro_imp_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
clearance_on_fluoro_imp_train_predictors_stone_dimensions <-
  subset(clearance_on_fluoro_imp_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_clearance_on_fluoro_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_train_predictors2,
    clearance_on_fluoro_imp_train_predictors_stone_dimensions
  )

clearance_on_fluoro_imp_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_clearance_on_fluoro_imp_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(clearance_on_fluoro_imp_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
clearance_on_fluoro_imp_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    clearance_on_fluoro_imp_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_clearance_on_fluoro_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_train_predictors2,
    clearance_on_fluoro_imp_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_clearance_on_fluoro_imp_train_predictors2 <-
  subset(
    PCNL_clearance_on_fluoro_imp_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_clearance_on_fluoro_imp_train_predictors2 <-
  as.matrix(PCNL_clearance_on_fluoro_imp_train_predictors2)


##### Oversampling of Imputed Dataset
PCNL_clearance_on_fluoro_imp_train_oversample <-
  ROSE::ovun.sample(
    complete_clearance_on_fluoroscopy ~ .,
    data = PCNL_clearance_on_fluoro_imp_train,
    seed = 1234,
    method = "over"
  )$data
summary(
  PCNL_clearance_on_fluoro_imp_train_oversample$complete_clearance_on_fluoroscopy
)

PCNL_clearance_on_fluoro_imp_oversample_train_outcome <-
  subset(PCNL_clearance_on_fluoro_imp_train_oversample,
         select = complete_clearance_on_fluoroscopy)
PCNL_clearance_on_fluoro_imp_oversample_train_outcome$complete_clearance_on_fluoroscopy <-
  as.numeric(
    PCNL_clearance_on_fluoro_imp_oversample_train_outcome$complete_clearance_on_fluoroscopy
  )
PCNL_clearance_on_fluoro_imp_oversample_train_outcome$complete_clearance_on_fluoroscopy <-
  ifelse(
    PCNL_clearance_on_fluoro_imp_oversample_train_outcome$complete_clearance_on_fluoroscopy == 2,
    1,
    0
  )
PCNL_clearance_on_fluoro_imp_oversample_train_outcome <-
  as.matrix(PCNL_clearance_on_fluoro_imp_oversample_train_outcome)

PCNL_clearance_on_fluoro_imp_oversample_train_predictors <-
  subset(
    PCNL_clearance_on_fluoro_imp_train_oversample,
    select = -complete_clearance_on_fluoroscopy
  )

PCNL_clearance_on_fluoro_imp_oversample_train_predictors2 <-
  PCNL_clearance_on_fluoro_imp_oversample_train_predictors
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$stone_complexity)
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$pre_operative_msu_result
  )
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$pre_operative_msu_result
  ) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$stone_dimensions)
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$puncture_site)
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$renogramdmsa <-
  ifelse(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$renogramdmsa == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$complete_staghorn <-
  ifelse(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$complete_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$partial_staghorn <-
  ifelse(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$partial_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$upper_ureteric <-
  ifelse(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$upper_ureteric == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$ureter_other <-
  ifelse(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$ureter_other == "2",
    1,
    0
  ) %>% as.numeric()



clearance_on_fluoro_imp_oversample_train_predictors_stone_complexity <-
  as_tibble(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$stone_complexity)
colnames(clearance_on_fluoro_imp_oversample_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
clearance_on_fluoro_imp_oversample_train_predictors_stone_complexity <-
  subset(
    clearance_on_fluoro_imp_oversample_train_predictors_stone_complexity,
    select = -GSS0
  )
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2,
    clearance_on_fluoro_imp_oversample_train_predictors_stone_complexity
  )

clearance_on_fluoro_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  clearance_on_fluoro_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
clearance_on_fluoro_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    clearance_on_fluoro_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2,
    clearance_on_fluoro_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

clearance_on_fluoro_imp_oversample_train_predictors_pre_operative_msu_result <-
  as_tibble(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$pre_operative_msu_result
  )
colnames(clearance_on_fluoro_imp_oversample_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
clearance_on_fluoro_imp_oversample_train_predictors_pre_operative_msu_result <-
  subset(
    clearance_on_fluoro_imp_oversample_train_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2,
    clearance_on_fluoro_imp_oversample_train_predictors_pre_operative_msu_result
  )

clearance_on_fluoro_imp_oversample_train_predictors_puncture_site <-
  as_tibble(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$puncture_site)
colnames(clearance_on_fluoro_imp_oversample_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
clearance_on_fluoro_imp_oversample_train_predictors_puncture_site <-
  subset(
    clearance_on_fluoro_imp_oversample_train_predictors_puncture_site,
    select = -puncture0
  )
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2,
    clearance_on_fluoro_imp_oversample_train_predictors_puncture_site
  )

clearance_on_fluoro_imp_oversample_train_predictors_stone_dimensions <-
  as_tibble(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$stone_dimensions)
colnames(clearance_on_fluoro_imp_oversample_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
clearance_on_fluoro_imp_oversample_train_predictors_stone_dimensions <-
  subset(
    clearance_on_fluoro_imp_oversample_train_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2,
    clearance_on_fluoro_imp_oversample_train_predictors_stone_dimensions
  )

clearance_on_fluoro_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(
  clearance_on_fluoro_imp_oversample_train_predictors_image_guidance_for_renal_puncture
) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
clearance_on_fluoro_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    clearance_on_fluoro_imp_oversample_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2,
    clearance_on_fluoro_imp_oversample_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_clearance_on_fluoro_imp_oversample_train_predictors2 <-
  subset(
    PCNL_clearance_on_fluoro_imp_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_clearance_on_fluoro_imp_oversample_train_predictors2 <-
  as.matrix(PCNL_clearance_on_fluoro_imp_oversample_train_predictors2)

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
) %>% as_tibble() %>% janitor::clean_names()

plot_missing(PCNL_original_visc_inj)


##### Omit NA Dataset
PCNL_visc_inj_omit_na <- na.omit(PCNL_original_visc_inj)
str(PCNL_visc_inj_omit_na)

PCNL_visc_inj_sample <-
  sample(1:nrow(PCNL_original_visc_inj),
         size = nrow(PCNL_original_visc_inj) * 0.7)
PCNL_visc_inj_train <-
  PCNL_original_visc_inj[PCNL_visc_inj_sample, ] %>% as_tibble()
PCNL_visc_inj_test <-
  PCNL_original_visc_inj[-PCNL_visc_inj_sample, ] %>% as_tibble()

PCNL_visc_inj_omit_na_sample <-
  sample(1:nrow(PCNL_visc_inj_omit_na),
         size = nrow(PCNL_visc_inj_omit_na) * 0.7)
PCNL_visc_inj_omit_na_train <-
  PCNL_visc_inj_omit_na[PCNL_visc_inj_omit_na_sample, ] %>% as_tibble()
PCNL_visc_inj_omit_na_test <-
  PCNL_visc_inj_omit_na[-PCNL_visc_inj_omit_na_sample, ] %>% as_tibble()
summary(PCNL_visc_inj_omit_na_train$visceral_injury)

PCNL_visc_inj_omit_na_train_outcome <-
  subset(PCNL_visc_inj_omit_na_train,
         select = visceral_injury)
PCNL_visc_inj_omit_na_train_outcome$visceral_injury <-
  as.numeric(PCNL_visc_inj_omit_na_train_outcome$visceral_injury)
PCNL_visc_inj_omit_na_train_outcome$visceral_injury <-
  ifelse(PCNL_visc_inj_omit_na_train_outcome$visceral_injury == 2, 1, 0)
PCNL_visc_inj_omit_na_train_outcome <-
  as.matrix(PCNL_visc_inj_omit_na_train_outcome)
PCNL_visc_inj_omit_na_test_outcome <-
  subset(PCNL_visc_inj_omit_na_test,
         select = visceral_injury)
PCNL_visc_inj_omit_na_test_outcome$visceral_injury <-
  as.numeric(PCNL_visc_inj_omit_na_test_outcome$visceral_injury)
PCNL_visc_inj_omit_na_test_outcome$visceral_injury <-
  ifelse(PCNL_visc_inj_omit_na_test_outcome$visceral_injury == 2, 1, 0)
PCNL_visc_inj_omit_na_test_outcome <-
  as.matrix(PCNL_visc_inj_omit_na_test_outcome)
PCNL_visc_inj_omit_na_train_predictors <-
  subset(PCNL_visc_inj_omit_na_train,
         select = -visceral_injury)
PCNL_visc_inj_omit_na_test_predictors <-
  subset(PCNL_visc_inj_omit_na_test,
         select = -visceral_injury)

PCNL_visc_inj_omit_na_train_predictors2 <-
  PCNL_visc_inj_omit_na_train_predictors
PCNL_visc_inj_omit_na_train_predictors2$stone_complexity <-
  as.integer(PCNL_visc_inj_omit_na_train_predictors2$stone_complexity)
PCNL_visc_inj_omit_na_train_predictors2$stone_complexity <-
  as.factor(PCNL_visc_inj_omit_na_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_visc_inj_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_visc_inj_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_visc_inj_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_visc_inj_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_visc_inj_omit_na_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_visc_inj_omit_na_train_predictors2$pre_operative_msu_result)
PCNL_visc_inj_omit_na_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_visc_inj_omit_na_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_visc_inj_omit_na_train_predictors2$stone_dimensions <-
  as.integer(PCNL_visc_inj_omit_na_train_predictors2$stone_dimensions)
PCNL_visc_inj_omit_na_train_predictors2$stone_dimensions <-
  as.factor(PCNL_visc_inj_omit_na_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_visc_inj_omit_na_train_predictors2$puncture_site <-
  as.integer(PCNL_visc_inj_omit_na_train_predictors2$puncture_site)
PCNL_visc_inj_omit_na_train_predictors2$puncture_site <-
  as.factor(PCNL_visc_inj_omit_na_train_predictors2$puncture_site) %>% to_categorical()
PCNL_visc_inj_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_visc_inj_omit_na_train_predictors2$image_guidance_for_renal_puncture)
PCNL_visc_inj_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_visc_inj_omit_na_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_visc_inj_omit_na_train_predictors2$renogramdmsa <-
  ifelse(PCNL_visc_inj_omit_na_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_visc_inj_omit_na_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_train_predictors2$calyceal_l <-
  ifelse(PCNL_visc_inj_omit_na_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_train_predictors2$calyceal_m <-
  ifelse(PCNL_visc_inj_omit_na_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_train_predictors2$complete_staghorn <-
  ifelse(PCNL_visc_inj_omit_na_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_train_predictors2$partial_staghorn <-
  ifelse(PCNL_visc_inj_omit_na_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_train_predictors2$pelvic <-
  ifelse(PCNL_visc_inj_omit_na_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_train_predictors2$upper_ureteric <-
  ifelse(PCNL_visc_inj_omit_na_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_train_predictors2$ureter_other <-
  ifelse(PCNL_visc_inj_omit_na_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



visc_inj_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_visc_inj_omit_na_train_predictors2$stone_complexity)
colnames(visc_inj_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
visc_inj_na_omit_train_predictors_stone_complexity <-
  subset(visc_inj_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_visc_inj_omit_na_train_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_train_predictors2,
    visc_inj_na_omit_train_predictors_stone_complexity
  )

visc_inj_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_visc_inj_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(visc_inj_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
visc_inj_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    visc_inj_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_visc_inj_omit_na_train_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_train_predictors2,
    visc_inj_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

visc_inj_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_visc_inj_omit_na_train_predictors2$pre_operative_msu_result)
colnames(visc_inj_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
visc_inj_na_omit_train_predictors_pre_operative_msu_result <-
  subset(visc_inj_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_visc_inj_omit_na_train_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_train_predictors2,
    visc_inj_na_omit_train_predictors_pre_operative_msu_result
  )

visc_inj_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_visc_inj_omit_na_train_predictors2$puncture_site)
colnames(visc_inj_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
visc_inj_na_omit_train_predictors_puncture_site <-
  subset(visc_inj_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_visc_inj_omit_na_train_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_train_predictors2,
    visc_inj_na_omit_train_predictors_puncture_site
  )

visc_inj_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_visc_inj_omit_na_train_predictors2$stone_dimensions)
colnames(visc_inj_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
visc_inj_na_omit_train_predictors_stone_dimensions <-
  subset(visc_inj_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_visc_inj_omit_na_train_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_train_predictors2,
    visc_inj_na_omit_train_predictors_stone_dimensions
  )

visc_inj_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_visc_inj_omit_na_train_predictors2$image_guidance_for_renal_puncture)
colnames(visc_inj_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
visc_inj_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    visc_inj_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_visc_inj_omit_na_train_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_train_predictors2,
    visc_inj_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_visc_inj_omit_na_train_predictors2 <-
  subset(
    PCNL_visc_inj_omit_na_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_visc_inj_omit_na_train_predictors2 <-
  as.matrix(PCNL_visc_inj_omit_na_train_predictors2)


PCNL_visc_inj_omit_na_test_predictors2 <-
  PCNL_visc_inj_omit_na_test_predictors
PCNL_visc_inj_omit_na_test_predictors2$stone_complexity <-
  as.integer(PCNL_visc_inj_omit_na_test_predictors2$stone_complexity)
PCNL_visc_inj_omit_na_test_predictors2$stone_complexity <-
  as.factor(PCNL_visc_inj_omit_na_test_predictors2$stone_complexity) %>% to_categorical()
PCNL_visc_inj_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_visc_inj_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_visc_inj_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_visc_inj_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_visc_inj_omit_na_test_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_visc_inj_omit_na_test_predictors2$pre_operative_msu_result)
PCNL_visc_inj_omit_na_test_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_visc_inj_omit_na_test_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_visc_inj_omit_na_test_predictors2$stone_dimensions <-
  as.integer(PCNL_visc_inj_omit_na_test_predictors2$stone_dimensions)
PCNL_visc_inj_omit_na_test_predictors2$stone_dimensions <-
  as.factor(PCNL_visc_inj_omit_na_test_predictors2$stone_dimensions) %>% to_categorical()
PCNL_visc_inj_omit_na_test_predictors2$puncture_site <-
  as.integer(PCNL_visc_inj_omit_na_test_predictors2$puncture_site)
PCNL_visc_inj_omit_na_test_predictors2$puncture_site <-
  as.factor(PCNL_visc_inj_omit_na_test_predictors2$puncture_site) %>% to_categorical()
PCNL_visc_inj_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_visc_inj_omit_na_test_predictors2$image_guidance_for_renal_puncture)
PCNL_visc_inj_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_visc_inj_omit_na_test_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_visc_inj_omit_na_test_predictors2$renogramdmsa <-
  ifelse(PCNL_visc_inj_omit_na_test_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_test_predictors2$calyceal_diverticular <-
  ifelse(PCNL_visc_inj_omit_na_test_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_test_predictors2$calyceal_l <-
  ifelse(PCNL_visc_inj_omit_na_test_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_test_predictors2$calyceal_m <-
  ifelse(PCNL_visc_inj_omit_na_test_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_test_predictors2$complete_staghorn <-
  ifelse(PCNL_visc_inj_omit_na_test_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_test_predictors2$partial_staghorn <-
  ifelse(PCNL_visc_inj_omit_na_test_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_test_predictors2$pelvic <-
  ifelse(PCNL_visc_inj_omit_na_test_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_test_predictors2$upper_ureteric <-
  ifelse(PCNL_visc_inj_omit_na_test_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_omit_na_test_predictors2$ureter_other <-
  ifelse(PCNL_visc_inj_omit_na_test_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



visc_inj_na_omit_test_predictors_stone_complexity <-
  as_tibble(PCNL_visc_inj_omit_na_test_predictors2$stone_complexity)
colnames(visc_inj_na_omit_test_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
visc_inj_na_omit_test_predictors_stone_complexity <-
  subset(visc_inj_na_omit_test_predictors_stone_complexity,
         select = -GSS0)
PCNL_visc_inj_omit_na_test_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_test_predictors2,
    visc_inj_na_omit_test_predictors_stone_complexity
  )

visc_inj_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_visc_inj_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(visc_inj_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
visc_inj_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    visc_inj_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_visc_inj_omit_na_test_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_test_predictors2,
    visc_inj_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
  )

visc_inj_na_omit_test_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_visc_inj_omit_na_test_predictors2$pre_operative_msu_result)
colnames(visc_inj_na_omit_test_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
visc_inj_na_omit_test_predictors_pre_operative_msu_result <-
  subset(visc_inj_na_omit_test_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_visc_inj_omit_na_test_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_test_predictors2,
    visc_inj_na_omit_test_predictors_pre_operative_msu_result
  )

visc_inj_na_omit_test_predictors_puncture_site <-
  as_tibble(PCNL_visc_inj_omit_na_test_predictors2$puncture_site)
colnames(visc_inj_na_omit_test_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
visc_inj_na_omit_test_predictors_puncture_site <-
  subset(visc_inj_na_omit_test_predictors_puncture_site,
         select = -puncture0)
PCNL_visc_inj_omit_na_test_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_test_predictors2,
    visc_inj_na_omit_test_predictors_puncture_site
  )

visc_inj_na_omit_test_predictors_stone_dimensions <-
  as_tibble(PCNL_visc_inj_omit_na_test_predictors2$stone_dimensions)
colnames(visc_inj_na_omit_test_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
visc_inj_na_omit_test_predictors_stone_dimensions <-
  subset(visc_inj_na_omit_test_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_visc_inj_omit_na_test_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_test_predictors2,
    visc_inj_na_omit_test_predictors_stone_dimensions
  )

visc_inj_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_visc_inj_omit_na_test_predictors2$image_guidance_for_renal_puncture)
colnames(visc_inj_na_omit_test_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
visc_inj_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  subset(
    visc_inj_na_omit_test_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_visc_inj_omit_na_test_predictors2 <-
  cbind(
    PCNL_visc_inj_omit_na_test_predictors2,
    visc_inj_na_omit_test_predictors_image_guidance_for_renal_puncture
  )

PCNL_visc_inj_omit_na_test_predictors2 <-
  subset(
    PCNL_visc_inj_omit_na_test_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_visc_inj_omit_na_test_predictors2 <-
  as.matrix(PCNL_visc_inj_omit_na_test_predictors2)

##### Oversampled Dataset
PCNL_visc_inj_omit_na_train_oversample <-
  ROSE::ovun.sample(visceral_injury ~ .,
                    data = PCNL_visc_inj_omit_na_train,
                    seed = 1234,
                    method = "over")$data
summary(PCNL_visc_inj_omit_na_train_oversample$visceral_injury)

PCNL_visc_inj_oversample_train_outcome <-
  subset(PCNL_visc_inj_omit_na_train_oversample,
         select = visceral_injury)
PCNL_visc_inj_oversample_train_outcome$visceral_injury <-
  as.numeric(PCNL_visc_inj_oversample_train_outcome$visceral_injury)
PCNL_visc_inj_oversample_train_outcome$visceral_injury <-
  ifelse(PCNL_visc_inj_oversample_train_outcome$visceral_injury == 2, 1, 0)
PCNL_visc_inj_oversample_train_outcome <-
  as.matrix(PCNL_visc_inj_oversample_train_outcome)

PCNL_visc_inj_oversample_train_predictors <-
  subset(PCNL_visc_inj_omit_na_train_oversample,
         select = -visceral_injury)

PCNL_visc_inj_oversample_train_predictors2 <-
  PCNL_visc_inj_oversample_train_predictors
PCNL_visc_inj_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_visc_inj_oversample_train_predictors2$stone_complexity)
PCNL_visc_inj_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_visc_inj_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_visc_inj_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_visc_inj_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_visc_inj_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_visc_inj_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_visc_inj_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_visc_inj_oversample_train_predictors2$pre_operative_msu_result)
PCNL_visc_inj_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_visc_inj_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_visc_inj_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_visc_inj_oversample_train_predictors2$stone_dimensions)
PCNL_visc_inj_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_visc_inj_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_visc_inj_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_visc_inj_oversample_train_predictors2$puncture_site)
PCNL_visc_inj_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_visc_inj_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_visc_inj_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_visc_inj_oversample_train_predictors2$image_guidance_for_renal_puncture)
PCNL_visc_inj_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_visc_inj_oversample_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_visc_inj_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_visc_inj_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_visc_inj_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_visc_inj_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_visc_inj_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_visc_inj_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_visc_inj_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_visc_inj_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_visc_inj_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_visc_inj_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



visc_inj_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_visc_inj_oversample_train_predictors2$stone_complexity)
colnames(visc_inj_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
visc_inj_na_omit_train_predictors_stone_complexity <-
  subset(visc_inj_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_visc_inj_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_oversample_train_predictors2,
    visc_inj_na_omit_train_predictors_stone_complexity
  )

visc_inj_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_visc_inj_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(visc_inj_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
visc_inj_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    visc_inj_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_visc_inj_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_oversample_train_predictors2,
    visc_inj_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

visc_inj_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_visc_inj_oversample_train_predictors2$pre_operative_msu_result)
colnames(visc_inj_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
visc_inj_na_omit_train_predictors_pre_operative_msu_result <-
  subset(visc_inj_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_visc_inj_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_oversample_train_predictors2,
    visc_inj_na_omit_train_predictors_pre_operative_msu_result
  )

visc_inj_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_visc_inj_oversample_train_predictors2$puncture_site)
colnames(visc_inj_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
visc_inj_na_omit_train_predictors_puncture_site <-
  subset(visc_inj_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_visc_inj_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_oversample_train_predictors2,
    visc_inj_na_omit_train_predictors_puncture_site
  )

visc_inj_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_visc_inj_oversample_train_predictors2$stone_dimensions)
colnames(visc_inj_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
visc_inj_na_omit_train_predictors_stone_dimensions <-
  subset(visc_inj_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_visc_inj_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_oversample_train_predictors2,
    visc_inj_na_omit_train_predictors_stone_dimensions
  )

visc_inj_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_visc_inj_oversample_train_predictors2$image_guidance_for_renal_puncture)
colnames(visc_inj_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
visc_inj_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    visc_inj_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_visc_inj_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_oversample_train_predictors2,
    visc_inj_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_visc_inj_oversample_train_predictors2 <-
  subset(
    PCNL_visc_inj_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_visc_inj_oversample_train_predictors2 <-
  as.matrix(PCNL_visc_inj_oversample_train_predictors2)

##### Imputed Dataset
PCNL_visc_inj_imp_pre <-
  PCNL_visc_inj_omit_na %>% drop_na(visceral_injury)
PCNL_visc_inj_imp1 <- mice(PCNL_visc_inj_imp_pre, m = 1)
summary(PCNL_visc_inj_imp1)

PCNL_visc_inj_imp <-
  mice::complete(PCNL_visc_inj_imp1, 1) %>% as_tibble()

PCNL_visc_inj_imp_sample <-
  sample(1:nrow(PCNL_visc_inj_imp), size = nrow(PCNL_visc_inj_imp) * 0.7)
PCNL_visc_inj_imp_train <-
  PCNL_visc_inj_imp[PCNL_visc_inj_imp_sample, ] %>% as_tibble()
PCNL_visc_inj_imp_test <-
  PCNL_visc_inj_imp[-PCNL_visc_inj_imp_sample, ] %>% as_tibble()
PCNL_visc_inj_imp_train$visceral_injury <-
  as.factor(PCNL_visc_inj_imp_train$visceral_injury)
PCNL_visc_inj_imp_test$visceral_injury <-
  as.factor(PCNL_visc_inj_imp_test$visceral_injury)
summary(PCNL_visc_inj_imp_train$visceral_injury)

PCNL_visc_inj_imp_train_outcome <-
  subset(PCNL_visc_inj_imp_train,
         select = visceral_injury)
PCNL_visc_inj_imp_train_outcome$visceral_injury <-
  as.numeric(PCNL_visc_inj_imp_train_outcome$visceral_injury)
PCNL_visc_inj_imp_train_outcome$visceral_injury <-
  ifelse(PCNL_visc_inj_imp_train_outcome$visceral_injury == 2, 1, 0)
PCNL_visc_inj_imp_train_outcome <-
  as.matrix(PCNL_visc_inj_imp_train_outcome)

PCNL_visc_inj_imp_train_predictors <-
  subset(PCNL_visc_inj_imp_train,
         select = -visceral_injury)

PCNL_visc_inj_imp_train_predictors2 <-
  PCNL_visc_inj_imp_train_predictors
PCNL_visc_inj_imp_train_predictors2$stone_complexity <-
  as.integer(PCNL_visc_inj_imp_train_predictors2$stone_complexity)
PCNL_visc_inj_imp_train_predictors2$stone_complexity <-
  as.factor(PCNL_visc_inj_imp_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_visc_inj_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_visc_inj_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_visc_inj_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_visc_inj_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_visc_inj_imp_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_visc_inj_imp_train_predictors2$pre_operative_msu_result)
PCNL_visc_inj_imp_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_visc_inj_imp_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_visc_inj_imp_train_predictors2$stone_dimensions <-
  as.integer(PCNL_visc_inj_imp_train_predictors2$stone_dimensions)
PCNL_visc_inj_imp_train_predictors2$stone_dimensions <-
  as.factor(PCNL_visc_inj_imp_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_visc_inj_imp_train_predictors2$puncture_site <-
  as.integer(PCNL_visc_inj_imp_train_predictors2$puncture_site)
PCNL_visc_inj_imp_train_predictors2$puncture_site <-
  as.factor(PCNL_visc_inj_imp_train_predictors2$puncture_site) %>% to_categorical()
PCNL_visc_inj_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_visc_inj_imp_train_predictors2$image_guidance_for_renal_puncture)
PCNL_visc_inj_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_visc_inj_imp_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_visc_inj_imp_train_predictors2$renogramdmsa <-
  ifelse(PCNL_visc_inj_imp_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_visc_inj_imp_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_train_predictors2$calyceal_l <-
  ifelse(PCNL_visc_inj_imp_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_train_predictors2$calyceal_m <-
  ifelse(PCNL_visc_inj_imp_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_train_predictors2$complete_staghorn <-
  ifelse(PCNL_visc_inj_imp_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_train_predictors2$partial_staghorn <-
  ifelse(PCNL_visc_inj_imp_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_train_predictors2$pelvic <-
  ifelse(PCNL_visc_inj_imp_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_train_predictors2$upper_ureteric <-
  ifelse(PCNL_visc_inj_imp_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_train_predictors2$ureter_other <-
  ifelse(PCNL_visc_inj_imp_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



visc_inj_imp_train_predictors_stone_complexity <-
  as_tibble(PCNL_visc_inj_imp_train_predictors2$stone_complexity)
colnames(visc_inj_imp_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
visc_inj_imp_train_predictors_stone_complexity <-
  subset(visc_inj_imp_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_visc_inj_imp_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_train_predictors2,
    visc_inj_imp_train_predictors_stone_complexity
  )

visc_inj_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_visc_inj_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(visc_inj_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
visc_inj_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    visc_inj_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_visc_inj_imp_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_train_predictors2,
    visc_inj_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

visc_inj_imp_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_visc_inj_imp_train_predictors2$pre_operative_msu_result)
colnames(visc_inj_imp_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
visc_inj_imp_train_predictors_pre_operative_msu_result <-
  subset(visc_inj_imp_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_visc_inj_imp_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_train_predictors2,
    visc_inj_imp_train_predictors_pre_operative_msu_result
  )

visc_inj_imp_train_predictors_puncture_site <-
  as_tibble(PCNL_visc_inj_imp_train_predictors2$puncture_site)
colnames(visc_inj_imp_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
visc_inj_imp_train_predictors_puncture_site <-
  subset(visc_inj_imp_train_predictors_puncture_site,
         select = -puncture0)
PCNL_visc_inj_imp_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_train_predictors2,
    visc_inj_imp_train_predictors_puncture_site
  )

visc_inj_imp_train_predictors_stone_dimensions <-
  as_tibble(PCNL_visc_inj_imp_train_predictors2$stone_dimensions)
colnames(visc_inj_imp_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
visc_inj_imp_train_predictors_stone_dimensions <-
  subset(visc_inj_imp_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_visc_inj_imp_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_train_predictors2,
    visc_inj_imp_train_predictors_stone_dimensions
  )

visc_inj_imp_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_visc_inj_imp_train_predictors2$image_guidance_for_renal_puncture)
colnames(visc_inj_imp_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
visc_inj_imp_train_predictors_image_guidance_for_renal_puncture <-
  subset(visc_inj_imp_train_predictors_image_guidance_for_renal_puncture,
         select = -image0)
PCNL_visc_inj_imp_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_train_predictors2,
    visc_inj_imp_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_visc_inj_imp_train_predictors2 <-
  subset(
    PCNL_visc_inj_imp_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_visc_inj_imp_train_predictors2 <-
  as.matrix(PCNL_visc_inj_imp_train_predictors2)


##### Oversampling of Imputed Dataset
PCNL_visc_inj_imp_train_oversample <-
  ROSE::ovun.sample(visceral_injury ~ .,
                    data = PCNL_visc_inj_imp_train,
                    seed = 1234,
                    method = "over")$data
summary(PCNL_visc_inj_imp_train_oversample$visceral_injury)

PCNL_visc_inj_imp_oversample_train_outcome <-
  subset(PCNL_visc_inj_imp_train_oversample,
         select = visceral_injury)
PCNL_visc_inj_imp_oversample_train_outcome$visceral_injury <-
  as.numeric(PCNL_visc_inj_imp_oversample_train_outcome$visceral_injury)
PCNL_visc_inj_imp_oversample_train_outcome$visceral_injury <-
  ifelse(PCNL_visc_inj_imp_oversample_train_outcome$visceral_injury == 2, 1, 0)
PCNL_visc_inj_imp_oversample_train_outcome <-
  as.matrix(PCNL_visc_inj_imp_oversample_train_outcome)

PCNL_visc_inj_imp_oversample_train_predictors <-
  subset(PCNL_visc_inj_imp_train_oversample,
         select = -visceral_injury)

PCNL_visc_inj_imp_oversample_train_predictors2 <-
  PCNL_visc_inj_imp_oversample_train_predictors
PCNL_visc_inj_imp_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_visc_inj_imp_oversample_train_predictors2$stone_complexity)
PCNL_visc_inj_imp_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_visc_inj_imp_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_visc_inj_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_visc_inj_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_visc_inj_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_visc_inj_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_visc_inj_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_visc_inj_imp_oversample_train_predictors2$pre_operative_msu_result)
PCNL_visc_inj_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_visc_inj_imp_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_visc_inj_imp_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_visc_inj_imp_oversample_train_predictors2$stone_dimensions)
PCNL_visc_inj_imp_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_visc_inj_imp_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_visc_inj_imp_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_visc_inj_imp_oversample_train_predictors2$puncture_site)
PCNL_visc_inj_imp_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_visc_inj_imp_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_visc_inj_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_visc_inj_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_visc_inj_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_visc_inj_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_visc_inj_imp_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_visc_inj_imp_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_visc_inj_imp_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_visc_inj_imp_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_visc_inj_imp_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_visc_inj_imp_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_visc_inj_imp_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_visc_inj_imp_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_visc_inj_imp_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_visc_inj_imp_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_visc_inj_imp_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



visc_inj_imp_oversample_train_predictors_stone_complexity <-
  as_tibble(PCNL_visc_inj_imp_oversample_train_predictors2$stone_complexity)
colnames(visc_inj_imp_oversample_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
visc_inj_imp_oversample_train_predictors_stone_complexity <-
  subset(visc_inj_imp_oversample_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_visc_inj_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_oversample_train_predictors2,
    visc_inj_imp_oversample_train_predictors_stone_complexity
  )

visc_inj_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_visc_inj_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(visc_inj_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
visc_inj_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    visc_inj_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_visc_inj_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_oversample_train_predictors2,
    visc_inj_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

visc_inj_imp_oversample_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_visc_inj_imp_oversample_train_predictors2$pre_operative_msu_result)
colnames(visc_inj_imp_oversample_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
visc_inj_imp_oversample_train_predictors_pre_operative_msu_result <-
  subset(visc_inj_imp_oversample_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_visc_inj_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_oversample_train_predictors2,
    visc_inj_imp_oversample_train_predictors_pre_operative_msu_result
  )

visc_inj_imp_oversample_train_predictors_puncture_site <-
  as_tibble(PCNL_visc_inj_imp_oversample_train_predictors2$puncture_site)
colnames(visc_inj_imp_oversample_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
visc_inj_imp_oversample_train_predictors_puncture_site <-
  subset(visc_inj_imp_oversample_train_predictors_puncture_site,
         select = -puncture0)
PCNL_visc_inj_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_oversample_train_predictors2,
    visc_inj_imp_oversample_train_predictors_puncture_site
  )

visc_inj_imp_oversample_train_predictors_stone_dimensions <-
  as_tibble(PCNL_visc_inj_imp_oversample_train_predictors2$stone_dimensions)
colnames(visc_inj_imp_oversample_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
visc_inj_imp_oversample_train_predictors_stone_dimensions <-
  subset(visc_inj_imp_oversample_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_visc_inj_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_oversample_train_predictors2,
    visc_inj_imp_oversample_train_predictors_stone_dimensions
  )

visc_inj_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_visc_inj_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(visc_inj_imp_oversample_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
visc_inj_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    visc_inj_imp_oversample_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_visc_inj_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_visc_inj_imp_oversample_train_predictors2,
    visc_inj_imp_oversample_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_visc_inj_imp_oversample_train_predictors2 <-
  subset(
    PCNL_visc_inj_imp_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_visc_inj_imp_oversample_train_predictors2 <-
  as.matrix(PCNL_visc_inj_imp_oversample_train_predictors2)


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
) %>% as_tibble() %>% janitor::clean_names()

plot_missing(PCNL_original_clearance_during_admission)
summary(
  PCNL_original_clearance_during_admission$clearance_on_post_operative_radiological_imaging_during_a
)

clearance_list <-
  split(
    PCNL_original_clearance_during_admission,
    PCNL_original_clearance_during_admission$clearance_on_post_operative_radiological_imaging_during_a
  )

PCNL_original_clearance_during_admission <-
  rbind(clearance_list$Yes,
        clearance_list$No)
PCNL_original_clearance_during_admission$clearance_on_post_operative_radiological_imaging_during_a <-
  factor(
    PCNL_original_clearance_during_admission$clearance_on_post_operative_radiological_imaging_during_a,
    levels = c("No",
               "Yes")
  )

##### Omit NA Dataset
PCNL_clearance_during_admission_omit_na <-
  na.omit(PCNL_original_clearance_during_admission)
str(PCNL_clearance_during_admission_omit_na)

PCNL_clearance_during_admission_sample <-
  sample(
    1:nrow(PCNL_original_clearance_during_admission),
    size = nrow(PCNL_original_clearance_during_admission) * 0.7
  )
PCNL_clearance_during_admission_train <-
  PCNL_original_clearance_during_admission[PCNL_clearance_during_admission_sample, ] %>% as_tibble()
PCNL_clearance_during_admission_test <-
  PCNL_original_clearance_during_admission[-PCNL_clearance_during_admission_sample, ] %>% as_tibble()

PCNL_clearance_during_admission_omit_na_sample <-
  sample(
    1:nrow(PCNL_clearance_during_admission_omit_na),
    size = nrow(PCNL_clearance_during_admission_omit_na) * 0.7
  )
PCNL_clearance_during_admission_omit_na_train <-
  PCNL_clearance_during_admission_omit_na[PCNL_clearance_during_admission_omit_na_sample, ] %>% as_tibble()
PCNL_clearance_during_admission_omit_na_test <-
  PCNL_clearance_during_admission_omit_na[-PCNL_clearance_during_admission_omit_na_sample, ] %>% as_tibble()
summary(
  PCNL_clearance_during_admission_omit_na_train$clearance_on_post_operative_radiological_imaging_during_a
)

PCNL_clearance_during_admission_omit_na_train_outcome <-
  subset(PCNL_clearance_during_admission_omit_na_train,
         select = clearance_on_post_operative_radiological_imaging_during_a)
PCNL_clearance_during_admission_omit_na_train_outcome$clearance_on_post_operative_radiological_imaging_during_a <-
  as.numeric(
    PCNL_clearance_during_admission_omit_na_train_outcome$clearance_on_post_operative_radiological_imaging_during_a
  )
PCNL_clearance_during_admission_omit_na_train_outcome$clearance_on_post_operative_radiological_imaging_during_a <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_train_outcome$clearance_on_post_operative_radiological_imaging_during_a == 2,
    1,
    0
  )
PCNL_clearance_during_admission_omit_na_train_outcome <-
  as.matrix(PCNL_clearance_during_admission_omit_na_train_outcome)
PCNL_clearance_during_admission_omit_na_test_outcome <-
  subset(PCNL_clearance_during_admission_omit_na_test,
         select = clearance_on_post_operative_radiological_imaging_during_a)
PCNL_clearance_during_admission_omit_na_test_outcome$clearance_on_post_operative_radiological_imaging_during_a <-
  as.numeric(
    PCNL_clearance_during_admission_omit_na_test_outcome$clearance_on_post_operative_radiological_imaging_during_a
  )
PCNL_clearance_during_admission_omit_na_test_outcome$clearance_on_post_operative_radiological_imaging_during_a <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_test_outcome$clearance_on_post_operative_radiological_imaging_during_a == 2,
    1,
    0
  )
PCNL_clearance_during_admission_omit_na_test_outcome <-
  as.matrix(PCNL_clearance_during_admission_omit_na_test_outcome)
PCNL_clearance_during_admission_omit_na_train_predictors <-
  subset(
    PCNL_clearance_during_admission_omit_na_train,
    select = -clearance_on_post_operative_radiological_imaging_during_a
  )
PCNL_clearance_during_admission_omit_na_test_predictors <-
  subset(
    PCNL_clearance_during_admission_omit_na_test,
    select = -clearance_on_post_operative_radiological_imaging_during_a
  )

PCNL_clearance_during_admission_omit_na_train_predictors2 <-
  PCNL_clearance_during_admission_omit_na_train_predictors
PCNL_clearance_during_admission_omit_na_train_predictors2$stone_complexity <-
  as.integer(PCNL_clearance_during_admission_omit_na_train_predictors2$stone_complexity)
PCNL_clearance_during_admission_omit_na_train_predictors2$stone_complexity <-
  as.factor(PCNL_clearance_during_admission_omit_na_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_clearance_during_admission_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_clearance_during_admission_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_clearance_during_admission_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_train_predictors2$pre_operative_msu_result <-
  as.integer(
    PCNL_clearance_during_admission_omit_na_train_predictors2$pre_operative_msu_result
  )
PCNL_clearance_during_admission_omit_na_train_predictors2$pre_operative_msu_result <-
  as.factor(
    PCNL_clearance_during_admission_omit_na_train_predictors2$pre_operative_msu_result
  ) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_train_predictors2$stone_dimensions <-
  as.integer(PCNL_clearance_during_admission_omit_na_train_predictors2$stone_dimensions)
PCNL_clearance_during_admission_omit_na_train_predictors2$stone_dimensions <-
  as.factor(PCNL_clearance_during_admission_omit_na_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_train_predictors2$puncture_site <-
  as.integer(PCNL_clearance_during_admission_omit_na_train_predictors2$puncture_site)
PCNL_clearance_during_admission_omit_na_train_predictors2$puncture_site <-
  as.factor(PCNL_clearance_during_admission_omit_na_train_predictors2$puncture_site) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_clearance_during_admission_omit_na_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_clearance_during_admission_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_clearance_during_admission_omit_na_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_train_predictors2$renogramdmsa <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_train_predictors2$renogramdmsa == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_train_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_train_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_train_predictors2$calyceal_l <-
  ifelse(PCNL_clearance_during_admission_omit_na_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_train_predictors2$calyceal_m <-
  ifelse(PCNL_clearance_during_admission_omit_na_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_train_predictors2$complete_staghorn <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_train_predictors2$complete_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_train_predictors2$partial_staghorn <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_train_predictors2$partial_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_train_predictors2$pelvic <-
  ifelse(PCNL_clearance_during_admission_omit_na_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_train_predictors2$upper_ureteric <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_train_predictors2$upper_ureteric == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_train_predictors2$ureter_other <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_train_predictors2$ureter_other == "2",
    1,
    0
  ) %>% as.numeric()



clearance_during_admission_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_clearance_during_admission_omit_na_train_predictors2$stone_complexity)
colnames(clearance_during_admission_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
clearance_during_admission_na_omit_train_predictors_stone_complexity <-
  subset(
    clearance_during_admission_na_omit_train_predictors_stone_complexity,
    select = -GSS0
  )
PCNL_clearance_during_admission_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_stone_complexity
  )

clearance_during_admission_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_clearance_during_admission_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  clearance_during_admission_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
clearance_during_admission_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    clearance_during_admission_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_clearance_during_admission_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

clearance_during_admission_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(
    PCNL_clearance_during_admission_omit_na_train_predictors2$pre_operative_msu_result
  )
colnames(clearance_during_admission_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
clearance_during_admission_na_omit_train_predictors_pre_operative_msu_result <-
  subset(
    clearance_during_admission_na_omit_train_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_clearance_during_admission_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_pre_operative_msu_result
  )

clearance_during_admission_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_clearance_during_admission_omit_na_train_predictors2$puncture_site)
colnames(clearance_during_admission_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
clearance_during_admission_na_omit_train_predictors_puncture_site <-
  subset(
    clearance_during_admission_na_omit_train_predictors_puncture_site,
    select = -puncture0
  )
PCNL_clearance_during_admission_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_puncture_site
  )

clearance_during_admission_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_clearance_during_admission_omit_na_train_predictors2$stone_dimensions)
colnames(clearance_during_admission_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
clearance_during_admission_na_omit_train_predictors_stone_dimensions <-
  subset(
    clearance_during_admission_na_omit_train_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_clearance_during_admission_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_stone_dimensions
  )

clearance_during_admission_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_clearance_during_admission_omit_na_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(
  clearance_during_admission_na_omit_train_predictors_image_guidance_for_renal_puncture
) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
clearance_during_admission_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    clearance_during_admission_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_clearance_during_admission_omit_na_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_clearance_during_admission_omit_na_train_predictors2 <-
  subset(
    PCNL_clearance_during_admission_omit_na_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_clearance_during_admission_omit_na_train_predictors2 <-
  as.matrix(PCNL_clearance_during_admission_omit_na_train_predictors2)


PCNL_clearance_during_admission_omit_na_test_predictors2 <-
  PCNL_clearance_during_admission_omit_na_test_predictors
PCNL_clearance_during_admission_omit_na_test_predictors2$stone_complexity <-
  as.integer(PCNL_clearance_during_admission_omit_na_test_predictors2$stone_complexity)
PCNL_clearance_during_admission_omit_na_test_predictors2$stone_complexity <-
  as.factor(PCNL_clearance_during_admission_omit_na_test_predictors2$stone_complexity) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_clearance_during_admission_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_clearance_during_admission_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_clearance_during_admission_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_test_predictors2$pre_operative_msu_result <-
  as.integer(
    PCNL_clearance_during_admission_omit_na_test_predictors2$pre_operative_msu_result
  )
PCNL_clearance_during_admission_omit_na_test_predictors2$pre_operative_msu_result <-
  as.factor(
    PCNL_clearance_during_admission_omit_na_test_predictors2$pre_operative_msu_result
  ) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_test_predictors2$stone_dimensions <-
  as.integer(PCNL_clearance_during_admission_omit_na_test_predictors2$stone_dimensions)
PCNL_clearance_during_admission_omit_na_test_predictors2$stone_dimensions <-
  as.factor(PCNL_clearance_during_admission_omit_na_test_predictors2$stone_dimensions) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_test_predictors2$puncture_site <-
  as.integer(PCNL_clearance_during_admission_omit_na_test_predictors2$puncture_site)
PCNL_clearance_during_admission_omit_na_test_predictors2$puncture_site <-
  as.factor(PCNL_clearance_during_admission_omit_na_test_predictors2$puncture_site) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_clearance_during_admission_omit_na_test_predictors2$image_guidance_for_renal_puncture
  )
PCNL_clearance_during_admission_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_clearance_during_admission_omit_na_test_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_clearance_during_admission_omit_na_test_predictors2$renogramdmsa <-
  ifelse(PCNL_clearance_during_admission_omit_na_test_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_test_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_test_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_test_predictors2$calyceal_l <-
  ifelse(PCNL_clearance_during_admission_omit_na_test_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_test_predictors2$calyceal_m <-
  ifelse(PCNL_clearance_during_admission_omit_na_test_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_test_predictors2$complete_staghorn <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_test_predictors2$complete_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_test_predictors2$partial_staghorn <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_test_predictors2$partial_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_test_predictors2$pelvic <-
  ifelse(PCNL_clearance_during_admission_omit_na_test_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_test_predictors2$upper_ureteric <-
  ifelse(
    PCNL_clearance_during_admission_omit_na_test_predictors2$upper_ureteric == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_omit_na_test_predictors2$ureter_other <-
  ifelse(PCNL_clearance_during_admission_omit_na_test_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



clearance_during_admission_na_omit_test_predictors_stone_complexity <-
  as_tibble(PCNL_clearance_during_admission_omit_na_test_predictors2$stone_complexity)
colnames(clearance_during_admission_na_omit_test_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
clearance_during_admission_na_omit_test_predictors_stone_complexity <-
  subset(
    clearance_during_admission_na_omit_test_predictors_stone_complexity,
    select = -GSS0
  )
PCNL_clearance_during_admission_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_test_predictors2,
    clearance_during_admission_na_omit_test_predictors_stone_complexity
  )

clearance_during_admission_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_clearance_during_admission_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  clearance_during_admission_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
clearance_during_admission_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    clearance_during_admission_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_clearance_during_admission_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_test_predictors2,
    clearance_during_admission_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
  )

clearance_during_admission_na_omit_test_predictors_pre_operative_msu_result <-
  as_tibble(
    PCNL_clearance_during_admission_omit_na_test_predictors2$pre_operative_msu_result
  )
colnames(clearance_during_admission_na_omit_test_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
clearance_during_admission_na_omit_test_predictors_pre_operative_msu_result <-
  subset(
    clearance_during_admission_na_omit_test_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_clearance_during_admission_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_test_predictors2,
    clearance_during_admission_na_omit_test_predictors_pre_operative_msu_result
  )

clearance_during_admission_na_omit_test_predictors_puncture_site <-
  as_tibble(PCNL_clearance_during_admission_omit_na_test_predictors2$puncture_site)
colnames(clearance_during_admission_na_omit_test_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
clearance_during_admission_na_omit_test_predictors_puncture_site <-
  subset(
    clearance_during_admission_na_omit_test_predictors_puncture_site,
    select = -puncture0
  )
PCNL_clearance_during_admission_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_test_predictors2,
    clearance_during_admission_na_omit_test_predictors_puncture_site
  )

clearance_during_admission_na_omit_test_predictors_stone_dimensions <-
  as_tibble(PCNL_clearance_during_admission_omit_na_test_predictors2$stone_dimensions)
colnames(clearance_during_admission_na_omit_test_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
clearance_during_admission_na_omit_test_predictors_stone_dimensions <-
  subset(
    clearance_during_admission_na_omit_test_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_clearance_during_admission_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_test_predictors2,
    clearance_during_admission_na_omit_test_predictors_stone_dimensions
  )

clearance_during_admission_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_clearance_during_admission_omit_na_test_predictors2$image_guidance_for_renal_puncture
  )
colnames(
  clearance_during_admission_na_omit_test_predictors_image_guidance_for_renal_puncture
) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
clearance_during_admission_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  subset(
    clearance_during_admission_na_omit_test_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_clearance_during_admission_omit_na_test_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_omit_na_test_predictors2,
    clearance_during_admission_na_omit_test_predictors_image_guidance_for_renal_puncture
  )

PCNL_clearance_during_admission_omit_na_test_predictors2 <-
  subset(
    PCNL_clearance_during_admission_omit_na_test_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_clearance_during_admission_omit_na_test_predictors2 <-
  as.matrix(PCNL_clearance_during_admission_omit_na_test_predictors2)

##### Oversampled Dataset
PCNL_clearance_during_admission_omit_na_train_oversample <-
  ROSE::ovun.sample(
    clearance_on_post_operative_radiological_imaging_during_a ~ .,
    data = PCNL_clearance_during_admission_omit_na_train,
    seed = 1234,
    method = "over"
  )$data
summary(
  PCNL_clearance_during_admission_omit_na_train_oversample$clearance_on_post_operative_radiological_imaging_during_a
)

PCNL_clearance_during_admission_oversample_train_outcome <-
  subset(PCNL_clearance_during_admission_omit_na_train_oversample,
         select = clearance_on_post_operative_radiological_imaging_during_a)
PCNL_clearance_during_admission_oversample_train_outcome$clearance_on_post_operative_radiological_imaging_during_a <-
  as.numeric(
    PCNL_clearance_during_admission_oversample_train_outcome$clearance_on_post_operative_radiological_imaging_during_a
  )
PCNL_clearance_during_admission_oversample_train_outcome <-
  as.matrix(PCNL_clearance_during_admission_oversample_train_outcome)
PCNL_clearance_during_admission_oversample_train_predictors <-
  subset(
    PCNL_clearance_during_admission_omit_na_train_oversample,
    select = -clearance_on_post_operative_radiological_imaging_during_a
  )

PCNL_clearance_during_admission_oversample_train_predictors2 <-
  PCNL_clearance_during_admission_oversample_train_predictors
PCNL_clearance_during_admission_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_clearance_during_admission_oversample_train_predictors2$stone_complexity)
PCNL_clearance_during_admission_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_clearance_during_admission_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_clearance_during_admission_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_clearance_during_admission_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_clearance_during_admission_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_clearance_during_admission_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_clearance_during_admission_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(
    PCNL_clearance_during_admission_oversample_train_predictors2$pre_operative_msu_result
  )
PCNL_clearance_during_admission_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(
    PCNL_clearance_during_admission_oversample_train_predictors2$pre_operative_msu_result
  ) %>% to_categorical()
PCNL_clearance_during_admission_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_clearance_during_admission_oversample_train_predictors2$stone_dimensions)
PCNL_clearance_during_admission_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_clearance_during_admission_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_clearance_during_admission_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_clearance_during_admission_oversample_train_predictors2$puncture_site)
PCNL_clearance_during_admission_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_clearance_during_admission_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_clearance_during_admission_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_clearance_during_admission_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_clearance_during_admission_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_clearance_during_admission_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_clearance_during_admission_oversample_train_predictors2$renogramdmsa <-
  ifelse(
    PCNL_clearance_during_admission_oversample_train_predictors2$renogramdmsa == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_clearance_during_admission_oversample_train_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_oversample_train_predictors2$calyceal_l <-
  ifelse(
    PCNL_clearance_during_admission_oversample_train_predictors2$calyceal_l == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_oversample_train_predictors2$calyceal_m <-
  ifelse(
    PCNL_clearance_during_admission_oversample_train_predictors2$calyceal_m == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_oversample_train_predictors2$complete_staghorn <-
  ifelse(
    PCNL_clearance_during_admission_oversample_train_predictors2$complete_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_oversample_train_predictors2$partial_staghorn <-
  ifelse(
    PCNL_clearance_during_admission_oversample_train_predictors2$partial_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_clearance_during_admission_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_oversample_train_predictors2$upper_ureteric <-
  ifelse(
    PCNL_clearance_during_admission_oversample_train_predictors2$upper_ureteric == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_oversample_train_predictors2$ureter_other <-
  ifelse(
    PCNL_clearance_during_admission_oversample_train_predictors2$ureter_other == "2",
    1,
    0
  ) %>% as.numeric()



clearance_during_admission_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_clearance_during_admission_oversample_train_predictors2$stone_complexity)
colnames(clearance_during_admission_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
clearance_during_admission_na_omit_train_predictors_stone_complexity <-
  subset(
    clearance_during_admission_na_omit_train_predictors_stone_complexity,
    select = -GSS0
  )
PCNL_clearance_during_admission_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_oversample_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_stone_complexity
  )

clearance_during_admission_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_clearance_during_admission_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  clearance_during_admission_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
clearance_during_admission_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    clearance_during_admission_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_clearance_during_admission_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_oversample_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

clearance_during_admission_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(
    PCNL_clearance_during_admission_oversample_train_predictors2$pre_operative_msu_result
  )
colnames(clearance_during_admission_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
clearance_during_admission_na_omit_train_predictors_pre_operative_msu_result <-
  subset(
    clearance_during_admission_na_omit_train_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_clearance_during_admission_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_oversample_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_pre_operative_msu_result
  )

clearance_during_admission_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_clearance_during_admission_oversample_train_predictors2$puncture_site)
colnames(clearance_during_admission_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
clearance_during_admission_na_omit_train_predictors_puncture_site <-
  subset(
    clearance_during_admission_na_omit_train_predictors_puncture_site,
    select = -puncture0
  )
PCNL_clearance_during_admission_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_oversample_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_puncture_site
  )

clearance_during_admission_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_clearance_during_admission_oversample_train_predictors2$stone_dimensions)
colnames(clearance_during_admission_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
clearance_during_admission_na_omit_train_predictors_stone_dimensions <-
  subset(
    clearance_during_admission_na_omit_train_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_clearance_during_admission_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_oversample_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_stone_dimensions
  )

clearance_during_admission_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_clearance_during_admission_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(
  clearance_during_admission_na_omit_train_predictors_image_guidance_for_renal_puncture
) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
clearance_during_admission_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    clearance_during_admission_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_clearance_during_admission_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_oversample_train_predictors2,
    clearance_during_admission_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_clearance_during_admission_oversample_train_predictors2 <-
  subset(
    PCNL_clearance_during_admission_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_clearance_during_admission_oversample_train_predictors2 <-
  as.matrix(PCNL_clearance_during_admission_oversample_train_predictors2)

##### Imputed Dataset
PCNL_clearance_during_admission_imp_pre <-
  PCNL_clearance_during_admission_omit_na %>% drop_na(clearance_on_post_operative_radiological_imaging_during_a)
PCNL_clearance_during_admission_imp1 <-
  mice(PCNL_clearance_during_admission_imp_pre, m = 1)
summary(PCNL_clearance_during_admission_imp1)

PCNL_clearance_during_admission_imp <-
  mice::complete(PCNL_clearance_during_admission_imp1, 1) %>% as_tibble()

PCNL_clearance_during_admission_imp_sample <-
  sample(
    1:nrow(PCNL_clearance_during_admission_imp),
    size = nrow(PCNL_clearance_during_admission_imp) * 0.7
  )
PCNL_clearance_during_admission_imp_train <-
  PCNL_clearance_during_admission_imp[PCNL_clearance_during_admission_imp_sample, ] %>% as_tibble()
PCNL_clearance_during_admission_imp_test <-
  PCNL_clearance_during_admission_imp[-PCNL_clearance_during_admission_imp_sample, ] %>% as_tibble()
PCNL_clearance_during_admission_imp_train$clearance_on_post_operative_radiological_imaging_during_a <-
  as.factor(
    PCNL_clearance_during_admission_imp_train$clearance_on_post_operative_radiological_imaging_during_a
  )
PCNL_clearance_during_admission_imp_test$clearance_on_post_operative_radiological_imaging_during_a <-
  as.factor(
    PCNL_clearance_during_admission_imp_test$clearance_on_post_operative_radiological_imaging_during_a
  )
summary(
  PCNL_clearance_during_admission_imp_train$clearance_on_post_operative_radiological_imaging_during_a
)

PCNL_clearance_during_admission_imp_train_outcome <-
  subset(PCNL_clearance_during_admission_imp_train,
         select = clearance_on_post_operative_radiological_imaging_during_a)
PCNL_clearance_during_admission_imp_train_outcome$clearance_on_post_operative_radiological_imaging_during_a <-
  as.numeric(
    PCNL_clearance_during_admission_imp_train_outcome$clearance_on_post_operative_radiological_imaging_during_a
  )
PCNL_clearance_during_admission_imp_train_outcome <-
  as.matrix(PCNL_clearance_during_admission_imp_train_outcome)
PCNL_clearance_during_admission_imp_train_predictors <-
  subset(
    PCNL_clearance_during_admission_imp_train,
    select = -clearance_on_post_operative_radiological_imaging_during_a
  )

PCNL_clearance_during_admission_imp_train_predictors2 <-
  PCNL_clearance_during_admission_imp_train_predictors
PCNL_clearance_during_admission_imp_train_predictors2$stone_complexity <-
  as.integer(PCNL_clearance_during_admission_imp_train_predictors2$stone_complexity)
PCNL_clearance_during_admission_imp_train_predictors2$stone_complexity <-
  as.factor(PCNL_clearance_during_admission_imp_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_clearance_during_admission_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_clearance_during_admission_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_clearance_during_admission_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_clearance_during_admission_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_clearance_during_admission_imp_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_clearance_during_admission_imp_train_predictors2$pre_operative_msu_result)
PCNL_clearance_during_admission_imp_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_clearance_during_admission_imp_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_clearance_during_admission_imp_train_predictors2$stone_dimensions <-
  as.integer(PCNL_clearance_during_admission_imp_train_predictors2$stone_dimensions)
PCNL_clearance_during_admission_imp_train_predictors2$stone_dimensions <-
  as.factor(PCNL_clearance_during_admission_imp_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_clearance_during_admission_imp_train_predictors2$puncture_site <-
  as.integer(PCNL_clearance_during_admission_imp_train_predictors2$puncture_site)
PCNL_clearance_during_admission_imp_train_predictors2$puncture_site <-
  as.factor(PCNL_clearance_during_admission_imp_train_predictors2$puncture_site) %>% to_categorical()
PCNL_clearance_during_admission_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_clearance_during_admission_imp_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_clearance_during_admission_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_clearance_during_admission_imp_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_clearance_during_admission_imp_train_predictors2$renogramdmsa <-
  ifelse(PCNL_clearance_during_admission_imp_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_imp_train_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_clearance_during_admission_imp_train_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_imp_train_predictors2$calyceal_l <-
  ifelse(PCNL_clearance_during_admission_imp_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_imp_train_predictors2$calyceal_m <-
  ifelse(PCNL_clearance_during_admission_imp_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_imp_train_predictors2$complete_staghorn <-
  ifelse(
    PCNL_clearance_during_admission_imp_train_predictors2$complete_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_imp_train_predictors2$partial_staghorn <-
  ifelse(
    PCNL_clearance_during_admission_imp_train_predictors2$partial_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_imp_train_predictors2$pelvic <-
  ifelse(PCNL_clearance_during_admission_imp_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_imp_train_predictors2$upper_ureteric <-
  ifelse(PCNL_clearance_during_admission_imp_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_clearance_during_admission_imp_train_predictors2$ureter_other <-
  ifelse(PCNL_clearance_during_admission_imp_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



clearance_during_admission_imp_train_predictors_stone_complexity <-
  as_tibble(PCNL_clearance_during_admission_imp_train_predictors2$stone_complexity)
colnames(clearance_during_admission_imp_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
clearance_during_admission_imp_train_predictors_stone_complexity <-
  subset(clearance_during_admission_imp_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_clearance_during_admission_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_train_predictors2,
    clearance_during_admission_imp_train_predictors_stone_complexity
  )

clearance_during_admission_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_clearance_during_admission_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  clearance_during_admission_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath
) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
clearance_during_admission_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    clearance_during_admission_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_clearance_during_admission_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_train_predictors2,
    clearance_during_admission_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

clearance_during_admission_imp_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_clearance_during_admission_imp_train_predictors2$pre_operative_msu_result)
colnames(clearance_during_admission_imp_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
clearance_during_admission_imp_train_predictors_pre_operative_msu_result <-
  subset(
    clearance_during_admission_imp_train_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_clearance_during_admission_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_train_predictors2,
    clearance_during_admission_imp_train_predictors_pre_operative_msu_result
  )

clearance_during_admission_imp_train_predictors_puncture_site <-
  as_tibble(PCNL_clearance_during_admission_imp_train_predictors2$puncture_site)
colnames(clearance_during_admission_imp_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
clearance_during_admission_imp_train_predictors_puncture_site <-
  subset(clearance_during_admission_imp_train_predictors_puncture_site,
         select = -puncture0)
PCNL_clearance_during_admission_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_train_predictors2,
    clearance_during_admission_imp_train_predictors_puncture_site
  )

clearance_during_admission_imp_train_predictors_stone_dimensions <-
  as_tibble(PCNL_clearance_during_admission_imp_train_predictors2$stone_dimensions)
colnames(clearance_during_admission_imp_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
clearance_during_admission_imp_train_predictors_stone_dimensions <-
  subset(
    clearance_during_admission_imp_train_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_clearance_during_admission_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_train_predictors2,
    clearance_during_admission_imp_train_predictors_stone_dimensions
  )

clearance_during_admission_imp_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_clearance_during_admission_imp_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(
  clearance_during_admission_imp_train_predictors_image_guidance_for_renal_puncture
) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
clearance_during_admission_imp_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    clearance_during_admission_imp_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_clearance_during_admission_imp_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_train_predictors2,
    clearance_during_admission_imp_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_clearance_during_admission_imp_train_predictors2 <-
  subset(
    PCNL_clearance_during_admission_imp_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_clearance_during_admission_imp_train_predictors2 <-
  as.matrix(PCNL_clearance_during_admission_imp_train_predictors2)


##### Oversampling of Imputed Dataset
PCNL_clearance_during_admission_imp_train_oversample <-
  ROSE::ovun.sample(
    clearance_on_post_operative_radiological_imaging_during_a ~ .,
    data = PCNL_clearance_during_admission_imp_train,
    seed = 1234,
    method = "over"
  )$data
summary(
  PCNL_clearance_during_admission_imp_train_oversample$clearance_on_post_operative_radiological_imaging_during_a
)

PCNL_clearance_during_admission_imp_oversample_train_outcome <-
  subset(PCNL_clearance_during_admission_imp_train_oversample,
         select = clearance_on_post_operative_radiological_imaging_during_a)
PCNL_clearance_during_admission_imp_oversample_train_outcome$clearance_on_post_operative_radiological_imaging_during_a <-
  as.numeric(
    PCNL_clearance_during_admission_imp_oversample_train_outcome$clearance_on_post_operative_radiological_imaging_during_a
  )
PCNL_clearance_during_admission_imp_oversample_train_outcome <-
  as.matrix(PCNL_clearance_during_admission_imp_oversample_train_outcome)
PCNL_clearance_during_admission_imp_oversample_train_predictors <-
  subset(
    PCNL_clearance_during_admission_imp_train_oversample,
    select = -clearance_on_post_operative_radiological_imaging_during_a
  )

PCNL_clearance_during_admission_imp_oversample_train_predictors2 <-
  PCNL_clearance_during_admission_imp_oversample_train_predictors
PCNL_clearance_during_admission_imp_oversample_train_predictors2$stone_complexity <-
  as.integer(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$stone_complexity
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2$stone_complexity <-
  as.factor(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$stone_complexity
  ) %>% to_categorical()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$pre_operative_msu_result
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$pre_operative_msu_result
  ) %>% to_categorical()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$stone_dimensions <-
  as.integer(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$stone_dimensions
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2$stone_dimensions <-
  as.factor(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$stone_dimensions
  ) %>% to_categorical()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_clearance_during_admission_imp_oversample_train_predictors2$puncture_site)
PCNL_clearance_during_admission_imp_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_clearance_during_admission_imp_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$renogramdmsa <-
  ifelse(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$renogramdmsa == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$calyceal_l <-
  ifelse(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$calyceal_l == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$calyceal_m <-
  ifelse(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$calyceal_m == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$complete_staghorn <-
  ifelse(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$complete_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$partial_staghorn <-
  ifelse(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$partial_staghorn == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$pelvic <-
  ifelse(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$pelvic == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$upper_ureteric <-
  ifelse(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$upper_ureteric == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_clearance_during_admission_imp_oversample_train_predictors2$ureter_other <-
  ifelse(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$ureter_other == "2",
    1,
    0
  ) %>% as.numeric()



clearance_during_admission_imp_oversample_train_predictors_stone_complexity <-
  as_tibble(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$stone_complexity
  )
colnames(clearance_during_admission_imp_oversample_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
clearance_during_admission_imp_oversample_train_predictors_stone_complexity <-
  subset(
    clearance_during_admission_imp_oversample_train_predictors_stone_complexity,
    select = -GSS0
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2,
    clearance_during_admission_imp_oversample_train_predictors_stone_complexity
  )

clearance_during_admission_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  clearance_during_admission_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
clearance_during_admission_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    clearance_during_admission_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2,
    clearance_during_admission_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

clearance_during_admission_imp_oversample_train_predictors_pre_operative_msu_result <-
  as_tibble(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$pre_operative_msu_result
  )
colnames(
  clearance_during_admission_imp_oversample_train_predictors_pre_operative_msu_result
) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
clearance_during_admission_imp_oversample_train_predictors_pre_operative_msu_result <-
  subset(
    clearance_during_admission_imp_oversample_train_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2,
    clearance_during_admission_imp_oversample_train_predictors_pre_operative_msu_result
  )

clearance_during_admission_imp_oversample_train_predictors_puncture_site <-
  as_tibble(PCNL_clearance_during_admission_imp_oversample_train_predictors2$puncture_site)
colnames(clearance_during_admission_imp_oversample_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
clearance_during_admission_imp_oversample_train_predictors_puncture_site <-
  subset(
    clearance_during_admission_imp_oversample_train_predictors_puncture_site,
    select = -puncture0
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2,
    clearance_during_admission_imp_oversample_train_predictors_puncture_site
  )

clearance_during_admission_imp_oversample_train_predictors_stone_dimensions <-
  as_tibble(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$stone_dimensions
  )
colnames(clearance_during_admission_imp_oversample_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
clearance_during_admission_imp_oversample_train_predictors_stone_dimensions <-
  subset(
    clearance_during_admission_imp_oversample_train_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2,
    clearance_during_admission_imp_oversample_train_predictors_stone_dimensions
  )

clearance_during_admission_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(
  clearance_during_admission_imp_oversample_train_predictors_image_guidance_for_renal_puncture
) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
clearance_during_admission_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    clearance_during_admission_imp_oversample_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2,
    clearance_during_admission_imp_oversample_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_clearance_during_admission_imp_oversample_train_predictors2 <-
  subset(
    PCNL_clearance_during_admission_imp_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_clearance_during_admission_imp_oversample_train_predictors2 <-
  as.matrix(PCNL_clearance_during_admission_imp_oversample_train_predictors2)

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
) %>% as_tibble() %>% janitor::clean_names()

plot_missing(PCNL_original_death)

##### Omit NA Dataset
PCNL_death_omit_na <- na.omit(PCNL_original_death)
str(PCNL_death_omit_na)

PCNL_death_sample <-
  sample(1:nrow(PCNL_original_death),
         size = nrow(PCNL_original_death) * 0.7)
PCNL_death_train <-
  PCNL_original_death[PCNL_death_sample, ] %>% as_tibble()
PCNL_death_test <-
  PCNL_original_death[-PCNL_death_sample, ] %>% as_tibble()

PCNL_death_omit_na_sample <-
  sample(1:nrow(PCNL_death_omit_na),
         size = nrow(PCNL_death_omit_na) * 0.7)
PCNL_death_omit_na_train <-
  PCNL_death_omit_na[PCNL_death_omit_na_sample, ] %>% as_tibble()
PCNL_death_omit_na_test <-
  PCNL_death_omit_na[-PCNL_death_omit_na_sample, ] %>% as_tibble()
summary(PCNL_death_omit_na_train$patient_statusdischarge)

PCNL_death_omit_na_train_outcome <-
  subset(PCNL_death_omit_na_train,
         select = patient_statusdischarge)
PCNL_death_omit_na_train_outcome$patient_statusdischarge <-
  as.numeric(PCNL_death_omit_na_train_outcome$patient_statusdischarge)
PCNL_death_omit_na_train_outcome$patient_statusdischarge <-
  ifelse(PCNL_death_omit_na_train_outcome$patient_statusdischarge == 2,
         1,
         0)
PCNL_death_omit_na_train_outcome <-
  as.matrix(PCNL_death_omit_na_train_outcome)
PCNL_death_omit_na_test_outcome <-
  subset(PCNL_death_omit_na_test,
         select = patient_statusdischarge)
PCNL_death_omit_na_test_outcome$patient_statusdischarge <-
  as.numeric(PCNL_death_omit_na_test_outcome$patient_statusdischarge)
PCNL_death_omit_na_test_outcome$patient_statusdischarge <-
  ifelse(PCNL_death_omit_na_test_outcome$patient_statusdischarge == 2,
         1,
         0)
PCNL_death_omit_na_test_outcome <-
  as.matrix(PCNL_death_omit_na_test_outcome)
PCNL_death_omit_na_train_predictors <-
  subset(PCNL_death_omit_na_train,
         select = -patient_statusdischarge)
PCNL_death_omit_na_test_predictors <-
  subset(PCNL_death_omit_na_test,
         select = -patient_statusdischarge)

PCNL_death_omit_na_train_predictors2 <-
  PCNL_death_omit_na_train_predictors
PCNL_death_omit_na_train_predictors2$stone_complexity <-
  as.integer(PCNL_death_omit_na_train_predictors2$stone_complexity)
PCNL_death_omit_na_train_predictors2$stone_complexity <-
  as.factor(PCNL_death_omit_na_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_death_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_death_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_death_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_death_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_death_omit_na_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_death_omit_na_train_predictors2$pre_operative_msu_result)
PCNL_death_omit_na_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_death_omit_na_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_death_omit_na_train_predictors2$stone_dimensions <-
  as.integer(PCNL_death_omit_na_train_predictors2$stone_dimensions)
PCNL_death_omit_na_train_predictors2$stone_dimensions <-
  as.factor(PCNL_death_omit_na_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_death_omit_na_train_predictors2$puncture_site <-
  as.integer(PCNL_death_omit_na_train_predictors2$puncture_site)
PCNL_death_omit_na_train_predictors2$puncture_site <-
  as.factor(PCNL_death_omit_na_train_predictors2$puncture_site) %>% to_categorical()
PCNL_death_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_death_omit_na_train_predictors2$image_guidance_for_renal_puncture)
PCNL_death_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_death_omit_na_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_death_omit_na_train_predictors2$renogramdmsa <-
  ifelse(PCNL_death_omit_na_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_death_omit_na_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_train_predictors2$calyceal_l <-
  ifelse(PCNL_death_omit_na_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_train_predictors2$calyceal_m <-
  ifelse(PCNL_death_omit_na_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_train_predictors2$complete_staghorn <-
  ifelse(PCNL_death_omit_na_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_train_predictors2$partial_staghorn <-
  ifelse(PCNL_death_omit_na_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_train_predictors2$pelvic <-
  ifelse(PCNL_death_omit_na_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_train_predictors2$upper_ureteric <-
  ifelse(PCNL_death_omit_na_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_train_predictors2$ureter_other <-
  ifelse(PCNL_death_omit_na_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



death_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_death_omit_na_train_predictors2$stone_complexity)
colnames(death_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
death_na_omit_train_predictors_stone_complexity <-
  subset(death_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_death_omit_na_train_predictors2 <-
  cbind(
    PCNL_death_omit_na_train_predictors2,
    death_na_omit_train_predictors_stone_complexity
  )

death_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_death_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(death_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
death_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    death_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_death_omit_na_train_predictors2 <-
  cbind(
    PCNL_death_omit_na_train_predictors2,
    death_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

death_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_death_omit_na_train_predictors2$pre_operative_msu_result)
colnames(death_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
death_na_omit_train_predictors_pre_operative_msu_result <-
  subset(death_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_death_omit_na_train_predictors2 <-
  cbind(
    PCNL_death_omit_na_train_predictors2,
    death_na_omit_train_predictors_pre_operative_msu_result
  )

death_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_death_omit_na_train_predictors2$puncture_site)
colnames(death_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
death_na_omit_train_predictors_puncture_site <-
  subset(death_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_death_omit_na_train_predictors2 <-
  cbind(
    PCNL_death_omit_na_train_predictors2,
    death_na_omit_train_predictors_puncture_site
  )

death_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_death_omit_na_train_predictors2$stone_dimensions)
colnames(death_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
death_na_omit_train_predictors_stone_dimensions <-
  subset(death_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_death_omit_na_train_predictors2 <-
  cbind(
    PCNL_death_omit_na_train_predictors2,
    death_na_omit_train_predictors_stone_dimensions
  )

death_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_death_omit_na_train_predictors2$image_guidance_for_renal_puncture)
colnames(death_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
death_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(death_na_omit_train_predictors_image_guidance_for_renal_puncture,
         select = -image0)
PCNL_death_omit_na_train_predictors2 <-
  cbind(
    PCNL_death_omit_na_train_predictors2,
    death_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_death_omit_na_train_predictors2 <-
  subset(
    PCNL_death_omit_na_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_death_omit_na_train_predictors2 <-
  as.matrix(PCNL_death_omit_na_train_predictors2)


PCNL_death_omit_na_test_predictors2 <-
  PCNL_death_omit_na_test_predictors
PCNL_death_omit_na_test_predictors2$stone_complexity <-
  as.integer(PCNL_death_omit_na_test_predictors2$stone_complexity)
PCNL_death_omit_na_test_predictors2$stone_complexity <-
  as.factor(PCNL_death_omit_na_test_predictors2$stone_complexity) %>% to_categorical()
PCNL_death_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_death_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_death_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_death_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_death_omit_na_test_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_death_omit_na_test_predictors2$pre_operative_msu_result)
PCNL_death_omit_na_test_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_death_omit_na_test_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_death_omit_na_test_predictors2$stone_dimensions <-
  as.integer(PCNL_death_omit_na_test_predictors2$stone_dimensions)
PCNL_death_omit_na_test_predictors2$stone_dimensions <-
  as.factor(PCNL_death_omit_na_test_predictors2$stone_dimensions) %>% to_categorical()
PCNL_death_omit_na_test_predictors2$puncture_site <-
  as.integer(PCNL_death_omit_na_test_predictors2$puncture_site)
PCNL_death_omit_na_test_predictors2$puncture_site <-
  as.factor(PCNL_death_omit_na_test_predictors2$puncture_site) %>% to_categorical()
PCNL_death_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_death_omit_na_test_predictors2$image_guidance_for_renal_puncture)
PCNL_death_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_death_omit_na_test_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_death_omit_na_test_predictors2$renogramdmsa <-
  ifelse(PCNL_death_omit_na_test_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_test_predictors2$calyceal_diverticular <-
  ifelse(PCNL_death_omit_na_test_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_test_predictors2$calyceal_l <-
  ifelse(PCNL_death_omit_na_test_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_test_predictors2$calyceal_m <-
  ifelse(PCNL_death_omit_na_test_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_test_predictors2$complete_staghorn <-
  ifelse(PCNL_death_omit_na_test_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_test_predictors2$partial_staghorn <-
  ifelse(PCNL_death_omit_na_test_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_test_predictors2$pelvic <-
  ifelse(PCNL_death_omit_na_test_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_test_predictors2$upper_ureteric <-
  ifelse(PCNL_death_omit_na_test_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_death_omit_na_test_predictors2$ureter_other <-
  ifelse(PCNL_death_omit_na_test_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



death_na_omit_test_predictors_stone_complexity <-
  as_tibble(PCNL_death_omit_na_test_predictors2$stone_complexity)
colnames(death_na_omit_test_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
death_na_omit_test_predictors_stone_complexity <-
  subset(death_na_omit_test_predictors_stone_complexity,
         select = -GSS0)
PCNL_death_omit_na_test_predictors2 <-
  cbind(
    PCNL_death_omit_na_test_predictors2,
    death_na_omit_test_predictors_stone_complexity
  )

death_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_death_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(death_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
death_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    death_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_death_omit_na_test_predictors2 <-
  cbind(
    PCNL_death_omit_na_test_predictors2,
    death_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
  )

death_na_omit_test_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_death_omit_na_test_predictors2$pre_operative_msu_result)
colnames(death_na_omit_test_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
death_na_omit_test_predictors_pre_operative_msu_result <-
  subset(death_na_omit_test_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_death_omit_na_test_predictors2 <-
  cbind(
    PCNL_death_omit_na_test_predictors2,
    death_na_omit_test_predictors_pre_operative_msu_result
  )

death_na_omit_test_predictors_puncture_site <-
  as_tibble(PCNL_death_omit_na_test_predictors2$puncture_site)
colnames(death_na_omit_test_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
death_na_omit_test_predictors_puncture_site <-
  subset(death_na_omit_test_predictors_puncture_site,
         select = -puncture0)
PCNL_death_omit_na_test_predictors2 <-
  cbind(
    PCNL_death_omit_na_test_predictors2,
    death_na_omit_test_predictors_puncture_site
  )

death_na_omit_test_predictors_stone_dimensions <-
  as_tibble(PCNL_death_omit_na_test_predictors2$stone_dimensions)
colnames(death_na_omit_test_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
death_na_omit_test_predictors_stone_dimensions <-
  subset(death_na_omit_test_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_death_omit_na_test_predictors2 <-
  cbind(
    PCNL_death_omit_na_test_predictors2,
    death_na_omit_test_predictors_stone_dimensions
  )

death_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_death_omit_na_test_predictors2$image_guidance_for_renal_puncture)
colnames(death_na_omit_test_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
death_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  subset(death_na_omit_test_predictors_image_guidance_for_renal_puncture,
         select = -image0)
PCNL_death_omit_na_test_predictors2 <-
  cbind(
    PCNL_death_omit_na_test_predictors2,
    death_na_omit_test_predictors_image_guidance_for_renal_puncture
  )

PCNL_death_omit_na_test_predictors2 <-
  subset(
    PCNL_death_omit_na_test_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_death_omit_na_test_predictors2 <-
  as.matrix(PCNL_death_omit_na_test_predictors2)

##### Oversampled Dataset
PCNL_death_omit_na_train_oversample <-
  ROSE::ovun.sample(
    patient_statusdischarge ~ .,
    data = PCNL_death_omit_na_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_death_omit_na_train_oversample$patient_statusdischarge)

PCNL_death_oversample_train_outcome <-
  subset(PCNL_death_omit_na_train_oversample,
         select = patient_statusdischarge)
PCNL_death_oversample_train_outcome$patient_statusdischarge <-
  as.numeric(PCNL_death_oversample_train_outcome$patient_statusdischarge)
PCNL_death_oversample_train_outcome <-
  as.matrix(PCNL_death_oversample_train_outcome)
PCNL_death_oversample_train_predictors <-
  subset(PCNL_death_omit_na_train_oversample,
         select = -patient_statusdischarge)

PCNL_death_oversample_train_predictors2 <-
  PCNL_death_oversample_train_predictors
PCNL_death_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_death_oversample_train_predictors2$stone_complexity)
PCNL_death_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_death_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_death_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_death_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_death_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_death_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_death_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_death_oversample_train_predictors2$pre_operative_msu_result)
PCNL_death_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_death_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_death_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_death_oversample_train_predictors2$stone_dimensions)
PCNL_death_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_death_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_death_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_death_oversample_train_predictors2$puncture_site)
PCNL_death_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_death_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_death_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_death_oversample_train_predictors2$image_guidance_for_renal_puncture)
PCNL_death_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_death_oversample_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_death_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_death_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_death_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_death_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_death_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_death_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_death_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_death_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_death_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_death_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_death_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_death_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_death_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_death_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_death_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_death_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_death_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_death_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



death_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_death_oversample_train_predictors2$stone_complexity)
colnames(death_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
death_na_omit_train_predictors_stone_complexity <-
  subset(death_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_death_oversample_train_predictors2 <-
  cbind(
    PCNL_death_oversample_train_predictors2,
    death_na_omit_train_predictors_stone_complexity
  )

death_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_death_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(death_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
death_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    death_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_death_oversample_train_predictors2 <-
  cbind(
    PCNL_death_oversample_train_predictors2,
    death_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

death_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_death_oversample_train_predictors2$pre_operative_msu_result)
colnames(death_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
death_na_omit_train_predictors_pre_operative_msu_result <-
  subset(death_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_death_oversample_train_predictors2 <-
  cbind(
    PCNL_death_oversample_train_predictors2,
    death_na_omit_train_predictors_pre_operative_msu_result
  )

death_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_death_oversample_train_predictors2$puncture_site)
colnames(death_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
death_na_omit_train_predictors_puncture_site <-
  subset(death_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_death_oversample_train_predictors2 <-
  cbind(
    PCNL_death_oversample_train_predictors2,
    death_na_omit_train_predictors_puncture_site
  )

death_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_death_oversample_train_predictors2$stone_dimensions)
colnames(death_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
death_na_omit_train_predictors_stone_dimensions <-
  subset(death_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_death_oversample_train_predictors2 <-
  cbind(
    PCNL_death_oversample_train_predictors2,
    death_na_omit_train_predictors_stone_dimensions
  )

death_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_death_oversample_train_predictors2$image_guidance_for_renal_puncture)
colnames(death_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
death_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(death_na_omit_train_predictors_image_guidance_for_renal_puncture,
         select = -image0)
PCNL_death_oversample_train_predictors2 <-
  cbind(
    PCNL_death_oversample_train_predictors2,
    death_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_death_oversample_train_predictors2 <-
  subset(
    PCNL_death_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_death_oversample_train_predictors2 <-
  as.matrix(PCNL_death_oversample_train_predictors2)

##### Imputed Dataset
PCNL_death_imp_pre <-
  PCNL_death_omit_na %>% drop_na(patient_statusdischarge)
PCNL_death_imp1 <- mice(PCNL_death_imp_pre, m = 1)
summary(PCNL_death_imp1)

PCNL_death_imp <-
  mice::complete(PCNL_death_imp1, 1) %>% as_tibble()

PCNL_death_imp_sample <-
  sample(1:nrow(PCNL_death_imp), size = nrow(PCNL_death_imp) * 0.7)
PCNL_death_imp_train <-
  PCNL_death_imp[PCNL_death_imp_sample, ] %>% as_tibble()
PCNL_death_imp_test <-
  PCNL_death_imp[-PCNL_death_imp_sample, ] %>% as_tibble()
PCNL_death_imp_train$patient_statusdischarge <-
  as.factor(PCNL_death_imp_train$patient_statusdischarge)
PCNL_death_imp_test$patient_statusdischarge <-
  as.factor(PCNL_death_imp_test$patient_statusdischarge)
summary(PCNL_death_imp_train$patient_statusdischarge)

PCNL_death_imp_train_outcome <-
  subset(PCNL_death_imp_train,
         select = patient_statusdischarge)
PCNL_death_imp_train_outcome$patient_statusdischarge <-
  as.numeric(PCNL_death_imp_train_outcome$patient_statusdischarge)
PCNL_death_imp_train_outcome <-
  as.matrix(PCNL_death_imp_train_outcome)
PCNL_death_imp_train_predictors <-
  subset(PCNL_death_imp_train,
         select = -patient_statusdischarge)

PCNL_death_imp_train_predictors2 <-
  PCNL_death_imp_train_predictors
PCNL_death_imp_train_predictors2$stone_complexity <-
  as.integer(PCNL_death_imp_train_predictors2$stone_complexity)
PCNL_death_imp_train_predictors2$stone_complexity <-
  as.factor(PCNL_death_imp_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_death_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_death_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_death_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_death_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_death_imp_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_death_imp_train_predictors2$pre_operative_msu_result)
PCNL_death_imp_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_death_imp_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_death_imp_train_predictors2$stone_dimensions <-
  as.integer(PCNL_death_imp_train_predictors2$stone_dimensions)
PCNL_death_imp_train_predictors2$stone_dimensions <-
  as.factor(PCNL_death_imp_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_death_imp_train_predictors2$puncture_site <-
  as.integer(PCNL_death_imp_train_predictors2$puncture_site)
PCNL_death_imp_train_predictors2$puncture_site <-
  as.factor(PCNL_death_imp_train_predictors2$puncture_site) %>% to_categorical()
PCNL_death_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_death_imp_train_predictors2$image_guidance_for_renal_puncture)
PCNL_death_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_death_imp_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_death_imp_train_predictors2$renogramdmsa <-
  ifelse(PCNL_death_imp_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_death_imp_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_train_predictors2$calyceal_l <-
  ifelse(PCNL_death_imp_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_train_predictors2$calyceal_m <-
  ifelse(PCNL_death_imp_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_train_predictors2$complete_staghorn <-
  ifelse(PCNL_death_imp_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_train_predictors2$partial_staghorn <-
  ifelse(PCNL_death_imp_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_train_predictors2$pelvic <-
  ifelse(PCNL_death_imp_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_train_predictors2$upper_ureteric <-
  ifelse(PCNL_death_imp_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_train_predictors2$ureter_other <-
  ifelse(PCNL_death_imp_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



death_imp_train_predictors_stone_complexity <-
  as_tibble(PCNL_death_imp_train_predictors2$stone_complexity)
colnames(death_imp_train_predictors_stone_complexity) <- c("GSS0",
                                                           "GSSI",
                                                           "GSSII",
                                                           "GSSIII",
                                                           "GSSIV")
death_imp_train_predictors_stone_complexity <-
  subset(death_imp_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_death_imp_train_predictors2 <-
  cbind(PCNL_death_imp_train_predictors2,
        death_imp_train_predictors_stone_complexity)

death_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_death_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(death_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
death_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(death_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath,
         select = -size0)
PCNL_death_imp_train_predictors2 <-
  cbind(
    PCNL_death_imp_train_predictors2,
    death_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

death_imp_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_death_imp_train_predictors2$pre_operative_msu_result)
colnames(death_imp_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
death_imp_train_predictors_pre_operative_msu_result <-
  subset(death_imp_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_death_imp_train_predictors2 <-
  cbind(
    PCNL_death_imp_train_predictors2,
    death_imp_train_predictors_pre_operative_msu_result
  )

death_imp_train_predictors_puncture_site <-
  as_tibble(PCNL_death_imp_train_predictors2$puncture_site)
colnames(death_imp_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
death_imp_train_predictors_puncture_site <-
  subset(death_imp_train_predictors_puncture_site,
         select = -puncture0)
PCNL_death_imp_train_predictors2 <-
  cbind(PCNL_death_imp_train_predictors2,
        death_imp_train_predictors_puncture_site)

death_imp_train_predictors_stone_dimensions <-
  as_tibble(PCNL_death_imp_train_predictors2$stone_dimensions)
colnames(death_imp_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
death_imp_train_predictors_stone_dimensions <-
  subset(death_imp_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_death_imp_train_predictors2 <-
  cbind(PCNL_death_imp_train_predictors2,
        death_imp_train_predictors_stone_dimensions)

death_imp_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_death_imp_train_predictors2$image_guidance_for_renal_puncture)
colnames(death_imp_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
death_imp_train_predictors_image_guidance_for_renal_puncture <-
  subset(death_imp_train_predictors_image_guidance_for_renal_puncture,
         select = -image0)
PCNL_death_imp_train_predictors2 <-
  cbind(
    PCNL_death_imp_train_predictors2,
    death_imp_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_death_imp_train_predictors2 <-
  subset(
    PCNL_death_imp_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_death_imp_train_predictors2 <-
  as.matrix(PCNL_death_imp_train_predictors2)


##### Oversampling of Imputed Dataset
PCNL_death_imp_train_oversample <-
  ROSE::ovun.sample(
    patient_statusdischarge ~ .,
    data = PCNL_death_imp_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_death_imp_train_oversample$patient_statusdischarge)

PCNL_death_imp_oversample_train_outcome <-
  subset(PCNL_death_imp_train_oversample,
         select = patient_statusdischarge)
PCNL_death_imp_oversample_train_outcome$patient_statusdischarge <-
  as.numeric(PCNL_death_imp_oversample_train_outcome$patient_statusdischarge)
PCNL_death_imp_oversample_train_outcome <-
  as.matrix(PCNL_death_imp_oversample_train_outcome)
PCNL_death_imp_oversample_train_predictors <-
  subset(PCNL_death_imp_train_oversample,
         select = -patient_statusdischarge)

PCNL_death_imp_oversample_train_predictors2 <-
  PCNL_death_imp_oversample_train_predictors
PCNL_death_imp_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_death_imp_oversample_train_predictors2$stone_complexity)
PCNL_death_imp_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_death_imp_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_death_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_death_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_death_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_death_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_death_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_death_imp_oversample_train_predictors2$pre_operative_msu_result)
PCNL_death_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_death_imp_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_death_imp_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_death_imp_oversample_train_predictors2$stone_dimensions)
PCNL_death_imp_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_death_imp_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_death_imp_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_death_imp_oversample_train_predictors2$puncture_site)
PCNL_death_imp_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_death_imp_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_death_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_death_imp_oversample_train_predictors2$image_guidance_for_renal_puncture)
PCNL_death_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_death_imp_oversample_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_death_imp_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_death_imp_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_death_imp_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_death_imp_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_death_imp_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_death_imp_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_death_imp_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_death_imp_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_death_imp_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_death_imp_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_death_imp_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



death_imp_oversample_train_predictors_stone_complexity <-
  as_tibble(PCNL_death_imp_oversample_train_predictors2$stone_complexity)
colnames(death_imp_oversample_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
death_imp_oversample_train_predictors_stone_complexity <-
  subset(death_imp_oversample_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_death_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_death_imp_oversample_train_predictors2,
    death_imp_oversample_train_predictors_stone_complexity
  )

death_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_death_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(death_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
death_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    death_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_death_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_death_imp_oversample_train_predictors2,
    death_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

death_imp_oversample_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_death_imp_oversample_train_predictors2$pre_operative_msu_result)
colnames(death_imp_oversample_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
death_imp_oversample_train_predictors_pre_operative_msu_result <-
  subset(death_imp_oversample_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_death_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_death_imp_oversample_train_predictors2,
    death_imp_oversample_train_predictors_pre_operative_msu_result
  )

death_imp_oversample_train_predictors_puncture_site <-
  as_tibble(PCNL_death_imp_oversample_train_predictors2$puncture_site)
colnames(death_imp_oversample_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
death_imp_oversample_train_predictors_puncture_site <-
  subset(death_imp_oversample_train_predictors_puncture_site,
         select = -puncture0)
PCNL_death_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_death_imp_oversample_train_predictors2,
    death_imp_oversample_train_predictors_puncture_site
  )

death_imp_oversample_train_predictors_stone_dimensions <-
  as_tibble(PCNL_death_imp_oversample_train_predictors2$stone_dimensions)
colnames(death_imp_oversample_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
death_imp_oversample_train_predictors_stone_dimensions <-
  subset(death_imp_oversample_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_death_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_death_imp_oversample_train_predictors2,
    death_imp_oversample_train_predictors_stone_dimensions
  )

death_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_death_imp_oversample_train_predictors2$image_guidance_for_renal_puncture)
colnames(death_imp_oversample_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
death_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    death_imp_oversample_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_death_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_death_imp_oversample_train_predictors2,
    death_imp_oversample_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_death_imp_oversample_train_predictors2 <-
  subset(
    PCNL_death_imp_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_death_imp_oversample_train_predictors2 <-
  as.matrix(PCNL_death_imp_oversample_train_predictors2)

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
) %>% as_tibble() %>% janitor::clean_names()

plot_missing(PCNL_original_post_op_comp)


##### Omit NA Dataset
PCNL_post_op_comp_omit_na <- na.omit(PCNL_original_post_op_comp)
str(PCNL_post_op_comp_omit_na)

PCNL_post_op_comp_sample <-
  sample(1:nrow(PCNL_original_post_op_comp),
         size = nrow(PCNL_original_post_op_comp) * 0.7)
PCNL_post_op_comp_train <-
  PCNL_original_post_op_comp[PCNL_post_op_comp_sample, ] %>% as_tibble()
PCNL_post_op_comp_test <-
  PCNL_original_post_op_comp[-PCNL_post_op_comp_sample, ] %>% as_tibble()

PCNL_post_op_comp_omit_na_sample <-
  sample(1:nrow(PCNL_post_op_comp_omit_na),
         size = nrow(PCNL_post_op_comp_omit_na) * 0.7)
PCNL_post_op_comp_omit_na_train <-
  PCNL_post_op_comp_omit_na[PCNL_post_op_comp_omit_na_sample, ] %>% as_tibble()
PCNL_post_op_comp_omit_na_test <-
  PCNL_post_op_comp_omit_na[-PCNL_post_op_comp_omit_na_sample, ] %>% as_tibble()
summary(PCNL_post_op_comp_omit_na_train$postop_complications)

PCNL_post_op_comp_omit_na_train_outcome <-
  subset(PCNL_post_op_comp_omit_na_train,
         select = postop_complications)
PCNL_post_op_comp_omit_na_train_outcome$postop_complications <-
  as.numeric(PCNL_post_op_comp_omit_na_train_outcome$postop_complications)
PCNL_post_op_comp_omit_na_train_outcome$postop_complications <-
  ifelse(PCNL_post_op_comp_omit_na_train_outcome$postop_complications == 2,
         1,
         0)
PCNL_post_op_comp_omit_na_train_outcome <-
  as.matrix(PCNL_post_op_comp_omit_na_train_outcome)
PCNL_post_op_comp_omit_na_test_outcome <-
  subset(PCNL_post_op_comp_omit_na_test,
         select = postop_complications)
PCNL_post_op_comp_omit_na_test_outcome$postop_complications <-
  as.numeric(PCNL_post_op_comp_omit_na_test_outcome$postop_complications)
PCNL_post_op_comp_omit_na_test_outcome$postop_complications <-
  ifelse(PCNL_post_op_comp_omit_na_test_outcome$postop_complications == 2,
         1,
         0)
PCNL_post_op_comp_omit_na_test_outcome <-
  as.matrix(PCNL_post_op_comp_omit_na_test_outcome)
PCNL_post_op_comp_omit_na_train_predictors <-
  subset(PCNL_post_op_comp_omit_na_train,
         select = -postop_complications)
PCNL_post_op_comp_omit_na_test_predictors <-
  subset(PCNL_post_op_comp_omit_na_test,
         select = -postop_complications)

PCNL_post_op_comp_omit_na_train_predictors2 <-
  PCNL_post_op_comp_omit_na_train_predictors
PCNL_post_op_comp_omit_na_train_predictors2$stone_complexity <-
  as.integer(PCNL_post_op_comp_omit_na_train_predictors2$stone_complexity)
PCNL_post_op_comp_omit_na_train_predictors2$stone_complexity <-
  as.factor(PCNL_post_op_comp_omit_na_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_post_op_comp_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_post_op_comp_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_post_op_comp_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_post_op_comp_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_post_op_comp_omit_na_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_post_op_comp_omit_na_train_predictors2$pre_operative_msu_result)
PCNL_post_op_comp_omit_na_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_post_op_comp_omit_na_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_post_op_comp_omit_na_train_predictors2$stone_dimensions <-
  as.integer(PCNL_post_op_comp_omit_na_train_predictors2$stone_dimensions)
PCNL_post_op_comp_omit_na_train_predictors2$stone_dimensions <-
  as.factor(PCNL_post_op_comp_omit_na_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_post_op_comp_omit_na_train_predictors2$puncture_site <-
  as.integer(PCNL_post_op_comp_omit_na_train_predictors2$puncture_site)
PCNL_post_op_comp_omit_na_train_predictors2$puncture_site <-
  as.factor(PCNL_post_op_comp_omit_na_train_predictors2$puncture_site) %>% to_categorical()
PCNL_post_op_comp_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_post_op_comp_omit_na_train_predictors2$image_guidance_for_renal_puncture)
PCNL_post_op_comp_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_post_op_comp_omit_na_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_post_op_comp_omit_na_train_predictors2$renogramdmsa <-
  ifelse(PCNL_post_op_comp_omit_na_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_post_op_comp_omit_na_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_train_predictors2$calyceal_l <-
  ifelse(PCNL_post_op_comp_omit_na_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_train_predictors2$calyceal_m <-
  ifelse(PCNL_post_op_comp_omit_na_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_train_predictors2$complete_staghorn <-
  ifelse(PCNL_post_op_comp_omit_na_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_train_predictors2$partial_staghorn <-
  ifelse(PCNL_post_op_comp_omit_na_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_train_predictors2$pelvic <-
  ifelse(PCNL_post_op_comp_omit_na_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_train_predictors2$upper_ureteric <-
  ifelse(PCNL_post_op_comp_omit_na_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_train_predictors2$ureter_other <-
  ifelse(PCNL_post_op_comp_omit_na_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



post_op_comp_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_post_op_comp_omit_na_train_predictors2$stone_complexity)
colnames(post_op_comp_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
post_op_comp_na_omit_train_predictors_stone_complexity <-
  subset(post_op_comp_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_post_op_comp_omit_na_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_train_predictors2,
    post_op_comp_na_omit_train_predictors_stone_complexity
  )

post_op_comp_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_post_op_comp_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(post_op_comp_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
post_op_comp_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    post_op_comp_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_post_op_comp_omit_na_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_train_predictors2,
    post_op_comp_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

post_op_comp_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_post_op_comp_omit_na_train_predictors2$pre_operative_msu_result)
colnames(post_op_comp_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
post_op_comp_na_omit_train_predictors_pre_operative_msu_result <-
  subset(post_op_comp_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_post_op_comp_omit_na_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_train_predictors2,
    post_op_comp_na_omit_train_predictors_pre_operative_msu_result
  )

post_op_comp_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_post_op_comp_omit_na_train_predictors2$puncture_site)
colnames(post_op_comp_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
post_op_comp_na_omit_train_predictors_puncture_site <-
  subset(post_op_comp_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_post_op_comp_omit_na_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_train_predictors2,
    post_op_comp_na_omit_train_predictors_puncture_site
  )

post_op_comp_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_post_op_comp_omit_na_train_predictors2$stone_dimensions)
colnames(post_op_comp_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
post_op_comp_na_omit_train_predictors_stone_dimensions <-
  subset(post_op_comp_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_post_op_comp_omit_na_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_train_predictors2,
    post_op_comp_na_omit_train_predictors_stone_dimensions
  )

post_op_comp_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_post_op_comp_omit_na_train_predictors2$image_guidance_for_renal_puncture)
colnames(post_op_comp_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
post_op_comp_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    post_op_comp_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_post_op_comp_omit_na_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_train_predictors2,
    post_op_comp_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_post_op_comp_omit_na_train_predictors2 <-
  subset(
    PCNL_post_op_comp_omit_na_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_post_op_comp_omit_na_train_predictors2 <-
  as.matrix(PCNL_post_op_comp_omit_na_train_predictors2)


PCNL_post_op_comp_omit_na_test_predictors2 <-
  PCNL_post_op_comp_omit_na_test_predictors
PCNL_post_op_comp_omit_na_test_predictors2$stone_complexity <-
  as.integer(PCNL_post_op_comp_omit_na_test_predictors2$stone_complexity)
PCNL_post_op_comp_omit_na_test_predictors2$stone_complexity <-
  as.factor(PCNL_post_op_comp_omit_na_test_predictors2$stone_complexity) %>% to_categorical()
PCNL_post_op_comp_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_post_op_comp_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_post_op_comp_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_post_op_comp_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_post_op_comp_omit_na_test_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_post_op_comp_omit_na_test_predictors2$pre_operative_msu_result)
PCNL_post_op_comp_omit_na_test_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_post_op_comp_omit_na_test_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_post_op_comp_omit_na_test_predictors2$stone_dimensions <-
  as.integer(PCNL_post_op_comp_omit_na_test_predictors2$stone_dimensions)
PCNL_post_op_comp_omit_na_test_predictors2$stone_dimensions <-
  as.factor(PCNL_post_op_comp_omit_na_test_predictors2$stone_dimensions) %>% to_categorical()
PCNL_post_op_comp_omit_na_test_predictors2$puncture_site <-
  as.integer(PCNL_post_op_comp_omit_na_test_predictors2$puncture_site)
PCNL_post_op_comp_omit_na_test_predictors2$puncture_site <-
  as.factor(PCNL_post_op_comp_omit_na_test_predictors2$puncture_site) %>% to_categorical()
PCNL_post_op_comp_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_post_op_comp_omit_na_test_predictors2$image_guidance_for_renal_puncture)
PCNL_post_op_comp_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_post_op_comp_omit_na_test_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_post_op_comp_omit_na_test_predictors2$renogramdmsa <-
  ifelse(PCNL_post_op_comp_omit_na_test_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_test_predictors2$calyceal_diverticular <-
  ifelse(PCNL_post_op_comp_omit_na_test_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_test_predictors2$calyceal_l <-
  ifelse(PCNL_post_op_comp_omit_na_test_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_test_predictors2$calyceal_m <-
  ifelse(PCNL_post_op_comp_omit_na_test_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_test_predictors2$complete_staghorn <-
  ifelse(PCNL_post_op_comp_omit_na_test_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_test_predictors2$partial_staghorn <-
  ifelse(PCNL_post_op_comp_omit_na_test_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_test_predictors2$pelvic <-
  ifelse(PCNL_post_op_comp_omit_na_test_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_test_predictors2$upper_ureteric <-
  ifelse(PCNL_post_op_comp_omit_na_test_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_omit_na_test_predictors2$ureter_other <-
  ifelse(PCNL_post_op_comp_omit_na_test_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



post_op_comp_na_omit_test_predictors_stone_complexity <-
  as_tibble(PCNL_post_op_comp_omit_na_test_predictors2$stone_complexity)
colnames(post_op_comp_na_omit_test_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
post_op_comp_na_omit_test_predictors_stone_complexity <-
  subset(post_op_comp_na_omit_test_predictors_stone_complexity,
         select = -GSS0)
PCNL_post_op_comp_omit_na_test_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_test_predictors2,
    post_op_comp_na_omit_test_predictors_stone_complexity
  )

post_op_comp_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_post_op_comp_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(post_op_comp_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
post_op_comp_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    post_op_comp_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_post_op_comp_omit_na_test_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_test_predictors2,
    post_op_comp_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
  )

post_op_comp_na_omit_test_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_post_op_comp_omit_na_test_predictors2$pre_operative_msu_result)
colnames(post_op_comp_na_omit_test_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
post_op_comp_na_omit_test_predictors_pre_operative_msu_result <-
  subset(post_op_comp_na_omit_test_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_post_op_comp_omit_na_test_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_test_predictors2,
    post_op_comp_na_omit_test_predictors_pre_operative_msu_result
  )

post_op_comp_na_omit_test_predictors_puncture_site <-
  as_tibble(PCNL_post_op_comp_omit_na_test_predictors2$puncture_site)
colnames(post_op_comp_na_omit_test_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
post_op_comp_na_omit_test_predictors_puncture_site <-
  subset(post_op_comp_na_omit_test_predictors_puncture_site,
         select = -puncture0)
PCNL_post_op_comp_omit_na_test_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_test_predictors2,
    post_op_comp_na_omit_test_predictors_puncture_site
  )

post_op_comp_na_omit_test_predictors_stone_dimensions <-
  as_tibble(PCNL_post_op_comp_omit_na_test_predictors2$stone_dimensions)
colnames(post_op_comp_na_omit_test_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
post_op_comp_na_omit_test_predictors_stone_dimensions <-
  subset(post_op_comp_na_omit_test_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_post_op_comp_omit_na_test_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_test_predictors2,
    post_op_comp_na_omit_test_predictors_stone_dimensions
  )

post_op_comp_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_post_op_comp_omit_na_test_predictors2$image_guidance_for_renal_puncture)
colnames(post_op_comp_na_omit_test_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
post_op_comp_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  subset(
    post_op_comp_na_omit_test_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_post_op_comp_omit_na_test_predictors2 <-
  cbind(
    PCNL_post_op_comp_omit_na_test_predictors2,
    post_op_comp_na_omit_test_predictors_image_guidance_for_renal_puncture
  )

PCNL_post_op_comp_omit_na_test_predictors2 <-
  subset(
    PCNL_post_op_comp_omit_na_test_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_post_op_comp_omit_na_test_predictors2 <-
  as.matrix(PCNL_post_op_comp_omit_na_test_predictors2)

##### Oversampled Dataset
PCNL_post_op_comp_omit_na_train_oversample <-
  ROSE::ovun.sample(
    postop_complications ~ .,
    data = PCNL_post_op_comp_omit_na_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_post_op_comp_omit_na_train_oversample$postop_complications)

PCNL_post_op_comp_oversample_train_outcome <-
  subset(PCNL_post_op_comp_omit_na_train_oversample,
         select = postop_complications)
PCNL_post_op_comp_oversample_train_outcome$postop_complications <-
  as.numeric(PCNL_post_op_comp_oversample_train_outcome$postop_complications)
PCNL_post_op_comp_oversample_train_outcome$postop_complications <-
  ifelse(PCNL_post_op_comp_oversample_train_outcome$postop_complications == 2,
         1,
         0)
PCNL_post_op_comp_oversample_train_outcome <-
  as.matrix(PCNL_post_op_comp_oversample_train_outcome)

PCNL_post_op_comp_oversample_train_predictors <-
  subset(PCNL_post_op_comp_omit_na_train_oversample,
         select = -postop_complications)

PCNL_post_op_comp_oversample_train_predictors2 <-
  PCNL_post_op_comp_oversample_train_predictors
PCNL_post_op_comp_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_post_op_comp_oversample_train_predictors2$stone_complexity)
PCNL_post_op_comp_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_post_op_comp_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_post_op_comp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_post_op_comp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_post_op_comp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_post_op_comp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_post_op_comp_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_post_op_comp_oversample_train_predictors2$pre_operative_msu_result)
PCNL_post_op_comp_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_post_op_comp_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_post_op_comp_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_post_op_comp_oversample_train_predictors2$stone_dimensions)
PCNL_post_op_comp_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_post_op_comp_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_post_op_comp_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_post_op_comp_oversample_train_predictors2$puncture_site)
PCNL_post_op_comp_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_post_op_comp_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_post_op_comp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_post_op_comp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_post_op_comp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_post_op_comp_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_post_op_comp_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_post_op_comp_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_post_op_comp_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_post_op_comp_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_post_op_comp_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_post_op_comp_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_post_op_comp_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_post_op_comp_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_post_op_comp_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_post_op_comp_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



post_op_comp_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_post_op_comp_oversample_train_predictors2$stone_complexity)
colnames(post_op_comp_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
post_op_comp_na_omit_train_predictors_stone_complexity <-
  subset(post_op_comp_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_post_op_comp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_oversample_train_predictors2,
    post_op_comp_na_omit_train_predictors_stone_complexity
  )

post_op_comp_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_post_op_comp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(post_op_comp_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
post_op_comp_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    post_op_comp_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_post_op_comp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_oversample_train_predictors2,
    post_op_comp_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

post_op_comp_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_post_op_comp_oversample_train_predictors2$pre_operative_msu_result)
colnames(post_op_comp_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
post_op_comp_na_omit_train_predictors_pre_operative_msu_result <-
  subset(post_op_comp_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_post_op_comp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_oversample_train_predictors2,
    post_op_comp_na_omit_train_predictors_pre_operative_msu_result
  )

post_op_comp_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_post_op_comp_oversample_train_predictors2$puncture_site)
colnames(post_op_comp_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
post_op_comp_na_omit_train_predictors_puncture_site <-
  subset(post_op_comp_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_post_op_comp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_oversample_train_predictors2,
    post_op_comp_na_omit_train_predictors_puncture_site
  )

post_op_comp_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_post_op_comp_oversample_train_predictors2$stone_dimensions)
colnames(post_op_comp_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
post_op_comp_na_omit_train_predictors_stone_dimensions <-
  subset(post_op_comp_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_post_op_comp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_oversample_train_predictors2,
    post_op_comp_na_omit_train_predictors_stone_dimensions
  )

post_op_comp_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_post_op_comp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(post_op_comp_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
post_op_comp_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    post_op_comp_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_post_op_comp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_oversample_train_predictors2,
    post_op_comp_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_post_op_comp_oversample_train_predictors2 <-
  subset(
    PCNL_post_op_comp_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_post_op_comp_oversample_train_predictors2 <-
  as.matrix(PCNL_post_op_comp_oversample_train_predictors2)

##### Imputed Dataset
PCNL_post_op_comp_imp_pre <-
  PCNL_post_op_comp_omit_na %>% drop_na(postop_complications)
PCNL_post_op_comp_imp1 <- mice(PCNL_post_op_comp_imp_pre, m = 1)
summary(PCNL_post_op_comp_imp1)

PCNL_post_op_comp_imp <-
  mice::complete(PCNL_post_op_comp_imp1, 1) %>% as_tibble()

PCNL_post_op_comp_imp_sample <-
  sample(1:nrow(PCNL_post_op_comp_imp),
         size = nrow(PCNL_post_op_comp_imp) * 0.7)
PCNL_post_op_comp_imp_train <-
  PCNL_post_op_comp_imp[PCNL_post_op_comp_imp_sample, ] %>% as_tibble()
PCNL_post_op_comp_imp_test <-
  PCNL_post_op_comp_imp[-PCNL_post_op_comp_imp_sample, ] %>% as_tibble()
PCNL_post_op_comp_imp_train$postop_complications <-
  as.factor(PCNL_post_op_comp_imp_train$postop_complications)
PCNL_post_op_comp_imp_test$postop_complications <-
  as.factor(PCNL_post_op_comp_imp_test$postop_complications)
summary(PCNL_post_op_comp_imp_train$postop_complications)

PCNL_post_op_comp_imp_train_outcome <-
  subset(PCNL_post_op_comp_imp_train,
         select = postop_complications)
PCNL_post_op_comp_imp_train_outcome$postop_complications <-
  as.numeric(PCNL_post_op_comp_imp_train_outcome$postop_complications)
PCNL_post_op_comp_imp_train_outcome$postop_complications <-
  ifelse(PCNL_post_op_comp_imp_train_outcome$postop_complications == 2,
         1,
         0)
PCNL_post_op_comp_imp_train_outcome <-
  as.matrix(PCNL_post_op_comp_imp_train_outcome)
PCNL_post_op_comp_imp_train_predictors <-
  subset(PCNL_post_op_comp_imp_train,
         select = -postop_complications)

PCNL_post_op_comp_imp_train_predictors2 <-
  PCNL_post_op_comp_imp_train_predictors
PCNL_post_op_comp_imp_train_predictors2$stone_complexity <-
  as.integer(PCNL_post_op_comp_imp_train_predictors2$stone_complexity)
PCNL_post_op_comp_imp_train_predictors2$stone_complexity <-
  as.factor(PCNL_post_op_comp_imp_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_post_op_comp_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_post_op_comp_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_post_op_comp_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_post_op_comp_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_post_op_comp_imp_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_post_op_comp_imp_train_predictors2$pre_operative_msu_result)
PCNL_post_op_comp_imp_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_post_op_comp_imp_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_post_op_comp_imp_train_predictors2$stone_dimensions <-
  as.integer(PCNL_post_op_comp_imp_train_predictors2$stone_dimensions)
PCNL_post_op_comp_imp_train_predictors2$stone_dimensions <-
  as.factor(PCNL_post_op_comp_imp_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_post_op_comp_imp_train_predictors2$puncture_site <-
  as.integer(PCNL_post_op_comp_imp_train_predictors2$puncture_site)
PCNL_post_op_comp_imp_train_predictors2$puncture_site <-
  as.factor(PCNL_post_op_comp_imp_train_predictors2$puncture_site) %>% to_categorical()
PCNL_post_op_comp_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_post_op_comp_imp_train_predictors2$image_guidance_for_renal_puncture)
PCNL_post_op_comp_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_post_op_comp_imp_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_post_op_comp_imp_train_predictors2$renogramdmsa <-
  ifelse(PCNL_post_op_comp_imp_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_post_op_comp_imp_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_train_predictors2$calyceal_l <-
  ifelse(PCNL_post_op_comp_imp_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_train_predictors2$calyceal_m <-
  ifelse(PCNL_post_op_comp_imp_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_train_predictors2$complete_staghorn <-
  ifelse(PCNL_post_op_comp_imp_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_train_predictors2$partial_staghorn <-
  ifelse(PCNL_post_op_comp_imp_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_train_predictors2$pelvic <-
  ifelse(PCNL_post_op_comp_imp_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_train_predictors2$upper_ureteric <-
  ifelse(PCNL_post_op_comp_imp_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_train_predictors2$ureter_other <-
  ifelse(PCNL_post_op_comp_imp_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



post_op_comp_imp_train_predictors_stone_complexity <-
  as_tibble(PCNL_post_op_comp_imp_train_predictors2$stone_complexity)
colnames(post_op_comp_imp_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
post_op_comp_imp_train_predictors_stone_complexity <-
  subset(post_op_comp_imp_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_post_op_comp_imp_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_train_predictors2,
    post_op_comp_imp_train_predictors_stone_complexity
  )

post_op_comp_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_post_op_comp_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(post_op_comp_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
post_op_comp_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    post_op_comp_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_post_op_comp_imp_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_train_predictors2,
    post_op_comp_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

post_op_comp_imp_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_post_op_comp_imp_train_predictors2$pre_operative_msu_result)
colnames(post_op_comp_imp_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
post_op_comp_imp_train_predictors_pre_operative_msu_result <-
  subset(post_op_comp_imp_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_post_op_comp_imp_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_train_predictors2,
    post_op_comp_imp_train_predictors_pre_operative_msu_result
  )

post_op_comp_imp_train_predictors_puncture_site <-
  as_tibble(PCNL_post_op_comp_imp_train_predictors2$puncture_site)
colnames(post_op_comp_imp_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
post_op_comp_imp_train_predictors_puncture_site <-
  subset(post_op_comp_imp_train_predictors_puncture_site,
         select = -puncture0)
PCNL_post_op_comp_imp_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_train_predictors2,
    post_op_comp_imp_train_predictors_puncture_site
  )

post_op_comp_imp_train_predictors_stone_dimensions <-
  as_tibble(PCNL_post_op_comp_imp_train_predictors2$stone_dimensions)
colnames(post_op_comp_imp_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
post_op_comp_imp_train_predictors_stone_dimensions <-
  subset(post_op_comp_imp_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_post_op_comp_imp_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_train_predictors2,
    post_op_comp_imp_train_predictors_stone_dimensions
  )

post_op_comp_imp_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_post_op_comp_imp_train_predictors2$image_guidance_for_renal_puncture)
colnames(post_op_comp_imp_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
post_op_comp_imp_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    post_op_comp_imp_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_post_op_comp_imp_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_train_predictors2,
    post_op_comp_imp_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_post_op_comp_imp_train_predictors2 <-
  subset(
    PCNL_post_op_comp_imp_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_post_op_comp_imp_train_predictors2 <-
  as.matrix(PCNL_post_op_comp_imp_train_predictors2)


##### Oversampling of Imputed Dataset
PCNL_post_op_comp_imp_train_oversample <-
  ROSE::ovun.sample(
    postop_complications ~ .,
    data = PCNL_post_op_comp_imp_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_post_op_comp_imp_train_oversample$postop_complications)

PCNL_post_op_comp_imp_oversample_train_outcome <-
  subset(PCNL_post_op_comp_imp_train_oversample,
         select = postop_complications)
PCNL_post_op_comp_imp_oversample_train_outcome$postop_complications <-
  as.numeric(PCNL_post_op_comp_imp_oversample_train_outcome$postop_complications)
PCNL_post_op_comp_imp_oversample_train_outcome$postop_complications <-
  ifelse(PCNL_post_op_comp_imp_oversample_train_outcome$postop_complications == 2,
         1,
         0)
PCNL_post_op_comp_imp_oversample_train_outcome <-
  as.matrix(PCNL_post_op_comp_imp_oversample_train_outcome)

PCNL_post_op_comp_imp_oversample_train_predictors <-
  subset(PCNL_post_op_comp_imp_train_oversample,
         select = -postop_complications)

PCNL_post_op_comp_imp_oversample_train_predictors2 <-
  PCNL_post_op_comp_imp_oversample_train_predictors
PCNL_post_op_comp_imp_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_post_op_comp_imp_oversample_train_predictors2$stone_complexity)
PCNL_post_op_comp_imp_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_post_op_comp_imp_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_post_op_comp_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_post_op_comp_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_post_op_comp_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_post_op_comp_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_post_op_comp_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_post_op_comp_imp_oversample_train_predictors2$pre_operative_msu_result)
PCNL_post_op_comp_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_post_op_comp_imp_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_post_op_comp_imp_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_post_op_comp_imp_oversample_train_predictors2$stone_dimensions)
PCNL_post_op_comp_imp_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_post_op_comp_imp_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_post_op_comp_imp_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_post_op_comp_imp_oversample_train_predictors2$puncture_site)
PCNL_post_op_comp_imp_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_post_op_comp_imp_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_post_op_comp_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_post_op_comp_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_post_op_comp_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_post_op_comp_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_post_op_comp_imp_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_post_op_comp_imp_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(
    PCNL_post_op_comp_imp_oversample_train_predictors2$calyceal_diverticular == "2",
    1,
    0
  ) %>% as.numeric()
PCNL_post_op_comp_imp_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_post_op_comp_imp_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_post_op_comp_imp_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_post_op_comp_imp_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_post_op_comp_imp_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_post_op_comp_imp_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_post_op_comp_imp_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_post_op_comp_imp_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_post_op_comp_imp_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



post_op_comp_imp_oversample_train_predictors_stone_complexity <-
  as_tibble(PCNL_post_op_comp_imp_oversample_train_predictors2$stone_complexity)
colnames(post_op_comp_imp_oversample_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
post_op_comp_imp_oversample_train_predictors_stone_complexity <-
  subset(post_op_comp_imp_oversample_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_post_op_comp_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_oversample_train_predictors2,
    post_op_comp_imp_oversample_train_predictors_stone_complexity
  )

post_op_comp_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_post_op_comp_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(
  post_op_comp_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
post_op_comp_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    post_op_comp_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_post_op_comp_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_oversample_train_predictors2,
    post_op_comp_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

post_op_comp_imp_oversample_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_post_op_comp_imp_oversample_train_predictors2$pre_operative_msu_result)
colnames(post_op_comp_imp_oversample_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
post_op_comp_imp_oversample_train_predictors_pre_operative_msu_result <-
  subset(
    post_op_comp_imp_oversample_train_predictors_pre_operative_msu_result,
    select = -msu0
  )
PCNL_post_op_comp_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_oversample_train_predictors2,
    post_op_comp_imp_oversample_train_predictors_pre_operative_msu_result
  )

post_op_comp_imp_oversample_train_predictors_puncture_site <-
  as_tibble(PCNL_post_op_comp_imp_oversample_train_predictors2$puncture_site)
colnames(post_op_comp_imp_oversample_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
post_op_comp_imp_oversample_train_predictors_puncture_site <-
  subset(post_op_comp_imp_oversample_train_predictors_puncture_site,
         select = -puncture0)
PCNL_post_op_comp_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_oversample_train_predictors2,
    post_op_comp_imp_oversample_train_predictors_puncture_site
  )

post_op_comp_imp_oversample_train_predictors_stone_dimensions <-
  as_tibble(PCNL_post_op_comp_imp_oversample_train_predictors2$stone_dimensions)
colnames(post_op_comp_imp_oversample_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
post_op_comp_imp_oversample_train_predictors_stone_dimensions <-
  subset(
    post_op_comp_imp_oversample_train_predictors_stone_dimensions,
    select = -stone_size0
  )
PCNL_post_op_comp_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_oversample_train_predictors2,
    post_op_comp_imp_oversample_train_predictors_stone_dimensions
  )

post_op_comp_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_post_op_comp_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(post_op_comp_imp_oversample_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
post_op_comp_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    post_op_comp_imp_oversample_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_post_op_comp_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_post_op_comp_imp_oversample_train_predictors2,
    post_op_comp_imp_oversample_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_post_op_comp_imp_oversample_train_predictors2 <-
  subset(
    PCNL_post_op_comp_imp_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_post_op_comp_imp_oversample_train_predictors2 <-
  as.matrix(PCNL_post_op_comp_imp_oversample_train_predictors2)

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
) %>% as_tibble() %>% janitor::clean_names()

plot_missing(PCNL_original_sf_at_fu)


##### Omit NA Dataset
PCNL_sf_at_fu_omit_na <- na.omit(PCNL_original_sf_at_fu)
str(PCNL_sf_at_fu_omit_na)

PCNL_sf_at_fu_sample <-
  sample(1:nrow(PCNL_original_sf_at_fu),
         size = nrow(PCNL_original_sf_at_fu) * 0.7)
PCNL_sf_at_fu_train <-
  PCNL_original_sf_at_fu[PCNL_sf_at_fu_sample, ] %>% as_tibble()
PCNL_sf_at_fu_test <-
  PCNL_original_sf_at_fu[-PCNL_sf_at_fu_sample, ] %>% as_tibble()

PCNL_sf_at_fu_omit_na_sample <-
  sample(1:nrow(PCNL_sf_at_fu_omit_na),
         size = nrow(PCNL_sf_at_fu_omit_na) * 0.7)
PCNL_sf_at_fu_omit_na_train <-
  PCNL_sf_at_fu_omit_na[PCNL_sf_at_fu_omit_na_sample, ] %>% as_tibble()
PCNL_sf_at_fu_omit_na_test <-
  PCNL_sf_at_fu_omit_na[-PCNL_sf_at_fu_omit_na_sample, ] %>% as_tibble()
summary(PCNL_sf_at_fu_omit_na_train$stone_free_at_follow_up)

PCNL_sf_at_fu_omit_na_train_outcome <-
  subset(PCNL_sf_at_fu_omit_na_train,
         select = stone_free_at_follow_up)
PCNL_sf_at_fu_omit_na_train_outcome$stone_free_at_follow_up <-
  as.numeric(PCNL_sf_at_fu_omit_na_train_outcome$stone_free_at_follow_up)
PCNL_sf_at_fu_omit_na_train_outcome$stone_free_at_follow_up <-
  ifelse(PCNL_sf_at_fu_omit_na_train_outcome$stone_free_at_follow_up == 2,
         1,
         0)
PCNL_sf_at_fu_omit_na_train_outcome <-
  as.matrix(PCNL_sf_at_fu_omit_na_train_outcome)
PCNL_sf_at_fu_omit_na_test_outcome <-
  subset(PCNL_sf_at_fu_omit_na_test,
         select = stone_free_at_follow_up)
PCNL_sf_at_fu_omit_na_test_outcome$stone_free_at_follow_up <-
  as.numeric(PCNL_sf_at_fu_omit_na_test_outcome$stone_free_at_follow_up)
PCNL_sf_at_fu_omit_na_test_outcome$stone_free_at_follow_up <-
  ifelse(PCNL_sf_at_fu_omit_na_test_outcome$stone_free_at_follow_up == 2,
         1,
         0)
PCNL_sf_at_fu_omit_na_test_outcome <-
  as.matrix(PCNL_sf_at_fu_omit_na_test_outcome)
PCNL_sf_at_fu_omit_na_train_predictors <-
  subset(PCNL_sf_at_fu_omit_na_train,
         select = -stone_free_at_follow_up)
PCNL_sf_at_fu_omit_na_test_predictors <-
  subset(PCNL_sf_at_fu_omit_na_test,
         select = -stone_free_at_follow_up)

PCNL_sf_at_fu_omit_na_train_predictors2 <-
  PCNL_sf_at_fu_omit_na_train_predictors
PCNL_sf_at_fu_omit_na_train_predictors2$stone_complexity <-
  as.integer(PCNL_sf_at_fu_omit_na_train_predictors2$stone_complexity)
PCNL_sf_at_fu_omit_na_train_predictors2$stone_complexity <-
  as.factor(PCNL_sf_at_fu_omit_na_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_sf_at_fu_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_sf_at_fu_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_sf_at_fu_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_sf_at_fu_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_sf_at_fu_omit_na_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_sf_at_fu_omit_na_train_predictors2$pre_operative_msu_result)
PCNL_sf_at_fu_omit_na_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_sf_at_fu_omit_na_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_sf_at_fu_omit_na_train_predictors2$stone_dimensions <-
  as.integer(PCNL_sf_at_fu_omit_na_train_predictors2$stone_dimensions)
PCNL_sf_at_fu_omit_na_train_predictors2$stone_dimensions <-
  as.factor(PCNL_sf_at_fu_omit_na_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_sf_at_fu_omit_na_train_predictors2$puncture_site <-
  as.integer(PCNL_sf_at_fu_omit_na_train_predictors2$puncture_site)
PCNL_sf_at_fu_omit_na_train_predictors2$puncture_site <-
  as.factor(PCNL_sf_at_fu_omit_na_train_predictors2$puncture_site) %>% to_categorical()
PCNL_sf_at_fu_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_sf_at_fu_omit_na_train_predictors2$image_guidance_for_renal_puncture)
PCNL_sf_at_fu_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_sf_at_fu_omit_na_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_sf_at_fu_omit_na_train_predictors2$renogramdmsa <-
  ifelse(PCNL_sf_at_fu_omit_na_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_sf_at_fu_omit_na_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_train_predictors2$calyceal_l <-
  ifelse(PCNL_sf_at_fu_omit_na_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_train_predictors2$calyceal_m <-
  ifelse(PCNL_sf_at_fu_omit_na_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_train_predictors2$complete_staghorn <-
  ifelse(PCNL_sf_at_fu_omit_na_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_train_predictors2$partial_staghorn <-
  ifelse(PCNL_sf_at_fu_omit_na_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_train_predictors2$pelvic <-
  ifelse(PCNL_sf_at_fu_omit_na_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_train_predictors2$upper_ureteric <-
  ifelse(PCNL_sf_at_fu_omit_na_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_train_predictors2$ureter_other <-
  ifelse(PCNL_sf_at_fu_omit_na_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



sf_at_fu_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_sf_at_fu_omit_na_train_predictors2$stone_complexity)
colnames(sf_at_fu_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
sf_at_fu_na_omit_train_predictors_stone_complexity <-
  subset(sf_at_fu_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_sf_at_fu_omit_na_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_train_predictors2,
    sf_at_fu_na_omit_train_predictors_stone_complexity
  )

sf_at_fu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_sf_at_fu_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(sf_at_fu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
sf_at_fu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    sf_at_fu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_sf_at_fu_omit_na_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_train_predictors2,
    sf_at_fu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

sf_at_fu_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_sf_at_fu_omit_na_train_predictors2$pre_operative_msu_result)
colnames(sf_at_fu_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
sf_at_fu_na_omit_train_predictors_pre_operative_msu_result <-
  subset(sf_at_fu_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_sf_at_fu_omit_na_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_train_predictors2,
    sf_at_fu_na_omit_train_predictors_pre_operative_msu_result
  )

sf_at_fu_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_sf_at_fu_omit_na_train_predictors2$puncture_site)
colnames(sf_at_fu_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
sf_at_fu_na_omit_train_predictors_puncture_site <-
  subset(sf_at_fu_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_sf_at_fu_omit_na_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_train_predictors2,
    sf_at_fu_na_omit_train_predictors_puncture_site
  )

sf_at_fu_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_sf_at_fu_omit_na_train_predictors2$stone_dimensions)
colnames(sf_at_fu_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
sf_at_fu_na_omit_train_predictors_stone_dimensions <-
  subset(sf_at_fu_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_sf_at_fu_omit_na_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_train_predictors2,
    sf_at_fu_na_omit_train_predictors_stone_dimensions
  )

sf_at_fu_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_sf_at_fu_omit_na_train_predictors2$image_guidance_for_renal_puncture)
colnames(sf_at_fu_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
sf_at_fu_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    sf_at_fu_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_sf_at_fu_omit_na_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_train_predictors2,
    sf_at_fu_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_sf_at_fu_omit_na_train_predictors2 <-
  subset(
    PCNL_sf_at_fu_omit_na_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_sf_at_fu_omit_na_train_predictors2 <-
  as.matrix(PCNL_sf_at_fu_omit_na_train_predictors2)


PCNL_sf_at_fu_omit_na_test_predictors2 <-
  PCNL_sf_at_fu_omit_na_test_predictors
PCNL_sf_at_fu_omit_na_test_predictors2$stone_complexity <-
  as.integer(PCNL_sf_at_fu_omit_na_test_predictors2$stone_complexity)
PCNL_sf_at_fu_omit_na_test_predictors2$stone_complexity <-
  as.factor(PCNL_sf_at_fu_omit_na_test_predictors2$stone_complexity) %>% to_categorical()
PCNL_sf_at_fu_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_sf_at_fu_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_sf_at_fu_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_sf_at_fu_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_sf_at_fu_omit_na_test_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_sf_at_fu_omit_na_test_predictors2$pre_operative_msu_result)
PCNL_sf_at_fu_omit_na_test_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_sf_at_fu_omit_na_test_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_sf_at_fu_omit_na_test_predictors2$stone_dimensions <-
  as.integer(PCNL_sf_at_fu_omit_na_test_predictors2$stone_dimensions)
PCNL_sf_at_fu_omit_na_test_predictors2$stone_dimensions <-
  as.factor(PCNL_sf_at_fu_omit_na_test_predictors2$stone_dimensions) %>% to_categorical()
PCNL_sf_at_fu_omit_na_test_predictors2$puncture_site <-
  as.integer(PCNL_sf_at_fu_omit_na_test_predictors2$puncture_site)
PCNL_sf_at_fu_omit_na_test_predictors2$puncture_site <-
  as.factor(PCNL_sf_at_fu_omit_na_test_predictors2$puncture_site) %>% to_categorical()
PCNL_sf_at_fu_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_sf_at_fu_omit_na_test_predictors2$image_guidance_for_renal_puncture)
PCNL_sf_at_fu_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_sf_at_fu_omit_na_test_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_sf_at_fu_omit_na_test_predictors2$renogramdmsa <-
  ifelse(PCNL_sf_at_fu_omit_na_test_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_test_predictors2$calyceal_diverticular <-
  ifelse(PCNL_sf_at_fu_omit_na_test_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_test_predictors2$calyceal_l <-
  ifelse(PCNL_sf_at_fu_omit_na_test_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_test_predictors2$calyceal_m <-
  ifelse(PCNL_sf_at_fu_omit_na_test_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_test_predictors2$complete_staghorn <-
  ifelse(PCNL_sf_at_fu_omit_na_test_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_test_predictors2$partial_staghorn <-
  ifelse(PCNL_sf_at_fu_omit_na_test_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_test_predictors2$pelvic <-
  ifelse(PCNL_sf_at_fu_omit_na_test_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_test_predictors2$upper_ureteric <-
  ifelse(PCNL_sf_at_fu_omit_na_test_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_omit_na_test_predictors2$ureter_other <-
  ifelse(PCNL_sf_at_fu_omit_na_test_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



sf_at_fu_na_omit_test_predictors_stone_complexity <-
  as_tibble(PCNL_sf_at_fu_omit_na_test_predictors2$stone_complexity)
colnames(sf_at_fu_na_omit_test_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
sf_at_fu_na_omit_test_predictors_stone_complexity <-
  subset(sf_at_fu_na_omit_test_predictors_stone_complexity,
         select = -GSS0)
PCNL_sf_at_fu_omit_na_test_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_test_predictors2,
    sf_at_fu_na_omit_test_predictors_stone_complexity
  )

sf_at_fu_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_sf_at_fu_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(sf_at_fu_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
sf_at_fu_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    sf_at_fu_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_sf_at_fu_omit_na_test_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_test_predictors2,
    sf_at_fu_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
  )

sf_at_fu_na_omit_test_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_sf_at_fu_omit_na_test_predictors2$pre_operative_msu_result)
colnames(sf_at_fu_na_omit_test_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
sf_at_fu_na_omit_test_predictors_pre_operative_msu_result <-
  subset(sf_at_fu_na_omit_test_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_sf_at_fu_omit_na_test_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_test_predictors2,
    sf_at_fu_na_omit_test_predictors_pre_operative_msu_result
  )

sf_at_fu_na_omit_test_predictors_puncture_site <-
  as_tibble(PCNL_sf_at_fu_omit_na_test_predictors2$puncture_site)
colnames(sf_at_fu_na_omit_test_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
sf_at_fu_na_omit_test_predictors_puncture_site <-
  subset(sf_at_fu_na_omit_test_predictors_puncture_site,
         select = -puncture0)
PCNL_sf_at_fu_omit_na_test_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_test_predictors2,
    sf_at_fu_na_omit_test_predictors_puncture_site
  )

sf_at_fu_na_omit_test_predictors_stone_dimensions <-
  as_tibble(PCNL_sf_at_fu_omit_na_test_predictors2$stone_dimensions)
colnames(sf_at_fu_na_omit_test_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
sf_at_fu_na_omit_test_predictors_stone_dimensions <-
  subset(sf_at_fu_na_omit_test_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_sf_at_fu_omit_na_test_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_test_predictors2,
    sf_at_fu_na_omit_test_predictors_stone_dimensions
  )

sf_at_fu_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_sf_at_fu_omit_na_test_predictors2$image_guidance_for_renal_puncture)
colnames(sf_at_fu_na_omit_test_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
sf_at_fu_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  subset(
    sf_at_fu_na_omit_test_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_sf_at_fu_omit_na_test_predictors2 <-
  cbind(
    PCNL_sf_at_fu_omit_na_test_predictors2,
    sf_at_fu_na_omit_test_predictors_image_guidance_for_renal_puncture
  )

PCNL_sf_at_fu_omit_na_test_predictors2 <-
  subset(
    PCNL_sf_at_fu_omit_na_test_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_sf_at_fu_omit_na_test_predictors2 <-
  as.matrix(PCNL_sf_at_fu_omit_na_test_predictors2)

##### Oversampled Dataset
PCNL_sf_at_fu_omit_na_train_oversample <-
  ROSE::ovun.sample(
    stone_free_at_follow_up ~ .,
    data = PCNL_sf_at_fu_omit_na_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_sf_at_fu_omit_na_train_oversample$stone_free_at_follow_up)

PCNL_sf_at_fu_oversample_train_outcome <-
  subset(PCNL_sf_at_fu_omit_na_train_oversample,
         select = stone_free_at_follow_up)
PCNL_sf_at_fu_oversample_train_outcome$stone_free_at_follow_up <-
  as.numeric(PCNL_sf_at_fu_oversample_train_outcome$stone_free_at_follow_up)
PCNL_sf_at_fu_oversample_train_outcome$stone_free_at_follow_up <-
  ifelse(PCNL_sf_at_fu_oversample_train_outcome$stone_free_at_follow_up == 2,
         1,
         0)
PCNL_sf_at_fu_oversample_train_outcome <-
  as.matrix(PCNL_sf_at_fu_oversample_train_outcome)

PCNL_sf_at_fu_oversample_train_predictors <-
  subset(PCNL_sf_at_fu_omit_na_train_oversample,
         select = -stone_free_at_follow_up)

PCNL_sf_at_fu_oversample_train_predictors2 <-
  PCNL_sf_at_fu_oversample_train_predictors
PCNL_sf_at_fu_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_sf_at_fu_oversample_train_predictors2$stone_complexity)
PCNL_sf_at_fu_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_sf_at_fu_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_sf_at_fu_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_sf_at_fu_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_sf_at_fu_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_sf_at_fu_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_sf_at_fu_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_sf_at_fu_oversample_train_predictors2$pre_operative_msu_result)
PCNL_sf_at_fu_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_sf_at_fu_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_sf_at_fu_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_sf_at_fu_oversample_train_predictors2$stone_dimensions)
PCNL_sf_at_fu_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_sf_at_fu_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_sf_at_fu_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_sf_at_fu_oversample_train_predictors2$puncture_site)
PCNL_sf_at_fu_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_sf_at_fu_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_sf_at_fu_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_sf_at_fu_oversample_train_predictors2$image_guidance_for_renal_puncture)
PCNL_sf_at_fu_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_sf_at_fu_oversample_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_sf_at_fu_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_sf_at_fu_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_sf_at_fu_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_sf_at_fu_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_sf_at_fu_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_sf_at_fu_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_sf_at_fu_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_sf_at_fu_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_sf_at_fu_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_sf_at_fu_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



sf_at_fu_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_sf_at_fu_oversample_train_predictors2$stone_complexity)
colnames(sf_at_fu_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
sf_at_fu_na_omit_train_predictors_stone_complexity <-
  subset(sf_at_fu_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_sf_at_fu_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_oversample_train_predictors2,
    sf_at_fu_na_omit_train_predictors_stone_complexity
  )

sf_at_fu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_sf_at_fu_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(sf_at_fu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
sf_at_fu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    sf_at_fu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_sf_at_fu_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_oversample_train_predictors2,
    sf_at_fu_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

sf_at_fu_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_sf_at_fu_oversample_train_predictors2$pre_operative_msu_result)
colnames(sf_at_fu_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
sf_at_fu_na_omit_train_predictors_pre_operative_msu_result <-
  subset(sf_at_fu_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_sf_at_fu_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_oversample_train_predictors2,
    sf_at_fu_na_omit_train_predictors_pre_operative_msu_result
  )

sf_at_fu_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_sf_at_fu_oversample_train_predictors2$puncture_site)
colnames(sf_at_fu_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
sf_at_fu_na_omit_train_predictors_puncture_site <-
  subset(sf_at_fu_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_sf_at_fu_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_oversample_train_predictors2,
    sf_at_fu_na_omit_train_predictors_puncture_site
  )

sf_at_fu_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_sf_at_fu_oversample_train_predictors2$stone_dimensions)
colnames(sf_at_fu_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
sf_at_fu_na_omit_train_predictors_stone_dimensions <-
  subset(sf_at_fu_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_sf_at_fu_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_oversample_train_predictors2,
    sf_at_fu_na_omit_train_predictors_stone_dimensions
  )

sf_at_fu_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_sf_at_fu_oversample_train_predictors2$image_guidance_for_renal_puncture)
colnames(sf_at_fu_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
sf_at_fu_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    sf_at_fu_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_sf_at_fu_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_oversample_train_predictors2,
    sf_at_fu_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_sf_at_fu_oversample_train_predictors2 <-
  subset(
    PCNL_sf_at_fu_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_sf_at_fu_oversample_train_predictors2 <-
  as.matrix(PCNL_sf_at_fu_oversample_train_predictors2)

##### Imputed Dataset
PCNL_sf_at_fu_imp_pre <-
  PCNL_sf_at_fu_omit_na %>% drop_na(stone_free_at_follow_up)
PCNL_sf_at_fu_imp1 <- mice(PCNL_sf_at_fu_imp_pre, m = 1)
summary(PCNL_sf_at_fu_imp1)

PCNL_sf_at_fu_imp <-
  mice::complete(PCNL_sf_at_fu_imp1, 1) %>% as_tibble()

PCNL_sf_at_fu_imp_sample <-
  sample(1:nrow(PCNL_sf_at_fu_imp), size = nrow(PCNL_sf_at_fu_imp) * 0.7)
PCNL_sf_at_fu_imp_train <-
  PCNL_sf_at_fu_imp[PCNL_sf_at_fu_imp_sample, ] %>% as_tibble()
PCNL_sf_at_fu_imp_test <-
  PCNL_sf_at_fu_imp[-PCNL_sf_at_fu_imp_sample, ] %>% as_tibble()
PCNL_sf_at_fu_imp_train$stone_free_at_follow_up <-
  as.factor(PCNL_sf_at_fu_imp_train$stone_free_at_follow_up)
PCNL_sf_at_fu_imp_test$stone_free_at_follow_up <-
  as.factor(PCNL_sf_at_fu_imp_test$stone_free_at_follow_up)
summary(PCNL_sf_at_fu_imp_train$stone_free_at_follow_up)

PCNL_sf_at_fu_imp_train_outcome <-
  subset(PCNL_sf_at_fu_imp_train,
         select = stone_free_at_follow_up)
PCNL_sf_at_fu_imp_train_outcome$stone_free_at_follow_up <-
  as.numeric(PCNL_sf_at_fu_imp_train_outcome$stone_free_at_follow_up)
PCNL_sf_at_fu_imp_train_outcome$stone_free_at_follow_up <-
  ifelse(PCNL_sf_at_fu_imp_train_outcome$stone_free_at_follow_up == 2,
         1,
         0)
PCNL_sf_at_fu_imp_train_outcome <-
  as.matrix(PCNL_sf_at_fu_imp_train_outcome)


PCNL_sf_at_fu_imp_train_predictors <-
  subset(PCNL_sf_at_fu_imp_train,
         select = -stone_free_at_follow_up)

PCNL_sf_at_fu_imp_train_predictors2 <-
  PCNL_sf_at_fu_imp_train_predictors
PCNL_sf_at_fu_imp_train_predictors2$stone_complexity <-
  as.integer(PCNL_sf_at_fu_imp_train_predictors2$stone_complexity)
PCNL_sf_at_fu_imp_train_predictors2$stone_complexity <-
  as.factor(PCNL_sf_at_fu_imp_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_sf_at_fu_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_sf_at_fu_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_sf_at_fu_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_sf_at_fu_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_sf_at_fu_imp_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_sf_at_fu_imp_train_predictors2$pre_operative_msu_result)
PCNL_sf_at_fu_imp_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_sf_at_fu_imp_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_sf_at_fu_imp_train_predictors2$stone_dimensions <-
  as.integer(PCNL_sf_at_fu_imp_train_predictors2$stone_dimensions)
PCNL_sf_at_fu_imp_train_predictors2$stone_dimensions <-
  as.factor(PCNL_sf_at_fu_imp_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_sf_at_fu_imp_train_predictors2$puncture_site <-
  as.integer(PCNL_sf_at_fu_imp_train_predictors2$puncture_site)
PCNL_sf_at_fu_imp_train_predictors2$puncture_site <-
  as.factor(PCNL_sf_at_fu_imp_train_predictors2$puncture_site) %>% to_categorical()
PCNL_sf_at_fu_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_sf_at_fu_imp_train_predictors2$image_guidance_for_renal_puncture)
PCNL_sf_at_fu_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_sf_at_fu_imp_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_sf_at_fu_imp_train_predictors2$renogramdmsa <-
  ifelse(PCNL_sf_at_fu_imp_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_sf_at_fu_imp_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_train_predictors2$calyceal_l <-
  ifelse(PCNL_sf_at_fu_imp_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_train_predictors2$calyceal_m <-
  ifelse(PCNL_sf_at_fu_imp_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_train_predictors2$complete_staghorn <-
  ifelse(PCNL_sf_at_fu_imp_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_train_predictors2$partial_staghorn <-
  ifelse(PCNL_sf_at_fu_imp_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_train_predictors2$pelvic <-
  ifelse(PCNL_sf_at_fu_imp_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_train_predictors2$upper_ureteric <-
  ifelse(PCNL_sf_at_fu_imp_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_train_predictors2$ureter_other <-
  ifelse(PCNL_sf_at_fu_imp_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



sf_at_fu_imp_train_predictors_stone_complexity <-
  as_tibble(PCNL_sf_at_fu_imp_train_predictors2$stone_complexity)
colnames(sf_at_fu_imp_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
sf_at_fu_imp_train_predictors_stone_complexity <-
  subset(sf_at_fu_imp_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_sf_at_fu_imp_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_train_predictors2,
    sf_at_fu_imp_train_predictors_stone_complexity
  )

sf_at_fu_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_sf_at_fu_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(sf_at_fu_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
sf_at_fu_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    sf_at_fu_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_sf_at_fu_imp_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_train_predictors2,
    sf_at_fu_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

sf_at_fu_imp_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_sf_at_fu_imp_train_predictors2$pre_operative_msu_result)
colnames(sf_at_fu_imp_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
sf_at_fu_imp_train_predictors_pre_operative_msu_result <-
  subset(sf_at_fu_imp_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_sf_at_fu_imp_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_train_predictors2,
    sf_at_fu_imp_train_predictors_pre_operative_msu_result
  )

sf_at_fu_imp_train_predictors_puncture_site <-
  as_tibble(PCNL_sf_at_fu_imp_train_predictors2$puncture_site)
colnames(sf_at_fu_imp_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
sf_at_fu_imp_train_predictors_puncture_site <-
  subset(sf_at_fu_imp_train_predictors_puncture_site,
         select = -puncture0)
PCNL_sf_at_fu_imp_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_train_predictors2,
    sf_at_fu_imp_train_predictors_puncture_site
  )

sf_at_fu_imp_train_predictors_stone_dimensions <-
  as_tibble(PCNL_sf_at_fu_imp_train_predictors2$stone_dimensions)
colnames(sf_at_fu_imp_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
sf_at_fu_imp_train_predictors_stone_dimensions <-
  subset(sf_at_fu_imp_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_sf_at_fu_imp_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_train_predictors2,
    sf_at_fu_imp_train_predictors_stone_dimensions
  )

sf_at_fu_imp_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_sf_at_fu_imp_train_predictors2$image_guidance_for_renal_puncture)
colnames(sf_at_fu_imp_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
sf_at_fu_imp_train_predictors_image_guidance_for_renal_puncture <-
  subset(sf_at_fu_imp_train_predictors_image_guidance_for_renal_puncture,
         select = -image0)
PCNL_sf_at_fu_imp_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_train_predictors2,
    sf_at_fu_imp_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_sf_at_fu_imp_train_predictors2 <-
  subset(
    PCNL_sf_at_fu_imp_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_sf_at_fu_imp_train_predictors2 <-
  as.matrix(PCNL_sf_at_fu_imp_train_predictors2)


##### Oversampling of Imputed Dataset
PCNL_sf_at_fu_imp_train_oversample <-
  ROSE::ovun.sample(
    stone_free_at_follow_up ~ .,
    data = PCNL_sf_at_fu_imp_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_sf_at_fu_imp_train_oversample$stone_free_at_follow_up)

PCNL_sf_at_fu_imp_oversample_train_outcome <-
  subset(PCNL_sf_at_fu_imp_train_oversample,
         select = stone_free_at_follow_up)
PCNL_sf_at_fu_imp_oversample_train_outcome$stone_free_at_follow_up <-
  as.numeric(PCNL_sf_at_fu_imp_oversample_train_outcome$stone_free_at_follow_up)
PCNL_sf_at_fu_imp_oversample_train_outcome$stone_free_at_follow_up <-
  ifelse(PCNL_sf_at_fu_imp_oversample_train_outcome$stone_free_at_follow_up == 2,
         1,
         0)
PCNL_sf_at_fu_imp_oversample_train_outcome <-
  as.matrix(PCNL_sf_at_fu_imp_oversample_train_outcome)

PCNL_sf_at_fu_imp_oversample_train_predictors <-
  subset(PCNL_sf_at_fu_imp_train_oversample,
         select = -stone_free_at_follow_up)

PCNL_sf_at_fu_imp_oversample_train_predictors2 <-
  PCNL_sf_at_fu_imp_oversample_train_predictors
PCNL_sf_at_fu_imp_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_sf_at_fu_imp_oversample_train_predictors2$stone_complexity)
PCNL_sf_at_fu_imp_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_sf_at_fu_imp_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_sf_at_fu_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_sf_at_fu_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_sf_at_fu_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_sf_at_fu_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_sf_at_fu_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_sf_at_fu_imp_oversample_train_predictors2$pre_operative_msu_result)
PCNL_sf_at_fu_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_sf_at_fu_imp_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_sf_at_fu_imp_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_sf_at_fu_imp_oversample_train_predictors2$stone_dimensions)
PCNL_sf_at_fu_imp_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_sf_at_fu_imp_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_sf_at_fu_imp_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_sf_at_fu_imp_oversample_train_predictors2$puncture_site)
PCNL_sf_at_fu_imp_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_sf_at_fu_imp_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_sf_at_fu_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(
    PCNL_sf_at_fu_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
PCNL_sf_at_fu_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(
    PCNL_sf_at_fu_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  ) %>% to_categorical()
PCNL_sf_at_fu_imp_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_sf_at_fu_imp_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_sf_at_fu_imp_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_sf_at_fu_imp_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_sf_at_fu_imp_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_sf_at_fu_imp_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_sf_at_fu_imp_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_sf_at_fu_imp_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_sf_at_fu_imp_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_sf_at_fu_imp_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_sf_at_fu_imp_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



sf_at_fu_imp_oversample_train_predictors_stone_complexity <-
  as_tibble(PCNL_sf_at_fu_imp_oversample_train_predictors2$stone_complexity)
colnames(sf_at_fu_imp_oversample_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
sf_at_fu_imp_oversample_train_predictors_stone_complexity <-
  subset(sf_at_fu_imp_oversample_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_sf_at_fu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_oversample_train_predictors2,
    sf_at_fu_imp_oversample_train_predictors_stone_complexity
  )

sf_at_fu_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_sf_at_fu_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(sf_at_fu_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
sf_at_fu_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    sf_at_fu_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_sf_at_fu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_oversample_train_predictors2,
    sf_at_fu_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

sf_at_fu_imp_oversample_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_sf_at_fu_imp_oversample_train_predictors2$pre_operative_msu_result)
colnames(sf_at_fu_imp_oversample_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
sf_at_fu_imp_oversample_train_predictors_pre_operative_msu_result <-
  subset(sf_at_fu_imp_oversample_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_sf_at_fu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_oversample_train_predictors2,
    sf_at_fu_imp_oversample_train_predictors_pre_operative_msu_result
  )

sf_at_fu_imp_oversample_train_predictors_puncture_site <-
  as_tibble(PCNL_sf_at_fu_imp_oversample_train_predictors2$puncture_site)
colnames(sf_at_fu_imp_oversample_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
sf_at_fu_imp_oversample_train_predictors_puncture_site <-
  subset(sf_at_fu_imp_oversample_train_predictors_puncture_site,
         select = -puncture0)
PCNL_sf_at_fu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_oversample_train_predictors2,
    sf_at_fu_imp_oversample_train_predictors_puncture_site
  )

sf_at_fu_imp_oversample_train_predictors_stone_dimensions <-
  as_tibble(PCNL_sf_at_fu_imp_oversample_train_predictors2$stone_dimensions)
colnames(sf_at_fu_imp_oversample_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
sf_at_fu_imp_oversample_train_predictors_stone_dimensions <-
  subset(sf_at_fu_imp_oversample_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_sf_at_fu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_oversample_train_predictors2,
    sf_at_fu_imp_oversample_train_predictors_stone_dimensions
  )

sf_at_fu_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(
    PCNL_sf_at_fu_imp_oversample_train_predictors2$image_guidance_for_renal_puncture
  )
colnames(sf_at_fu_imp_oversample_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
sf_at_fu_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    sf_at_fu_imp_oversample_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_sf_at_fu_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_sf_at_fu_imp_oversample_train_predictors2,
    sf_at_fu_imp_oversample_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_sf_at_fu_imp_oversample_train_predictors2 <-
  subset(
    PCNL_sf_at_fu_imp_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_sf_at_fu_imp_oversample_train_predictors2 <-
  as.matrix(PCNL_sf_at_fu_imp_oversample_train_predictors2)


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
) %>% as_tibble() %>% janitor::clean_names()

plot_missing(PCNL_original_adj_rx)


##### Omit NA Dataset
PCNL_adj_rx_omit_na <- na.omit(PCNL_original_adj_rx)
str(PCNL_adj_rx_omit_na)

PCNL_adj_rx_sample <-
  sample(1:nrow(PCNL_original_adj_rx),
         size = nrow(PCNL_original_adj_rx) * 0.7)
PCNL_adj_rx_train <-
  PCNL_original_adj_rx[PCNL_adj_rx_sample, ] %>% as_tibble()
PCNL_adj_rx_test <-
  PCNL_original_adj_rx[-PCNL_adj_rx_sample, ] %>% as_tibble()

PCNL_adj_rx_omit_na_sample <-
  sample(1:nrow(PCNL_adj_rx_omit_na),
         size = nrow(PCNL_adj_rx_omit_na) * 0.7)
PCNL_adj_rx_omit_na_train <-
  PCNL_adj_rx_omit_na[PCNL_adj_rx_omit_na_sample, ] %>% as_tibble()
PCNL_adj_rx_omit_na_test <-
  PCNL_adj_rx_omit_na[-PCNL_adj_rx_omit_na_sample, ] %>% as_tibble()
summary(PCNL_adj_rx_omit_na_train$adjuvant_treatment)

PCNL_adj_rx_omit_na_train_outcome <-
  subset(PCNL_adj_rx_omit_na_train,
         select = adjuvant_treatment)
PCNL_adj_rx_omit_na_train_outcome$adjuvant_treatment <-
  as.numeric(PCNL_adj_rx_omit_na_train_outcome$adjuvant_treatment)
PCNL_adj_rx_omit_na_train_outcome$adjuvant_treatment <-
  ifelse(PCNL_adj_rx_omit_na_train_outcome$adjuvant_treatment == 2,
         1,
         0)
PCNL_adj_rx_omit_na_train_outcome <-
  as.matrix(PCNL_adj_rx_omit_na_train_outcome)
PCNL_adj_rx_omit_na_test_outcome <-
  subset(PCNL_adj_rx_omit_na_test,
         select = adjuvant_treatment)
PCNL_adj_rx_omit_na_test_outcome$adjuvant_treatment <-
  as.numeric(PCNL_adj_rx_omit_na_test_outcome$adjuvant_treatment)
PCNL_adj_rx_omit_na_test_outcome$adjuvant_treatment <-
  ifelse(PCNL_adj_rx_omit_na_test_outcome$adjuvant_treatment == 2, 1, 0)
PCNL_adj_rx_omit_na_test_outcome <-
  as.matrix(PCNL_adj_rx_omit_na_test_outcome)
PCNL_adj_rx_omit_na_train_predictors <-
  subset(PCNL_adj_rx_omit_na_train,
         select = -adjuvant_treatment)
PCNL_adj_rx_omit_na_test_predictors <-
  subset(PCNL_adj_rx_omit_na_test,
         select = -adjuvant_treatment)

PCNL_adj_rx_omit_na_train_predictors2 <-
  PCNL_adj_rx_omit_na_train_predictors
PCNL_adj_rx_omit_na_train_predictors2$stone_complexity <-
  as.integer(PCNL_adj_rx_omit_na_train_predictors2$stone_complexity)
PCNL_adj_rx_omit_na_train_predictors2$stone_complexity <-
  as.factor(PCNL_adj_rx_omit_na_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_adj_rx_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_adj_rx_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_adj_rx_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_adj_rx_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_adj_rx_omit_na_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_adj_rx_omit_na_train_predictors2$pre_operative_msu_result)
PCNL_adj_rx_omit_na_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_adj_rx_omit_na_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_adj_rx_omit_na_train_predictors2$stone_dimensions <-
  as.integer(PCNL_adj_rx_omit_na_train_predictors2$stone_dimensions)
PCNL_adj_rx_omit_na_train_predictors2$stone_dimensions <-
  as.factor(PCNL_adj_rx_omit_na_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_adj_rx_omit_na_train_predictors2$puncture_site <-
  as.integer(PCNL_adj_rx_omit_na_train_predictors2$puncture_site)
PCNL_adj_rx_omit_na_train_predictors2$puncture_site <-
  as.factor(PCNL_adj_rx_omit_na_train_predictors2$puncture_site) %>% to_categorical()
PCNL_adj_rx_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_adj_rx_omit_na_train_predictors2$image_guidance_for_renal_puncture)
PCNL_adj_rx_omit_na_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_adj_rx_omit_na_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_adj_rx_omit_na_train_predictors2$renogramdmsa <-
  ifelse(PCNL_adj_rx_omit_na_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_adj_rx_omit_na_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_train_predictors2$calyceal_l <-
  ifelse(PCNL_adj_rx_omit_na_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_train_predictors2$calyceal_m <-
  ifelse(PCNL_adj_rx_omit_na_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_train_predictors2$complete_staghorn <-
  ifelse(PCNL_adj_rx_omit_na_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_train_predictors2$partial_staghorn <-
  ifelse(PCNL_adj_rx_omit_na_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_train_predictors2$pelvic <-
  ifelse(PCNL_adj_rx_omit_na_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_train_predictors2$upper_ureteric <-
  ifelse(PCNL_adj_rx_omit_na_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_train_predictors2$ureter_other <-
  ifelse(PCNL_adj_rx_omit_na_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



adj_rx_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_adj_rx_omit_na_train_predictors2$stone_complexity)
colnames(adj_rx_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
adj_rx_na_omit_train_predictors_stone_complexity <-
  subset(adj_rx_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_adj_rx_omit_na_train_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_train_predictors2,
    adj_rx_na_omit_train_predictors_stone_complexity
  )

adj_rx_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_adj_rx_omit_na_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(adj_rx_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
adj_rx_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    adj_rx_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_adj_rx_omit_na_train_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_train_predictors2,
    adj_rx_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

adj_rx_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_adj_rx_omit_na_train_predictors2$pre_operative_msu_result)
colnames(adj_rx_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
adj_rx_na_omit_train_predictors_pre_operative_msu_result <-
  subset(adj_rx_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_adj_rx_omit_na_train_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_train_predictors2,
    adj_rx_na_omit_train_predictors_pre_operative_msu_result
  )

adj_rx_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_adj_rx_omit_na_train_predictors2$puncture_site)
colnames(adj_rx_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
adj_rx_na_omit_train_predictors_puncture_site <-
  subset(adj_rx_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_adj_rx_omit_na_train_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_train_predictors2,
    adj_rx_na_omit_train_predictors_puncture_site
  )

adj_rx_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_adj_rx_omit_na_train_predictors2$stone_dimensions)
colnames(adj_rx_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
adj_rx_na_omit_train_predictors_stone_dimensions <-
  subset(adj_rx_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_adj_rx_omit_na_train_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_train_predictors2,
    adj_rx_na_omit_train_predictors_stone_dimensions
  )

adj_rx_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_adj_rx_omit_na_train_predictors2$image_guidance_for_renal_puncture)
colnames(adj_rx_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
adj_rx_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    adj_rx_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_adj_rx_omit_na_train_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_train_predictors2,
    adj_rx_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_adj_rx_omit_na_train_predictors2 <-
  subset(
    PCNL_adj_rx_omit_na_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_adj_rx_omit_na_train_predictors2 <-
  as.matrix(PCNL_adj_rx_omit_na_train_predictors2)


PCNL_adj_rx_omit_na_test_predictors2 <-
  PCNL_adj_rx_omit_na_test_predictors
PCNL_adj_rx_omit_na_test_predictors2$stone_complexity <-
  as.integer(PCNL_adj_rx_omit_na_test_predictors2$stone_complexity)
PCNL_adj_rx_omit_na_test_predictors2$stone_complexity <-
  as.factor(PCNL_adj_rx_omit_na_test_predictors2$stone_complexity) %>% to_categorical()
PCNL_adj_rx_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_adj_rx_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_adj_rx_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_adj_rx_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_adj_rx_omit_na_test_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_adj_rx_omit_na_test_predictors2$pre_operative_msu_result)
PCNL_adj_rx_omit_na_test_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_adj_rx_omit_na_test_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_adj_rx_omit_na_test_predictors2$stone_dimensions <-
  as.integer(PCNL_adj_rx_omit_na_test_predictors2$stone_dimensions)
PCNL_adj_rx_omit_na_test_predictors2$stone_dimensions <-
  as.factor(PCNL_adj_rx_omit_na_test_predictors2$stone_dimensions) %>% to_categorical()
PCNL_adj_rx_omit_na_test_predictors2$puncture_site <-
  as.integer(PCNL_adj_rx_omit_na_test_predictors2$puncture_site)
PCNL_adj_rx_omit_na_test_predictors2$puncture_site <-
  as.factor(PCNL_adj_rx_omit_na_test_predictors2$puncture_site) %>% to_categorical()
PCNL_adj_rx_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_adj_rx_omit_na_test_predictors2$image_guidance_for_renal_puncture)
PCNL_adj_rx_omit_na_test_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_adj_rx_omit_na_test_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_adj_rx_omit_na_test_predictors2$renogramdmsa <-
  ifelse(PCNL_adj_rx_omit_na_test_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_test_predictors2$calyceal_diverticular <-
  ifelse(PCNL_adj_rx_omit_na_test_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_test_predictors2$calyceal_l <-
  ifelse(PCNL_adj_rx_omit_na_test_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_test_predictors2$calyceal_m <-
  ifelse(PCNL_adj_rx_omit_na_test_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_test_predictors2$complete_staghorn <-
  ifelse(PCNL_adj_rx_omit_na_test_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_test_predictors2$partial_staghorn <-
  ifelse(PCNL_adj_rx_omit_na_test_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_test_predictors2$pelvic <-
  ifelse(PCNL_adj_rx_omit_na_test_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_test_predictors2$upper_ureteric <-
  ifelse(PCNL_adj_rx_omit_na_test_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_omit_na_test_predictors2$ureter_other <-
  ifelse(PCNL_adj_rx_omit_na_test_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



adj_rx_na_omit_test_predictors_stone_complexity <-
  as_tibble(PCNL_adj_rx_omit_na_test_predictors2$stone_complexity)
colnames(adj_rx_na_omit_test_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
adj_rx_na_omit_test_predictors_stone_complexity <-
  subset(adj_rx_na_omit_test_predictors_stone_complexity,
         select = -GSS0)
PCNL_adj_rx_omit_na_test_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_test_predictors2,
    adj_rx_na_omit_test_predictors_stone_complexity
  )

adj_rx_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_adj_rx_omit_na_test_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(adj_rx_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
adj_rx_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    adj_rx_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_adj_rx_omit_na_test_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_test_predictors2,
    adj_rx_na_omit_test_predictors_sizeouter_diameter_of_amplatz_sheath
  )

adj_rx_na_omit_test_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_adj_rx_omit_na_test_predictors2$pre_operative_msu_result)
colnames(adj_rx_na_omit_test_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
adj_rx_na_omit_test_predictors_pre_operative_msu_result <-
  subset(adj_rx_na_omit_test_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_adj_rx_omit_na_test_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_test_predictors2,
    adj_rx_na_omit_test_predictors_pre_operative_msu_result
  )

adj_rx_na_omit_test_predictors_puncture_site <-
  as_tibble(PCNL_adj_rx_omit_na_test_predictors2$puncture_site)
colnames(adj_rx_na_omit_test_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
adj_rx_na_omit_test_predictors_puncture_site <-
  subset(adj_rx_na_omit_test_predictors_puncture_site,
         select = -puncture0)
PCNL_adj_rx_omit_na_test_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_test_predictors2,
    adj_rx_na_omit_test_predictors_puncture_site
  )

adj_rx_na_omit_test_predictors_stone_dimensions <-
  as_tibble(PCNL_adj_rx_omit_na_test_predictors2$stone_dimensions)
colnames(adj_rx_na_omit_test_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
adj_rx_na_omit_test_predictors_stone_dimensions <-
  subset(adj_rx_na_omit_test_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_adj_rx_omit_na_test_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_test_predictors2,
    adj_rx_na_omit_test_predictors_stone_dimensions
  )

adj_rx_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_adj_rx_omit_na_test_predictors2$image_guidance_for_renal_puncture)
colnames(adj_rx_na_omit_test_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
adj_rx_na_omit_test_predictors_image_guidance_for_renal_puncture <-
  subset(adj_rx_na_omit_test_predictors_image_guidance_for_renal_puncture,
         select = -image0)
PCNL_adj_rx_omit_na_test_predictors2 <-
  cbind(
    PCNL_adj_rx_omit_na_test_predictors2,
    adj_rx_na_omit_test_predictors_image_guidance_for_renal_puncture
  )

PCNL_adj_rx_omit_na_test_predictors2 <-
  subset(
    PCNL_adj_rx_omit_na_test_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_adj_rx_omit_na_test_predictors2 <-
  as.matrix(PCNL_adj_rx_omit_na_test_predictors2)

##### Oversampled Dataset
PCNL_adj_rx_omit_na_train_oversample <-
  ROSE::ovun.sample(
    adjuvant_treatment ~ .,
    data = PCNL_adj_rx_omit_na_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_adj_rx_omit_na_train_oversample$adjuvant_treatment)

PCNL_adj_rx_oversample_train_outcome <-
  subset(PCNL_adj_rx_omit_na_train_oversample,
         select = adjuvant_treatment)
PCNL_adj_rx_oversample_train_outcome$adjuvant_treatment <-
  as.numeric(PCNL_adj_rx_oversample_train_outcome$adjuvant_treatment)
PCNL_adj_rx_oversample_train_outcome$adjuvant_treatment <-
  ifelse(PCNL_adj_rx_oversample_train_outcome$adjuvant_treatment == 2,
         1,
         0)
PCNL_adj_rx_oversample_train_outcome <-
  as.matrix(PCNL_adj_rx_oversample_train_outcome)

PCNL_adj_rx_oversample_train_predictors <-
  subset(PCNL_adj_rx_omit_na_train_oversample,
         select = -adjuvant_treatment)

PCNL_adj_rx_oversample_train_predictors2 <-
  PCNL_adj_rx_oversample_train_predictors
PCNL_adj_rx_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_adj_rx_oversample_train_predictors2$stone_complexity)
PCNL_adj_rx_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_adj_rx_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_adj_rx_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_adj_rx_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_adj_rx_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_adj_rx_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_adj_rx_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_adj_rx_oversample_train_predictors2$pre_operative_msu_result)
PCNL_adj_rx_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_adj_rx_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_adj_rx_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_adj_rx_oversample_train_predictors2$stone_dimensions)
PCNL_adj_rx_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_adj_rx_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_adj_rx_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_adj_rx_oversample_train_predictors2$puncture_site)
PCNL_adj_rx_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_adj_rx_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_adj_rx_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_adj_rx_oversample_train_predictors2$image_guidance_for_renal_puncture)
PCNL_adj_rx_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_adj_rx_oversample_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_adj_rx_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_adj_rx_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_adj_rx_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_adj_rx_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_adj_rx_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_adj_rx_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_adj_rx_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_adj_rx_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_adj_rx_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_adj_rx_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



adj_rx_na_omit_train_predictors_stone_complexity <-
  as_tibble(PCNL_adj_rx_oversample_train_predictors2$stone_complexity)
colnames(adj_rx_na_omit_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
adj_rx_na_omit_train_predictors_stone_complexity <-
  subset(adj_rx_na_omit_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_adj_rx_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_oversample_train_predictors2,
    adj_rx_na_omit_train_predictors_stone_complexity
  )

adj_rx_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_adj_rx_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(adj_rx_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
adj_rx_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    adj_rx_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_adj_rx_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_oversample_train_predictors2,
    adj_rx_na_omit_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

adj_rx_na_omit_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_adj_rx_oversample_train_predictors2$pre_operative_msu_result)
colnames(adj_rx_na_omit_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
adj_rx_na_omit_train_predictors_pre_operative_msu_result <-
  subset(adj_rx_na_omit_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_adj_rx_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_oversample_train_predictors2,
    adj_rx_na_omit_train_predictors_pre_operative_msu_result
  )

adj_rx_na_omit_train_predictors_puncture_site <-
  as_tibble(PCNL_adj_rx_oversample_train_predictors2$puncture_site)
colnames(adj_rx_na_omit_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
adj_rx_na_omit_train_predictors_puncture_site <-
  subset(adj_rx_na_omit_train_predictors_puncture_site,
         select = -puncture0)
PCNL_adj_rx_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_oversample_train_predictors2,
    adj_rx_na_omit_train_predictors_puncture_site
  )

adj_rx_na_omit_train_predictors_stone_dimensions <-
  as_tibble(PCNL_adj_rx_oversample_train_predictors2$stone_dimensions)
colnames(adj_rx_na_omit_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
adj_rx_na_omit_train_predictors_stone_dimensions <-
  subset(adj_rx_na_omit_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_adj_rx_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_oversample_train_predictors2,
    adj_rx_na_omit_train_predictors_stone_dimensions
  )

adj_rx_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_adj_rx_oversample_train_predictors2$image_guidance_for_renal_puncture)
colnames(adj_rx_na_omit_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
adj_rx_na_omit_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    adj_rx_na_omit_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_adj_rx_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_oversample_train_predictors2,
    adj_rx_na_omit_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_adj_rx_oversample_train_predictors2 <-
  subset(
    PCNL_adj_rx_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_adj_rx_oversample_train_predictors2 <-
  as.matrix(PCNL_adj_rx_oversample_train_predictors2)

##### Imputed Dataset
PCNL_adj_rx_imp_pre <-
  PCNL_adj_rx_omit_na %>% drop_na(adjuvant_treatment)
PCNL_adj_rx_imp1 <- mice(PCNL_adj_rx_imp_pre, m = 1)
summary(PCNL_adj_rx_imp1)

PCNL_adj_rx_imp <-
  mice::complete(PCNL_adj_rx_imp1, 1) %>% as_tibble()

PCNL_adj_rx_imp_sample <-
  sample(1:nrow(PCNL_adj_rx_imp), size = nrow(PCNL_adj_rx_imp) * 0.7)
PCNL_adj_rx_imp_train <-
  PCNL_adj_rx_imp[PCNL_adj_rx_imp_sample, ] %>% as_tibble()
PCNL_adj_rx_imp_test <-
  PCNL_adj_rx_imp[-PCNL_adj_rx_imp_sample, ] %>% as_tibble()
PCNL_adj_rx_imp_train$adjuvant_treatment <-
  as.factor(PCNL_adj_rx_imp_train$adjuvant_treatment)
PCNL_adj_rx_imp_test$adjuvant_treatment <-
  as.factor(PCNL_adj_rx_imp_test$adjuvant_treatment)
summary(PCNL_adj_rx_imp_train$adjuvant_treatment)

PCNL_adj_rx_imp_train_outcome <-
  subset(PCNL_adj_rx_imp_train,
         select = adjuvant_treatment)
PCNL_adj_rx_imp_train_outcome$adjuvant_treatment <-
  as.numeric(PCNL_adj_rx_imp_train_outcome$adjuvant_treatment)
PCNL_adj_rx_imp_train_outcome$adjuvant_treatment <-
  ifelse(PCNL_adj_rx_imp_train_outcome$adjuvant_treatment == 2,
         1,
         0)
PCNL_adj_rx_imp_train_outcome <-
  as.matrix(PCNL_adj_rx_imp_train_outcome)

PCNL_adj_rx_imp_train_predictors <-
  subset(PCNL_adj_rx_imp_train,
         select = -adjuvant_treatment)

PCNL_adj_rx_imp_train_predictors2 <-
  PCNL_adj_rx_imp_train_predictors
PCNL_adj_rx_imp_train_predictors2$stone_complexity <-
  as.integer(PCNL_adj_rx_imp_train_predictors2$stone_complexity)
PCNL_adj_rx_imp_train_predictors2$stone_complexity <-
  as.factor(PCNL_adj_rx_imp_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_adj_rx_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(PCNL_adj_rx_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
PCNL_adj_rx_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(PCNL_adj_rx_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath) %>% to_categorical()
PCNL_adj_rx_imp_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_adj_rx_imp_train_predictors2$pre_operative_msu_result)
PCNL_adj_rx_imp_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_adj_rx_imp_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_adj_rx_imp_train_predictors2$stone_dimensions <-
  as.integer(PCNL_adj_rx_imp_train_predictors2$stone_dimensions)
PCNL_adj_rx_imp_train_predictors2$stone_dimensions <-
  as.factor(PCNL_adj_rx_imp_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_adj_rx_imp_train_predictors2$puncture_site <-
  as.integer(PCNL_adj_rx_imp_train_predictors2$puncture_site)
PCNL_adj_rx_imp_train_predictors2$puncture_site <-
  as.factor(PCNL_adj_rx_imp_train_predictors2$puncture_site) %>% to_categorical()
PCNL_adj_rx_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_adj_rx_imp_train_predictors2$image_guidance_for_renal_puncture)
PCNL_adj_rx_imp_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_adj_rx_imp_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_adj_rx_imp_train_predictors2$renogramdmsa <-
  ifelse(PCNL_adj_rx_imp_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_adj_rx_imp_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_train_predictors2$calyceal_l <-
  ifelse(PCNL_adj_rx_imp_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_train_predictors2$calyceal_m <-
  ifelse(PCNL_adj_rx_imp_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_train_predictors2$complete_staghorn <-
  ifelse(PCNL_adj_rx_imp_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_train_predictors2$partial_staghorn <-
  ifelse(PCNL_adj_rx_imp_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_train_predictors2$pelvic <-
  ifelse(PCNL_adj_rx_imp_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_train_predictors2$upper_ureteric <-
  ifelse(PCNL_adj_rx_imp_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_train_predictors2$ureter_other <-
  ifelse(PCNL_adj_rx_imp_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



adj_rx_imp_train_predictors_stone_complexity <-
  as_tibble(PCNL_adj_rx_imp_train_predictors2$stone_complexity)
colnames(adj_rx_imp_train_predictors_stone_complexity) <- c("GSS0",
                                                            "GSSI",
                                                            "GSSII",
                                                            "GSSIII",
                                                            "GSSIV")
adj_rx_imp_train_predictors_stone_complexity <-
  subset(adj_rx_imp_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_adj_rx_imp_train_predictors2 <-
  cbind(PCNL_adj_rx_imp_train_predictors2,
        adj_rx_imp_train_predictors_stone_complexity)

adj_rx_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(PCNL_adj_rx_imp_train_predictors2$sizeouter_diameter_of_amplatz_sheath)
colnames(adj_rx_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
adj_rx_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(adj_rx_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath,
         select = -size0)
PCNL_adj_rx_imp_train_predictors2 <-
  cbind(
    PCNL_adj_rx_imp_train_predictors2,
    adj_rx_imp_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

adj_rx_imp_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_adj_rx_imp_train_predictors2$pre_operative_msu_result)
colnames(adj_rx_imp_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
adj_rx_imp_train_predictors_pre_operative_msu_result <-
  subset(adj_rx_imp_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_adj_rx_imp_train_predictors2 <-
  cbind(
    PCNL_adj_rx_imp_train_predictors2,
    adj_rx_imp_train_predictors_pre_operative_msu_result
  )

adj_rx_imp_train_predictors_puncture_site <-
  as_tibble(PCNL_adj_rx_imp_train_predictors2$puncture_site)
colnames(adj_rx_imp_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
adj_rx_imp_train_predictors_puncture_site <-
  subset(adj_rx_imp_train_predictors_puncture_site,
         select = -puncture0)
PCNL_adj_rx_imp_train_predictors2 <-
  cbind(PCNL_adj_rx_imp_train_predictors2,
        adj_rx_imp_train_predictors_puncture_site)

adj_rx_imp_train_predictors_stone_dimensions <-
  as_tibble(PCNL_adj_rx_imp_train_predictors2$stone_dimensions)
colnames(adj_rx_imp_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
adj_rx_imp_train_predictors_stone_dimensions <-
  subset(adj_rx_imp_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_adj_rx_imp_train_predictors2 <-
  cbind(PCNL_adj_rx_imp_train_predictors2,
        adj_rx_imp_train_predictors_stone_dimensions)

adj_rx_imp_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_adj_rx_imp_train_predictors2$image_guidance_for_renal_puncture)
colnames(adj_rx_imp_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
adj_rx_imp_train_predictors_image_guidance_for_renal_puncture <-
  subset(adj_rx_imp_train_predictors_image_guidance_for_renal_puncture,
         select = -image0)
PCNL_adj_rx_imp_train_predictors2 <-
  cbind(
    PCNL_adj_rx_imp_train_predictors2,
    adj_rx_imp_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_adj_rx_imp_train_predictors2 <-
  subset(
    PCNL_adj_rx_imp_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_adj_rx_imp_train_predictors2 <-
  as.matrix(PCNL_adj_rx_imp_train_predictors2)


##### Oversampling of Imputed Dataset
PCNL_adj_rx_imp_train_oversample <-
  ROSE::ovun.sample(
    adjuvant_treatment ~ .,
    data = PCNL_adj_rx_imp_train,
    seed = 1234,
    method = "over"
  )$data
summary(PCNL_adj_rx_imp_train_oversample$adjuvant_treatment)


PCNL_adj_rx_imp_oversample_train_outcome <-
  subset(PCNL_adj_rx_imp_train_oversample,
         select = adjuvant_treatment)
PCNL_adj_rx_imp_oversample_train_outcome$adjuvant_treatment <-
  as.numeric(PCNL_adj_rx_imp_oversample_train_outcome$adjuvant_treatment)
PCNL_adj_rx_imp_oversample_train_outcome$adjuvant_treatment <-
  ifelse(PCNL_adj_rx_imp_oversample_train_outcome$adjuvant_treatment == 2,
         1,
         0)
PCNL_adj_rx_imp_oversample_train_outcome <-
  as.matrix(PCNL_adj_rx_imp_oversample_train_outcome)

PCNL_adj_rx_imp_oversample_train_predictors <-
  subset(PCNL_adj_rx_imp_train_oversample,
         select = -adjuvant_treatment)

PCNL_adj_rx_imp_oversample_train_predictors2 <-
  PCNL_adj_rx_imp_oversample_train_predictors
PCNL_adj_rx_imp_oversample_train_predictors2$stone_complexity <-
  as.integer(PCNL_adj_rx_imp_oversample_train_predictors2$stone_complexity)
PCNL_adj_rx_imp_oversample_train_predictors2$stone_complexity <-
  as.factor(PCNL_adj_rx_imp_oversample_train_predictors2$stone_complexity) %>% to_categorical()
PCNL_adj_rx_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.integer(
    PCNL_adj_rx_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
PCNL_adj_rx_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath <-
  as.factor(
    PCNL_adj_rx_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  ) %>% to_categorical()
PCNL_adj_rx_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.integer(PCNL_adj_rx_imp_oversample_train_predictors2$pre_operative_msu_result)
PCNL_adj_rx_imp_oversample_train_predictors2$pre_operative_msu_result <-
  as.factor(PCNL_adj_rx_imp_oversample_train_predictors2$pre_operative_msu_result) %>% to_categorical()
PCNL_adj_rx_imp_oversample_train_predictors2$stone_dimensions <-
  as.integer(PCNL_adj_rx_imp_oversample_train_predictors2$stone_dimensions)
PCNL_adj_rx_imp_oversample_train_predictors2$stone_dimensions <-
  as.factor(PCNL_adj_rx_imp_oversample_train_predictors2$stone_dimensions) %>% to_categorical()
PCNL_adj_rx_imp_oversample_train_predictors2$puncture_site <-
  as.integer(PCNL_adj_rx_imp_oversample_train_predictors2$puncture_site)
PCNL_adj_rx_imp_oversample_train_predictors2$puncture_site <-
  as.factor(PCNL_adj_rx_imp_oversample_train_predictors2$puncture_site) %>% to_categorical()
PCNL_adj_rx_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.integer(PCNL_adj_rx_imp_oversample_train_predictors2$image_guidance_for_renal_puncture)
PCNL_adj_rx_imp_oversample_train_predictors2$image_guidance_for_renal_puncture <-
  as.factor(PCNL_adj_rx_imp_oversample_train_predictors2$image_guidance_for_renal_puncture) %>% to_categorical()
PCNL_adj_rx_imp_oversample_train_predictors2$renogramdmsa <-
  ifelse(PCNL_adj_rx_imp_oversample_train_predictors2$renogramdmsa == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_oversample_train_predictors2$calyceal_diverticular <-
  ifelse(PCNL_adj_rx_imp_oversample_train_predictors2$calyceal_diverticular == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_oversample_train_predictors2$calyceal_l <-
  ifelse(PCNL_adj_rx_imp_oversample_train_predictors2$calyceal_l == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_oversample_train_predictors2$calyceal_m <-
  ifelse(PCNL_adj_rx_imp_oversample_train_predictors2$calyceal_m == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_oversample_train_predictors2$complete_staghorn <-
  ifelse(PCNL_adj_rx_imp_oversample_train_predictors2$complete_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_oversample_train_predictors2$partial_staghorn <-
  ifelse(PCNL_adj_rx_imp_oversample_train_predictors2$partial_staghorn == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_oversample_train_predictors2$pelvic <-
  ifelse(PCNL_adj_rx_imp_oversample_train_predictors2$pelvic == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_oversample_train_predictors2$upper_ureteric <-
  ifelse(PCNL_adj_rx_imp_oversample_train_predictors2$upper_ureteric == "2",
         1,
         0) %>% as.numeric()
PCNL_adj_rx_imp_oversample_train_predictors2$ureter_other <-
  ifelse(PCNL_adj_rx_imp_oversample_train_predictors2$ureter_other == "2",
         1,
         0) %>% as.numeric()



adj_rx_imp_oversample_train_predictors_stone_complexity <-
  as_tibble(PCNL_adj_rx_imp_oversample_train_predictors2$stone_complexity)
colnames(adj_rx_imp_oversample_train_predictors_stone_complexity) <-
  c("GSS0",
    "GSSI",
    "GSSII",
    "GSSIII",
    "GSSIV")
adj_rx_imp_oversample_train_predictors_stone_complexity <-
  subset(adj_rx_imp_oversample_train_predictors_stone_complexity,
         select = -GSS0)
PCNL_adj_rx_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_imp_oversample_train_predictors2,
    adj_rx_imp_oversample_train_predictors_stone_complexity
  )

adj_rx_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  as_tibble(
    PCNL_adj_rx_imp_oversample_train_predictors2$sizeouter_diameter_of_amplatz_sheath
  )
colnames(adj_rx_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath) <-
  c("size0",
    "size1",
    "size2",
    "size3",
    "size4",
    "size5",
    "size6",
    "size7")
adj_rx_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath <-
  subset(
    adj_rx_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath,
    select = -size0
  )
PCNL_adj_rx_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_imp_oversample_train_predictors2,
    adj_rx_imp_oversample_train_predictors_sizeouter_diameter_of_amplatz_sheath
  )

adj_rx_imp_oversample_train_predictors_pre_operative_msu_result <-
  as_tibble(PCNL_adj_rx_imp_oversample_train_predictors2$pre_operative_msu_result)
colnames(adj_rx_imp_oversample_train_predictors_pre_operative_msu_result) <-
  c("msu0",
    "msu1",
    "msu2",
    "msu3",
    "msu4",
    "msu5",
    "msu6")
adj_rx_imp_oversample_train_predictors_pre_operative_msu_result <-
  subset(adj_rx_imp_oversample_train_predictors_pre_operative_msu_result,
         select = -msu0)
PCNL_adj_rx_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_imp_oversample_train_predictors2,
    adj_rx_imp_oversample_train_predictors_pre_operative_msu_result
  )

adj_rx_imp_oversample_train_predictors_puncture_site <-
  as_tibble(PCNL_adj_rx_imp_oversample_train_predictors2$puncture_site)
colnames(adj_rx_imp_oversample_train_predictors_puncture_site) <-
  c("puncture0",
    "puncture1",
    "puncture2",
    "puncture3")
adj_rx_imp_oversample_train_predictors_puncture_site <-
  subset(adj_rx_imp_oversample_train_predictors_puncture_site,
         select = -puncture0)
PCNL_adj_rx_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_imp_oversample_train_predictors2,
    adj_rx_imp_oversample_train_predictors_puncture_site
  )

adj_rx_imp_oversample_train_predictors_stone_dimensions <-
  as_tibble(PCNL_adj_rx_imp_oversample_train_predictors2$stone_dimensions)
colnames(adj_rx_imp_oversample_train_predictors_stone_dimensions) <-
  c(
    "stone_size0",
    "stone_size1",
    "stone_size2",
    "stone_size3",
    "stone_size4",
    "stone_size5"
  )
adj_rx_imp_oversample_train_predictors_stone_dimensions <-
  subset(adj_rx_imp_oversample_train_predictors_stone_dimensions,
         select = -stone_size0)
PCNL_adj_rx_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_imp_oversample_train_predictors2,
    adj_rx_imp_oversample_train_predictors_stone_dimensions
  )

adj_rx_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  as_tibble(PCNL_adj_rx_imp_oversample_train_predictors2$image_guidance_for_renal_puncture)
colnames(adj_rx_imp_oversample_train_predictors_image_guidance_for_renal_puncture) <-
  c("image0",
    "image1",
    "image2",
    "image3",
    "image4")
adj_rx_imp_oversample_train_predictors_image_guidance_for_renal_puncture <-
  subset(
    adj_rx_imp_oversample_train_predictors_image_guidance_for_renal_puncture,
    select = -image0
  )
PCNL_adj_rx_imp_oversample_train_predictors2 <-
  cbind(
    PCNL_adj_rx_imp_oversample_train_predictors2,
    adj_rx_imp_oversample_train_predictors_image_guidance_for_renal_puncture
  )

PCNL_adj_rx_imp_oversample_train_predictors2 <-
  subset(
    PCNL_adj_rx_imp_oversample_train_predictors2,
    select = -c(
      stone_complexity,
      sizeouter_diameter_of_amplatz_sheath,
      pre_operative_msu_result,
      puncture_site,
      stone_dimensions,
      image_guidance_for_renal_puncture
    )
  )
PCNL_adj_rx_imp_oversample_train_predictors2 <-
  as.matrix(PCNL_adj_rx_imp_oversample_train_predictors2)


