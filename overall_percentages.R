library(tidyverse)
PCNL_new3 <- read.csv("PCNL_truncated.csv", na = c("", " ")) %>% as_tibble() %>% subset(select = -c(X))
PCNL_new3$age<-as.integer(PCNL_new3$age)
PCNL_new3$gender<-as.factor(PCNL_new3$gender)
PCNL_new3$side_of_stones<-as.factor(PCNL_new3$side_of_stones)
PCNL_new3$bmi<-as.numeric(PCNL_new3$bmi)
PCNL_new3$charlson_comorbidities<-as.factor(PCNL_new3$charlson_comorbidities)
PCNL_new3$charlson_score<-factor(PCNL_new3$charlson_score, 
                                 ordered=TRUE,
                                 levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, NA))
PCNL_new3$previous_urinary_tract_infection<-as.factor(PCNL_new3$previous_urinary_tract_infection)
PCNL_new3$pre_operative_antibiotic_course<-as.factor(PCNL_new3$pre_operative_antibiotic_course)
PCNL_new3$pre_operative_msu<-as.factor(PCNL_new3$pre_operative_msu_result)
PCNL_new3$pre_operative_msu_result<-as.factor(PCNL_new3$pre_operative_msu_result)
PCNL_new3$pre_op_egfr<-as.factor(PCNL_new3$pre_op_egfr)
PCNL_new3$stone_dimensions<-as.factor(PCNL_new3$stone_dimensions)
PCNL_new3$number_of_stones<-as.factor(PCNL_new3$number_of_stones)
PCNL_new3$index_stone_location<-as.factor(PCNL_new3$index_stone_location)
PCNL_new3$other_stone_location<-as.factor(PCNL_new3$other_stone_location)
PCNL_new3$stone_complexity<-as.factor(PCNL_new3$stone_complexity)
PCNL_new3$maximum_hounsfield_units_of_the_index_stone_on_ctkub<-as.factor(PCNL_new3$maximum_hounsfield_units_of_the_index_stone_on_ctkub)
PCNL_new3$pre_existing_nephrostomy_tube<-as.factor(PCNL_new3$pre_existing_nephrostomy_tube)
PCNL_new3$patient_position<-as.factor(PCNL_new3$patient_position)
PCNL_new3$predicted_difficulty<-as.factor(PCNL_new3$predicted_difficulty)

PCNL_new3$complete_clearance_on_fluoroscopy <- as.factor(PCNL_new3$complete_clearance_on_fluoroscopy)
PCNL_new3$visceral_injury<-as.factor(PCNL_new3$visceral_injury)
PCNL_new3$itu_hdu_admission <- as.factor(PCNL_new3$itu_hdu_admission)
PCNL_new3$clearance_on_post_operative_radiological_imaging_during_a <- as.factor(PCNL_new3$clearance_on_post_operative_radiological_imaging_during_a)
PCNL_new3$patient_statusdischarge <- as.factor(PCNL_new3$patient_statusdischarge)
PCNL_new3$postop_complications <- as.factor(PCNL_new3$postop_complications)
PCNL_new3$clavien_dindo_grade_of_complications <- as.factor(PCNL_new3$clavien_dindo_grade_of_complications)
PCNL_new3$blood_transfusion <- as.factor(PCNL_new3$blood_transfusion)
PCNL_new3$post_operative_infection <-as.factor(PCNL_new3$post_operative_infection)
PCNL_new3$post_operative_stay <- as.integer(PCNL_new3$post_operative_stay)
PCNL_new3$stone_free_at_follow_up <-as.factor(PCNL_new3$stone_free_at_follow_up)
PCNL_new3$adjuvant_treatment <-as.factor(PCNL_new3$adjuvant_treatment)
PCNL_new3$intraop_complications <-as.factor(PCNL_new3$intraop_complications)
PCNL_new3$age_group <-factor(PCNL_new3$age_group,
                             levels = c("10-19",
                                        "20-29",
                                        "30-39",
                                        "40-49",
                                        "50-59",
                                        "60-69",
                                        "70-79",
                                        "80-89",
                                        "90-100"
                             ))

complete_clearance_on_fluoroscopy_x <-
  PCNL_new3 %>% group_by('x' = complete_clearance_on_fluoroscopy == "Yes") %>% tally() %>% drop_na()
complete_clearance_on_fluoroscopy_y <-
  ifelse(
    complete_clearance_on_fluoroscopy_x$x,
    TRUE,
    NA
  ) %>% cbind(complete_clearance_on_fluoroscopy_x) %>% drop_na()


clearance_x <-
  PCNL_new3 %>% group_by("x" = clearance_on_post_operative_radiological_imaging_during_a == "Yes") %>% tally() %>% drop_na()
clearance_y <-
  ifelse(clearance_x$x,
         TRUE,
         NA) %>% cbind(clearance_x) %>% drop_na()

sf_x <-
  PCNL_new3 %>% group_by("x" = stone_free_at_follow_up == "Yes") %>% tally() %>% drop_na()
sf_y <-
  ifelse(sf_x$x,
         TRUE,
         NA) %>% cbind(sf_x) %>% drop_na()

adjuvant_x <-
  PCNL_new3 %>% group_by("x" = adjuvant_treatment == "Yes") %>% tally() %>% drop_na()
adjuvant_y <-
  ifelse(adjuvant_x$x,
         TRUE,
         NA) %>% cbind(adjuvant_x) %>% drop_na()

infection_x1 <-
  PCNL_new3 %>% group_by("x" = post_operative_infection == "Fever") %>% tally() %>% drop_na()
infection_x2 <-
  PCNL_new3 %>% group_by("x" = post_operative_infection == "SIRS - sepsis") %>% tally() %>% drop_na()
infection_x3 <-
  PCNL_new3 %>% group_by("x" = post_operative_infection == "Fever,SIRS - sepsis") %>% tally() %>% drop_na()
infection_x <-
  rbind(infection_x1,
        infection_x2,
        infection_x3)

infection_y <-
  ifelse(infection_x$x,
         TRUE,
         NA) %>% cbind(infection_x) %>% drop_na()

sepsis_x1 <-
  PCNL_new3 %>% group_by("x" = post_operative_infection == "SIRS - sepsis") %>% tally() %>% drop_na()
sepsis_x2 <-
  PCNL_new3 %>% group_by("x" = post_operative_infection == "Fever,SIRS - sepsis") %>% tally() %>% drop_na()
sepsis_x <-
  rbind(sepsis_x1,
        sepsis_x2)

sepsis_y <-
  ifelse(sepsis_x$x,
         TRUE,
         NA) %>% cbind(sepsis_x) %>% drop_na()

itu_hdu_x <-
  PCNL_new3 %>% group_by("x" = itu_hdu_admission == "Yes") %>% tally() %>% drop_na()
itu_hdu_y <-
  ifelse(itu_hdu_x$x,
         TRUE,
         NA) %>% cbind(itu_hdu_x) %>% drop_na()

transfusion_x <-
  PCNL_new3 %>% group_by("x" = blood_transfusion == "Yes") %>% tally() %>% drop_na()
transfusion_y <-
  ifelse(transfusion_x$x,
         TRUE,
         NA) %>% cbind(transfusion_x) %>% drop_na()

visceral_injury_x <-
  PCNL_new3 %>% group_by("x" = visceral_injury == "Yes") %>% tally() %>% drop_na()
visceral_injury_y <-
  ifelse(visceral_injury_x$x,
         TRUE,
         NA) %>% cbind(visceral_injury_x) %>% drop_na()

postop_complications_x <-
  PCNL_new3 %>% group_by("x" = postop_complications == "Yes") %>% tally() %>% drop_na()
postop_complications_y <-
  ifelse(postop_complications_x$x,
         TRUE,
         NA) %>% cbind(postop_complications_x) %>% drop_na()

I_x <- PCNL_new3 %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade I") %>% tally() %>% drop_na()
I_y <- ifelse(I_x$x,
              TRUE,
              NA) %>% cbind(I_x) %>% drop_na()

II_x <- PCNL_new3 %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade II") %>% tally() %>% drop_na()
II_y <- ifelse(II_x$x,
              TRUE,
              NA) %>% cbind(II_x) %>% drop_na()

IIIa_x <- PCNL_new3 %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IIIa") %>% tally() %>% drop_na()
IIIa_y <- ifelse(IIIa_x$x,
              TRUE,
              NA) %>% cbind(IIIa_x) %>% drop_na()

IIIb_x <- PCNL_new3 %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IIIb") %>% tally() %>% drop_na()
IIIb_y <- ifelse(IIIb_x$x,
              TRUE,
              NA) %>% cbind(IIIb_x) %>% drop_na()

IVa_x <- PCNL_new3 %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IVa") %>% tally() %>% drop_na()
IVa_y <- ifelse(IVa_x$x,
              TRUE,
              NA) %>% cbind(IVa_x) %>% drop_na()

IVb_x <- PCNL_new3 %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IVb") %>% tally() %>% drop_na()
IVb_y <- ifelse(IVb_x$x,
              TRUE,
              NA) %>% cbind(IVb_x) %>% drop_na()

V_x <- PCNL_new3 %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade V") %>% tally() %>% drop_na()
V_y <- ifelse(V_x$x,
              TRUE,
              NA) %>% cbind(V_x) %>% drop_na()

death_x <- PCNL_new3 %>% group_by("x" = patient_statusdischarge == "Died in hospital") %>% tally() %>% drop_na()
death_y <- ifelse(death_x$x,
              TRUE,
              NA) %>% cbind(death_x) %>% drop_na()


z <- cbind("Outcome" = c("Complete Clearance on Fluoroscopy",
                         "Clearance on Post-Operative Imaging",
                         "Stone Free at Follow-up",
                         "Adjuvant treatment",
                         "Post-operative Infection",
                         "Post-operative Sepsis",
                         "ITU/HDU Admission",
                         "Blood Transfusion",
                         "Visceral Injury",
                         "Overall Post-Operative Complications",
                         "Grade I",
                         "Grade II",
                         "Grade IIIa",
                         "Grade IIIb",
                         "Grade IVa",
                         "Grade IVb",
                         "Grade V",
                         "Death"),
           "Overall Dataset (%)" = round(c(
            (complete_clearance_on_fluoroscopy_y$n / sum(complete_clearance_on_fluoroscopy_x$n) * 100),
            (clearance_y$n / sum(clearance_x$n) * 100),
            (sf_y$n / sum(sf_x$n) * 100),
            (adjuvant_y$n / sum(adjuvant_x$n) * 100),
            (sum(infection_y$n) / sum(infection_x1$n) * 100),
            (sum(sepsis_y$n) / sum(sepsis_x1$n) * 100),
            (itu_hdu_y$n / sum(itu_hdu_x$n) * 100),
            (transfusion_y$n / sum(transfusion_x$n) * 100),
            (visceral_injury_y$n / sum(visceral_injury_x$n) * 100),
            (postop_complications_y$n / sum(postop_complications_x$n) * 100),
            (I_y$n / sum(I_x$n) * 100),
            (II_y$n / sum(II_x$n) * 100),
            (IIIa_y$n / sum(IIIa_x$n) * 100),
            (IIIb_y$n / sum(IIIb_x$n) * 100),
            (IVa_y$n / sum(IVa_x$n) * 100),
            (IVb_y$n / sum(IVb_x$n) * 100),
            (V_y$n / sum(V_x$n) * 100),
            (death_y$n / sum(death_x$n) * 100)
           )
           , digits = 1)
           ) %>% as_tibble()