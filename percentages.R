library(tidyverse)


complete_clearance_on_fluoroscopy_xa <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble()  %>% group_by('x' = complete_clearance_on_fluoroscopy == "Yes") %>% tally() })
complete_clearance_on_fluoroscopy_ya <- reactive({complete_clearance_on_fluoroscopy_xa() %>% ifelse(
    TRUE,
    NA
  ) %>% cbind(complete_clearance_on_fluoroscopy_xa()) %>% drop_na() })


clearance_xa <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = clearance_on_post_operative_radiological_imaging_during_a == "Yes") %>% tally() %>% drop_na() })
clearance_ya<- reactive({clearance_xa() %>% ifelse(
         TRUE,
         NA) %>% cbind(clearance_xa()) %>% drop_na() })

sf_xa <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = stone_free_at_follow_up == "Yes") %>% tally() %>% drop_na() })
sf_ya <- reactive({sf_xa() %>% ifelse(
         TRUE,
         NA) %>% cbind(sf_xa()) %>% drop_na() })

adjuvant_xa <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = adjuvant_treatment == "Yes") %>% tally() %>% drop_na() })
adjuvant_ya <- reactive({adjuvant_xa() %>% ifelse(
         TRUE,
         NA) %>% cbind(adjuvant_xa()) %>% drop_na() })

infection_x1a <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = post_operative_infection == "Fever") %>% tally() %>% drop_na() })
infection_x2a <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = post_operative_infection == "SIRS - sepsis") %>% tally() %>% drop_na() })
infection_x3a <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = post_operative_infection == "Fever,SIRS - sepsis") %>% tally() %>% drop_na() })
infection_xa <- reactive({
  rbind(infection_x1a(),
        infection_x2a(),
        infection_x3a()) }) %>% as_tibble()

infection_ya <- reactive({ infection_xa() %>%
  ifelse(
         TRUE,
         NA) %>% cbind(infection_xa()) %>% drop_na() })

sepsis_x1a <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = post_operative_infection == "SIRS - sepsis") %>% tally() %>% drop_na() })
sepsis_x2a <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = post_operative_infection == "Fever,SIRS - sepsis") %>% tally() %>% drop_na() })
sepsis_xa <- reactive({
  rbind(sepsis_x1a(),
        sepsis_x2a()) })

sepsis_ya <- reactive({sepsis_xa() %>% ifelse(
         TRUE,
         NA) %>% cbind(sepsis_xa()) %>% drop_na() })

itu_hdu_xa <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = itu_hdu_admission == "Yes") %>% tally() %>% drop_na() })
itu_hdu_ya <- reactive({itu_hdu_xa() <-
  ifelse(
         TRUE,
         NA) %>% cbind(itu_hdu_xa()) %>% drop_na()})

transfusion_xa <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = blood_transfusion == "Yes") %>% tally() %>% drop_na() })
transfusion_ya <- reactive({transfusion_xa() %>% ifelse(
         TRUE,
         NA) %>% cbind(transfusion_xa()) %>% drop_na()}) 

visceral_injury_xa <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = visceral_injury == "Yes") %>% tally() %>% drop_na() })
visceral_injury_ya<-reactive({visceral_injury_xa() %>%  ifelse(
         TRUE,
         NA) %>% cbind(visceral_injury_xa()) %>% drop_na()})

postop_complications_xa <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = postop_complications == "Yes") %>% tally() %>% drop_na() })
postop_complications_ya<-reactive({postop_complications_xa() %>% ifelse(
         TRUE,
         NA) %>% cbind(postop_complications_xa()) %>% drop_na()}) 

I_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade I") %>% tally() %>% drop_na() })
I_ya<-reactive({I_xa %>% ifelse(
              TRUE,
              NA) %>% cbind(I_xa()) %>% drop_na()})

II_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade II") %>% tally() %>% drop_na() })
II_ya <- reactive({II_xa() %>% ifelse(
              TRUE,
              NA) %>% cbind(II_xa()) %>% drop_na()})

IIIa_xa <- reactive({
  selection <- PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IIIa") %>% tally() %>% drop_na() })
IIIa_ya<-reactive({IIIa_xa() %>% ifelse(
              TRUE,
              NA) %>% cbind(IIIa_xa()) %>% drop_na()})

IIIb_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IIIb") %>% tally() %>% drop_na() })
IIIb_ya<-reactive({IIIb_xa() %>% ifelse(
              TRUE,
              NA) %>% cbind(IIIb_xa()) %>% drop_na()})

IVa_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble()  %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IVa") %>% tally() %>% drop_na() })
IVa_ya<-reactive({IVa_xa() %>% ifelse(
              TRUE,
              NA) %>% cbind(IVa_xa()) %>% drop_na()})

IVb_xa <- reactive({
  PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble()  %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IVb") %>% tally() %>% drop_na() })
IVb_ya<-reactive({IVb_xa() %>% ifelse(
              TRUE,
              NA) %>% cbind(IVb_xa()) %>% drop_na()})

V_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade V") %>% tally() %>% drop_na() })
V_ya<-reactive({V_xa %>% ifelse(
              TRUE,
              NA) %>% cbind(V_xa()) %>% drop_na()})

death_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = patient_statusdischarge == "Died in hospital") %>% tally() %>% drop_na() })
death_ya<-reactive({death_xa() %>% ifelse(
              TRUE,
              NA) %>% cbind(death_xa()) %>% drop_na()})