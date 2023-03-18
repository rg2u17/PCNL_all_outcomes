library(tidyverse)


complete_clearance_on_fluoroscopy_xa <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble()  %>% group_by('x' = complete_clearance_on_fluoroscopy == "Yes") %>% tally() %>% subset(x == TRUE) %>% subset(select = n) })

clearance_xa <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = clearance_on_post_operative_radiological_imaging_during_a == "Yes") %>% tally() %>% subset(x == TRUE) %>% subset(select = n) })

sf_xa <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = stone_free_at_follow_up == "Yes") %>% tally() %>% subset(x == TRUE) %>% subset(select = n) })

adjuvant_xa <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = adjuvant_treatment == "Yes") %>% tally() %>% subset(x == FALSE) %>% subset(select = n)})


infection_x1a <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = post_operative_infection == "Fever") %>% tally() %>% subset(x == TRUE) %>% subset(select = n) })
infection_x2a <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = post_operative_infection == "SIRS - sepsis") %>% tally() %>% subset(x == TRUE) %>% subset(select = n) })
infection_x3a <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = post_operative_infection == "Fever,SIRS - sepsis") %>% tally() %>% subset(x == TRUE) %>% subset(select = n) })
infection_xa <- cbind(
  "n" = sum(
  (infection_x1a()),
  (infection_x2a()),
  (infection_x3a())
),
"value" = 1
) %>% as_tibble() %>% subset(select = n)


sepsis_x1a <-
  reactive({
    PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = post_operative_infection == "SIRS - sepsis") %>% tally() %>% subset(x == TRUE) %>% subset(select = n) })
sepsis_x2a <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = post_operative_infection == "Fever,SIRS - sepsis") %>% tally() %>% subset(x == TRUE) %>% subset(select = n) })
sepsis_xa <- cbind(
  "n" = sum((sepsis_x1a() %>% subset(select = n)),
                 (sepsis_x2a() %>% subset(select = n))),
  "value" = 1
  ) %>% as_tibble() %>% subset(select = n)

itu_hdu_xa <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = itu_hdu_admission == "Yes") %>% tally() %>% subset(x == FALSE) %>% subset(select = n) })

transfusion_xa <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = blood_transfusion == "Yes") %>% tally() %>% subset(x == FALSE) %>% subset(select = n) })

visceral_injury_xa <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = visceral_injury == "Yes") %>% tally() %>% subset(x == FALSE) %>% subset(select = n)  })

postop_complications_xa <-
  reactive({
     PCNL_new3 %>% filter(
      age_group %in% input$age_group,
      gender %in% input$gender,
      charlson_score %in% input$charlson_score,
      stone_complexity %in% input$stone_complexity
    ) %>% as_tibble() %>% group_by("x" = postop_complications == "Yes") %>% tally()  %>% subset(x == FALSE) %>% subset(select = n) })

I_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade I") %>% tally()  %>% subset(x == FALSE)  %>% subset(select = n)})

II_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade II") %>% tally()  %>% subset(x == FALSE) %>% subset(select = n) })

IIIa_xa <- reactive({
  selection <- PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IIIa") %>% tally()  %>% subset(x == FALSE) %>% subset(select = n) })

IIIb_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IIIb") %>% tally() %>% subset(x == FALSE) %>% subset(select = n) })

IVa_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble()  %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IVa") %>% tally()  %>% subset(x == FALSE) %>% subset(select = n) })

IVb_xa <- reactive({
  PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble()  %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade IVb") %>% tally()  %>% subset(x == FALSE) %>% subset(select = n) })

V_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = clavien_dindo_grade_of_complications == "Grade V") %>% tally() %>% subset(x == FALSE) %>% subset(select = n) })

death_xa <- reactive({
   PCNL_new3 %>% filter(
    age_group %in% input$age_group,
    gender %in% input$gender,
    charlson_score %in% input$charlson_score,
    stone_complexity %in% input$stone_complexity
  ) %>% as_tibble() %>% group_by("x" = patient_statusdischarge == "Died in hospital") %>% tally()  %>% subset(x == FALSE) %>% subset(select = n) })