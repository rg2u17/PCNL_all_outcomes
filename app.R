library(shiny)
library(tidyverse)
library(gt)
library(gtExtras)
library(xgboost)
library(shapr)

PCNL_new3<-read.csv("PCNL_truncated.csv", na = c("", " ")) %>% as_tibble() %>% subset(select = -c(X))
itu_hdu_model<-readRDS("itu_hdu_model.rds")
itu_hdu_explainer<-readRDS("itu_hdu_explainer.rds")
infection_model<-readRDS("infection_model.rds")
infection_explainer<-readRDS("infection_explainer.rds")
post_op_comp_model<-readRDS("post_op_comp_model.rds")
post_op_comp_explainer<-readRDS("post_op_comp_explainer.rds")
sf_at_fu_model<-readRDS("sf_at_fu_model.rds")
sf_at_fu_explainer<-readRDS("sf_at_fu_explainer.rds")
transfusion_model<-readRDS("transfusion_model.rds")
transfusion_explainer<-readRDS("transfusion_explainer.rds")
visc_inj_model<-readRDS("visc_inj_model.rds")
visc_inj_explainer<-readRDS("visc_inj_explainer.rds")
adj_rx_model<-readRDS("adj_rx_model.rds")
adj_rx_explainer<-readRDS("adj_rx_explainer.rds")

source("overall_percentages.R", local = TRUE)
source("charlson_table.R", local = TRUE)

PCNL_new3$age<-as.integer(PCNL_new3$age)
PCNL_new3$gender<-as.factor(PCNL_new3$gender)
PCNL_new3$side_of_stones<-as.factor(PCNL_new3$side_of_stones)
PCNL_new3$bmi<-as.numeric(PCNL_new3$bmi)
PCNL_new3$charlson_comorbidities<-as.factor(PCNL_new3$charlson_comorbidities)
PCNL_new3$charlson_score<-factor(PCNL_new3$charlson_score, 
                                 ordered=TRUE,
                                 levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, NA))
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




# Define UI 
ui <- fluidPage(
  titlePanel("BAUS PCNL Outcome Prediction Tool"),
  tabsetPanel(
  tabPanel(title = "Disclaimer",
           tags$img(src='logo.png', align = "center", alt = " "),
           h2("Disclaimer"),
           "This Dashboard is designed for research purposes only",
           h3("Key:"),
           h4("Charlson Comorbidity index:"),
           h6("Score is cumulative"),
           gt_output(outputId = "charlson_table"),
           h6("Reference: Yang, H., Chen, Y.H., Hsieh, T.F., Chuang, S.Y. and Wu, M.J., 2016. Prediction of mortality in incident hemodialysis patients: a validation and comparison of CHADS2, CHA2DS2, and CCI scores. PLoS One, 11(5), p.e0154627."),
           h6(""),
           h4("Guy's Stone Score"),
           tags$img(src = "guys_stone_score.jpeg"),
           h6("Reference: Thomas K, Smith NC, Hegarty N, Glass JM. The Guy's stone score—grading the complexity of percutaneous nephrolithotomy procedures. Urology. 2011 Aug 1;78(2):277-81.")
           ),
  
  tabPanel(title = "Demographics",
      sidebarLayout(
        sidebarPanel(
             h2("Input Parameters"),
             h5("Please refer to Disclaimer tab for further explanation on Charlson score and Guy's Stone score"),
             fluidRow(
               selectInput(
                 inputId = "age_group",
                 label = "Age Group",
                 choices = levels(PCNL_new3$age_group),
                 selected = "50-59"
               ),
               selectInput(
                 inputId = "gender",
                 label = "Gender",
                 choices = levels(PCNL_new3$gender)
               ),
               selectInput(
                 inputId = "charlson_score",
                 label = "Charlson Score",
                 choices = levels(PCNL_new3$charlson_score)
               ),
               selectInput(
                 inputId = "stone_complexity",
                 label = "Guy's Stone Score",
                 choices = levels(PCNL_new3$stone_complexity)
               ),
  gt_output(outputId = "explanation_table")
           )),
  mainPanel(
    tags$img(src='logo.png', align = "center"),
    h2("Summary Table"),
    p(
      "These summary statistics based on inputted demographics, using the BAUS PCNL dataset (n=12,810)"
    ),
    p(),
    gt_output(outputId = "table1")
  )
)
),
### ML Predictions
tabPanel(title = "ML Predictions",
         sidebarLayout(
           sidebarPanel(
             h2("Input Variables"),
             fluidRow(
               sliderInput(
                 inputId = "age",
                 label = "Age",
                 min = 0,
                 max = 100,
                 value = 55
               ),
               sliderInput(
                 inputId = "charlson_score1",
                 label = "Charlson Score",
                 min = 0,
                 max = 15,
                 value = 0
               ),
               sliderInput(
                 inputId = "pre_op_haemoglobin",
                 label = "Pre-Operative Haemoglobin (g/L)",
                 min = 50,
                 max = 200,
                 value = 120
               ),
                 selectInput(
                   inputId = "stone_complexity1",
                   label = "Guy's Stone Score",
                   choices = c("I","II","III","IV"),
                   selected = "II"
                 ),                    
               selectInput(
                 inputId = "calyceal_diverticular",
                 label = "Location: Calyceal Diverticulum",
                 choices = c("Yes","No"),
                 selected = "Yes"
               ),                
               selectInput(
                 inputId = "calyceal_l",
                 label = "Location: Lower Pole Calyx",
                 choices = c("Yes","No"),
                 selected = "No"
               ),                
               selectInput(
                 inputId = "calyceal_m",
                 label = "Location: Equatorial/Upper Calyx",
                 choices = c("Yes","No"),
                 selected = "No"
               ),            
               selectInput(
                 inputId = "complete_staghorn",
                 label = "Location: Complete Staghorn",
                 choices = c("Yes","No"),
                 selected = "No"
               ),                       
               selectInput(
                 inputId = "partial_staghorn",
                 label = "Location: Partial Staghorn",
                 choices = c("Yes","No"),
                 selected = "No"
               ),                         
               selectInput(
                 inputId = "pelvic",
                 label = "Location: Renal Pelvis",
                 choices = c("Yes","No"),
                 selected = "No"
               ),                    
               selectInput(
                 inputId = "ureter_other",
                 label = "Location: Mid/Distal Ureter",
                 choices = c("Yes","No"),
                 selected = "No"
               ),  
               selectInput(
                 inputId = "upper_ureteric",
                 label = "Location: Proximal Ureter",
                 choices = c("Yes","No"),
                 selected = "No"
               ), 
               selectInput(
                 inputId = "sizeouter_diameter_of_amplatz_sheath",
                 label = "Amplatz Sheath Diameter",
                 choices = c("<6 Fr",
                             "11-14 Fr",
                             "15-18 Fr",
                             "19-24 Fr",
                             "25-26 Fr",
                             "27-30 Fr",
                             "6-10 Fr"),
                 selected = "27-30 Fr"
               ),                              
               selectInput(
                 inputId = "pre_operative_msu_result",
                 label = "Pre-Operative MSU Result",
                 choices = c("Heavy mixed growth treated pre-operatively",
                             "Heavy mixed growth untreated",
                             "No growth",
                             "Not Done",
                             "UTI treated pre-operatively",
                             "UTI untreated"),
                 selected = "No growth"
               ),                      
               selectInput(
                 inputId = "puncture_site",
                 label = "Primary Puncture Site",
                 choices = c("Interpolar",
                             "Lower Pole",
                             "Upper Pole")
               ),                       
               selectInput(
                 inputId = "renogramdmsa",
                 label = "Pre-Operative DMSA",
                 choices = c("Yes",
                             "No")
               ), 
               selectInput(
                 inputId = "stone_dimensions",
                 label = "Stone Size (cm)",
                 choices = c(">2 cm",
                             ">4 cm",
                             "0-1 cm",
                             "1-2 cm",
                             "2-4 cm" )
               ), 
               selectInput(
                 inputId = "image_guidance_for_renal_puncture",
                 label = "Image Guidance for Puncture",
                 choices = c("CT",
                             "Fluoroscopy & ultrasound",
                             "Fluoroscopy",
                             "Ultrasound"),
                 selected = "Fluoroscopy"
               ), 
             )
           ),
           mainPanel(tags$img(src='logo.png', align = "center"),
                     h2("Summary Table for Outcome Prediction"),
                     gt_output(outputId = "summary_table"),
                     h5("For further explanatory graphs demonstrating why the models are predicting these outcomes please continue to the ML Explanations tab, where you will find graphs of the Shapley weights ranked by influence on prediction")
                     )
         )
         ),
### ML Explanations
tabPanel(title = "ML Explanations",
         navlistPanel(
           tabPanel(title = "Stone Free at Follow-up",
           plotOutput(outputId = "sf_at_fu_shapley"),
           h4("pred = predicted likelihood of outcome according to model")),
tabPanel(title = "Infection",
         plotOutput(outputId = "infection_shapley"),
         h4("pred = predicted likelihood of outcome according to model")),
tabPanel(title = "Transfusion",
         plotOutput(outputId = "transfusion_shapley"),
         h4("pred = predicted likelihood of outcome according to model")),
tabPanel(title = "ITU/HDU Admission",
         plotOutput(outputId = "itu_hdu_shapley"),
         h4("pred = predicted likelihood of outcome according to model")),
tabPanel(title = "Visceral Injury",
         plotOutput(outputId = "visc_inj_shapley"),
         h4("pred = predicted likelihood of outcome according to model")),
tabPanel(title = "Post-Operative Complications",
         plotOutput(outputId = "post_op_comp_shapley"),
         h4("pred = predicted likelihood of outcome according to model")),
tabPanel(title = "Need for Adjuvant Treatment",
         plotOutput(outputId = "adj_rx_shapley"),
         h4("pred = predicted likelihood of outcome according to model"))
)
)
)
)

# Define server logic
server <- function(input, output) {
  
 ## Get data for Demographics table
  getData <- reactive({
    selection <- PCNL_new3 %>% group_by(age_group,
                                        gender,
                                        charlson_score,
                                        stone_complexity) %>% filter(
                                          age_group %in% input$age_group,
                                          gender %in% input$gender,
                                          charlson_score %in% input$charlson_score,
                                          stone_complexity %in% input$stone_complexity
                                        ) %>% as_tibble() %>% subset(
                                          select = c(
                                            complete_clearance_on_fluoroscopy,
                                            visceral_injury,
                                            itu_hdu_admission,
                                            clearance_on_post_operative_radiological_imaging_during_a,
                                            patient_statusdischarge,
                                            postop_complications,
                                            clavien_dindo_grade_of_complications,
                                            blood_transfusion,
                                            post_operative_infection,
                                            post_operative_stay,
                                            stone_free_at_follow_up,
                                            adjuvant_treatment
                                          ) 
                                        ) 
  })
  
  ## Get Data for ML Predictions  
  
  getData1 <- reactive({
    
    selection1 <-
      cbind(
        "age" = as.numeric(input$age),
        "charlson_score" = as.numeric(input$charlson_score1),
        "pre_op_haemoglobin" = as.numeric(input$pre_op_haemoglobin),
        "stone_complexity" = (if (input$stone_complexity1 == "I") {
          print(1)
        } else if(input$stone_complexity1 == "II") {
          print(2)
        } else if(input$stone_complexity1 == "III") {
          print(3)
        }
        else {print(4)}),
        "calyceal_diverticular" = (if (input$calyceal_diverticular == "Yes") {
          print(2)
        } else {
          print(1)
        }),
        "calyceal_l" = (if (input$calyceal_l == "Yes") {
          print(2)
        } else {
          print(1)
        }),
        "calyceal_m" = (if (input$calyceal_m == "Yes") {
          print(2)
        } else {
          print(1)
        }),
        "complete_staghorn" = (if (input$complete_staghorn == "Yes") {
          print(2)
        } else {
          print(1)
        }),
        "partial_staghorn" = (if (input$partial_staghorn == "Yes") {
          print(2)
        } else {
          print(1)
        }),
        "pelvic" = (if (input$pelvic == "Yes") {
          print(2)
        } else {
          print(1)
        }),
        "ureter_other" = (if (input$ureter_other == "Yes") {
          print(2)
        } else {
          print(1)
        }),
        "upper_ureteric" = (if (input$upper_ureteric == "Yes") {
          print(2)
        } else {
          print(1)
        }),
        "sizeouter_diameter_of_amplatz_sheath" = (if(input$sizeouter_diameter_of_amplatz_sheath=="<6 Fr") {1}
                                                  else if(input$sizeouter_diameter_of_amplatz_sheath== "11-14 Fr") {2}
                                                  else if(input$sizeouter_diameter_of_amplatz_sheath== "15-18 Fr") {3}
                                                  else if(input$sizeouter_diameter_of_amplatz_sheath=="19-24 Fr") {4}
                                                  else if(input$sizeouter_diameter_of_amplatz_sheath=="25-26 Fr") {5}
                                                  else if(input$sizeouter_diameter_of_amplatz_sheath=="27-30 Fr") {6}
                                                  else {7}
                                                  ),
        "pre_operative_msu_result" = (if(input$pre_operative_msu_result=="Heavy mixed growth treated pre-operatively") {1}
                                      else if(input$pre_operative_msu_result=="Heavy mixed growth untreated") {2}
                                      else if(input$pre_operative_msu_result=="No growth") {3}
                                      else if(input$pre_operative_msu_result=="Not Done") {4}
                                      else if(input$pre_operative_msu_result=="UTI treated pre-operatively") {5}
                                      else {6}
                                      ),
        "puncture_site" = (if(input$puncture_site == "Interpolar") {1}
                           else if(input$puncture_site == "Lower Pole") {2}
                           else {3}
                           ),
        "renogramdmsa" =  (if (input$renogramdmsa == "Yes") {
          print(2)
        } else {
          print(1)
        }),
        "stone_dimensions" = (if(input$stone_dimensions==">2 cm") {1}
                              else if(input$stone_dimensions==">4 cm") {2}
                              else if(input$stone_dimensions=="0-1 cm") {3}
                              else if(input$stone_dimensions=="1-2 cm") {4}
                              else {5}
                              ),
        "image_guidance_for_renal_puncture" = (if(input$image_guidance_for_renal_puncture=="CT") {1}
                                               else if(input$image_guidance_for_renal_puncture=="Fluoroscopy & ultrasound") {2}
                                               else if(input$image_guidance_for_renal_puncture=="Fluoroscopy") {3}
                                               else {4}
                                               )
      ) %>% as_tibble()
  
  })
    
  output$charlson_table <- render_gt({charlson_score_table})
  
  output$table_summary <- render_gt({
    cbind("n" = (round(nrow(getData(
      
    )), digits = 0)),
    "perc" = round((nrow(getData(
      
    )) / 12810 * 100), digits = 2)) %>% as_tibble() %>% gt() %>% cols_merge(columns = c("n", "perc"),
                                                                            pattern = "{1} ({2}%)") %>% cols_label("n" = "n (% of total dataset)") %>% tab_header("Patients matching input demographics") %>% gt_theme_espn()
  })
  
  output$table1 <- render_gt({
    source("tallies.R", local = TRUE)
    
    selection <- cbind(
      "Outcome" = c(
        "Complete Clearance on Fluoroscopy",
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
        "Grade V",
        "Death"
      ),
      "data_available_n" = c(
        (nrow(
          getData() %>% drop_na(complete_clearance_on_fluoroscopy)
        )),
        (nrow(
          getData() %>% drop_na(clearance_on_post_operative_radiological_imaging_during_a)
        )),
        (nrow(
          getData() %>% drop_na(stone_free_at_follow_up)
        )),
        (nrow(
          getData() %>% drop_na(adjuvant_treatment)
        )),
        (nrow(
          getData() %>% drop_na(post_operative_infection)
        )),
        (nrow(
          getData() %>% drop_na(post_operative_infection)
        )),
        (nrow(getData() %>% drop_na(itu_hdu_admission))),
        (nrow(getData() %>% drop_na(blood_transfusion))),
        (nrow(getData() %>% drop_na(visceral_injury))),
        (nrow(
          getData() %>% drop_na(postop_complications)
        )),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )),
        (nrow(
          getData() %>% drop_na(patient_statusdischarge)
        ))
      ),
      "number" = c(
        complete_clearance_on_fluoroscopy_xa(),
        clearance_xa(),
        sf_xa(),
        (nrow(
          getData() %>% drop_na(adjuvant_treatment)
        )) - adjuvant_xa(),
        infection_xa,
        sepsis_xa,
        (nrow(getData() %>% drop_na(itu_hdu_admission))) - itu_hdu_xa(),
        (nrow(getData() %>% drop_na(blood_transfusion))) - transfusion_xa(),
        (nrow(getData() %>% drop_na(visceral_injury))) - visceral_injury_xa(),
        (nrow(
          getData() %>% drop_na(postop_complications)
        )) - postop_complications_xa(),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) - I_xa(),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) - II_xa(),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) - IIIa_xa(),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) - IIIb_xa(),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) - IVa_xa(),
        (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) - V_xa(),
        (nrow(
          getData() %>% drop_na(patient_statusdischarge)
        )) - death_xa()
      ),
      "perc" = c(
        (complete_clearance_on_fluoroscopy_xa() / (nrow(
          getData() %>% drop_na(complete_clearance_on_fluoroscopy)
        )) * 100),
        (clearance_xa() / (nrow(
          getData() %>% drop_na(clearance_on_post_operative_radiological_imaging_during_a)
        )) * 100),
        (sf_xa() / (nrow(
          getData() %>% drop_na(stone_free_at_follow_up)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(adjuvant_treatment))
        ) - adjuvant_xa()) / (nrow(
          getData() %>% drop_na(adjuvant_treatment)
        )) * 100),
        (infection_xa / (nrow(
          getData() %>% drop_na(post_operative_infection)
        )) * 100),
        (sepsis_xa / (nrow(
          getData() %>% drop_na(post_operative_infection)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(itu_hdu_admission))
        ) - itu_hdu_xa()) / (nrow(
          getData() %>% drop_na(itu_hdu_admission)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(blood_transfusion))
        ) - transfusion_xa()) / (nrow(
          getData() %>% drop_na(blood_transfusion)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(visceral_injury))
        ) - visceral_injury_xa()) / (nrow(
          getData() %>% drop_na(visceral_injury)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(postop_complications))
        ) - postop_complications_xa()) /  (nrow(
          getData() %>% drop_na(postop_complications)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(clavien_dindo_grade_of_complications))
        ) - I_xa()) / (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(clavien_dindo_grade_of_complications))
        ) - II_xa()) / (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(clavien_dindo_grade_of_complications))
        ) - IIIa_xa()) / (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(clavien_dindo_grade_of_complications))
        ) - IIIb_xa()) / (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(clavien_dindo_grade_of_complications))
        ) - IVa_xa()) / (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(clavien_dindo_grade_of_complications))
        ) - V_xa()) / (nrow(
          getData() %>% drop_na(clavien_dindo_grade_of_complications)
        )) * 100),
        (((
          nrow(getData() %>% drop_na(patient_statusdischarge))
        ) - death_xa()) / (nrow(
          getData() %>% drop_na(patient_statusdischarge)
        )) * 100)
      ) %>% as.numeric() %>% round(digits = 1)
    ) %>% as_tibble() %>% cbind(
      "overall_perc" = c(
        71.7,
        53.2,
        74.2,
        15.5,
        11.6,
        2.7,
        5.1,
        2.0,
        0.2,
        11.5,
        4.9,
        6.4,
        2.1,
        1.3,
        0.5,
        0.1,
        0.2
      )
    )  %>% gt() %>% cols_move(columns = data_available_n,
                              after = number) %>% cols_move(columns = perc,
                                                            after = Outcome) %>% cols_move(columns = overall_perc,
                                                                                           after = perc) %>% cols_label(
                                                                                             Outcome = "Complication",
                                                                                             overall_perc = "Specified Complication for Total Dataset (%)",
                                                                                             data_available_n = "Data Available for Inputted Demographics, n",
                                                                                             number = "Number with Specified Complication for Inputted demographics, n",
                                                                                             perc = "Specified Complication for Inputted Demographics (%)"
                                                                                           ) %>% gt_theme_espn()
  })
  
  output$summary_table<-render_gt({
    
    
    infection_explanation <- explain(getData1(),
                                     approach = "gaussian",
                                     explainer = infection_explainer,
                                     prediction_zero = 0)
    transfusion_explanation<- explain(getData1(),
                                      approach = "gaussian",
                                      explainer = transfusion_explainer,
                                      prediction_zero = 0)
    itu_hdu_explanation<- explain(getData1(),
                                  approach = "gaussian",
                                  explainer = itu_hdu_explainer,
                                  prediction_zero = 0)
    visc_inj_explanation<- explain(getData1(),
                                   approach = "gaussian",
                                   explainer = visc_inj_explainer,
                                   prediction_zero = 0)
    post_op_comp_explanation <- explain(getData1(),
                                        approach = "gaussian",
                                        explainer = post_op_comp_explainer,
                                        prediction_zero = 0)
    sf_at_fu_explanation<- explain(getData1(),
                                   approach = "gaussian",
                                   explainer = sf_at_fu_explainer,
                                   prediction_zero = 0)
    adj_rx_explanation<- explain(getData1(),
                                 approach = "gaussian",
                                 explainer = adj_rx_explainer,
                                 prediction_zero = 0)
    
    
    
    explanation_table <- cbind(
      "Outcome" = c(
        "Stone Free at Follow-up",
        "Infection",
        "Transfusion",
        "ITU/HDU Admission",
        "Visceral Injury",
        "Post-operative Complication",
        "Need for Adjuvant Treatment"
      ),
      "Prediction" = (c(
        (sf_at_fu_explanation$p %>% round()),
        (infection_explanation$p %>% round()),
        (transfusion_explanation$p %>% round()),
        (itu_hdu_explanation$p %>% round()),
        (visc_inj_explanation$p %>% round()),
        (post_op_comp_explanation$p %>% round()),
        (adj_rx_explanation$p %>% round()) 
      ) %>% ifelse("Likely", "Unlikely")
      ),
      "Predicted likelihood (%)" = c(
        sf_at_fu_explanation$p,
        infection_explanation$p,
        transfusion_explanation$p,
        itu_hdu_explanation$p,
        visc_inj_explanation$p,
        post_op_comp_explanation$p,
        adj_rx_explanation$p 
      )
    ) %>% as_tibble()
    explanation_table$`Predicted likelihood (%)` <-
      as.numeric(explanation_table$`Predicted likelihood (%)`)
    explanation_table$`Predicted likelihood (%)` <-
      (round(explanation_table$`Predicted likelihood (%)`, digits = 2)) * 100
    
    explanation_table %>% gt() %>% gt_theme_espn()
   
    
    })
  
  output$sf_at_fu_shapley <- renderPlot({
    explain(
      getData1(),
      approach = "gaussian",
      explainer = sf_at_fu_explainer,
      prediction_zero = 0
    ) %>% plot()
  })
  
  output$infection_shapley <- renderPlot({
    explain(
      getData1(),
      approach = "gaussian",
      explainer = infection_explainer,
      prediction_zero = 0
    ) %>% plot()
  })
  
  output$transfusion_shapley <- renderPlot({
    explain(
      getData1(),
      approach = "gaussian",
      explainer = transfusion_explainer,
      prediction_zero = 0
    ) %>% plot()
  })
  
  output$itu_hdu_shapley <- renderPlot({
    explain(
      getData1(),
      approach = "gaussian",
      explainer = itu_hdu_explainer,
      prediction_zero = 0
    ) %>% plot()
  })
  
  output$visc_inj_shapley <- renderPlot({
    explain(
      getData1(),
      approach = "gaussian",
      explainer = visc_inj_explainer,
      prediction_zero = 0
    ) %>% plot()
  })
  
  output$post_op_comp_shapley <- renderPlot({
    explain(
      getData1(),
      approach = "gaussian",
      explainer = post_op_comp_explainer,
      prediction_zero = 0
    ) %>% plot()
  })
  
  output$adj_rx_shapley <- renderPlot({
    explain(
      getData1(),
      approach = "gaussian",
      explainer = adj_rx_explainer,
      prediction_zero = 0
    ) %>% plot()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
