#single_infection_outcome 
single_infection_na_omit_outcome <-
  ifelse(PCNL_single_infection_omit_na_test$single_infection_outcome == "Yes",
         "1",
         "0") %>% as.factor()

single_infection_oversample_outcome <-
  ifelse(PCNL_single_infection_omit_na_test$single_infection_outcome == "Yes",
         "1",
         "0") %>% as.factor()

### Omit NA only
PCNL_single_infection_na_omit_glm_predict1 <-
  predict(PCNL_Post_Infection_glm,
          PCNL_single_infection_omit_na_test)
PCNL_single_infection_na_omit_glm_predict <-
  ifelse(PCNL_single_infection_na_omit_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                            "Yes"))

PCNL_single_infection_na_omit_glm_tb <-
  table(
    pred = PCNL_single_infection_na_omit_glm_predict,
    ref = PCNL_single_infection_omit_na_test$single_infection_outcome
  )
confusionMatrix(PCNL_single_infection_na_omit_glm_tb)
PCNL_single_infection_na_omit_glm_roc <-
  roc(
    single_infection_na_omit_outcome,
    PCNL_single_infection_na_omit_glm_predict1)
infection_na_omit_glm_auc<-auc(PCNL_single_infection_na_omit_glm_roc)
print(infection_na_omit_glm_auc)
print(ci.auc(PCNL_single_infection_na_omit_glm_roc))
varImp(PCNL_Post_Infection_glm)
summary(PCNL_Post_Infection_glm)

### Oversampled
PCNL_single_infection_oversample_glm_predict1 <-
  predict(PCNL_Post_Infection_oversample_glm,
          PCNL_single_infection_omit_na_test)
PCNL_single_infection_oversample_glm_predict <-
  ifelse(PCNL_single_infection_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                            "Yes"))

PCNL_single_infection_oversample_glm_tb <-
  table(
    pred = PCNL_single_infection_oversample_glm_predict,
    ref = PCNL_single_infection_omit_na_test$single_infection_outcome
  )
confusionMatrix(PCNL_single_infection_oversample_glm_tb)
PCNL_single_infection_oversample_glm_roc <-
  roc(
    single_infection_oversample_outcome,
    PCNL_single_infection_oversample_glm_predict1)
infection_oversample_glm_auc<-auc(PCNL_single_infection_oversample_glm_roc)
print(infection_oversample_glm_auc)
print(ci.auc(PCNL_single_infection_oversample_glm_roc))
varImp(PCNL_Post_Infection_oversample_glm)
summary(PCNL_Post_Infection_oversample_glm)


### Imputed
PCNL_single_infection_imp_glm_predict1 <-
  predict(PCNL_Post_Infection_imp_glm,
          PCNL_single_infection_omit_na_test)
PCNL_single_infection_imp_glm_predict <-
  ifelse(PCNL_single_infection_imp_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                        "Yes"))

PCNL_single_infection_imp_glm_tb <-
  table(
    pred = PCNL_single_infection_imp_glm_predict,
    ref = PCNL_single_infection_omit_na_test$single_infection_outcome
  )
confusionMatrix(PCNL_single_infection_imp_glm_tb)
PCNL_single_infection_imp_glm_roc <-
  roc(
    single_infection_na_omit_outcome,
    PCNL_single_infection_imp_glm_predict1)
infection_imp_glm_auc<-auc(PCNL_single_infection_imp_glm_roc)
print(infection_imp_glm_auc)
print(ci.auc(PCNL_single_infection_imp_glm_roc))
varImp(PCNL_Post_Infection_imp_glm)
summary(PCNL_Post_Infection_imp_glm)

### Imputed and Oversampled
PCNL_single_infection_imp_oversample_glm_predict1 <-
  predict(PCNL_Post_Infection_imp_oversample_glm,
          PCNL_single_infection_omit_na_test)
PCNL_single_infection_imp_oversample_glm_predict <-
  ifelse(PCNL_single_infection_imp_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                        "Yes"))

PCNL_single_infection_imp_oversample_glm_tb <-
  table(pred = PCNL_single_infection_imp_oversample_glm_predict,
        ref = PCNL_single_infection_omit_na_test$single_infection_outcome)
confusionMatrix(PCNL_single_infection_imp_oversample_glm_tb)
PCNL_single_infection_imp_oversample_glm_roc <-
  roc(single_infection_na_omit_outcome,
      PCNL_single_infection_imp_oversample_glm_predict1)
infection_imp_oversample_glm_auc <- auc(PCNL_single_infection_imp_oversample_glm_roc)
print(infection_imp_oversample_glm_auc)
print(ci.auc(PCNL_single_infection_imp_oversample_glm_roc))
varImp(PCNL_Post_Infection_imp_oversample_glm)
summary(PCNL_Post_Infection_imp_oversample_glm)


### ROC
infection_glm_roc_list<-list(PCNL_single_infection_na_omit_glm_roc,
                         PCNL_single_infection_oversample_glm_roc,
                         PCNL_single_infection_imp_glm_roc,
                         PCNL_single_infection_imp_oversample_glm_roc)

infection_glm_ci_list <-
  lapply(infection_glm_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_infection_glm_ci_list <- lapply(infection_glm_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


infection_glm_roc_combined <-
  ggroc(infection_glm_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Post-Operative Infection",
    x = "Specificity",
    y = "Sensitivity",
    color = "Model"
  ) + scale_color_discrete(
    labels = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"
    )
  ) + geom_abline(slope = 1, intercept = 1)

for(i in 1:4) {
  infection_glm_roc_combined <- infection_glm_roc_combined + geom_ribbon(
    data = dat_infection_glm_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

infection_glm_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_single_infection_na_omit_glm_roc),
      ci.auc(PCNL_single_infection_oversample_glm_roc),
      ci.auc(PCNL_single_infection_imp_glm_roc),
      ci.auc(PCNL_single_infection_imp_oversample_glm_roc)
    )
  ) %>% as_tibble()
colnames(infection_glm_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
infection_glm_auc_data_overall$AUC <- as.numeric(infection_glm_auc_data_overall$AUC) %>% round(digits = 2)
infection_glm_auc_data_overall$LB <- as.numeric(infection_glm_auc_data_overall$LB) %>% round(digits = 2)
infection_glm_auc_data_overall$UB <- as.numeric(infection_glm_auc_data_overall$UB) %>% round(digits = 2)
infection_glm_auc_data_overall <-infection_glm_auc_data_overall %>% relocate(AUC, .before = LB) 

infection_glm_auc_data_overall <- infection_glm_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

infection_glm_auc_table <- infection_glm_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(infection_glm_roc_combined))
infection_glm_auc_table_vp <- viewport(x = 0.70, y = 0.22, 
                         just = c("right", "bottom"),
                         height = 0.1, width = 0.2)
pushViewport(infection_glm_auc_table_vp)
grid.draw(infection_glm_auc_table)

popViewport()

#blood_transfusion
transfusion_na_omit_outcome <-
  PCNL_transfusion_omit_na_test$blood_transfusion %>% factor(levels = c("No", "Yes"))

transfusion_oversample_outcome <-
  PCNL_transfusion_omit_na_test$blood_transfusion %>% factor(levels = c("No", "Yes"))

## NA Omit
PCNL_transfusion_na_omit_glm_predict1 <-
  predict(PCNL_transfusion_glm,
          PCNL_transfusion_omit_na_test)
PCNL_transfusion_na_omit_glm_predict <-
  ifelse(PCNL_transfusion_na_omit_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                            "Yes"))

PCNL_transfusion_na_omit_glm_tb <-
  table(
    pred = PCNL_transfusion_na_omit_glm_predict,
    ref = transfusion_na_omit_outcome
  )
confusionMatrix(PCNL_transfusion_na_omit_glm_tb)
PCNL_transfusion_na_omit_glm_roc <-
  roc(
    transfusion_na_omit_outcome,
    PCNL_transfusion_na_omit_glm_predict1)
transfusion_na_omit_glm_auc<-auc(PCNL_transfusion_na_omit_glm_roc)
print(transfusion_na_omit_glm_auc)
print(ci.auc(PCNL_transfusion_na_omit_glm_roc))
varImp(PCNL_transfusion_glm)
summary(PCNL_transfusion_glm)

### Oversampled
PCNL_transfusion_oversample_glm_predict1 <-
  predict(PCNL_transfusion_oversample_glm,
          PCNL_transfusion_omit_na_test)
PCNL_transfusion_oversample_glm_predict <-
  ifelse(PCNL_transfusion_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                          "Yes"))

PCNL_transfusion_oversample_glm_tb <-
  table(pred = PCNL_transfusion_oversample_glm_predict,
        ref = transfusion_na_omit_outcome)
confusionMatrix(PCNL_transfusion_oversample_glm_tb)
PCNL_transfusion_oversample_glm_roc <-
  roc(transfusion_oversample_outcome,
      PCNL_transfusion_oversample_glm_predict1)
transfusion_oversample_glm_auc <-
  auc(PCNL_transfusion_oversample_glm_roc)
print(transfusion_oversample_glm_auc)
print(ci.auc(PCNL_transfusion_oversample_glm_roc))
varImp(PCNL_transfusion_oversample_glm)
summary(PCNL_transfusion_oversample_glm)


### Imputed
PCNL_transfusion_imp_glm_predict1 <-
  predict(PCNL_transfusion_imp_glm,
          PCNL_transfusion_omit_na_test)
PCNL_transfusion_imp_glm_predict <-
  ifelse(PCNL_transfusion_imp_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                   "Yes"))

PCNL_transfusion_imp_glm_tb <-
  table(pred = PCNL_transfusion_imp_glm_predict,
        ref = transfusion_na_omit_outcome)
confusionMatrix(PCNL_transfusion_imp_glm_tb)
PCNL_transfusion_imp_glm_roc <-
  roc(transfusion_na_omit_outcome,
      PCNL_transfusion_imp_glm_predict1)
transfusion_imp_glm_auc <- auc(PCNL_transfusion_imp_glm_roc)
print(transfusion_imp_glm_auc)
print(ci.auc(PCNL_transfusion_imp_glm_roc))
varImp(PCNL_transfusion_imp_glm)
summary(PCNL_transfusion_imp_glm)

### Imputed and Oversampled
PCNL_transfusion_imp_oversample_glm_predict1 <-
  predict(PCNL_transfusion_imp_oversample_glm,
          PCNL_transfusion_omit_na_test)
PCNL_transfusion_imp_oversample_glm_predict <-
  ifelse(PCNL_transfusion_imp_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                              "Yes"))

PCNL_transfusion_imp_oversample_glm_tb <-
  table(pred = PCNL_transfusion_imp_oversample_glm_predict,
        ref = transfusion_na_omit_outcome)
confusionMatrix(PCNL_transfusion_imp_oversample_glm_tb)
PCNL_transfusion_imp_oversample_glm_roc <-
  roc(transfusion_na_omit_outcome,
      PCNL_transfusion_imp_oversample_glm_predict1)
transfusion_imp_oversample_glm_auc <-
  auc(PCNL_transfusion_imp_oversample_glm_roc)
print(transfusion_imp_oversample_glm_auc)
print(ci.auc(PCNL_transfusion_imp_oversample_glm_roc))
varImp(PCNL_transfusion_imp_oversample_glm)
summary(PCNL_transfusion_imp_oversample_glm)


### ROC
transfusion_glm_roc_list <- list(
  PCNL_transfusion_na_omit_glm_roc,
  PCNL_transfusion_oversample_glm_roc,
  PCNL_transfusion_imp_glm_roc,
  PCNL_transfusion_imp_oversample_glm_roc
)

transfusion_glm_ci_list <-
  lapply(transfusion_glm_roc_list,
         ci.se,
         specificities = seq(0, 1, l = 25))

dat_transfusion_glm_ci_list <-
  lapply(transfusion_glm_ci_list, function(ciobj)
    data.frame(
      x = as.numeric(rownames(ciobj)),
      lower = ciobj[, 1],
      upper = ciobj[, 3]
    ))


transfusion_glm_roc_combined <-
  ggroc(transfusion_glm_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Post-Operative Transfusion",
    x = "Specificity",
    y = "Sensitivity",
    color = "Model"
  ) + scale_color_discrete(labels = c(
    "Original Dataset",
    "Oversampled",
    "Imputed",
    "Imputed and Oversampled"
  )) + geom_abline(slope = 1, intercept = 1)

for (i in 1:4) {
  transfusion_glm_roc_combined <-
    transfusion_glm_roc_combined + geom_ribbon(
      data = dat_transfusion_glm_ci_list[[i]],
      aes(x = x, ymin = lower, ymax = upper),
      fill = i + 1,
      alpha = 0.2,
      inherit.aes = F
    )
}

transfusion_glm_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"
    ),
    "CI" = rbind(
      ci.auc(PCNL_transfusion_na_omit_glm_roc),
      ci.auc(PCNL_transfusion_oversample_glm_roc),
      ci.auc(PCNL_transfusion_imp_glm_roc),
      ci.auc(PCNL_transfusion_imp_oversample_glm_roc)
    )
  ) %>% as_tibble()
colnames(transfusion_glm_auc_data_overall) <-
  c("Model", "LB", "AUC", "UB")
transfusion_glm_auc_data_overall$AUC <-
  as.numeric(transfusion_glm_auc_data_overall$AUC) %>% round(digits = 2)
transfusion_glm_auc_data_overall$LB <-
  as.numeric(transfusion_glm_auc_data_overall$LB) %>% round(digits = 2)
transfusion_glm_auc_data_overall$UB <-
  as.numeric(transfusion_glm_auc_data_overall$UB) %>% round(digits = 2)
transfusion_glm_auc_data_overall <-
  transfusion_glm_auc_data_overall %>% relocate(AUC, .before = LB)

transfusion_glm_auc_data_overall <-
  transfusion_glm_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

transfusion_glm_auc_table <-
  transfusion_glm_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(transfusion_glm_roc_combined))
transfusion_glm_auc_table_vp <- viewport(
  x = 0.70,
  y = 0.22,
  just = c("right", "bottom"),
  height = 0.1,
  width = 0.2
)
pushViewport(transfusion_glm_auc_table_vp)
grid.draw(transfusion_glm_auc_table)

popViewport()



#itu_hdu_admission
itu_hdu_na_omit_outcome <-
PCNL_itu_hdu_omit_na_test$itu_hdu_admission %>% factor(levels = c("No", "Yes"))

itu_hdu_oversample_outcome <-
  PCNL_itu_hdu_omit_na_test$itu_hdu_admission %>% factor(levels = c("No", "Yes"))

## NA Omit
PCNL_itu_hdu_na_omit_glm_predict1 <-
  predict(PCNL_itu_hdu_glm,
          PCNL_itu_hdu_omit_na_test)
PCNL_itu_hdu_na_omit_glm_predict <-
  ifelse(PCNL_itu_hdu_na_omit_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                       "Yes"))

PCNL_itu_hdu_na_omit_glm_tb <-
  table(
    pred = PCNL_itu_hdu_na_omit_glm_predict,
    ref = itu_hdu_na_omit_outcome
  )
confusionMatrix(PCNL_itu_hdu_na_omit_glm_tb)
PCNL_itu_hdu_na_omit_glm_roc <-
  roc(
    itu_hdu_na_omit_outcome,
    PCNL_itu_hdu_na_omit_glm_predict1)
itu_hdu_na_omit_glm_auc<-auc(PCNL_itu_hdu_na_omit_glm_roc)
print(itu_hdu_na_omit_glm_auc)
print(ci.auc(PCNL_itu_hdu_na_omit_glm_roc))
varImp(PCNL_itu_hdu_glm)
summary(PCNL_itu_hdu_glm)

### Oversampled
PCNL_itu_hdu_oversample_glm_predict1 <-
  predict(PCNL_itu_hdu_oversample_glm,
          PCNL_itu_hdu_omit_na_test)
PCNL_itu_hdu_oversample_glm_predict <-
  ifelse(PCNL_itu_hdu_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                          "Yes"))

PCNL_itu_hdu_oversample_glm_tb <-
  table(pred = PCNL_itu_hdu_oversample_glm_predict,
        ref = itu_hdu_na_omit_outcome)
confusionMatrix(PCNL_itu_hdu_oversample_glm_tb)
PCNL_itu_hdu_oversample_glm_roc <-
  roc(itu_hdu_oversample_outcome,
      PCNL_itu_hdu_oversample_glm_predict1)
itu_hdu_oversample_glm_auc <-
  auc(PCNL_itu_hdu_oversample_glm_roc)
print(itu_hdu_oversample_glm_auc)
print(ci.auc(PCNL_itu_hdu_oversample_glm_roc))
varImp(PCNL_itu_hdu_oversample_glm)
summary(PCNL_itu_hdu_oversample_glm)


### Imputed
PCNL_itu_hdu_imp_glm_predict1 <-
  predict(PCNL_itu_hdu_imp_glm,
          PCNL_itu_hdu_omit_na_test)
PCNL_itu_hdu_imp_glm_predict <-
  ifelse(PCNL_itu_hdu_imp_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                   "Yes"))

PCNL_itu_hdu_imp_glm_tb <-
  table(pred = PCNL_itu_hdu_imp_glm_predict,
        ref = itu_hdu_na_omit_outcome)
confusionMatrix(PCNL_itu_hdu_imp_glm_tb)
PCNL_itu_hdu_imp_glm_roc <-
  roc(itu_hdu_na_omit_outcome,
      PCNL_itu_hdu_imp_glm_predict1)
itu_hdu_imp_glm_auc <- auc(PCNL_itu_hdu_imp_glm_roc)
print(itu_hdu_imp_glm_auc)
print(ci.auc(PCNL_itu_hdu_imp_glm_roc))
varImp(PCNL_itu_hdu_imp_glm)
summary(PCNL_itu_hdu_imp_glm)

### Imputed and Oversampled
PCNL_itu_hdu_imp_oversample_glm_predict1 <-
  predict(PCNL_itu_hdu_imp_oversample_glm,
          PCNL_itu_hdu_omit_na_test)
PCNL_itu_hdu_imp_oversample_glm_predict <-
  ifelse(PCNL_itu_hdu_imp_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                              "Yes"))

PCNL_itu_hdu_imp_oversample_glm_tb <-
  table(pred = PCNL_itu_hdu_imp_oversample_glm_predict,
        ref = itu_hdu_na_omit_outcome)
confusionMatrix(PCNL_itu_hdu_imp_oversample_glm_tb)
PCNL_itu_hdu_imp_oversample_glm_roc <-
  roc(itu_hdu_na_omit_outcome,
      PCNL_itu_hdu_imp_oversample_glm_predict1)
itu_hdu_imp_oversample_glm_auc <-
  auc(PCNL_itu_hdu_imp_oversample_glm_roc)
print(itu_hdu_imp_oversample_glm_auc)
print(ci.auc(PCNL_itu_hdu_imp_oversample_glm_roc))
varImp(PCNL_itu_hdu_imp_oversample_glm)
summary(PCNL_itu_hdu_imp_oversample_glm)


### ROC
itu_hdu_glm_roc_list <- list(
  PCNL_itu_hdu_na_omit_glm_roc,
  PCNL_itu_hdu_oversample_glm_roc,
  PCNL_itu_hdu_imp_glm_roc,
  PCNL_itu_hdu_imp_oversample_glm_roc
)

itu_hdu_glm_ci_list <-
  lapply(itu_hdu_glm_roc_list,
         ci.se,
         specificities = seq(0, 1, l = 25))

dat_itu_hdu_glm_ci_list <-
  lapply(itu_hdu_glm_ci_list, function(ciobj)
    data.frame(
      x = as.numeric(rownames(ciobj)),
      lower = ciobj[, 1],
      upper = ciobj[, 3]
    ))


itu_hdu_glm_roc_combined <-
  ggroc(itu_hdu_glm_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "ITU/HDU Admission",
    x = "Specificity",
    y = "Sensitivity",
    color = "Model"
  ) + scale_color_discrete(labels = c(
    "Original Dataset",
    "Oversampled",
    "Imputed",
    "Imputed and Oversampled"
  )) + geom_abline(slope = 1, intercept = 1)

for (i in 1:4) {
  itu_hdu_glm_roc_combined <-
    itu_hdu_glm_roc_combined + geom_ribbon(
      data = dat_itu_hdu_glm_ci_list[[i]],
      aes(x = x, ymin = lower, ymax = upper),
      fill = i + 1,
      alpha = 0.2,
      inherit.aes = F
    )
}

itu_hdu_glm_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"
    ),
    "CI" = rbind(
      ci.auc(PCNL_itu_hdu_na_omit_glm_roc),
      ci.auc(PCNL_itu_hdu_oversample_glm_roc),
      ci.auc(PCNL_itu_hdu_imp_glm_roc),
      ci.auc(PCNL_itu_hdu_imp_oversample_glm_roc)
    )
  ) %>% as_tibble()
colnames(itu_hdu_glm_auc_data_overall) <-
  c("Model", "LB", "AUC", "UB")
itu_hdu_glm_auc_data_overall$AUC <-
  as.numeric(itu_hdu_glm_auc_data_overall$AUC) %>% round(digits = 2)
itu_hdu_glm_auc_data_overall$LB <-
  as.numeric(itu_hdu_glm_auc_data_overall$LB) %>% round(digits = 2)
itu_hdu_glm_auc_data_overall$UB <-
  as.numeric(itu_hdu_glm_auc_data_overall$UB) %>% round(digits = 2)
itu_hdu_glm_auc_data_overall <-
  itu_hdu_glm_auc_data_overall %>% relocate(AUC, .before = LB)

itu_hdu_glm_auc_data_overall <-
  itu_hdu_glm_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

itu_hdu_glm_auc_table <-
  itu_hdu_glm_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(itu_hdu_glm_roc_combined))
itu_hdu_glm_auc_table_vp <- viewport(
  x = 0.70,
  y = 0.22,
  just = c("right", "bottom"),
  height = 0.1,
  width = 0.2
)
pushViewport(itu_hdu_glm_auc_table_vp)
grid.draw(itu_hdu_glm_auc_table)

popViewport()



#complete_clearance_on_fluoroscopy
clearance_on_fluoro_na_omit_outcome <- PCNL_clearance_on_fluoro_omit_na_test$complete_clearance_on_fluoroscopy %>% factor(levels = c("No",
                                                                                                                                     "Yes"))

clearance_on_fluoro_oversample_outcome <-
  PCNL_clearance_on_fluoro_omit_na_test$complete_clearance_on_fluoroscopy %>% factor(levels = c("No",
                                                                                                "Yes"))

## NA Omit
PCNL_clearance_on_fluoro_na_omit_glm_predict1 <-
  predict(PCNL_clearance_on_fluoro_glm,
          PCNL_clearance_on_fluoro_omit_na_test)
PCNL_clearance_on_fluoro_na_omit_glm_predict <-
  ifelse(PCNL_clearance_on_fluoro_na_omit_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                   "Yes"))

PCNL_clearance_on_fluoro_na_omit_glm_tb <-
  table(
    pred = PCNL_clearance_on_fluoro_na_omit_glm_predict,
    ref = clearance_on_fluoro_na_omit_outcome
  )
confusionMatrix(PCNL_clearance_on_fluoro_na_omit_glm_tb)
PCNL_clearance_on_fluoro_na_omit_glm_roc <-
  roc(
    clearance_on_fluoro_na_omit_outcome,
    PCNL_clearance_on_fluoro_na_omit_glm_predict1)
clearance_on_fluoro_na_omit_glm_auc<-auc(PCNL_clearance_on_fluoro_na_omit_glm_roc)
print(clearance_on_fluoro_na_omit_glm_auc)
print(ci.auc(PCNL_clearance_on_fluoro_na_omit_glm_roc))
varImp(PCNL_clearance_on_fluoro_glm)
summary(PCNL_clearance_on_fluoro_glm)

### Oversampled
PCNL_clearance_on_fluoro_oversample_glm_predict1 <-
  predict(PCNL_clearance_on_fluoro_oversample_glm,
          PCNL_clearance_on_fluoro_omit_na_test)
PCNL_clearance_on_fluoro_oversample_glm_predict <-
  ifelse(PCNL_clearance_on_fluoro_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                      "Yes"))

PCNL_clearance_on_fluoro_oversample_glm_tb <-
  table(pred = PCNL_clearance_on_fluoro_oversample_glm_predict,
        ref = clearance_on_fluoro_na_omit_outcome)
confusionMatrix(PCNL_clearance_on_fluoro_oversample_glm_tb)
PCNL_clearance_on_fluoro_oversample_glm_roc <-
  roc(clearance_on_fluoro_oversample_outcome,
      PCNL_clearance_on_fluoro_oversample_glm_predict1)
clearance_on_fluoro_oversample_glm_auc <-
  auc(PCNL_clearance_on_fluoro_oversample_glm_roc)
print(clearance_on_fluoro_oversample_glm_auc)
print(ci.auc(PCNL_clearance_on_fluoro_oversample_glm_roc))
varImp(PCNL_clearance_on_fluoro_oversample_glm)
summary(PCNL_clearance_on_fluoro_oversample_glm)


### Imputed
PCNL_clearance_on_fluoro_imp_glm_predict1 <-
  predict(PCNL_clearance_on_fluoro_imp_glm,
          PCNL_clearance_on_fluoro_omit_na_test)
PCNL_clearance_on_fluoro_imp_glm_predict <-
  ifelse(PCNL_clearance_on_fluoro_imp_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                               "Yes"))

PCNL_clearance_on_fluoro_imp_glm_tb <-
  table(pred = PCNL_clearance_on_fluoro_imp_glm_predict,
        ref = clearance_on_fluoro_na_omit_outcome)
confusionMatrix(PCNL_clearance_on_fluoro_imp_glm_tb)
PCNL_clearance_on_fluoro_imp_glm_roc <-
  roc(clearance_on_fluoro_na_omit_outcome,
      PCNL_clearance_on_fluoro_imp_glm_predict1)
clearance_on_fluoro_imp_glm_auc <- auc(PCNL_clearance_on_fluoro_imp_glm_roc)
print(clearance_on_fluoro_imp_glm_auc)
print(ci.auc(PCNL_clearance_on_fluoro_imp_glm_roc))
varImp(PCNL_clearance_on_fluoro_imp_glm)
summary(PCNL_clearance_on_fluoro_imp_glm)

### Imputed and Oversampled
PCNL_clearance_on_fluoro_imp_oversample_glm_predict1 <-
  predict(PCNL_clearance_on_fluoro_imp_oversample_glm,
          PCNL_clearance_on_fluoro_omit_na_test)
PCNL_clearance_on_fluoro_imp_oversample_glm_predict <-
  ifelse(PCNL_clearance_on_fluoro_imp_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                          "Yes"))

PCNL_clearance_on_fluoro_imp_oversample_glm_tb <-
  table(pred = PCNL_clearance_on_fluoro_imp_oversample_glm_predict,
        ref = clearance_on_fluoro_na_omit_outcome)
confusionMatrix(PCNL_clearance_on_fluoro_imp_oversample_glm_tb)
PCNL_clearance_on_fluoro_imp_oversample_glm_roc <-
  roc(clearance_on_fluoro_na_omit_outcome,
      PCNL_clearance_on_fluoro_imp_oversample_glm_predict1)
clearance_on_fluoro_imp_oversample_glm_auc <-
  auc(PCNL_clearance_on_fluoro_imp_oversample_glm_roc)
print(clearance_on_fluoro_imp_oversample_glm_auc)
print(ci.auc(PCNL_clearance_on_fluoro_imp_oversample_glm_roc))
varImp(PCNL_clearance_on_fluoro_imp_oversample_glm)
summary(PCNL_clearance_on_fluoro_imp_oversample_glm)


### ROC
clearance_on_fluoro_glm_roc_list <- list(
  PCNL_clearance_on_fluoro_na_omit_glm_roc,
  PCNL_clearance_on_fluoro_oversample_glm_roc,
  PCNL_clearance_on_fluoro_imp_glm_roc,
  PCNL_clearance_on_fluoro_imp_oversample_glm_roc
)

clearance_on_fluoro_glm_ci_list <-
  lapply(clearance_on_fluoro_glm_roc_list,
         ci.se,
         specificities = seq(0, 1, l = 25))

dat_clearance_on_fluoro_glm_ci_list <-
  lapply(clearance_on_fluoro_glm_ci_list, function(ciobj)
    data.frame(
      x = as.numeric(rownames(ciobj)),
      lower = ciobj[, 1],
      upper = ciobj[, 3]
    ))


clearance_on_fluoro_glm_roc_combined <-
  ggroc(clearance_on_fluoro_glm_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Clearance on Intra-operative Fluoroscopy",
    x = "Specificity",
    y = "Sensitivity",
    color = "Model"
  ) + scale_color_discrete(labels = c(
    "Original Dataset",
    "Oversampled",
    "Imputed",
    "Imputed and Oversampled"
  )) + geom_abline(slope = 1, intercept = 1)

for (i in 1:4) {
  clearance_on_fluoro_glm_roc_combined <-
    clearance_on_fluoro_glm_roc_combined + geom_ribbon(
      data = dat_clearance_on_fluoro_glm_ci_list[[i]],
      aes(x = x, ymin = lower, ymax = upper),
      fill = i + 1,
      alpha = 0.2,
      inherit.aes = F
    )
}

clearance_on_fluoro_glm_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"
    ),
    "CI" = rbind(
      ci.auc(PCNL_clearance_on_fluoro_na_omit_glm_roc),
      ci.auc(PCNL_clearance_on_fluoro_oversample_glm_roc),
      ci.auc(PCNL_clearance_on_fluoro_imp_glm_roc),
      ci.auc(PCNL_clearance_on_fluoro_imp_oversample_glm_roc)
    )
  ) %>% as_tibble()
colnames(clearance_on_fluoro_glm_auc_data_overall) <-
  c("Model", "LB", "AUC", "UB")
clearance_on_fluoro_glm_auc_data_overall$AUC <-
  as.numeric(clearance_on_fluoro_glm_auc_data_overall$AUC) %>% round(digits = 2)
clearance_on_fluoro_glm_auc_data_overall$LB <-
  as.numeric(clearance_on_fluoro_glm_auc_data_overall$LB) %>% round(digits = 2)
clearance_on_fluoro_glm_auc_data_overall$UB <-
  as.numeric(clearance_on_fluoro_glm_auc_data_overall$UB) %>% round(digits = 2)
clearance_on_fluoro_glm_auc_data_overall <-
  clearance_on_fluoro_glm_auc_data_overall %>% relocate(AUC, .before = LB)

clearance_on_fluoro_glm_auc_data_overall <-
  clearance_on_fluoro_glm_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

clearance_on_fluoro_glm_auc_table <-
  clearance_on_fluoro_glm_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(clearance_on_fluoro_glm_roc_combined))
clearance_on_fluoro_glm_auc_table_vp <- viewport(
  x = 0.70,
  y = 0.22,
  just = c("right", "bottom"),
  height = 0.1,
  width = 0.2
)
pushViewport(clearance_on_fluoro_glm_auc_table_vp)
grid.draw(clearance_on_fluoro_glm_auc_table)

popViewport()


#visceral_injury
visc_inj_na_omit_outcome <-
  PCNL_visc_inj_omit_na_test$visceral_injury %>% factor(levels = c("No",
                                                                          "Yes"))

visc_inj_oversample_outcome <-
  PCNL_visc_inj_omit_na_test$visceral_injury %>% factor(levels = c("No",
                                                                          "Yes"))

## NA Omit
PCNL_visc_inj_na_omit_glm_predict1 <-
  predict(PCNL_visc_inj_glm,
          PCNL_visc_inj_omit_na_test)
PCNL_visc_inj_na_omit_glm_predict <-
  ifelse(PCNL_visc_inj_na_omit_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                    "Yes"))

PCNL_visc_inj_na_omit_glm_tb <-
  table(
    pred = PCNL_visc_inj_na_omit_glm_predict,
    ref = visc_inj_na_omit_outcome
  )
confusionMatrix(PCNL_visc_inj_na_omit_glm_tb)
PCNL_visc_inj_na_omit_glm_roc <-
  roc(
    visc_inj_na_omit_outcome,
    PCNL_visc_inj_na_omit_glm_predict1)
visc_inj_na_omit_glm_auc<-auc(PCNL_visc_inj_na_omit_glm_roc)
print(visc_inj_na_omit_glm_auc)
print(ci.auc(PCNL_visc_inj_na_omit_glm_roc))
varImp(PCNL_visc_inj_glm)
summary(PCNL_visc_inj_glm)

### Oversampled
PCNL_visc_inj_oversample_glm_predict1 <-
  predict(PCNL_visc_inj_oversample_glm,
          PCNL_visc_inj_omit_na_test)
PCNL_visc_inj_oversample_glm_predict <-
  ifelse(PCNL_visc_inj_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                                  "Yes"))

PCNL_visc_inj_oversample_glm_tb <-
  table(pred = PCNL_visc_inj_oversample_glm_predict,
        ref = visc_inj_na_omit_outcome)
confusionMatrix(PCNL_visc_inj_oversample_glm_tb)
PCNL_visc_inj_oversample_glm_roc <-
  roc(visc_inj_oversample_outcome,
      PCNL_visc_inj_oversample_glm_predict1)
visc_inj_oversample_glm_auc <-
  auc(PCNL_visc_inj_oversample_glm_roc)
print(visc_inj_oversample_glm_auc)
print(ci.auc(PCNL_visc_inj_oversample_glm_roc))
varImp(PCNL_visc_inj_oversample_glm)
summary(PCNL_visc_inj_oversample_glm)


### Imputed
PCNL_visc_inj_imp_glm_predict1 <-
  predict(PCNL_visc_inj_imp_glm,
          PCNL_visc_inj_omit_na_test)
PCNL_visc_inj_imp_glm_predict <-
  ifelse(PCNL_visc_inj_imp_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                           "Yes"))

PCNL_visc_inj_imp_glm_tb <-
  table(pred = PCNL_visc_inj_imp_glm_predict,
        ref = visc_inj_na_omit_outcome)
confusionMatrix(PCNL_visc_inj_imp_glm_tb)
PCNL_visc_inj_imp_glm_roc <-
  roc(visc_inj_na_omit_outcome,
      PCNL_visc_inj_imp_glm_predict1)
visc_inj_imp_glm_auc <- auc(PCNL_visc_inj_imp_glm_roc)
print(visc_inj_imp_glm_auc)
print(ci.auc(PCNL_visc_inj_imp_glm_roc))
varImp(PCNL_visc_inj_imp_glm)
summary(PCNL_visc_inj_imp_glm)

### Imputed and Oversampled
PCNL_visc_inj_imp_oversample_glm_predict1 <-
  predict(PCNL_visc_inj_imp_oversample_glm,
          PCNL_visc_inj_omit_na_test)
PCNL_visc_inj_imp_oversample_glm_predict <-
  ifelse(PCNL_visc_inj_imp_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                                      "Yes"))

PCNL_visc_inj_imp_oversample_glm_tb <-
  table(pred = PCNL_visc_inj_imp_oversample_glm_predict,
        ref = visc_inj_na_omit_outcome)
confusionMatrix(PCNL_visc_inj_imp_oversample_glm_tb)
PCNL_visc_inj_imp_oversample_glm_roc <-
  roc(visc_inj_na_omit_outcome,
      PCNL_visc_inj_imp_oversample_glm_predict1)
visc_inj_imp_oversample_glm_auc <-
  auc(PCNL_visc_inj_imp_oversample_glm_roc)
print(visc_inj_imp_oversample_glm_auc)
print(ci.auc(PCNL_visc_inj_imp_oversample_glm_roc))
varImp(PCNL_visc_inj_imp_oversample_glm)
summary(PCNL_visc_inj_imp_oversample_glm)


### ROC
visc_inj_glm_roc_list <- list(
  PCNL_visc_inj_na_omit_glm_roc,
  PCNL_visc_inj_oversample_glm_roc,
  PCNL_visc_inj_imp_glm_roc,
  PCNL_visc_inj_imp_oversample_glm_roc
)

visc_inj_glm_ci_list <-
  lapply(visc_inj_glm_roc_list,
         ci.se,
         specificities = seq(0, 1, l = 25))

dat_visc_inj_glm_ci_list <-
  lapply(visc_inj_glm_ci_list, function(ciobj)
    data.frame(
      x = as.numeric(rownames(ciobj)),
      lower = ciobj[, 1],
      upper = ciobj[, 3]
    ))


visc_inj_glm_roc_combined <-
  ggroc(visc_inj_glm_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Visceral Injury",
    x = "Specificity",
    y = "Sensitivity",
    color = "Model"
  ) + scale_color_discrete(labels = c(
    "Original Dataset",
    "Oversampled",
    "Imputed",
    "Imputed and Oversampled"
  )) + geom_abline(slope = 1, intercept = 1)

for (i in 1:4) {
  visc_inj_glm_roc_combined <-
    visc_inj_glm_roc_combined + geom_ribbon(
      data = dat_visc_inj_glm_ci_list[[i]],
      aes(x = x, ymin = lower, ymax = upper),
      fill = i + 1,
      alpha = 0.2,
      inherit.aes = F
    )
}

visc_inj_glm_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"
    ),
    "CI" = rbind(
      ci.auc(PCNL_visc_inj_na_omit_glm_roc),
      ci.auc(PCNL_visc_inj_oversample_glm_roc),
      ci.auc(PCNL_visc_inj_imp_glm_roc),
      ci.auc(PCNL_visc_inj_imp_oversample_glm_roc)
    )
  ) %>% as_tibble()
colnames(visc_inj_glm_auc_data_overall) <-
  c("Model", "LB", "AUC", "UB")
visc_inj_glm_auc_data_overall$AUC <-
  as.numeric(visc_inj_glm_auc_data_overall$AUC) %>% round(digits = 2)
visc_inj_glm_auc_data_overall$LB <-
  as.numeric(visc_inj_glm_auc_data_overall$LB) %>% round(digits = 2)
visc_inj_glm_auc_data_overall$UB <-
  as.numeric(visc_inj_glm_auc_data_overall$UB) %>% round(digits = 2)
visc_inj_glm_auc_data_overall <-
  visc_inj_glm_auc_data_overall %>% relocate(AUC, .before = LB)

visc_inj_glm_auc_data_overall <-
  visc_inj_glm_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

visc_inj_glm_auc_table <-
  visc_inj_glm_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(visc_inj_glm_roc_combined))
visc_inj_glm_auc_table_vp <- viewport(
  x = 0.70,
  y = 0.22,
  just = c("right", "bottom"),
  height = 0.1,
  width = 0.2
)
pushViewport(visc_inj_glm_auc_table_vp)
grid.draw(visc_inj_glm_auc_table)

popViewport()



#clearance_on_post_operative_radiological_imaging_during_a
clearance_during_admission_na_omit_outcome <-
PCNL_clearance_during_admission_omit_na_test$clearance_on_post_operative_radiological_imaging_during_a %>% factor(levels = c("No",
                                                                                                                             "Yes"))

clearance_during_admission_oversample_outcome <-
 PCNL_clearance_during_admission_omit_na_test$clearance_on_post_operative_radiological_imaging_during_a %>% factor(levels = c("No",
                                                                                                                                      "Yes"))

## NA Omit
PCNL_clearance_during_admission_na_omit_glm_predict1 <-
  predict(PCNL_clearance_during_admission_glm,
          PCNL_clearance_during_admission_omit_na_test)
PCNL_clearance_during_admission_na_omit_glm_predict <-
  ifelse(PCNL_clearance_during_admission_na_omit_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                    "Yes"))

PCNL_clearance_during_admission_na_omit_glm_tb <-
  table(
    pred = PCNL_clearance_during_admission_na_omit_glm_predict,
    ref = clearance_during_admission_na_omit_outcome
  )
confusionMatrix(PCNL_clearance_during_admission_na_omit_glm_tb)
PCNL_clearance_during_admission_na_omit_glm_roc <-
  roc(
    clearance_during_admission_na_omit_outcome,
    PCNL_clearance_during_admission_na_omit_glm_predict1)
clearance_during_admission_na_omit_glm_auc<-auc(PCNL_clearance_during_admission_na_omit_glm_roc)
print(clearance_during_admission_na_omit_glm_auc)
print(ci.auc(PCNL_clearance_during_admission_na_omit_glm_roc))
varImp(PCNL_clearance_during_admission_glm)
summary(PCNL_clearance_during_admission_glm)

### Oversampled
PCNL_clearance_during_admission_oversample_glm_predict1 <-
  predict(PCNL_clearance_during_admission_oversample_glm,
          PCNL_clearance_during_admission_omit_na_test)
PCNL_clearance_during_admission_oversample_glm_predict <-
  ifelse(PCNL_clearance_during_admission_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                       "Yes"))

PCNL_clearance_during_admission_oversample_glm_tb <-
  table(pred = PCNL_clearance_during_admission_oversample_glm_predict,
        ref = clearance_during_admission_na_omit_outcome)
confusionMatrix(PCNL_clearance_during_admission_oversample_glm_tb)
PCNL_clearance_during_admission_oversample_glm_roc <-
  roc(clearance_during_admission_oversample_outcome,
      PCNL_clearance_during_admission_oversample_glm_predict1)
clearance_during_admission_oversample_glm_auc <-
  auc(PCNL_clearance_during_admission_oversample_glm_roc)
print(clearance_during_admission_oversample_glm_auc)
print(ci.auc(PCNL_clearance_during_admission_oversample_glm_roc))
varImp(PCNL_clearance_during_admission_oversample_glm)
summary(PCNL_clearance_during_admission_oversample_glm)


### Imputed
PCNL_clearance_during_admission_imp_glm_predict1 <-
  predict(PCNL_clearance_during_admission_imp_glm,
          PCNL_clearance_during_admission_omit_na_test)
PCNL_clearance_during_admission_imp_glm_predict <-
  ifelse(PCNL_clearance_during_admission_imp_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                "Yes"))

PCNL_clearance_during_admission_imp_glm_tb <-
  table(pred = PCNL_clearance_during_admission_imp_glm_predict,
        ref = clearance_during_admission_na_omit_outcome)
confusionMatrix(PCNL_clearance_during_admission_imp_glm_tb)
PCNL_clearance_during_admission_imp_glm_roc <-
  roc(clearance_during_admission_na_omit_outcome,
      PCNL_clearance_during_admission_imp_glm_predict1)
clearance_during_admission_imp_glm_auc <- auc(PCNL_clearance_during_admission_imp_glm_roc)
print(clearance_during_admission_imp_glm_auc)
print(ci.auc(PCNL_clearance_during_admission_imp_glm_roc))
varImp(PCNL_clearance_during_admission_imp_glm)
summary(PCNL_clearance_during_admission_imp_glm)

### Imputed and Oversampled
PCNL_clearance_during_admission_imp_oversample_glm_predict1 <-
  predict(PCNL_clearance_during_admission_imp_oversample_glm,
          PCNL_clearance_during_admission_omit_na_test)
PCNL_clearance_during_admission_imp_oversample_glm_predict <-
  ifelse(PCNL_clearance_during_admission_imp_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                           "Yes"))

PCNL_clearance_during_admission_imp_oversample_glm_tb <-
  table(pred = PCNL_clearance_during_admission_imp_oversample_glm_predict,
        ref = clearance_during_admission_na_omit_outcome)
confusionMatrix(PCNL_clearance_during_admission_imp_oversample_glm_tb)
PCNL_clearance_during_admission_imp_oversample_glm_roc <-
  roc(clearance_during_admission_na_omit_outcome,
      PCNL_clearance_during_admission_imp_oversample_glm_predict1)
clearance_during_admission_imp_oversample_glm_auc <-
  auc(PCNL_clearance_during_admission_imp_oversample_glm_roc)
print(clearance_during_admission_imp_oversample_glm_auc)
print(ci.auc(PCNL_clearance_during_admission_imp_oversample_glm_roc))
varImp(PCNL_clearance_during_admission_imp_oversample_glm)
summary(PCNL_clearance_during_admission_imp_oversample_glm)


### ROC
clearance_during_admission_glm_roc_list <- list(
  PCNL_clearance_during_admission_na_omit_glm_roc,
  PCNL_clearance_during_admission_oversample_glm_roc,
  PCNL_clearance_during_admission_imp_glm_roc,
  PCNL_clearance_during_admission_imp_oversample_glm_roc
)

clearance_during_admission_glm_ci_list <-
  lapply(clearance_during_admission_glm_roc_list,
         ci.se,
         specificities = seq(0, 1, l = 25))

dat_clearance_during_admission_glm_ci_list <-
  lapply(clearance_during_admission_glm_ci_list, function(ciobj)
    data.frame(
      x = as.numeric(rownames(ciobj)),
      lower = ciobj[, 1],
      upper = ciobj[, 3]
    ))


clearance_during_admission_glm_roc_combined <-
  ggroc(clearance_during_admission_glm_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Clearance on Post-Operative, Inpatient Imaging",
    x = "Specificity",
    y = "Sensitivity",
    color = "Model"
  ) + scale_color_discrete(labels = c(
    "Original Dataset",
    "Oversampled",
    "Imputed",
    "Imputed and Oversampled"
  )) + geom_abline(slope = 1, intercept = 1)

for (i in 1:4) {
  clearance_during_admission_glm_roc_combined <-
    clearance_during_admission_glm_roc_combined + geom_ribbon(
      data = dat_clearance_during_admission_glm_ci_list[[i]],
      aes(x = x, ymin = lower, ymax = upper),
      fill = i + 1,
      alpha = 0.2,
      inherit.aes = F
    )
}

clearance_during_admission_glm_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"
    ),
    "CI" = rbind(
      ci.auc(PCNL_clearance_during_admission_na_omit_glm_roc),
      ci.auc(PCNL_clearance_during_admission_oversample_glm_roc),
      ci.auc(PCNL_clearance_during_admission_imp_glm_roc),
      ci.auc(PCNL_clearance_during_admission_imp_oversample_glm_roc)
    )
  ) %>% as_tibble()
colnames(clearance_during_admission_glm_auc_data_overall) <-
  c("Model", "LB", "AUC", "UB")
clearance_during_admission_glm_auc_data_overall$AUC <-
  as.numeric(clearance_during_admission_glm_auc_data_overall$AUC) %>% round(digits = 2)
clearance_during_admission_glm_auc_data_overall$LB <-
  as.numeric(clearance_during_admission_glm_auc_data_overall$LB) %>% round(digits = 2)
clearance_during_admission_glm_auc_data_overall$UB <-
  as.numeric(clearance_during_admission_glm_auc_data_overall$UB) %>% round(digits = 2)
clearance_during_admission_glm_auc_data_overall <-
  clearance_during_admission_glm_auc_data_overall %>% relocate(AUC, .before = LB)

clearance_during_admission_glm_auc_data_overall <-
  clearance_during_admission_glm_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

clearance_during_admission_glm_auc_table <-
  clearance_during_admission_glm_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(clearance_during_admission_glm_roc_combined))
clearance_during_admission_glm_auc_table_vp <- viewport(
  x = 0.70,
  y = 0.22,
  just = c("right", "bottom"),
  height = 0.1,
  width = 0.2
)
pushViewport(clearance_during_admission_glm_auc_table_vp)
grid.draw(clearance_during_admission_glm_auc_table)

popViewport()


#postop_complications
post_op_comp_na_omit_outcome <-
  PCNL_post_op_comp_omit_na_test$postop_complications %>% factor(levels = c("No",
                                                                            "Yes"))


post_op_comp_oversample_outcome <-
  PCNL_post_op_comp_omit_na_test$postop_complications %>% factor(levels = c("No",
                                                                            "Yes"))


## NA Omit
PCNL_post_op_comp_na_omit_glm_predict1 <-
  predict(PCNL_post_op_comp_glm,
          PCNL_post_op_comp_omit_na_test)
PCNL_post_op_comp_na_omit_glm_predict <-
  ifelse(PCNL_post_op_comp_na_omit_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                        "Yes"))

PCNL_post_op_comp_na_omit_glm_tb <-
  table(
    pred = PCNL_post_op_comp_na_omit_glm_predict,
    ref = post_op_comp_na_omit_outcome
  )
confusionMatrix(PCNL_post_op_comp_na_omit_glm_tb)
PCNL_post_op_comp_na_omit_glm_roc <-
  roc(
    post_op_comp_na_omit_outcome,
    PCNL_post_op_comp_na_omit_glm_predict1)
post_op_comp_na_omit_glm_auc<-auc(PCNL_post_op_comp_na_omit_glm_roc)
print(post_op_comp_na_omit_glm_auc)
print(ci.auc(PCNL_post_op_comp_na_omit_glm_roc))
varImp(PCNL_post_op_comp_glm)
summary(PCNL_post_op_comp_glm)

### Oversampled
PCNL_post_op_comp_oversample_glm_predict1 <-
  predict(PCNL_post_op_comp_oversample_glm,
          PCNL_post_op_comp_omit_na_test)
PCNL_post_op_comp_oversample_glm_predict <-
  ifelse(PCNL_post_op_comp_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                                         "Yes"))

PCNL_post_op_comp_oversample_glm_tb <-
  table(pred = PCNL_post_op_comp_oversample_glm_predict,
        ref = post_op_comp_na_omit_outcome)
confusionMatrix(PCNL_post_op_comp_oversample_glm_tb)
PCNL_post_op_comp_oversample_glm_roc <-
  roc(post_op_comp_oversample_outcome,
      PCNL_post_op_comp_oversample_glm_predict1)
post_op_comp_oversample_glm_auc <-
  auc(PCNL_post_op_comp_oversample_glm_roc)
print(post_op_comp_oversample_glm_auc)
print(ci.auc(PCNL_post_op_comp_oversample_glm_roc))
varImp(PCNL_post_op_comp_oversample_glm)
summary(PCNL_post_op_comp_oversample_glm)


### Imputed
PCNL_post_op_comp_imp_glm_predict1 <-
  predict(PCNL_post_op_comp_imp_glm,
          PCNL_post_op_comp_omit_na_test)
PCNL_post_op_comp_imp_glm_predict <-
  ifelse(PCNL_post_op_comp_imp_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                                  "Yes"))

PCNL_post_op_comp_imp_glm_tb <-
  table(pred = PCNL_post_op_comp_imp_glm_predict,
        ref = post_op_comp_na_omit_outcome)
confusionMatrix(PCNL_post_op_comp_imp_glm_tb)
PCNL_post_op_comp_imp_glm_roc <-
  roc(post_op_comp_na_omit_outcome,
      PCNL_post_op_comp_imp_glm_predict1)
post_op_comp_imp_glm_auc <- auc(PCNL_post_op_comp_imp_glm_roc)
print(post_op_comp_imp_glm_auc)
print(ci.auc(PCNL_post_op_comp_imp_glm_roc))
varImp(PCNL_post_op_comp_imp_glm)
summary(PCNL_post_op_comp_imp_glm)

### Imputed and Oversampled
PCNL_post_op_comp_imp_oversample_glm_predict1 <-
  predict(PCNL_post_op_comp_imp_oversample_glm,
          PCNL_post_op_comp_omit_na_test)
PCNL_post_op_comp_imp_oversample_glm_predict <-
  ifelse(PCNL_post_op_comp_imp_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                                             "Yes"))

PCNL_post_op_comp_imp_oversample_glm_tb <-
  table(pred = PCNL_post_op_comp_imp_oversample_glm_predict,
        ref = post_op_comp_na_omit_outcome)
confusionMatrix(PCNL_post_op_comp_imp_oversample_glm_tb)
PCNL_post_op_comp_imp_oversample_glm_roc <-
  roc(post_op_comp_na_omit_outcome,
      PCNL_post_op_comp_imp_oversample_glm_predict1)
post_op_comp_imp_oversample_glm_auc <-
  auc(PCNL_post_op_comp_imp_oversample_glm_roc)
print(post_op_comp_imp_oversample_glm_auc)
print(ci.auc(PCNL_post_op_comp_imp_oversample_glm_roc))
varImp(PCNL_post_op_comp_imp_oversample_glm)
summary(PCNL_post_op_comp_imp_oversample_glm)


### ROC
post_op_comp_glm_roc_list <- list(
  PCNL_post_op_comp_na_omit_glm_roc,
  PCNL_post_op_comp_oversample_glm_roc,
  PCNL_post_op_comp_imp_glm_roc,
  PCNL_post_op_comp_imp_oversample_glm_roc
)

post_op_comp_glm_ci_list <-
  lapply(post_op_comp_glm_roc_list,
         ci.se,
         specificities = seq(0, 1, l = 25))

dat_post_op_comp_glm_ci_list <-
  lapply(post_op_comp_glm_ci_list, function(ciobj)
    data.frame(
      x = as.numeric(rownames(ciobj)),
      lower = ciobj[, 1],
      upper = ciobj[, 3]
    ))


post_op_comp_glm_roc_combined <-
  ggroc(post_op_comp_glm_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Post-Operative Complications (Any)",
    x = "Specificity",
    y = "Sensitivity",
    color = "Model"
  ) + scale_color_discrete(labels = c(
    "Original Dataset",
    "Oversampled",
    "Imputed",
    "Imputed and Oversampled"
  )) + geom_abline(slope = 1, intercept = 1)

for (i in 1:4) {
  post_op_comp_glm_roc_combined <-
    post_op_comp_glm_roc_combined + geom_ribbon(
      data = dat_post_op_comp_glm_ci_list[[i]],
      aes(x = x, ymin = lower, ymax = upper),
      fill = i + 1,
      alpha = 0.2,
      inherit.aes = F
    )
}

post_op_comp_glm_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"
    ),
    "CI" = rbind(
      ci.auc(PCNL_post_op_comp_na_omit_glm_roc),
      ci.auc(PCNL_post_op_comp_oversample_glm_roc),
      ci.auc(PCNL_post_op_comp_imp_glm_roc),
      ci.auc(PCNL_post_op_comp_imp_oversample_glm_roc)
    )
  ) %>% as_tibble()
colnames(post_op_comp_glm_auc_data_overall) <-
  c("Model", "LB", "AUC", "UB")
post_op_comp_glm_auc_data_overall$AUC <-
  as.numeric(post_op_comp_glm_auc_data_overall$AUC) %>% round(digits = 2)
post_op_comp_glm_auc_data_overall$LB <-
  as.numeric(post_op_comp_glm_auc_data_overall$LB) %>% round(digits = 2)
post_op_comp_glm_auc_data_overall$UB <-
  as.numeric(post_op_comp_glm_auc_data_overall$UB) %>% round(digits = 2)
post_op_comp_glm_auc_data_overall <-
  post_op_comp_glm_auc_data_overall %>% relocate(AUC, .before = LB)

post_op_comp_glm_auc_data_overall <-
  post_op_comp_glm_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

post_op_comp_glm_auc_table <-
  post_op_comp_glm_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(post_op_comp_glm_roc_combined))
post_op_comp_glm_auc_table_vp <- viewport(
  x = 0.70,
  y = 0.22,
  just = c("right", "bottom"),
  height = 0.1,
  width = 0.2
)
pushViewport(post_op_comp_glm_auc_table_vp)
grid.draw(post_op_comp_glm_auc_table)

popViewport()

#stone_free_at_follow_up
sf_at_fu_na_omit_outcome <-
  PCNL_sf_at_fu_omit_na_test$stone_free_at_follow_up %>% factor(levels = c("No",
                                                                            "Yes"))


sf_at_fu_oversample_outcome <-
  PCNL_sf_at_fu_omit_na_test$stone_free_at_follow_up %>% factor(levels = c("No",
                                                                            "Yes"))


## NA Omit
PCNL_sf_at_fu_na_omit_glm_predict1 <-
  predict(PCNL_sf_at_fu_glm,
          PCNL_sf_at_fu_omit_na_test)
PCNL_sf_at_fu_na_omit_glm_predict <-
  ifelse(PCNL_sf_at_fu_na_omit_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                        "Yes"))

PCNL_sf_at_fu_na_omit_glm_tb <-
  table(
    pred = PCNL_sf_at_fu_na_omit_glm_predict,
    ref = sf_at_fu_na_omit_outcome
  )
confusionMatrix(PCNL_sf_at_fu_na_omit_glm_tb)
PCNL_sf_at_fu_na_omit_glm_roc <-
  roc(
    sf_at_fu_na_omit_outcome,
    PCNL_sf_at_fu_na_omit_glm_predict1)
sf_at_fu_na_omit_glm_auc<-auc(PCNL_sf_at_fu_na_omit_glm_roc)
print(sf_at_fu_na_omit_glm_auc)
print(ci.auc(PCNL_sf_at_fu_na_omit_glm_roc))
varImp(PCNL_sf_at_fu_glm)
summary(PCNL_sf_at_fu_glm)

### Oversampled
PCNL_sf_at_fu_oversample_glm_predict1 <-
  predict(PCNL_sf_at_fu_oversample_glm,
          PCNL_sf_at_fu_omit_na_test)
PCNL_sf_at_fu_oversample_glm_predict <-
  ifelse(PCNL_sf_at_fu_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                           "Yes"))

PCNL_sf_at_fu_oversample_glm_tb <-
  table(pred = PCNL_sf_at_fu_oversample_glm_predict,
        ref = sf_at_fu_na_omit_outcome)
confusionMatrix(PCNL_sf_at_fu_oversample_glm_tb)
PCNL_sf_at_fu_oversample_glm_roc <-
  roc(sf_at_fu_oversample_outcome,
      PCNL_sf_at_fu_oversample_glm_predict1)
sf_at_fu_oversample_glm_auc <-
  auc(PCNL_sf_at_fu_oversample_glm_roc)
print(sf_at_fu_oversample_glm_auc)
print(ci.auc(PCNL_sf_at_fu_oversample_glm_roc))
varImp(PCNL_sf_at_fu_oversample_glm)
summary(PCNL_sf_at_fu_oversample_glm)


### Imputed
PCNL_sf_at_fu_imp_glm_predict1 <-
  predict(PCNL_sf_at_fu_imp_glm,
          PCNL_sf_at_fu_omit_na_test)
PCNL_sf_at_fu_imp_glm_predict <-
  ifelse(PCNL_sf_at_fu_imp_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                    "Yes"))

PCNL_sf_at_fu_imp_glm_tb <-
  table(pred = PCNL_sf_at_fu_imp_glm_predict,
        ref = sf_at_fu_na_omit_outcome)
confusionMatrix(PCNL_sf_at_fu_imp_glm_tb)
PCNL_sf_at_fu_imp_glm_roc <-
  roc(sf_at_fu_na_omit_outcome,
      PCNL_sf_at_fu_imp_glm_predict1)
sf_at_fu_imp_glm_auc <- auc(PCNL_sf_at_fu_imp_glm_roc)
print(sf_at_fu_imp_glm_auc)
print(ci.auc(PCNL_sf_at_fu_imp_glm_roc))
varImp(PCNL_sf_at_fu_imp_glm)
summary(PCNL_sf_at_fu_imp_glm)

### Imputed and Oversampled
PCNL_sf_at_fu_imp_oversample_glm_predict1 <-
  predict(PCNL_sf_at_fu_imp_oversample_glm,
          PCNL_sf_at_fu_omit_na_test)
PCNL_sf_at_fu_imp_oversample_glm_predict <-
  ifelse(PCNL_sf_at_fu_imp_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                               "Yes"))

PCNL_sf_at_fu_imp_oversample_glm_tb <-
  table(pred = PCNL_sf_at_fu_imp_oversample_glm_predict,
        ref = sf_at_fu_na_omit_outcome)
confusionMatrix(PCNL_sf_at_fu_imp_oversample_glm_tb)
PCNL_sf_at_fu_imp_oversample_glm_roc <-
  roc(sf_at_fu_na_omit_outcome,
      PCNL_sf_at_fu_imp_oversample_glm_predict1)
sf_at_fu_imp_oversample_glm_auc <-
  auc(PCNL_sf_at_fu_imp_oversample_glm_roc)
print(sf_at_fu_imp_oversample_glm_auc)
print(ci.auc(PCNL_sf_at_fu_imp_oversample_glm_roc))
varImp(PCNL_sf_at_fu_imp_oversample_glm)
summary(PCNL_sf_at_fu_imp_oversample_glm)


### ROC
sf_at_fu_glm_roc_list <- list(
  PCNL_sf_at_fu_na_omit_glm_roc,
  PCNL_sf_at_fu_oversample_glm_roc,
  PCNL_sf_at_fu_imp_glm_roc,
  PCNL_sf_at_fu_imp_oversample_glm_roc
)

sf_at_fu_glm_ci_list <-
  lapply(sf_at_fu_glm_roc_list,
         ci.se,
         specificities = seq(0, 1, l = 25))

dat_sf_at_fu_glm_ci_list <-
  lapply(sf_at_fu_glm_ci_list, function(ciobj)
    data.frame(
      x = as.numeric(rownames(ciobj)),
      lower = ciobj[, 1],
      upper = ciobj[, 3]
    ))


sf_at_fu_glm_roc_combined <-
  ggroc(sf_at_fu_glm_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Stone Free at Follow-up (Clinician Defined, Any Imaging)",
    x = "Specificity",
    y = "Sensitivity",
    color = "Model"
  ) + scale_color_discrete(labels = c(
    "Original Dataset",
    "Oversampled",
    "Imputed",
    "Imputed and Oversampled"
  )) + geom_abline(slope = 1, intercept = 1)

for (i in 1:4) {
  sf_at_fu_glm_roc_combined <-
    sf_at_fu_glm_roc_combined + geom_ribbon(
      data = dat_sf_at_fu_glm_ci_list[[i]],
      aes(x = x, ymin = lower, ymax = upper),
      fill = i + 1,
      alpha = 0.2,
      inherit.aes = F
    )
}

sf_at_fu_glm_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"
    ),
    "CI" = rbind(
      ci.auc(PCNL_sf_at_fu_na_omit_glm_roc),
      ci.auc(PCNL_sf_at_fu_oversample_glm_roc),
      ci.auc(PCNL_sf_at_fu_imp_glm_roc),
      ci.auc(PCNL_sf_at_fu_imp_oversample_glm_roc)
    )
  ) %>% as_tibble()
colnames(sf_at_fu_glm_auc_data_overall) <-
  c("Model", "LB", "AUC", "UB")
sf_at_fu_glm_auc_data_overall$AUC <-
  as.numeric(sf_at_fu_glm_auc_data_overall$AUC) %>% round(digits = 2)
sf_at_fu_glm_auc_data_overall$LB <-
  as.numeric(sf_at_fu_glm_auc_data_overall$LB) %>% round(digits = 2)
sf_at_fu_glm_auc_data_overall$UB <-
  as.numeric(sf_at_fu_glm_auc_data_overall$UB) %>% round(digits = 2)
sf_at_fu_glm_auc_data_overall <-
  sf_at_fu_glm_auc_data_overall %>% relocate(AUC, .before = LB)

sf_at_fu_glm_auc_data_overall <-
  sf_at_fu_glm_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

sf_at_fu_glm_auc_table <-
  sf_at_fu_glm_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(sf_at_fu_glm_roc_combined))
sf_at_fu_glm_auc_table_vp <- viewport(
  x = 0.70,
  y = 0.22,
  just = c("right", "bottom"),
  height = 0.1,
  width = 0.2
)
pushViewport(sf_at_fu_glm_auc_table_vp)
grid.draw(sf_at_fu_glm_auc_table)

popViewport()


#adjuvant_treatment
adj_rx_na_omit_outcome <-
  PCNL_adj_rx_omit_na_test$adjuvant_treatment  %>% factor(levels = c("No",
                                                                            "Yes"))
adj_rx_oversample_outcome <-
  PCNL_adj_rx_omit_na_test$adjuvant_treatment %>% factor(levels = c("No",
                                                                     "Yes"))

### Omit NA only
PCNL_adj_rx_na_omit_glm_predict1 <-
  predict(PCNL_adj_rx_glm,
          PCNL_adj_rx_omit_na_test)
PCNL_adj_rx_na_omit_glm_predict <-
  ifelse(PCNL_adj_rx_na_omit_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                    "Yes"))

PCNL_adj_rx_na_omit_glm_tb <-
  table(
    pred = PCNL_adj_rx_na_omit_glm_predict,
    ref = adj_rx_na_omit_outcome
  )
confusionMatrix(PCNL_adj_rx_na_omit_glm_tb)
PCNL_adj_rx_na_omit_glm_roc <-
  roc(
    adj_rx_na_omit_outcome,
    PCNL_adj_rx_na_omit_glm_predict1)
adj_rx_na_omit_glm_auc<-auc(PCNL_adj_rx_na_omit_glm_roc)
print(adj_rx_na_omit_glm_auc)
print(ci.auc(PCNL_adj_rx_na_omit_glm_roc))
varImp(PCNL_adj_rx_glm)
summary(PCNL_adj_rx_glm)

### Oversampled
PCNL_adj_rx_oversample_glm_predict1 <-
  predict(PCNL_adj_rx_oversample_glm,
          PCNL_adj_rx_omit_na_test)
PCNL_adj_rx_oversample_glm_predict <-
  ifelse(PCNL_adj_rx_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                       "Yes"))

PCNL_adj_rx_oversample_glm_tb <-
  table(pred = PCNL_adj_rx_oversample_glm_predict,
        ref = adj_rx_na_omit_outcome)
confusionMatrix(PCNL_adj_rx_oversample_glm_tb)
PCNL_adj_rx_oversample_glm_roc <-
  roc(adj_rx_oversample_outcome,
      PCNL_adj_rx_oversample_glm_predict1)
adj_rx_oversample_glm_auc <-
  auc(PCNL_adj_rx_oversample_glm_roc)
print(adj_rx_oversample_glm_auc)
print(ci.auc(PCNL_adj_rx_oversample_glm_roc))
varImp(PCNL_adj_rx_oversample_glm)
summary(PCNL_adj_rx_oversample_glm)


### Imputed
PCNL_adj_rx_imp_glm_predict1 <-
  predict(PCNL_adj_rx_imp_glm,
          PCNL_adj_rx_omit_na_test)
PCNL_adj_rx_imp_glm_predict <-
  ifelse(PCNL_adj_rx_imp_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                "Yes"))

PCNL_adj_rx_imp_glm_tb <-
  table(pred = PCNL_adj_rx_imp_glm_predict,
        ref = adj_rx_na_omit_outcome)
confusionMatrix(PCNL_adj_rx_imp_glm_tb)
PCNL_adj_rx_imp_glm_roc <-
  roc(adj_rx_na_omit_outcome,
      PCNL_adj_rx_imp_glm_predict1)
adj_rx_imp_glm_auc <- auc(PCNL_adj_rx_imp_glm_roc)
print(adj_rx_imp_glm_auc)
print(ci.auc(PCNL_adj_rx_imp_glm_roc))
varImp(PCNL_adj_rx_imp_glm)
summary(PCNL_adj_rx_imp_glm)

### Imputed and Oversampled
PCNL_adj_rx_imp_oversample_glm_predict1 <-
  predict(PCNL_adj_rx_imp_oversample_glm,
          PCNL_adj_rx_omit_na_test)
PCNL_adj_rx_imp_oversample_glm_predict <-
  ifelse(PCNL_adj_rx_imp_oversample_glm_predict1 > 0, "Yes", "No") %>% factor(levels = c("No",
                                                                                           "Yes"))

PCNL_adj_rx_imp_oversample_glm_tb <-
  table(pred = PCNL_adj_rx_imp_oversample_glm_predict,
        ref = adj_rx_na_omit_outcome)
confusionMatrix(PCNL_adj_rx_imp_oversample_glm_tb)
PCNL_adj_rx_imp_oversample_glm_roc <-
  roc(adj_rx_na_omit_outcome,
      PCNL_adj_rx_imp_oversample_glm_predict1)
adj_rx_imp_oversample_glm_auc <-
  auc(PCNL_adj_rx_imp_oversample_glm_roc)
print(adj_rx_imp_oversample_glm_auc)
print(ci.auc(PCNL_adj_rx_imp_oversample_glm_roc))
varImp(PCNL_adj_rx_imp_oversample_glm)
summary(PCNL_adj_rx_imp_oversample_glm)


### ROC
adj_rx_glm_roc_list <- list(
  PCNL_adj_rx_na_omit_glm_roc,
  PCNL_adj_rx_oversample_glm_roc,
  PCNL_adj_rx_imp_glm_roc,
  PCNL_adj_rx_imp_oversample_glm_roc
)

adj_rx_glm_ci_list <-
  lapply(adj_rx_glm_roc_list,
         ci.se,
         specificities = seq(0, 1, l = 25))

dat_adj_rx_glm_ci_list <-
  lapply(adj_rx_glm_ci_list, function(ciobj)
    data.frame(
      x = as.numeric(rownames(ciobj)),
      lower = ciobj[, 1],
      upper = ciobj[, 3]
    ))


adj_rx_glm_roc_combined <-
  ggroc(adj_rx_glm_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Need for Adjuvant Treatment",
    x = "Specificity",
    y = "Sensitivity",
    color = "Model"
  ) + scale_color_discrete(labels = c(
    "Original Dataset",
    "Oversampled",
    "Imputed",
    "Imputed and Oversampled"
  )) + geom_abline(slope = 1, intercept = 1)

for (i in 1:4) {
  adj_rx_glm_roc_combined <-
    adj_rx_glm_roc_combined + geom_ribbon(
      data = dat_adj_rx_glm_ci_list[[i]],
      aes(x = x, ymin = lower, ymax = upper),
      fill = i + 1,
      alpha = 0.2,
      inherit.aes = F
    )
}

adj_rx_glm_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"
    ),
    "CI" = rbind(
      ci.auc(PCNL_adj_rx_na_omit_glm_roc),
      ci.auc(PCNL_adj_rx_oversample_glm_roc),
      ci.auc(PCNL_adj_rx_imp_glm_roc),
      ci.auc(PCNL_adj_rx_imp_oversample_glm_roc)
    )
  ) %>% as_tibble()
colnames(adj_rx_glm_auc_data_overall) <-
  c("Model", "LB", "AUC", "UB")
adj_rx_glm_auc_data_overall$AUC <-
  as.numeric(adj_rx_glm_auc_data_overall$AUC) %>% round(digits = 2)
adj_rx_glm_auc_data_overall$LB <-
  as.numeric(adj_rx_glm_auc_data_overall$LB) %>% round(digits = 2)
adj_rx_glm_auc_data_overall$UB <-
  as.numeric(adj_rx_glm_auc_data_overall$UB) %>% round(digits = 2)
adj_rx_glm_auc_data_overall <-
  adj_rx_glm_auc_data_overall %>% relocate(AUC, .before = LB)

adj_rx_glm_auc_data_overall <-
  adj_rx_glm_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

adj_rx_glm_auc_table <-
  adj_rx_glm_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(adj_rx_glm_roc_combined))
adj_rx_glm_auc_table_vp <- viewport(
  x = 0.70,
  y = 0.22,
  just = c("right", "bottom"),
  height = 0.1,
  width = 0.2
)
pushViewport(adj_rx_glm_auc_table_vp)
grid.draw(adj_rx_glm_auc_table)

popViewport()



#intraop_complications