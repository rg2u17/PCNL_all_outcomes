#single_infection_outcome 
single_infection_na_omit_outcome_keras1 <- as_tibble(PCNL_single_infection_omit_na_test_outcome)
single_infection_na_omit_outcome_keras <- single_infection_na_omit_outcome_keras1$single_infection_outcome

single_infection_oversample_outcome <-
  ifelse(PCNL_single_infection_omit_na_test$single_infection_outcome == "Yes",
         "1",
         "0") 

### Omit NA only
PCNL_Post_Infection_keras_results <-
  PCNL_Post_Infection_keras %>% evaluate(PCNL_single_infection_omit_na_test_predictors2, single_infection_na_omit_outcome_keras)
PCNL_Post_Infection_keras_prediction <-
  PCNL_Post_Infection_keras %>% predict(PCNL_single_infection_omit_na_test_predictors2) %>% as.numeric()
PCNL_Post_Infection_keras_prediction1 <-
  round(PCNL_Post_Infection_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_Post_Infection_keras_tb <-
  table(single_infection_na_omit_outcome_keras, 
        PCNL_Post_Infection_keras_prediction1)
PCNL_single_infection_na_omit_keras_roc <-
  roc(single_infection_na_omit_outcome_keras, 
      PCNL_Post_Infection_keras_prediction)
auc(PCNL_single_infection_na_omit_keras_roc)
ci.auc(PCNL_single_infection_na_omit_keras_roc)
confusionMatrix(PCNL_Post_Infection_keras_tb) 
plot(PCNL_single_infection_na_omit_keras_roc)


### Oversampled
PCNL_Post_Infection_keras_oversample_results <-
  PCNL_Post_Infection_oversample_keras %>% evaluate(
    PCNL_single_infection_omit_na_test_predictors2,
    single_infection_na_omit_outcome_keras
  )
PCNL_Post_Infection_keras_oversample_prediction <-
  PCNL_Post_Infection_oversample_keras %>% predict(PCNL_single_infection_omit_na_test_predictors2) %>% as.numeric()
PCNL_Post_Infection_keras_oversample_prediction1 <-
  round(PCNL_Post_Infection_keras_oversample_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_Post_Infection_keras_oversample_tb <-
  table(
    single_infection_na_omit_outcome_keras,
    PCNL_Post_Infection_keras_oversample_prediction1
  )
PCNL_single_infection_oversample_keras_roc <-
  roc(
    single_infection_na_omit_outcome_keras,
    PCNL_Post_Infection_keras_oversample_prediction
  )
auc(PCNL_single_infection_oversample_keras_roc)
ci.auc(PCNL_single_infection_oversample_keras_roc)
confusionMatrix(PCNL_Post_Infection_keras_oversample_tb)
plot(PCNL_single_infection_oversample_keras_roc)


### Imputed
PCNL_Post_Infection_keras_imp_results <-
  PCNL_Post_Infection_imp_keras %>% evaluate(PCNL_single_infection_omit_na_test_predictors2, single_infection_na_omit_outcome_keras)
PCNL_Post_Infection_keras_imp_prediction <-
  PCNL_Post_Infection_imp_keras %>% predict(PCNL_single_infection_omit_na_test_predictors2) %>% as.numeric()
PCNL_Post_Infection_keras_imp_prediction1 <-
  round(PCNL_Post_Infection_keras_imp_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_Post_Infection_keras_imp_tb <-
  table(single_infection_na_omit_outcome_keras, 
        PCNL_Post_Infection_keras_imp_prediction1)
PCNL_single_infection_imp_keras_roc <-
  roc(single_infection_na_omit_outcome_keras, 
      PCNL_Post_Infection_keras_imp_prediction)
auc(PCNL_single_infection_imp_keras_roc)
ci.auc(PCNL_single_infection_imp_keras_roc)
confusionMatrix(PCNL_Post_Infection_keras_imp_tb) 
plot(PCNL_single_infection_imp_keras_roc)

### Imputed and Oversampled
PCNL_Post_Infection_keras_imp_oversample_results <-
  PCNL_Post_Infection_imp_oversample_keras %>% evaluate(
    PCNL_single_infection_omit_na_test_predictors2,
    single_infection_na_omit_outcome_keras
  )
PCNL_Post_Infection_keras_imp_oversample_prediction <-
  PCNL_Post_Infection_imp_oversample_keras %>% predict(PCNL_single_infection_omit_na_test_predictors2) %>% as.numeric()
PCNL_Post_Infection_keras_imp_oversample_prediction1 <-
  round(PCNL_Post_Infection_keras_imp_oversample_prediction,
        digits = 0) %>% factor(levels = c("0", "1"))
PCNL_Post_Infection_keras_imp_oversample_tb <-
  table(
    single_infection_na_omit_outcome_keras,
    PCNL_Post_Infection_keras_imp_oversample_prediction1
  )
PCNL_single_infection_imp_oversample_keras_roc <-
  roc(
    single_infection_na_omit_outcome_keras,
    PCNL_Post_Infection_keras_imp_oversample_prediction
  )
auc(PCNL_single_infection_imp_oversample_keras_roc)
ci.auc(PCNL_single_infection_imp_oversample_keras_roc)
confusionMatrix(PCNL_Post_Infection_keras_imp_oversample_tb)
plot(PCNL_single_infection_imp_oversample_keras_roc)


### ROC
infection_keras_roc_list<-list(PCNL_single_infection_na_omit_keras_roc,
                         PCNL_single_infection_oversample_keras_roc,
                         PCNL_single_infection_imp_keras_roc,
                         PCNL_single_infection_imp_oversample_keras_roc)

infection_keras_ci_list <-
  lapply(infection_keras_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_infection_keras_ci_list <- lapply(infection_keras_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


post_infection_keras_roc_combined <-
  ggroc(infection_keras_roc_list, aes = c("color")) + theme_minimal() + labs(
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
  post_infection_keras_roc_combined <- post_infection_keras_roc_combined + geom_ribbon(
    data = dat_infection_keras_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

infection_keras_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_single_infection_na_omit_keras_roc),
      ci.auc(PCNL_single_infection_oversample_keras_roc),
      ci.auc(PCNL_single_infection_imp_keras_roc),
      ci.auc(PCNL_single_infection_imp_oversample_keras_roc)
    )
  ) %>% as_tibble()
colnames(infection_keras_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
infection_keras_auc_data_overall$AUC <- as.numeric(infection_keras_auc_data_overall$AUC) %>% round(digits = 2)
infection_keras_auc_data_overall$LB <- as.numeric(infection_keras_auc_data_overall$LB) %>% round(digits = 2)
infection_keras_auc_data_overall$UB <- as.numeric(infection_keras_auc_data_overall$UB) %>% round(digits = 2)
infection_keras_auc_data_overall <-infection_keras_auc_data_overall %>% relocate(AUC, .before = LB) 

infection_keras_auc_data_overall <- infection_keras_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

infection_keras_auc_table <- infection_keras_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(post_infection_keras_roc_combined))
infection_keras_auc_table_vp <- viewport(x = 0.70, y = 0.22, 
                         just = c("right", "bottom"),
                         height = 0.1, width = 0.2)
pushViewport(infection_keras_auc_table_vp)
grid.draw(infection_keras_auc_table)

popViewport()

#blood_transfusion
transfusion_na_omit_outcome <-
  ifelse(PCNL_transfusion_omit_na_test$blood_transfusion == "Yes",
         "1",
         "0") %>% as.numeric()

transfusion_oversample_outcome <-
  ifelse(PCNL_transfusion_omit_na_test$blood_transfusion == "Yes",
         "1",
         "0") %>% as.numeric()

### Omit NA only
PCNL_transfusion_keras_results <-
  PCNL_transfusion_keras %>% evaluate(PCNL_transfusion_omit_na_test_predictors2, transfusion_na_omit_outcome)
PCNL_transfusion_keras_prediction <-
  PCNL_transfusion_keras %>% predict(PCNL_transfusion_omit_na_test_predictors2) %>% as.numeric()
PCNL_transfusion_keras_prediction1 <-
  round(PCNL_transfusion_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_transfusion_keras_tb <-
  table(transfusion_na_omit_outcome, 
        PCNL_transfusion_keras_prediction1)
PCNL_transfusion_na_omit_keras_roc <-
  roc(transfusion_na_omit_outcome, 
      PCNL_transfusion_keras_prediction)
auc(PCNL_transfusion_na_omit_keras_roc)
ci.auc(PCNL_transfusion_na_omit_keras_roc)
confusionMatrix(PCNL_transfusion_keras_tb) 
plot(PCNL_transfusion_na_omit_keras_roc)


### Oversampled
PCNL_transfusion_keras_oversample_results <-
  PCNL_transfusion_oversample_keras %>% evaluate(PCNL_transfusion_omit_na_test_predictors2, transfusion_na_omit_outcome)
PCNL_transfusion_keras_oversample_prediction <-
  PCNL_transfusion_oversample_keras %>% predict(PCNL_transfusion_omit_na_test_predictors2) %>% as.numeric()
PCNL_transfusion_keras_oversample_prediction1 <-
  round(PCNL_transfusion_keras_oversample_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_transfusion_keras_oversample_tb <-
  table(transfusion_na_omit_outcome, 
        PCNL_transfusion_keras_oversample_prediction1)
PCNL_transfusion_oversample_keras_roc <-
  roc(transfusion_na_omit_outcome, 
      PCNL_transfusion_keras_oversample_prediction)
auc(PCNL_transfusion_oversample_keras_roc)
ci.auc(PCNL_transfusion_oversample_keras_roc)
confusionMatrix(PCNL_transfusion_keras_oversample_tb) 
plot(PCNL_transfusion_oversample_keras_roc)


### Imputed
PCNL_transfusion_keras_imp_results <-
  PCNL_transfusion_imp_keras %>% evaluate(PCNL_transfusion_omit_na_test_predictors2, transfusion_na_omit_outcome)
PCNL_transfusion_keras_imp_prediction <-
  PCNL_transfusion_imp_keras %>% predict(PCNL_transfusion_omit_na_test_predictors2) %>% as.numeric()
PCNL_transfusion_keras_imp_prediction1 <-
  round(PCNL_transfusion_keras_imp_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_transfusion_keras_imp_tb <-
  table(transfusion_na_omit_outcome, 
        PCNL_transfusion_keras_imp_prediction1)
PCNL_transfusion_imp_keras_roc <-
  roc(transfusion_na_omit_outcome, 
      PCNL_transfusion_keras_imp_prediction)
auc(PCNL_transfusion_imp_keras_roc)
ci.auc(PCNL_transfusion_imp_keras_roc)
confusionMatrix(PCNL_transfusion_keras_imp_tb) 
plot(PCNL_transfusion_imp_keras_roc)

### Imputed and Oversampled
PCNL_transfusion_keras_imp_oversample_results <-
  PCNL_transfusion_imp_oversample_keras %>% evaluate(
    PCNL_transfusion_omit_na_test_predictors2,
    transfusion_na_omit_outcome
  )
PCNL_transfusion_keras_imp_oversample_prediction <-
  PCNL_transfusion_imp_oversample_keras %>% predict(PCNL_transfusion_omit_na_test_predictors2) %>% as.numeric()
PCNL_transfusion_keras_imp_oversample_prediction1 <-
  round(PCNL_transfusion_keras_imp_oversample_prediction,
        digits = 0) %>% factor(levels = c("0", "1"))
PCNL_transfusion_keras_imp_oversample_tb <-
  table(
    transfusion_na_omit_outcome,
    PCNL_transfusion_keras_imp_oversample_prediction1
  )
PCNL_transfusion_imp_oversample_keras_roc <-
  roc(
    transfusion_na_omit_outcome,
    PCNL_transfusion_keras_imp_oversample_prediction
  )
auc(PCNL_transfusion_imp_oversample_keras_roc)
ci.auc(PCNL_transfusion_imp_oversample_keras_roc)
confusionMatrix(PCNL_transfusion_keras_imp_oversample_tb)
plot(PCNL_transfusion_imp_oversample_keras_roc)


### ROC
transfusion_keras_roc_list<-list(PCNL_transfusion_na_omit_keras_roc,
                               PCNL_transfusion_oversample_keras_roc,
                               PCNL_transfusion_imp_keras_roc,
                               PCNL_transfusion_imp_oversample_keras_roc)

transfusion_keras_ci_list <-
  lapply(transfusion_keras_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_transfusion_keras_ci_list <- lapply(transfusion_keras_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


transfusion_keras_roc_combined <-
  ggroc(transfusion_keras_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Post-Operative Transfusion",
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
  transfusion_keras_roc_combined <- transfusion_keras_roc_combined + geom_ribbon(
    data = dat_transfusion_keras_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

transfusion_keras_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_transfusion_na_omit_keras_roc),
      ci.auc(PCNL_transfusion_oversample_keras_roc),
      ci.auc(PCNL_transfusion_imp_keras_roc),
      ci.auc(PCNL_transfusion_imp_oversample_keras_roc)
    )
  ) %>% as_tibble()
colnames(transfusion_keras_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
transfusion_keras_auc_data_overall$AUC <- as.numeric(transfusion_keras_auc_data_overall$AUC) %>% round(digits = 2)
transfusion_keras_auc_data_overall$LB <- as.numeric(transfusion_keras_auc_data_overall$LB) %>% round(digits = 2)
transfusion_keras_auc_data_overall$UB <- as.numeric(transfusion_keras_auc_data_overall$UB) %>% round(digits = 2)
transfusion_keras_auc_data_overall <-transfusion_keras_auc_data_overall %>% relocate(AUC, .before = LB) 

transfusion_keras_auc_data_overall <- transfusion_keras_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

transfusion_keras_auc_table <- transfusion_keras_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(transfusion_keras_roc_combined))
transfusion_keras_auc_table_vp <- viewport(x = 0.70, y = 0.22, 
                                         just = c("right", "bottom"),
                                         height = 0.1, width = 0.2)
pushViewport(transfusion_keras_auc_table_vp)
grid.draw(transfusion_keras_auc_table)

popViewport()



#itu_hdu_admission
itu_hdu_na_omit_outcome <-
  ifelse(PCNL_itu_hdu_omit_na_test$itu_hdu_admission == "Yes",
         "1",
         "0") %>% as.numeric()

itu_hdu_oversample_outcome <-
  ifelse(PCNL_itu_hdu_omit_na_test$itu_hdu_admission == "Yes",
         "1",
         "0") %>% as.numeric()

### Omit NA only
PCNL_itu_hdu_keras_results <-
  PCNL_itu_hdu_keras %>% evaluate(PCNL_itu_hdu_omit_na_test_predictors2, itu_hdu_na_omit_outcome)
PCNL_itu_hdu_keras_prediction <-
  PCNL_itu_hdu_keras %>% predict(PCNL_itu_hdu_omit_na_test_predictors2) %>% as.numeric()
PCNL_itu_hdu_keras_prediction1 <-
  round(PCNL_itu_hdu_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_itu_hdu_keras_tb <-
  table(itu_hdu_na_omit_outcome, 
        PCNL_itu_hdu_keras_prediction1)
PCNL_itu_hdu_na_omit_keras_roc <-
  roc(itu_hdu_na_omit_outcome, 
      PCNL_itu_hdu_keras_prediction)
auc(PCNL_itu_hdu_na_omit_keras_roc)
ci.auc(PCNL_itu_hdu_na_omit_keras_roc)
confusionMatrix(PCNL_itu_hdu_keras_tb) 
plot(PCNL_itu_hdu_na_omit_keras_roc)


### Oversampled
PCNL_itu_hdu_keras_oversample_results <-
  PCNL_itu_hdu_oversample_keras %>% evaluate(PCNL_itu_hdu_omit_na_test_predictors2, itu_hdu_na_omit_outcome)
PCNL_itu_hdu_keras_oversample_prediction <-
  PCNL_itu_hdu_oversample_keras %>% predict(PCNL_itu_hdu_omit_na_test_predictors2) %>% as.numeric()
PCNL_itu_hdu_keras_oversample_prediction1 <-
  round(PCNL_itu_hdu_keras_oversample_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_itu_hdu_keras_oversample_tb <-
  table(itu_hdu_na_omit_outcome, 
        PCNL_itu_hdu_keras_oversample_prediction1)
PCNL_itu_hdu_oversample_keras_roc <-
  roc(itu_hdu_na_omit_outcome, 
      PCNL_itu_hdu_keras_oversample_prediction)
auc(PCNL_itu_hdu_oversample_keras_roc)
ci.auc(PCNL_itu_hdu_oversample_keras_roc)
confusionMatrix(PCNL_itu_hdu_keras_oversample_tb) 
plot(PCNL_itu_hdu_oversample_keras_roc)


### Imputed
PCNL_itu_hdu_keras_imp_results <-
  PCNL_itu_hdu_imp_keras %>% evaluate(PCNL_itu_hdu_omit_na_test_predictors2, itu_hdu_na_omit_outcome)
PCNL_itu_hdu_keras_imp_prediction <-
  PCNL_itu_hdu_imp_keras %>% predict(PCNL_itu_hdu_omit_na_test_predictors2) %>% as.numeric()
PCNL_itu_hdu_keras_imp_prediction1 <-
  round(PCNL_itu_hdu_keras_imp_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_itu_hdu_keras_imp_tb <-
  table(itu_hdu_na_omit_outcome, 
        PCNL_itu_hdu_keras_imp_prediction1)
PCNL_itu_hdu_imp_keras_roc <-
  roc(itu_hdu_na_omit_outcome, 
      PCNL_itu_hdu_keras_imp_prediction)
auc(PCNL_itu_hdu_imp_keras_roc)
ci.auc(PCNL_itu_hdu_imp_keras_roc)
confusionMatrix(PCNL_itu_hdu_keras_imp_tb) 
plot(PCNL_itu_hdu_imp_keras_roc)

### Imputed and Oversampled
PCNL_itu_hdu_keras_imp_oversample_results <-
  PCNL_itu_hdu_imp_oversample_keras %>% evaluate(
    PCNL_itu_hdu_omit_na_test_predictors2,
    itu_hdu_na_omit_outcome
  )
PCNL_itu_hdu_keras_imp_oversample_prediction <-
  PCNL_itu_hdu_imp_oversample_keras %>% predict(PCNL_itu_hdu_omit_na_test_predictors2) %>% as.numeric()
PCNL_itu_hdu_keras_imp_oversample_prediction1 <-
  round(PCNL_itu_hdu_keras_imp_oversample_prediction,
        digits = 0) %>% factor(levels = c("0", "1"))
PCNL_itu_hdu_keras_imp_oversample_tb <-
  table(
    itu_hdu_na_omit_outcome,
    PCNL_itu_hdu_keras_imp_oversample_prediction1
  )
PCNL_itu_hdu_imp_oversample_keras_roc <-
  roc(
    itu_hdu_na_omit_outcome,
    PCNL_itu_hdu_keras_imp_oversample_prediction
  )
auc(PCNL_itu_hdu_imp_oversample_keras_roc)
ci.auc(PCNL_itu_hdu_imp_oversample_keras_roc)
confusionMatrix(PCNL_itu_hdu_keras_imp_oversample_tb)
plot(PCNL_itu_hdu_imp_oversample_keras_roc)


### ROC
itu_hdu_keras_roc_list<-list(PCNL_itu_hdu_na_omit_keras_roc,
                                 PCNL_itu_hdu_oversample_keras_roc,
                                 PCNL_itu_hdu_imp_keras_roc,
                                 PCNL_itu_hdu_imp_oversample_keras_roc)

itu_hdu_keras_ci_list <-
  lapply(itu_hdu_keras_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_itu_hdu_keras_ci_list <- lapply(itu_hdu_keras_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


itu_hdu_keras_roc_combined <-
  ggroc(itu_hdu_keras_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Need for ITU/HDU Admission",
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
  itu_hdu_keras_roc_combined <- itu_hdu_keras_roc_combined + geom_ribbon(
    data = dat_itu_hdu_keras_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

itu_hdu_keras_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_itu_hdu_na_omit_keras_roc),
      ci.auc(PCNL_itu_hdu_oversample_keras_roc),
      ci.auc(PCNL_itu_hdu_imp_keras_roc),
      ci.auc(PCNL_itu_hdu_imp_oversample_keras_roc)
    )
  ) %>% as_tibble()
colnames(itu_hdu_keras_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
itu_hdu_keras_auc_data_overall$AUC <- as.numeric(itu_hdu_keras_auc_data_overall$AUC) %>% round(digits = 2)
itu_hdu_keras_auc_data_overall$LB <- as.numeric(itu_hdu_keras_auc_data_overall$LB) %>% round(digits = 2)
itu_hdu_keras_auc_data_overall$UB <- as.numeric(itu_hdu_keras_auc_data_overall$UB) %>% round(digits = 2)
itu_hdu_keras_auc_data_overall <-itu_hdu_keras_auc_data_overall %>% relocate(AUC, .before = LB) 

itu_hdu_keras_auc_data_overall <- itu_hdu_keras_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

itu_hdu_keras_auc_table <- itu_hdu_keras_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(itu_hdu_keras_roc_combined))
itu_hdu_keras_auc_table_vp <- viewport(x = 0.70, y = 0.22, 
                                           just = c("right", "bottom"),
                                           height = 0.1, width = 0.2)
pushViewport(itu_hdu_keras_auc_table_vp)
grid.draw(itu_hdu_keras_auc_table)

popViewport()



#complete_clearance_on_fluoroscopy
clearance_on_fluoro_na_omit_outcome <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_test$complete_clearance_on_fluoroscopy == "Yes",
         "1",
         "0") %>% as.numeric()

clearance_on_fluoro_oversample_outcome <-
  ifelse(PCNL_clearance_on_fluoro_omit_na_test$complete_clearance_on_fluoroscopy == "Yes",
         "1",
         "0") %>% as.numeric()

### Omit NA only
PCNL_clearance_on_fluoro_keras_results <-
  PCNL_clearance_on_fluoro_keras %>% evaluate(PCNL_clearance_on_fluoro_omit_na_test_predictors2, clearance_on_fluoro_na_omit_outcome)
PCNL_clearance_on_fluoro_keras_prediction <-
  PCNL_clearance_on_fluoro_keras %>% predict(PCNL_clearance_on_fluoro_omit_na_test_predictors2) %>% as.numeric()
PCNL_clearance_on_fluoro_keras_prediction1 <-
  round(PCNL_clearance_on_fluoro_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_clearance_on_fluoro_keras_tb <-
  table(clearance_on_fluoro_na_omit_outcome, 
        PCNL_clearance_on_fluoro_keras_prediction1)
PCNL_clearance_on_fluoro_na_omit_keras_roc <-
  roc(clearance_on_fluoro_na_omit_outcome, 
      PCNL_clearance_on_fluoro_keras_prediction)
auc(PCNL_clearance_on_fluoro_na_omit_keras_roc)
ci.auc(PCNL_clearance_on_fluoro_na_omit_keras_roc)
confusionMatrix(PCNL_clearance_on_fluoro_keras_tb) 
plot(PCNL_clearance_on_fluoro_na_omit_keras_roc)


### Oversampled
PCNL_clearance_on_fluoro_keras_oversample_results <-
  PCNL_clearance_on_fluoro_oversample_keras %>% evaluate(PCNL_clearance_on_fluoro_omit_na_test_predictors2, clearance_on_fluoro_na_omit_outcome)
PCNL_clearance_on_fluoro_keras_oversample_prediction <-
  PCNL_clearance_on_fluoro_oversample_keras %>% predict(PCNL_clearance_on_fluoro_omit_na_test_predictors2) %>% as.numeric()
PCNL_clearance_on_fluoro_keras_oversample_prediction1 <-
  round(PCNL_clearance_on_fluoro_keras_oversample_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_clearance_on_fluoro_keras_oversample_tb <-
  table(clearance_on_fluoro_na_omit_outcome, 
        PCNL_clearance_on_fluoro_keras_oversample_prediction1)
PCNL_clearance_on_fluoro_oversample_keras_roc <-
  roc(clearance_on_fluoro_na_omit_outcome, 
      PCNL_clearance_on_fluoro_keras_oversample_prediction)
auc(PCNL_clearance_on_fluoro_oversample_keras_roc)
ci.auc(PCNL_clearance_on_fluoro_oversample_keras_roc)
confusionMatrix(PCNL_clearance_on_fluoro_keras_oversample_tb) 
plot(PCNL_clearance_on_fluoro_oversample_keras_roc)


### Imputed
PCNL_clearance_on_fluoro_keras_imp_results <-
  PCNL_clearance_on_fluoro_imp_keras %>% evaluate(PCNL_clearance_on_fluoro_omit_na_test_predictors2, clearance_on_fluoro_na_omit_outcome)
PCNL_clearance_on_fluoro_keras_imp_prediction <-
  PCNL_clearance_on_fluoro_imp_keras %>% predict(PCNL_clearance_on_fluoro_omit_na_test_predictors2) %>% as.numeric()
PCNL_clearance_on_fluoro_keras_imp_prediction1 <-
  round(PCNL_clearance_on_fluoro_keras_imp_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_clearance_on_fluoro_keras_imp_tb <-
  table(clearance_on_fluoro_na_omit_outcome, 
        PCNL_clearance_on_fluoro_keras_imp_prediction1)
PCNL_clearance_on_fluoro_imp_keras_roc <-
  roc(clearance_on_fluoro_na_omit_outcome, 
      PCNL_clearance_on_fluoro_keras_imp_prediction)
auc(PCNL_clearance_on_fluoro_imp_keras_roc)
ci.auc(PCNL_clearance_on_fluoro_imp_keras_roc)
confusionMatrix(PCNL_clearance_on_fluoro_keras_imp_tb) 
plot(PCNL_clearance_on_fluoro_imp_keras_roc)

### Imputed and Oversampled
PCNL_clearance_on_fluoro_keras_imp_oversample_results <-
  PCNL_clearance_on_fluoro_imp_oversample_keras %>% evaluate(
    PCNL_clearance_on_fluoro_omit_na_test_predictors2,
    clearance_on_fluoro_na_omit_outcome
  )
PCNL_clearance_on_fluoro_keras_imp_oversample_prediction <-
  PCNL_clearance_on_fluoro_imp_oversample_keras %>% predict(PCNL_clearance_on_fluoro_omit_na_test_predictors2) %>% as.numeric()
PCNL_clearance_on_fluoro_keras_imp_oversample_prediction1 <-
  round(PCNL_clearance_on_fluoro_keras_imp_oversample_prediction,
        digits = 0) %>% factor(levels = c("0", "1"))
PCNL_clearance_on_fluoro_keras_imp_oversample_tb <-
  table(
    clearance_on_fluoro_na_omit_outcome,
    PCNL_clearance_on_fluoro_keras_imp_oversample_prediction1
  )
PCNL_clearance_on_fluoro_imp_oversample_keras_roc <-
  roc(
    clearance_on_fluoro_na_omit_outcome,
    PCNL_clearance_on_fluoro_keras_imp_oversample_prediction
  )
auc(PCNL_clearance_on_fluoro_imp_oversample_keras_roc)
ci.auc(PCNL_clearance_on_fluoro_imp_oversample_keras_roc)
confusionMatrix(PCNL_clearance_on_fluoro_keras_imp_oversample_tb)
plot(PCNL_clearance_on_fluoro_imp_oversample_keras_roc)


### ROC
clearance_on_fluoro_keras_roc_list<-list(PCNL_clearance_on_fluoro_na_omit_keras_roc,
                             PCNL_clearance_on_fluoro_oversample_keras_roc,
                             PCNL_clearance_on_fluoro_imp_keras_roc,
                             PCNL_clearance_on_fluoro_imp_oversample_keras_roc)

clearance_on_fluoro_keras_ci_list <-
  lapply(clearance_on_fluoro_keras_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_clearance_on_fluoro_keras_ci_list <- lapply(clearance_on_fluoro_keras_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


clearance_on_fluoro_keras_roc_combined <-
  ggroc(clearance_on_fluoro_keras_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Clearance on Intra-operative Fluoroscopy",
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
  clearance_on_fluoro_keras_roc_combined <- clearance_on_fluoro_keras_roc_combined + geom_ribbon(
    data = dat_clearance_on_fluoro_keras_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

clearance_on_fluoro_keras_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_clearance_on_fluoro_na_omit_keras_roc),
      ci.auc(PCNL_clearance_on_fluoro_oversample_keras_roc),
      ci.auc(PCNL_clearance_on_fluoro_imp_keras_roc),
      ci.auc(PCNL_clearance_on_fluoro_imp_oversample_keras_roc)
    )
  ) %>% as_tibble()
colnames(clearance_on_fluoro_keras_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
clearance_on_fluoro_keras_auc_data_overall$AUC <- as.numeric(clearance_on_fluoro_keras_auc_data_overall$AUC) %>% round(digits = 2)
clearance_on_fluoro_keras_auc_data_overall$LB <- as.numeric(clearance_on_fluoro_keras_auc_data_overall$LB) %>% round(digits = 2)
clearance_on_fluoro_keras_auc_data_overall$UB <- as.numeric(clearance_on_fluoro_keras_auc_data_overall$UB) %>% round(digits = 2)
clearance_on_fluoro_keras_auc_data_overall <-clearance_on_fluoro_keras_auc_data_overall %>% relocate(AUC, .before = LB) 

clearance_on_fluoro_keras_auc_data_overall <- clearance_on_fluoro_keras_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

clearance_on_fluoro_keras_auc_table <- clearance_on_fluoro_keras_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(clearance_on_fluoro_keras_roc_combined))
clearance_on_fluoro_keras_auc_table_vp <- viewport(x = 0.70, y = 0.22, 
                                       just = c("right", "bottom"),
                                       height = 0.1, width = 0.2)
pushViewport(clearance_on_fluoro_keras_auc_table_vp)
grid.draw(clearance_on_fluoro_keras_auc_table)

popViewport()


#visceral_injury
visc_inj_na_omit_outcome <-
  ifelse(PCNL_visc_inj_omit_na_test$visceral_injury == "Yes",
         "1",
         "0") %>% as.numeric()

visc_inj_oversample_outcome <-
  ifelse(PCNL_visc_inj_omit_na_test$visceral_injury == "Yes",
         "1",
         "0") %>% as.numeric()

### Omit NA only
PCNL_visc_inj_keras_results <-
  PCNL_visc_inj_keras %>% evaluate(PCNL_visc_inj_omit_na_test_predictors2, visc_inj_na_omit_outcome)
PCNL_visc_inj_keras_prediction <-
  PCNL_visc_inj_keras %>% predict(PCNL_visc_inj_omit_na_test_predictors2) %>% as.numeric()
PCNL_visc_inj_keras_prediction1 <-
  round(PCNL_visc_inj_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_visc_inj_keras_tb <-
  table(visc_inj_na_omit_outcome, 
        PCNL_visc_inj_keras_prediction1)
PCNL_visc_inj_na_omit_keras_roc <-
  roc(visc_inj_na_omit_outcome, 
      PCNL_visc_inj_keras_prediction)
auc(PCNL_visc_inj_na_omit_keras_roc)
ci.auc(PCNL_visc_inj_na_omit_keras_roc)
confusionMatrix(PCNL_visc_inj_keras_tb) 
plot(PCNL_visc_inj_na_omit_keras_roc)


### Oversampled
PCNL_visc_inj_keras_oversample_results <-
  PCNL_visc_inj_oversample_keras %>% evaluate(PCNL_visc_inj_omit_na_test_predictors2, visc_inj_na_omit_outcome)
PCNL_visc_inj_keras_oversample_prediction <-
  PCNL_visc_inj_oversample_keras %>% predict(PCNL_visc_inj_omit_na_test_predictors2) %>% as.numeric()
PCNL_visc_inj_keras_oversample_prediction1 <-
  round(PCNL_visc_inj_keras_oversample_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_visc_inj_keras_oversample_tb <-
  table(visc_inj_na_omit_outcome, 
        PCNL_visc_inj_keras_oversample_prediction1)
PCNL_visc_inj_oversample_keras_roc <-
  roc(visc_inj_na_omit_outcome, 
      PCNL_visc_inj_keras_oversample_prediction)
auc(PCNL_visc_inj_oversample_keras_roc)
ci.auc(PCNL_visc_inj_oversample_keras_roc)
confusionMatrix(PCNL_visc_inj_keras_oversample_tb) 
plot(PCNL_visc_inj_oversample_keras_roc)


### Imputed
PCNL_visc_inj_keras_imp_results <-
  PCNL_visc_inj_imp_keras %>% evaluate(PCNL_visc_inj_omit_na_test_predictors2, visc_inj_na_omit_outcome)
PCNL_visc_inj_keras_imp_prediction <-
  PCNL_visc_inj_imp_keras %>% predict(PCNL_visc_inj_omit_na_test_predictors2) %>% as.numeric()
PCNL_visc_inj_keras_imp_prediction1 <-
  round(PCNL_visc_inj_keras_imp_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_visc_inj_keras_imp_tb <-
  table(visc_inj_na_omit_outcome, 
        PCNL_visc_inj_keras_imp_prediction1)
PCNL_visc_inj_imp_keras_roc <-
  roc(visc_inj_na_omit_outcome, 
      PCNL_visc_inj_keras_imp_prediction)
auc(PCNL_visc_inj_imp_keras_roc)
ci.auc(PCNL_visc_inj_imp_keras_roc)
confusionMatrix(PCNL_visc_inj_keras_imp_tb) 
plot(PCNL_visc_inj_imp_keras_roc)

### Imputed and Oversampled
PCNL_visc_inj_keras_imp_oversample_results <-
  PCNL_visc_inj_imp_oversample_keras %>% evaluate(
    PCNL_visc_inj_omit_na_test_predictors2,
    visc_inj_na_omit_outcome
  )
PCNL_visc_inj_keras_imp_oversample_prediction <-
  PCNL_visc_inj_imp_oversample_keras %>% predict(PCNL_visc_inj_omit_na_test_predictors2) %>% as.numeric()
PCNL_visc_inj_keras_imp_oversample_prediction1 <-
  round(PCNL_visc_inj_keras_imp_oversample_prediction,
        digits = 0) %>% factor(levels = c("0", "1"))
PCNL_visc_inj_keras_imp_oversample_tb <-
  table(
    visc_inj_na_omit_outcome,
    PCNL_visc_inj_keras_imp_oversample_prediction1
  )
PCNL_visc_inj_imp_oversample_keras_roc <-
  roc(
    visc_inj_na_omit_outcome,
    PCNL_visc_inj_keras_imp_oversample_prediction
  )
auc(PCNL_visc_inj_imp_oversample_keras_roc)
ci.auc(PCNL_visc_inj_imp_oversample_keras_roc)
confusionMatrix(PCNL_visc_inj_keras_imp_oversample_tb)
plot(PCNL_visc_inj_imp_oversample_keras_roc)


### ROC
visc_inj_keras_roc_list<-list(PCNL_visc_inj_na_omit_keras_roc,
                                         PCNL_visc_inj_oversample_keras_roc,
                                         PCNL_visc_inj_imp_keras_roc,
                                         PCNL_visc_inj_imp_oversample_keras_roc)

visc_inj_keras_ci_list <-
  lapply(visc_inj_keras_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_visc_inj_keras_ci_list <- lapply(visc_inj_keras_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


visc_inj_keras_roc_combined <-
  ggroc(visc_inj_keras_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Visceral Injury",
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
  visc_inj_keras_roc_combined <- visc_inj_keras_roc_combined + geom_ribbon(
    data = dat_visc_inj_keras_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

visc_inj_keras_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_visc_inj_na_omit_keras_roc),
      ci.auc(PCNL_visc_inj_oversample_keras_roc),
      ci.auc(PCNL_visc_inj_imp_keras_roc),
      ci.auc(PCNL_visc_inj_imp_oversample_keras_roc)
    )
  ) %>% as_tibble()
colnames(visc_inj_keras_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
visc_inj_keras_auc_data_overall$AUC <- as.numeric(visc_inj_keras_auc_data_overall$AUC) %>% round(digits = 2)
visc_inj_keras_auc_data_overall$LB <- as.numeric(visc_inj_keras_auc_data_overall$LB) %>% round(digits = 2)
visc_inj_keras_auc_data_overall$UB <- as.numeric(visc_inj_keras_auc_data_overall$UB) %>% round(digits = 2)
visc_inj_keras_auc_data_overall <-visc_inj_keras_auc_data_overall %>% relocate(AUC, .before = LB) 

visc_inj_keras_auc_data_overall <- visc_inj_keras_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

visc_inj_keras_auc_table <- visc_inj_keras_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(visc_inj_keras_roc_combined))
visc_inj_keras_auc_table_vp <- viewport(x = 0.70, y = 0.22, 
                                                   just = c("right", "bottom"),
                                                   height = 0.1, width = 0.2)
pushViewport(visc_inj_keras_auc_table_vp)
grid.draw(visc_inj_keras_auc_table)

popViewport()


#postop_complications
post_op_comp_na_omit_outcome <-
  ifelse(PCNL_post_op_comp_omit_na_test$postop_complications == "Yes",
         "1",
         "0") %>% as.numeric()

post_op_comp_oversample_outcome <-
  ifelse(PCNL_post_op_comp_omit_na_test$postop_complications == "Yes",
         "1",
         "0") %>% as.numeric()

### NA Omit
PCNL_post_op_comp_keras_results <-
  PCNL_post_op_comp_keras %>% evaluate(PCNL_post_op_comp_omit_na_test_predictors2, post_op_comp_na_omit_outcome)
PCNL_post_op_comp_keras_prediction <-
  PCNL_post_op_comp_keras %>% predict(PCNL_post_op_comp_omit_na_test_predictors2) %>% as.numeric()
PCNL_post_op_comp_keras_prediction1 <-
  round(PCNL_post_op_comp_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_post_op_comp_keras_tb <-
  table(post_op_comp_na_omit_outcome, 
        PCNL_post_op_comp_keras_prediction1)
PCNL_post_op_comp_na_omit_keras_roc <-
  roc(post_op_comp_na_omit_outcome, 
      PCNL_post_op_comp_keras_prediction)
auc(PCNL_post_op_comp_na_omit_keras_roc)
ci.auc(PCNL_post_op_comp_na_omit_keras_roc)
confusionMatrix(PCNL_post_op_comp_keras_tb) 
plot(PCNL_post_op_comp_na_omit_keras_roc)


### Oversampled
PCNL_post_op_comp_keras_oversample_results <-
  PCNL_post_op_comp_oversample_keras %>% evaluate(PCNL_post_op_comp_omit_na_test_predictors2, post_op_comp_na_omit_outcome)
PCNL_post_op_comp_keras_oversample_prediction <-
  PCNL_post_op_comp_oversample_keras %>% predict(PCNL_post_op_comp_omit_na_test_predictors2) %>% as.numeric()
PCNL_post_op_comp_keras_oversample_prediction1 <-
  round(PCNL_post_op_comp_keras_oversample_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_post_op_comp_keras_oversample_tb <-
  table(post_op_comp_na_omit_outcome, 
        PCNL_post_op_comp_keras_oversample_prediction1)
PCNL_post_op_comp_oversample_keras_roc <-
  roc(post_op_comp_na_omit_outcome, 
      PCNL_post_op_comp_keras_oversample_prediction)
auc(PCNL_post_op_comp_oversample_keras_roc)
ci.auc(PCNL_post_op_comp_oversample_keras_roc)
confusionMatrix(PCNL_post_op_comp_keras_oversample_tb) 
plot(PCNL_post_op_comp_oversample_keras_roc)


### Imputed
PCNL_post_op_comp_keras_imp_results <-
  PCNL_post_op_comp_imp_keras %>% evaluate(PCNL_post_op_comp_omit_na_test_predictors2, post_op_comp_na_omit_outcome)
PCNL_post_op_comp_keras_imp_prediction <-
  PCNL_post_op_comp_imp_keras %>% predict(PCNL_post_op_comp_omit_na_test_predictors2) %>% as.numeric()
PCNL_post_op_comp_keras_imp_prediction1 <-
  round(PCNL_post_op_comp_keras_imp_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_post_op_comp_keras_imp_tb <-
  table(post_op_comp_na_omit_outcome, 
        PCNL_post_op_comp_keras_imp_prediction1)
PCNL_post_op_comp_imp_keras_roc <-
  roc(post_op_comp_na_omit_outcome, 
      PCNL_post_op_comp_keras_imp_prediction)
auc(PCNL_post_op_comp_imp_keras_roc)
ci.auc(PCNL_post_op_comp_imp_keras_roc)
confusionMatrix(PCNL_post_op_comp_keras_imp_tb) 
plot(PCNL_post_op_comp_imp_keras_roc)

### Imputed and Oversampled
PCNL_post_op_comp_keras_imp_oversample_results <-
  PCNL_post_op_comp_imp_oversample_keras %>% evaluate(
    PCNL_post_op_comp_omit_na_test_predictors2,
    post_op_comp_na_omit_outcome
  )
PCNL_post_op_comp_keras_imp_oversample_prediction <-
  PCNL_post_op_comp_imp_oversample_keras %>% predict(PCNL_post_op_comp_omit_na_test_predictors2) %>% as.numeric()
PCNL_post_op_comp_keras_imp_oversample_prediction1 <-
  round(PCNL_post_op_comp_keras_imp_oversample_prediction,
        digits = 0) %>% factor(levels = c("0", "1"))
PCNL_post_op_comp_keras_imp_oversample_tb <-
  table(
    post_op_comp_na_omit_outcome,
    PCNL_post_op_comp_keras_imp_oversample_prediction1
  )
PCNL_post_op_comp_imp_oversample_keras_roc <-
  roc(
    post_op_comp_na_omit_outcome,
    PCNL_post_op_comp_keras_imp_oversample_prediction
  )
auc(PCNL_post_op_comp_imp_oversample_keras_roc)
ci.auc(PCNL_post_op_comp_imp_oversample_keras_roc)
confusionMatrix(PCNL_post_op_comp_keras_imp_oversample_tb)
plot(PCNL_post_op_comp_imp_oversample_keras_roc)


### ROC
post_op_comp_keras_roc_list<-list(PCNL_post_op_comp_na_omit_keras_roc,
                                                PCNL_post_op_comp_oversample_keras_roc,
                                                PCNL_post_op_comp_imp_keras_roc,
                                                PCNL_post_op_comp_imp_oversample_keras_roc)

post_op_comp_keras_ci_list <-
  lapply(post_op_comp_keras_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_post_op_comp_keras_ci_list <- lapply(post_op_comp_keras_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


post_op_comp_keras_roc_combined <-
  ggroc(post_op_comp_keras_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Post-Operative Complications (Any)",
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
  post_op_comp_keras_roc_combined <- post_op_comp_keras_roc_combined + geom_ribbon(
    data = dat_post_op_comp_keras_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

post_op_comp_keras_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_post_op_comp_na_omit_keras_roc),
      ci.auc(PCNL_post_op_comp_oversample_keras_roc),
      ci.auc(PCNL_post_op_comp_imp_keras_roc),
      ci.auc(PCNL_post_op_comp_imp_oversample_keras_roc)
    )
  ) %>% as_tibble()
colnames(post_op_comp_keras_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
post_op_comp_keras_auc_data_overall$AUC <- as.numeric(post_op_comp_keras_auc_data_overall$AUC) %>% round(digits = 2)
post_op_comp_keras_auc_data_overall$LB <- as.numeric(post_op_comp_keras_auc_data_overall$LB) %>% round(digits = 2)
post_op_comp_keras_auc_data_overall$UB <- as.numeric(post_op_comp_keras_auc_data_overall$UB) %>% round(digits = 2)
post_op_comp_keras_auc_data_overall <-post_op_comp_keras_auc_data_overall %>% relocate(AUC, .before = LB) 

post_op_comp_keras_auc_data_overall <- post_op_comp_keras_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

post_op_comp_keras_auc_table <- post_op_comp_keras_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(post_op_comp_keras_roc_combined))
post_op_comp_keras_auc_table_vp <- viewport(x = 0.70, y = 0.22, 
                                                          just = c("right", "bottom"),
                                                          height = 0.1, width = 0.2)
pushViewport(post_op_comp_keras_auc_table_vp)
grid.draw(post_op_comp_keras_auc_table)

popViewport()

#stone_free_at_follow_up
sf_at_fu_na_omit_outcome <-
  ifelse(PCNL_sf_at_fu_omit_na_test$stone_free_at_follow_up == "Yes",
         "1",
         "0") %>% as.numeric()

sf_at_fu_oversample_outcome <-
  ifelse(PCNL_sf_at_fu_omit_na_test$stone_free_at_follow_up == "Yes",
         "1",
         "0") %>% as.numeric()

### NA Omit
PCNL_sf_at_fu_keras_results <-
  PCNL_sf_at_fu_keras %>% evaluate(PCNL_sf_at_fu_omit_na_test_predictors2, sf_at_fu_na_omit_outcome)
PCNL_sf_at_fu_keras_prediction <-
  PCNL_sf_at_fu_keras %>% predict(PCNL_sf_at_fu_omit_na_test_predictors2) %>% as.numeric()
PCNL_sf_at_fu_keras_prediction1 <-
  round(PCNL_sf_at_fu_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_sf_at_fu_keras_tb <-
  table(sf_at_fu_na_omit_outcome, 
        PCNL_sf_at_fu_keras_prediction1)
PCNL_sf_at_fu_na_omit_keras_roc <-
  roc(sf_at_fu_na_omit_outcome, 
      PCNL_sf_at_fu_keras_prediction)
auc(PCNL_sf_at_fu_na_omit_keras_roc)
ci.auc(PCNL_sf_at_fu_na_omit_keras_roc)
confusionMatrix(PCNL_sf_at_fu_keras_tb) 
plot(PCNL_sf_at_fu_na_omit_keras_roc)


### Oversampled
PCNL_sf_at_fu_keras_oversample_results <-
  PCNL_sf_at_fu_oversample_keras %>% evaluate(PCNL_sf_at_fu_omit_na_test_predictors2, sf_at_fu_na_omit_outcome)
PCNL_sf_at_fu_keras_oversample_prediction <-
  PCNL_sf_at_fu_oversample_keras %>% predict(PCNL_sf_at_fu_omit_na_test_predictors2) %>% as.numeric()
PCNL_sf_at_fu_keras_oversample_prediction1 <-
  round(PCNL_sf_at_fu_keras_oversample_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_sf_at_fu_keras_oversample_tb <-
  table(sf_at_fu_na_omit_outcome, 
        PCNL_sf_at_fu_keras_oversample_prediction1)
PCNL_sf_at_fu_oversample_keras_roc <-
  roc(sf_at_fu_na_omit_outcome, 
      PCNL_sf_at_fu_keras_oversample_prediction)
auc(PCNL_sf_at_fu_oversample_keras_roc)
ci.auc(PCNL_sf_at_fu_oversample_keras_roc)
confusionMatrix(PCNL_sf_at_fu_keras_oversample_tb) 
plot(PCNL_sf_at_fu_oversample_keras_roc)


### Imputed
PCNL_sf_at_fu_keras_imp_results <-
  PCNL_sf_at_fu_imp_keras %>% evaluate(PCNL_sf_at_fu_omit_na_test_predictors2, sf_at_fu_na_omit_outcome)
PCNL_sf_at_fu_keras_imp_prediction <-
  PCNL_sf_at_fu_imp_keras %>% predict(PCNL_sf_at_fu_omit_na_test_predictors2) %>% as.numeric()
PCNL_sf_at_fu_keras_imp_prediction1 <-
  round(PCNL_sf_at_fu_keras_imp_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_sf_at_fu_keras_imp_tb <-
  table(sf_at_fu_na_omit_outcome, 
        PCNL_sf_at_fu_keras_imp_prediction1)
PCNL_sf_at_fu_imp_keras_roc <-
  roc(sf_at_fu_na_omit_outcome, 
      PCNL_sf_at_fu_keras_imp_prediction)
auc(PCNL_sf_at_fu_imp_keras_roc)
ci.auc(PCNL_sf_at_fu_imp_keras_roc)
confusionMatrix(PCNL_sf_at_fu_keras_imp_tb) 
plot(PCNL_sf_at_fu_imp_keras_roc)

### Imputed and Oversampled
PCNL_sf_at_fu_keras_imp_oversample_results <-
  PCNL_sf_at_fu_imp_oversample_keras %>% evaluate(
    PCNL_sf_at_fu_omit_na_test_predictors2,
    sf_at_fu_na_omit_outcome
  )
PCNL_sf_at_fu_keras_imp_oversample_prediction <-
  PCNL_sf_at_fu_imp_oversample_keras %>% predict(PCNL_sf_at_fu_omit_na_test_predictors2) %>% as.numeric()
PCNL_sf_at_fu_keras_imp_oversample_prediction1 <-
  round(PCNL_sf_at_fu_keras_imp_oversample_prediction,
        digits = 0) %>% factor(levels = c("0", "1"))
PCNL_sf_at_fu_keras_imp_oversample_tb <-
  table(
    sf_at_fu_na_omit_outcome,
    PCNL_sf_at_fu_keras_imp_oversample_prediction1
  )
PCNL_sf_at_fu_imp_oversample_keras_roc <-
  roc(
    sf_at_fu_na_omit_outcome,
    PCNL_sf_at_fu_keras_imp_oversample_prediction
  )
auc(PCNL_sf_at_fu_imp_oversample_keras_roc)
ci.auc(PCNL_sf_at_fu_imp_oversample_keras_roc)
confusionMatrix(PCNL_sf_at_fu_keras_imp_oversample_tb)
plot(PCNL_sf_at_fu_imp_oversample_keras_roc)


### ROC
sf_at_fu_keras_roc_list<-list(PCNL_sf_at_fu_na_omit_keras_roc,
                                  PCNL_sf_at_fu_oversample_keras_roc,
                                  PCNL_sf_at_fu_imp_keras_roc,
                                  PCNL_sf_at_fu_imp_oversample_keras_roc)

sf_at_fu_keras_ci_list <-
  lapply(sf_at_fu_keras_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_sf_at_fu_keras_ci_list <- lapply(sf_at_fu_keras_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


sf_at_fu_keras_roc_combined <-
  ggroc(sf_at_fu_keras_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Stone Free at Follow-up (Clinician Defined)",
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
  sf_at_fu_keras_roc_combined <- sf_at_fu_keras_roc_combined + geom_ribbon(
    data = dat_sf_at_fu_keras_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

sf_at_fu_keras_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_sf_at_fu_na_omit_keras_roc),
      ci.auc(PCNL_sf_at_fu_oversample_keras_roc),
      ci.auc(PCNL_sf_at_fu_imp_keras_roc),
      ci.auc(PCNL_sf_at_fu_imp_oversample_keras_roc)
    )
  ) %>% as_tibble()
colnames(sf_at_fu_keras_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
sf_at_fu_keras_auc_data_overall$AUC <- as.numeric(sf_at_fu_keras_auc_data_overall$AUC) %>% round(digits = 2)
sf_at_fu_keras_auc_data_overall$LB <- as.numeric(sf_at_fu_keras_auc_data_overall$LB) %>% round(digits = 2)
sf_at_fu_keras_auc_data_overall$UB <- as.numeric(sf_at_fu_keras_auc_data_overall$UB) %>% round(digits = 2)
sf_at_fu_keras_auc_data_overall <-sf_at_fu_keras_auc_data_overall %>% relocate(AUC, .before = LB) 

sf_at_fu_keras_auc_data_overall <- sf_at_fu_keras_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

sf_at_fu_keras_auc_table <- sf_at_fu_keras_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(sf_at_fu_keras_roc_combined))
sf_at_fu_keras_auc_table_vp <- viewport(x = 0.70, y = 0.22, 
                                            just = c("right", "bottom"),
                                            height = 0.1, width = 0.2)
pushViewport(sf_at_fu_keras_auc_table_vp)
grid.draw(sf_at_fu_keras_auc_table)

popViewport()


#adjuvant_treatment
adj_rx_na_omit_outcome <-
  ifelse(PCNL_adj_rx_omit_na_test$adjuvant_treatment == "Yes",
         "1",
         "0") %>% as.numeric()

adj_rx_oversample_outcome <-
  ifelse(PCNL_adj_rx_omit_na_test$adjuvant_treatment == "Yes",
         "1",
         "0") %>% as.numeric()

### NA Omit
PCNL_adj_rx_keras_results <-
  PCNL_adj_rx_keras %>% evaluate(PCNL_adj_rx_omit_na_test_predictors2, adj_rx_na_omit_outcome)
PCNL_adj_rx_keras_prediction <-
  PCNL_adj_rx_keras %>% predict(PCNL_adj_rx_omit_na_test_predictors2) %>% as.numeric()
PCNL_adj_rx_keras_prediction1 <-
  round(PCNL_adj_rx_keras_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_adj_rx_keras_tb <-
  table(adj_rx_na_omit_outcome, 
        PCNL_adj_rx_keras_prediction1)
PCNL_adj_rx_na_omit_keras_roc <-
  roc(adj_rx_na_omit_outcome, 
      PCNL_adj_rx_keras_prediction)
auc(PCNL_adj_rx_na_omit_keras_roc)
ci.auc(PCNL_adj_rx_na_omit_keras_roc)
confusionMatrix(PCNL_adj_rx_keras_tb) 
plot(PCNL_adj_rx_na_omit_keras_roc)


### Oversampled
PCNL_adj_rx_keras_oversample_results <-
  PCNL_adj_rx_oversample_keras %>% evaluate(PCNL_adj_rx_omit_na_test_predictors2, adj_rx_na_omit_outcome)
PCNL_adj_rx_keras_oversample_prediction <-
  PCNL_adj_rx_oversample_keras %>% predict(PCNL_adj_rx_omit_na_test_predictors2) %>% as.numeric()
PCNL_adj_rx_keras_oversample_prediction1 <-
  round(PCNL_adj_rx_keras_oversample_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_adj_rx_keras_oversample_tb <-
  table(adj_rx_na_omit_outcome, 
        PCNL_adj_rx_keras_oversample_prediction1)
PCNL_adj_rx_oversample_keras_roc <-
  roc(adj_rx_na_omit_outcome, 
      PCNL_adj_rx_keras_oversample_prediction)
auc(PCNL_adj_rx_oversample_keras_roc)
ci.auc(PCNL_adj_rx_oversample_keras_roc)
confusionMatrix(PCNL_adj_rx_keras_oversample_tb) 
plot(PCNL_adj_rx_oversample_keras_roc)


### Imputed
PCNL_adj_rx_keras_imp_results <-
  PCNL_adj_rx_imp_keras %>% evaluate(PCNL_adj_rx_omit_na_test_predictors2, adj_rx_na_omit_outcome)
PCNL_adj_rx_keras_imp_prediction <-
  PCNL_adj_rx_imp_keras %>% predict(PCNL_adj_rx_omit_na_test_predictors2) %>% as.numeric()
PCNL_adj_rx_keras_imp_prediction1 <-
  round(PCNL_adj_rx_keras_imp_prediction, digits = 0) %>% factor(levels = c("0", "1"))
PCNL_adj_rx_keras_imp_tb <-
  table(adj_rx_na_omit_outcome, 
        PCNL_adj_rx_keras_imp_prediction1)
PCNL_adj_rx_imp_keras_roc <-
  roc(adj_rx_na_omit_outcome, 
      PCNL_adj_rx_keras_imp_prediction)
auc(PCNL_adj_rx_imp_keras_roc)
ci.auc(PCNL_adj_rx_imp_keras_roc)
confusionMatrix(PCNL_adj_rx_keras_imp_tb) 
plot(PCNL_adj_rx_imp_keras_roc)

### Imputed and Oversampled
PCNL_adj_rx_keras_imp_oversample_results <-
  PCNL_adj_rx_imp_oversample_keras %>% evaluate(
    PCNL_adj_rx_omit_na_test_predictors2,
    adj_rx_na_omit_outcome
  )
PCNL_adj_rx_keras_imp_oversample_prediction <-
  PCNL_adj_rx_imp_oversample_keras %>% predict(PCNL_adj_rx_omit_na_test_predictors2) %>% as.numeric()
PCNL_adj_rx_keras_imp_oversample_prediction1 <-
  round(PCNL_adj_rx_keras_imp_oversample_prediction,
        digits = 0) %>% factor(levels = c("0", "1"))
PCNL_adj_rx_keras_imp_oversample_tb <-
  table(
    adj_rx_na_omit_outcome,
    PCNL_adj_rx_keras_imp_oversample_prediction1
  )
PCNL_adj_rx_imp_oversample_keras_roc <-
  roc(
    adj_rx_na_omit_outcome,
    PCNL_adj_rx_keras_imp_oversample_prediction
  )
auc(PCNL_adj_rx_imp_oversample_keras_roc)
ci.auc(PCNL_adj_rx_imp_oversample_keras_roc)
confusionMatrix(PCNL_adj_rx_keras_imp_oversample_tb)
plot(PCNL_adj_rx_imp_oversample_keras_roc)


### ROC
adj_rx_keras_roc_list<-list(PCNL_adj_rx_na_omit_keras_roc,
                              PCNL_adj_rx_oversample_keras_roc,
                              PCNL_adj_rx_imp_keras_roc,
                              PCNL_adj_rx_imp_oversample_keras_roc)

adj_rx_keras_ci_list <-
  lapply(adj_rx_keras_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_adj_rx_keras_ci_list <- lapply(adj_rx_keras_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


adj_rx_keras_roc_combined <-
  ggroc(adj_rx_keras_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Need for Adjuvant Treatment",
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
  adj_rx_keras_roc_combined <- adj_rx_keras_roc_combined + geom_ribbon(
    data = dat_adj_rx_keras_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

adj_rx_keras_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_adj_rx_na_omit_keras_roc),
      ci.auc(PCNL_adj_rx_oversample_keras_roc),
      ci.auc(PCNL_adj_rx_imp_keras_roc),
      ci.auc(PCNL_adj_rx_imp_oversample_keras_roc)
    )
  ) %>% as_tibble()
colnames(adj_rx_keras_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
adj_rx_keras_auc_data_overall$AUC <- as.numeric(adj_rx_keras_auc_data_overall$AUC) %>% round(digits = 2)
adj_rx_keras_auc_data_overall$LB <- as.numeric(adj_rx_keras_auc_data_overall$LB) %>% round(digits = 2)
adj_rx_keras_auc_data_overall$UB <- as.numeric(adj_rx_keras_auc_data_overall$UB) %>% round(digits = 2)
adj_rx_keras_auc_data_overall <-adj_rx_keras_auc_data_overall %>% relocate(AUC, .before = LB) 

adj_rx_keras_auc_data_overall <- adj_rx_keras_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

adj_rx_keras_auc_table <- adj_rx_keras_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(adj_rx_keras_roc_combined))
adj_rx_keras_auc_table_vp <- viewport(x = 0.70, y = 0.22, 
                                        just = c("right", "bottom"),
                                        height = 0.1, width = 0.2)
pushViewport(adj_rx_keras_auc_table_vp)
grid.draw(adj_rx_keras_auc_table)

popViewport()