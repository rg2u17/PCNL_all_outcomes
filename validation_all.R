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
PCNL_single_infection_na_omit_xgboost_predict <-
  predict(PCNL_Post_Infection_xgboost,
          PCNL_single_infection_omit_na_test,
          type = "prob")
PCNL_single_infection_na_omit_xgboost_predict2 <-
  predict(PCNL_Post_Infection_xgboost,
          PCNL_single_infection_omit_na_test,
          type = "raw") 

PCNL_single_infection_na_omit_xgboost_tb <-
  table(
    pred = factor(PCNL_single_infection_na_omit_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_single_infection_omit_na_test$single_infection_outcome
  )
print(confusionMatrix(PCNL_single_infection_na_omit_xgboost_tb))
res_PCNL_single_infection_na_omit_xgboost <-
  evalm(PCNL_Post_Infection_xgboost)
PCNL_single_infection_na_omit_xgboost_roc <-
  roc(
    single_infection_na_omit_outcome,
    PCNL_single_infection_na_omit_xgboost_predict$Yes)
infection_na_omit_auc<-auc(PCNL_single_infection_na_omit_xgboost_roc)
print(infection_na_omit_auc)
print(ci.auc(PCNL_single_infection_na_omit_xgboost_roc))
varImp(PCNL_Post_Infection_xgboost)

### Oversampled
PCNL_single_infection_oversample_xgboost_predict <-
  predict(PCNL_Post_Infection_oversample_xgboost,
          PCNL_single_infection_omit_na_test,
          type = "prob")
PCNL_single_infection_oversample_xgboost_predict2 <-
  predict(PCNL_Post_Infection_oversample_xgboost,
          PCNL_single_infection_omit_na_test,
          type = "raw") 

PCNL_single_infection_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_single_infection_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_single_infection_omit_na_test$single_infection_outcome
  )
print(confusionMatrix(PCNL_single_infection_oversample_xgboost_tb))
res_PCNL_single_infection_oversample_xgboost <-
  evalm(PCNL_Post_Infection_oversample_xgboost)
PCNL_single_infection_oversample_xgboost_roc <-
  roc(
    single_infection_oversample_outcome,
    PCNL_single_infection_oversample_xgboost_predict$Yes)
infection_oversample_auc<-auc(PCNL_single_infection_oversample_xgboost_roc)
print(infection_oversample_auc)
print(ci.auc(PCNL_single_infection_oversample_xgboost_roc))
varImp(PCNL_Post_Infection_oversample_xgboost)


### Imputed
PCNL_single_infection_imp_xgboost_predict <-
  predict(PCNL_Post_Infection_imp_xgboost,
          PCNL_single_infection_omit_na_test,
          type = "prob")
PCNL_single_infection_imp_xgboost_predict2 <-
  predict(PCNL_Post_Infection_imp_xgboost,
          PCNL_single_infection_omit_na_test,
          type = "raw") 


PCNL_single_infection_imp_xgboost_tb <-
  table(
    pred = factor(PCNL_single_infection_imp_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_single_infection_omit_na_test$single_infection_outcome
  )
print(confusionMatrix(PCNL_single_infection_imp_xgboost_tb))
res_PCNL_single_infection_imp_xgboost <-
  evalm(PCNL_Post_Infection_imp_xgboost)
PCNL_single_infection_imp_xgboost_roc <-
  roc(
    single_infection_oversample_outcome,
    PCNL_single_infection_imp_xgboost_predict$Yes)
infection_imp_auc<-auc(PCNL_single_infection_imp_xgboost_roc)
print(infection_imp_auc)
print(ci.auc(PCNL_single_infection_imp_xgboost_roc))
varImp(PCNL_Post_Infection_imp_xgboost)

### Imputed and Oversampled
PCNL_single_infection_imp_oversample_xgboost_predict <-
  predict(PCNL_Post_Infection_imp_oversample_xgboost,
          PCNL_single_infection_omit_na_test,
          type = "prob")
PCNL_single_infection_imp_oversample_xgboost_predict2 <-
  predict(PCNL_Post_Infection_imp_oversample_xgboost,
          PCNL_single_infection_omit_na_test,
          type = "raw") 

PCNL_single_infection_imp_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_single_infection_imp_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_single_infection_omit_na_test$single_infection_outcome
  )
print(confusionMatrix(PCNL_single_infection_imp_oversample_xgboost_tb))
res_PCNL_single_infection_imp_oversample_xgboost <-
  evalm(PCNL_Post_Infection_imp_oversample_xgboost)
PCNL_single_infection_imp_oversample_xgboost_roc <-
  roc(
    single_infection_oversample_outcome,
    PCNL_single_infection_imp_oversample_xgboost_predict$Yes)
infection_imp_oversample_auc<-auc(PCNL_single_infection_imp_oversample_xgboost_roc)
print(infection_imp_oversample_auc)
print(ci.auc(PCNL_single_infection_imp_oversample_xgboost_roc))
varImp(PCNL_Post_Infection_imp_oversample_xgboost)


### ROC
infection_roc_list<-list(PCNL_single_infection_na_omit_xgboost_roc,
                         PCNL_single_infection_oversample_xgboost_roc,
                         PCNL_single_infection_imp_xgboost_roc,
                         PCNL_single_infection_imp_oversample_xgboost_roc)

infection_ci_list <-
  lapply(infection_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_infection_ci_list <- lapply(infection_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


post_infection_roc_combined <-
  ggroc(infection_roc_list, aes = c("color")) + theme_minimal() + labs(
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
  post_infection_roc_combined <- post_infection_roc_combined + geom_ribbon(
    data = dat_infection_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

infection_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_single_infection_na_omit_xgboost_roc),
      ci.auc(PCNL_single_infection_oversample_xgboost_roc),
      ci.auc(PCNL_single_infection_imp_xgboost_roc),
      ci.auc(PCNL_single_infection_imp_oversample_xgboost_roc)
    )
  ) %>% as_tibble()
colnames(infection_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
infection_auc_data_overall$AUC <- as.numeric(infection_auc_data_overall$AUC) %>% round(digits = 2)
infection_auc_data_overall$LB <- as.numeric(infection_auc_data_overall$LB) %>% round(digits = 2)
infection_auc_data_overall$UB <- as.numeric(infection_auc_data_overall$UB) %>% round(digits = 2)
infection_auc_data_overall <-infection_auc_data_overall %>% relocate(AUC, .before = LB) 

infection_auc_data_overall <- infection_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

infection_auc_table <- infection_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(post_infection_roc_combined))
infection_auc_table_vp <- viewport(x = 0.70, y = 0.22, 
                         just = c("right", "bottom"),
                         height = 0.1, width = 0.2)
pushViewport(infection_auc_table_vp)
grid.draw(infection_auc_table)

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
PCNL_transfusion_xgboost_predict <-
  predict(PCNL_transfusion_xgboost,
          PCNL_transfusion_omit_na_test,
          type = "prob")
PCNL_transfusion_xgboost_predict2 <-
  predict(PCNL_transfusion_xgboost,
          PCNL_transfusion_omit_na_test,
          type = "raw")

PCNL_transfusion_na_omit_xgboost_tb <-
  table(
    pred = factor(PCNL_transfusion_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_transfusion_omit_na_test$blood_transfusion
  )
print(confusionMatrix(PCNL_transfusion_na_omit_xgboost_tb))
res_PCNL_transfusion_na_omit_xgboost <-
  evalm(PCNL_transfusion_xgboost)
PCNL_transfusion_na_omit_xgboost_roc <-
  roc(
      transfusion_na_omit_outcome,
      PCNL_transfusion_xgboost_predict$Yes)
transfusion_na_omit_auc<-auc(PCNL_transfusion_na_omit_xgboost_roc)
print(transfusion_na_omit_auc)
print(ci.auc(PCNL_transfusion_na_omit_xgboost_roc))
varImp(PCNL_transfusion_xgboost)

### Oversampled
PCNL_transfusion_oversample_xgboost_predict <-
  predict(PCNL_transfusion_oversample_xgboost,
          PCNL_transfusion_omit_na_test,
          type = "prob")
PCNL_transfusion_oversample_xgboost_predict2 <-
  predict(PCNL_transfusion_oversample_xgboost,
          PCNL_transfusion_omit_na_test,
          type = "raw")

PCNL_transfusion_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_transfusion_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_transfusion_omit_na_test$blood_transfusion
  )
print(confusionMatrix(PCNL_transfusion_oversample_xgboost_tb))
res_PCNL_transfusion_oversample_xgboost <-
  evalm(PCNL_transfusion_oversample_xgboost)
PCNL_transfusion_oversample_xgboost_roc <-
  roc(
    transfusion_oversample_outcome,
    PCNL_transfusion_oversample_xgboost_predict$Yes)
transfusion_oversample_auc<-auc(PCNL_transfusion_oversample_xgboost_roc)
print(transfusion_oversample_auc)
print(ci.auc(PCNL_transfusion_oversample_xgboost_roc))
varImp(PCNL_transfusion_oversample_xgboost)


### Imputed
PCNL_transfusion_imp_xgboost_predict <-
  predict(PCNL_transfusion_imp_xgboost,
          PCNL_transfusion_omit_na_test,
          type = "prob")
PCNL_transfusion_imp_xgboost_predict2 <-
  predict(PCNL_transfusion_imp_xgboost,
          PCNL_transfusion_omit_na_test,
          type = "raw")

PCNL_transfusion_imp_xgboost_tb <-
  table(
    pred = factor(PCNL_transfusion_imp_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_transfusion_omit_na_test$blood_transfusion
  )
print(confusionMatrix(PCNL_transfusion_imp_xgboost_tb))
res_PCNL_transfusion_imp_xgboost <-
  evalm(PCNL_transfusion_imp_xgboost)

PCNL_transfusion_imp_xgboost_roc <-
  roc(
    transfusion_oversample_outcome,
    PCNL_transfusion_imp_xgboost_predict$Yes)
transfusion_imp_auc<-auc(PCNL_transfusion_imp_xgboost_roc)
print(transfusion_imp_auc)
print(ci.auc(PCNL_transfusion_imp_xgboost_roc))
varImp(PCNL_transfusion_imp_xgboost)

### Imputed and Oversampled
PCNL_transfusion_imp_oversample_xgboost_predict <-
  predict(PCNL_transfusion_imp_oversample_xgboost,
          PCNL_transfusion_omit_na_test,
          type = "prob")
PCNL_transfusion_imp_oversample_xgboost_predict2 <-
  predict(PCNL_transfusion_imp_oversample_xgboost,
          PCNL_transfusion_omit_na_test,
          type = "raw")

PCNL_transfusion_imp_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_transfusion_imp_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_transfusion_omit_na_test$blood_transfusion
  )
print(confusionMatrix(PCNL_transfusion_imp_oversample_xgboost_tb))
res_PCNL_transfusion_imp_oversample_xgboost <-
  evalm(PCNL_transfusion_imp_oversample_xgboost)
PCNL_transfusion_imp_oversample_xgboost_roc <-
  roc(
    transfusion_oversample_outcome,
    PCNL_transfusion_imp_oversample_xgboost_predict$Yes)
transfusion_imp_oversample_auc<-auc(PCNL_transfusion_imp_oversample_xgboost_roc)
print(transfusion_imp_oversample_auc)
print(ci.auc(PCNL_transfusion_imp_oversample_xgboost_roc))
varImp(PCNL_transfusion_imp_oversample_xgboost)


### ROC
transfusion_roc_list<-list(PCNL_transfusion_na_omit_xgboost_roc,
                         PCNL_transfusion_oversample_xgboost_roc,
                         PCNL_transfusion_imp_xgboost_roc,
                         PCNL_transfusion_imp_oversample_xgboost_roc)

transfusion_ci_list <-
  lapply(transfusion_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_transfusion_ci_list <- lapply(transfusion_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


transfusion_roc_combined <-
  ggroc(transfusion_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Post-Operative Tranfusion",
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
  transfusion_roc_combined <- transfusion_roc_combined + geom_ribbon(
    data = dat_transfusion_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

transfusion_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_transfusion_na_omit_xgboost_roc),
      ci.auc(PCNL_transfusion_oversample_xgboost_roc),
      ci.auc(PCNL_transfusion_imp_xgboost_roc),
      ci.auc(PCNL_transfusion_imp_oversample_xgboost_roc)
    )
  ) %>% as_tibble()
colnames(transfusion_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
transfusion_auc_data_overall$AUC <- as.numeric(transfusion_auc_data_overall$AUC) %>% round(digits = 2)
transfusion_auc_data_overall$LB <- as.numeric(transfusion_auc_data_overall$LB) %>% round(digits = 2)
transfusion_auc_data_overall$UB <- as.numeric(transfusion_auc_data_overall$UB) %>% round(digits = 2)
transfusion_auc_data_overall <-transfusion_auc_data_overall %>% relocate(AUC, .before = LB) 

transfusion_auc_data_overall <- transfusion_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

transfusion_auc_table <- transfusion_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(transfusion_roc_combined))
transfusion_auc_table_vp <- viewport(x = 0.7, y = 0.22, 
                                   just = c("right", "bottom"),
                                   height = 0.1, width = 0.2)
pushViewport(transfusion_auc_table_vp)
grid.draw(transfusion_auc_table)

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
PCNL_itu_hdu_na_omit_xgboost_predict <-
  predict(PCNL_itu_hdu_xgboost,
          PCNL_itu_hdu_omit_na_test,
          type = "prob")
PCNL_itu_hdu_xgboost_predict2 <-
  predict(PCNL_itu_hdu_xgboost,
          PCNL_itu_hdu_omit_na_test,
          type = "raw")

PCNL_itu_hdu_na_omit_xgboost_tb <-
  table(
    pred = factor(PCNL_itu_hdu_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_itu_hdu_omit_na_test$itu_hdu_admission
  )
print(confusionMatrix(PCNL_itu_hdu_na_omit_xgboost_tb))
res_PCNL_itu_hdu_na_omit_xgboost <-
  evalm(PCNL_itu_hdu_xgboost)
PCNL_itu_hdu_na_omit_xgboost_roc <-
  roc(
    itu_hdu_na_omit_outcome,
    PCNL_itu_hdu_na_omit_xgboost_predict$Yes)
itu_hdu_na_omit_auc<-auc(PCNL_itu_hdu_na_omit_xgboost_roc)
print(itu_hdu_na_omit_auc)
print(ci.auc(PCNL_itu_hdu_na_omit_xgboost_roc))
varImp(PCNL_itu_hdu_xgboost)

### Oversampled
PCNL_itu_hdu_oversample_xgboost_predict <-
  predict(PCNL_itu_hdu_oversample_xgboost,
          PCNL_itu_hdu_omit_na_test,
          type = "prob")
PCNL_itu_hdu_oversample_xgboost_predict2 <-
  predict(PCNL_itu_hdu_oversample_xgboost,
          PCNL_itu_hdu_omit_na_test,
          type = "raw")
PCNL_itu_hdu_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_itu_hdu_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_itu_hdu_omit_na_test$itu_hdu_admission
  )
print(confusionMatrix(PCNL_itu_hdu_oversample_xgboost_tb))
res_PCNL_itu_hdu_oversample_xgboost <-
  evalm(PCNL_itu_hdu_oversample_xgboost)
PCNL_itu_hdu_oversample_xgboost_roc <-
  roc(
    itu_hdu_oversample_outcome,
    PCNL_itu_hdu_oversample_xgboost_predict$Yes)
itu_hdu_oversample_auc<-auc(PCNL_itu_hdu_oversample_xgboost_roc)
print(itu_hdu_oversample_auc)
print(ci.auc(PCNL_itu_hdu_oversample_xgboost_roc))
varImp(PCNL_itu_hdu_oversample_xgboost)


### Imputed
PCNL_itu_hdu_imp_xgboost_predict <-
  predict(PCNL_itu_hdu_imp_xgboost,
          PCNL_itu_hdu_omit_na_test,
          type = "prob")
PCNL_itu_hdu_imp_xgboost_predict2 <-
  predict(PCNL_itu_hdu_imp_xgboost,
          PCNL_itu_hdu_omit_na_test,
          type = "raw")
PCNL_itu_hdu_imp_xgboost_tb <-
  table(
    pred = factor(PCNL_itu_hdu_imp_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_itu_hdu_omit_na_test$itu_hdu_admission
  )
print(confusionMatrix(PCNL_itu_hdu_imp_xgboost_tb))
res_PCNL_itu_hdu_imp_xgboost <-
  evalm(PCNL_itu_hdu_imp_xgboost)
PCNL_itu_hdu_imp_xgboost_roc <-
  roc(
    itu_hdu_oversample_outcome,
    PCNL_itu_hdu_imp_xgboost_predict$Yes)
itu_hdu_imp_auc<-auc(PCNL_itu_hdu_imp_xgboost_roc)
print(itu_hdu_imp_auc)
print(ci.auc(PCNL_itu_hdu_imp_xgboost_roc))
varImp(PCNL_itu_hdu_imp_xgboost)

### Imputed and Oversampled
PCNL_itu_hdu_imp_oversample_xgboost_predict <-
  predict(PCNL_itu_hdu_imp_oversample_xgboost,
          PCNL_itu_hdu_omit_na_test,
          type = "prob")
PCNL_itu_hdu_imp_oversample_xgboost_predict2 <-
  predict(PCNL_itu_hdu_imp_oversample_xgboost,
          PCNL_itu_hdu_omit_na_test,
          type = "raw")
PCNL_itu_hdu_imp_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_itu_hdu_imp_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_itu_hdu_omit_na_test$itu_hdu_admission
  )
print(confusionMatrix(PCNL_itu_hdu_imp_oversample_xgboost_tb))
res_PCNL_itu_hdu_imp_oversample_xgboost <-
  evalm(PCNL_itu_hdu_imp_oversample_xgboost)
PCNL_itu_hdu_imp_oversample_xgboost_roc <-
  roc(
    itu_hdu_oversample_outcome,
    PCNL_itu_hdu_imp_oversample_xgboost_predict$Yes)
itu_hdu_imp_oversample_auc<-auc(PCNL_itu_hdu_imp_oversample_xgboost_roc)
print(itu_hdu_imp_oversample_auc)
print(ci.auc(PCNL_itu_hdu_imp_oversample_xgboost_roc))
varImp(PCNL_itu_hdu_imp_oversample_xgboost)


### ROC
itu_hdu_roc_list<-list(PCNL_itu_hdu_na_omit_xgboost_roc,
                           PCNL_itu_hdu_oversample_xgboost_roc,
                           PCNL_itu_hdu_imp_xgboost_roc,
                           PCNL_itu_hdu_imp_oversample_xgboost_roc)

itu_hdu_ci_list <-
  lapply(itu_hdu_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_itu_hdu_ci_list <- lapply(itu_hdu_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))


itu_hdu_roc_combined <-
  ggroc(itu_hdu_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "ITU/HDU Admission",
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
  itu_hdu_roc_combined <- itu_hdu_roc_combined + geom_ribbon(
    data = dat_itu_hdu_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

itu_hdu_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_itu_hdu_na_omit_xgboost_roc),
      ci.auc(PCNL_itu_hdu_oversample_xgboost_roc),
      ci.auc(PCNL_itu_hdu_imp_xgboost_roc),
      ci.auc(PCNL_itu_hdu_imp_oversample_xgboost_roc)
    )
  ) %>% as_tibble()
colnames(itu_hdu_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
itu_hdu_auc_data_overall$AUC <- as.numeric(itu_hdu_auc_data_overall$AUC) %>% round(digits = 2)
itu_hdu_auc_data_overall$LB <- as.numeric(itu_hdu_auc_data_overall$LB) %>% round(digits = 2)
itu_hdu_auc_data_overall$UB <- as.numeric(itu_hdu_auc_data_overall$UB) %>% round(digits = 2)
itu_hdu_auc_data_overall <-itu_hdu_auc_data_overall %>% relocate(AUC, .before = LB) 

itu_hdu_auc_data_overall <- itu_hdu_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

itu_hdu_auc_table <- itu_hdu_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(itu_hdu_roc_combined))
itu_hdu_auc_table_vp <- viewport(x = 0.7, y = 0.22, 
                                     just = c("right", "bottom"),
                                     height = 0.1, width = 0.2)
pushViewport(itu_hdu_auc_table_vp)
grid.draw(itu_hdu_auc_table)

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
PCNL_clearance_on_fluoro_xgboost_predict <-
  predict(PCNL_clearance_on_fluoro_xgboost,
          PCNL_clearance_on_fluoro_omit_na_test,
          type = "prob")
PCNL_clearance_on_fluoro_xgboost_predict2 <-
  predict(PCNL_clearance_on_fluoro_xgboost,
          PCNL_clearance_on_fluoro_omit_na_test,
          type = "raw")
PCNL_clearance_on_fluoro_na_omit_xgboost_tb <-
  table(
    pred = factor(PCNL_clearance_on_fluoro_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_clearance_on_fluoro_omit_na_test$complete_clearance_on_fluoroscopy
  )
print(confusionMatrix(PCNL_clearance_on_fluoro_na_omit_xgboost_tb))
res_PCNL_clearance_on_fluoro_na_omit_xgboost <-
  evalm(PCNL_clearance_on_fluoro_xgboost)
PCNL_clearance_on_fluoro_na_omit_xgboost_roc <-
  roc(
    clearance_on_fluoro_na_omit_outcome,
    PCNL_clearance_on_fluoro_xgboost_predict$Yes)
clearance_on_fluoro_na_omit_auc<-auc(PCNL_clearance_on_fluoro_na_omit_xgboost_roc)
print(clearance_on_fluoro_na_omit_auc)
print(ci.auc(PCNL_clearance_on_fluoro_na_omit_xgboost_roc))
varImp(PCNL_clearance_on_fluoro_xgboost)

### Oversampled
PCNL_clearance_on_fluoro_oversample_xgboost_predict <-
  predict(PCNL_clearance_on_fluoro_oversample_xgboost,
          PCNL_clearance_on_fluoro_omit_na_test,
          type = "prob")
PCNL_clearance_on_fluoro_oversample_xgboost_predict2 <-
  predict(PCNL_clearance_on_fluoro_oversample_xgboost,
          PCNL_clearance_on_fluoro_omit_na_test,
          type = "raw")
PCNL_clearance_on_fluoro_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_clearance_on_fluoro_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_clearance_on_fluoro_omit_na_test$complete_clearance_on_fluoroscopy
  )
print(confusionMatrix(PCNL_clearance_on_fluoro_oversample_xgboost_tb))
res_PCNL_clearance_on_fluoro_oversample_xgboost <-
  evalm(PCNL_clearance_on_fluoro_oversample_xgboost)
PCNL_clearance_on_fluoro_oversample_xgboost_predict2 <-
  ifelse(PCNL_clearance_on_fluoro_oversample_xgboost_predict == "Yes", "1", "0") %>% as.numeric()
PCNL_clearance_on_fluoro_oversample_xgboost_roc <-
  roc(
    clearance_on_fluoro_oversample_outcome,
    PCNL_clearance_on_fluoro_oversample_xgboost_predict$Yes)
clearance_on_fluoro_oversample_auc<-auc(PCNL_clearance_on_fluoro_oversample_xgboost_roc)
print(clearance_on_fluoro_oversample_auc)
print(ci.auc(PCNL_clearance_on_fluoro_oversample_xgboost_roc))
varImp(PCNL_clearance_on_fluoro_oversample_xgboost)


### Imputed
PCNL_clearance_on_fluoro_imp_xgboost_predict <-
  predict(PCNL_clearance_on_fluoro_imp_xgboost,
          PCNL_clearance_on_fluoro_omit_na_test,
          type = "prob")
PCNL_clearance_on_fluoro_imp_xgboost_predict2 <-
  predict(PCNL_clearance_on_fluoro_imp_xgboost,
          PCNL_clearance_on_fluoro_omit_na_test,
          type = "raw")
PCNL_clearance_on_fluoro_imp_xgboost_tb <-
  table(
    pred = factor(PCNL_clearance_on_fluoro_imp_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_clearance_on_fluoro_omit_na_test$complete_clearance_on_fluoroscopy
  )
print(confusionMatrix(PCNL_clearance_on_fluoro_imp_xgboost_tb))
res_PCNL_clearance_on_fluoro_imp_xgboost <-
  evalm(PCNL_clearance_on_fluoro_imp_xgboost)

PCNL_clearance_on_fluoro_imp_xgboost_roc <-
  roc(
    clearance_on_fluoro_oversample_outcome,
    PCNL_clearance_on_fluoro_imp_xgboost_predict$Yes)
clearance_on_fluoro_imp_auc<-auc(PCNL_clearance_on_fluoro_imp_xgboost_roc)
print(clearance_on_fluoro_imp_auc)
print(ci.auc(PCNL_clearance_on_fluoro_imp_xgboost_roc))
varImp(PCNL_clearance_on_fluoro_imp_xgboost)

### Imputed and Oversampled
PCNL_clearance_on_fluoro_imp_oversample_xgboost_predict <-
  predict(PCNL_clearance_on_fluoro_imp_oversample_xgboost,
          PCNL_clearance_on_fluoro_omit_na_test,
          type = "prob")
PCNL_clearance_on_fluoro_imp_oversample_xgboost_predict2 <-
  predict(PCNL_clearance_on_fluoro_imp_oversample_xgboost,
          PCNL_clearance_on_fluoro_omit_na_test,
          type = "raw")
PCNL_clearance_on_fluoro_imp_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_clearance_on_fluoro_imp_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_clearance_on_fluoro_omit_na_test$complete_clearance_on_fluoroscopy
  )
print(confusionMatrix(PCNL_clearance_on_fluoro_imp_oversample_xgboost_tb))
res_PCNL_clearance_on_fluoro_imp_oversample_xgboost <-
  evalm(PCNL_clearance_on_fluoro_imp_oversample_xgboost)

PCNL_clearance_on_fluoro_imp_oversample_xgboost_roc <-
  roc(
    clearance_on_fluoro_oversample_outcome,
    PCNL_clearance_on_fluoro_imp_oversample_xgboost_predict$Yes)
clearance_on_fluoro_imp_oversample_auc<-auc(PCNL_clearance_on_fluoro_imp_oversample_xgboost_roc)
print(clearance_on_fluoro_imp_oversample_auc)
print(ci.auc(PCNL_clearance_on_fluoro_imp_oversample_xgboost_roc))
varImp(PCNL_clearance_on_fluoro_imp_oversample_xgboost)


### ROC
clearance_on_fluoro_roc_list <-
  list(
    PCNL_clearance_on_fluoro_na_omit_xgboost_roc,
    PCNL_clearance_on_fluoro_oversample_xgboost_roc,
    PCNL_clearance_on_fluoro_imp_xgboost_roc,
    PCNL_clearance_on_fluoro_imp_oversample_xgboost_roc
  )

clearance_on_fluoro_ci_list <-
  lapply(clearance_on_fluoro_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_clearance_on_fluoro_ci_list <- lapply(clearance_on_fluoro_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))

clearance_on_fluoro_roc_combined <-
  ggroc(clearance_on_fluoro_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Complete Clearance on Fluoroscopy (intra-operative)",
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
  clearance_on_fluoro_roc_combined <- clearance_on_fluoro_roc_combined + geom_ribbon(
    data = dat_clearance_on_fluoro_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

clearance_on_fluoro_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_clearance_on_fluoro_na_omit_xgboost_roc),
      ci.auc(PCNL_clearance_on_fluoro_oversample_xgboost_roc),
      ci.auc(PCNL_clearance_on_fluoro_imp_xgboost_roc),
      ci.auc(PCNL_clearance_on_fluoro_imp_oversample_xgboost_roc)
    )
  ) %>% as_tibble()
colnames(clearance_on_fluoro_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
clearance_on_fluoro_auc_data_overall$AUC <- as.numeric(clearance_on_fluoro_auc_data_overall$AUC) %>% round(digits = 2)
clearance_on_fluoro_auc_data_overall$LB <- as.numeric(clearance_on_fluoro_auc_data_overall$LB) %>% round(digits = 2)
clearance_on_fluoro_auc_data_overall$UB <- as.numeric(clearance_on_fluoro_auc_data_overall$UB) %>% round(digits = 2)
clearance_on_fluoro_auc_data_overall <-clearance_on_fluoro_auc_data_overall %>% relocate(AUC, .before = LB) 

clearance_on_fluoro_auc_data_overall <- clearance_on_fluoro_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

clearance_on_fluoro_auc_table <- clearance_on_fluoro_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(clearance_on_fluoro_roc_combined))
clearance_on_fluoro_auc_table_vp <- viewport(x = 0.7, y = 0.22, 
                                 just = c("right", "bottom"),
                                 height = 0.1, width = 0.2)
pushViewport(clearance_on_fluoro_auc_table_vp)
grid.draw(clearance_on_fluoro_auc_table)

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
PCNL_visc_inj_xgboost_predict <-
  predict(PCNL_visc_inj_xgboost,
          PCNL_visc_inj_omit_na_test,
          type = "prob")
PCNL_visc_inj_xgboost_predict2 <-
  predict(PCNL_visc_inj_xgboost,
          PCNL_visc_inj_omit_na_test,
          type = "raw")
PCNL_visc_inj_na_omit_xgboost_tb <-
  table(
    pred = factor(PCNL_visc_inj_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_visc_inj_omit_na_test$visceral_injury
  )
print(confusionMatrix(PCNL_visc_inj_na_omit_xgboost_tb))
res_PCNL_visc_inj_na_omit_xgboost <-
  evalm(PCNL_visc_inj_xgboost)

PCNL_visc_inj_na_omit_xgboost_roc <-
  roc(
    visc_inj_na_omit_outcome,
    PCNL_visc_inj_xgboost_predict$Yes)
visc_inj_na_omit_auc<-auc(PCNL_visc_inj_na_omit_xgboost_roc)
print(visc_inj_na_omit_auc)
print(ci.auc(PCNL_visc_inj_na_omit_xgboost_roc))
varImp(PCNL_visc_inj_xgboost)

### Oversampled
PCNL_visc_inj_oversample_xgboost_predict <-
  predict(PCNL_visc_inj_oversample_xgboost,
          PCNL_visc_inj_omit_na_test,
          type = "prob")
PCNL_visc_inj_oversample_xgboost_predict2 <-
  predict(PCNL_visc_inj_oversample_xgboost,
          PCNL_visc_inj_omit_na_test,
          type = "raw")
PCNL_visc_inj_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_visc_inj_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_visc_inj_omit_na_test$visceral_injury
  )
print(confusionMatrix(PCNL_visc_inj_oversample_xgboost_tb))
res_PCNL_visc_inj_oversample_xgboost <-
  evalm(PCNL_visc_inj_oversample_xgboost)
PCNL_visc_inj_oversample_xgboost_predict2 <-
  ifelse(PCNL_visc_inj_oversample_xgboost_predict == "Yes", "1", "0") %>% as.numeric()
PCNL_visc_inj_oversample_xgboost_roc <-
  roc(
    visc_inj_oversample_outcome,
    PCNL_visc_inj_oversample_xgboost_predict$Yes)
visc_inj_oversample_auc<-auc(PCNL_visc_inj_oversample_xgboost_roc)
print(visc_inj_oversample_auc)
print(ci.auc(PCNL_visc_inj_oversample_xgboost_roc))
varImp(PCNL_visc_inj_oversample_xgboost)


### Imputed
PCNL_visc_inj_imp_xgboost_predict <-
  predict(PCNL_visc_inj_imp_xgboost,
          PCNL_visc_inj_omit_na_test,
          type = "prob")
PCNL_visc_inj_imp_xgboost_predict2 <-
  predict(PCNL_visc_inj_imp_xgboost,
          PCNL_visc_inj_omit_na_test,
          type = "raw")
PCNL_visc_inj_imp_xgboost_tb <-
  table(
    pred = factor(PCNL_visc_inj_imp_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_visc_inj_omit_na_test$visceral_injury
  )
print(confusionMatrix(PCNL_visc_inj_imp_xgboost_tb))
res_PCNL_visc_inj_imp_xgboost <-
  evalm(PCNL_visc_inj_imp_xgboost)

PCNL_visc_inj_imp_xgboost_roc <-
  roc(
    visc_inj_oversample_outcome,
    PCNL_visc_inj_imp_xgboost_predict$Yes)
visc_inj_imp_auc<-auc(PCNL_visc_inj_imp_xgboost_roc)
print(visc_inj_imp_auc)
print(ci.auc(PCNL_visc_inj_imp_xgboost_roc))
varImp(PCNL_visc_inj_imp_xgboost)

### Imputed and Oversampled
PCNL_visc_inj_imp_oversample_xgboost_predict <-
  predict(PCNL_visc_inj_imp_oversample_xgboost,
          PCNL_visc_inj_omit_na_test,
          type = "prob")
PCNL_visc_inj_imp_oversample_xgboost_predict2 <-
  predict(PCNL_visc_inj_imp_oversample_xgboost,
          PCNL_visc_inj_omit_na_test,
          type = "raw")
PCNL_visc_inj_imp_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_visc_inj_imp_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_visc_inj_omit_na_test$visceral_injury
  )
print(confusionMatrix(PCNL_visc_inj_imp_oversample_xgboost_tb))
res_PCNL_visc_inj_imp_oversample_xgboost <-
  evalm(PCNL_visc_inj_imp_oversample_xgboost)
PCNL_visc_inj_imp_oversample_xgboost_predict2 <-
  ifelse(PCNL_visc_inj_imp_oversample_xgboost_predict == "Yes", "1", "0") %>% as.numeric()
PCNL_visc_inj_imp_oversample_xgboost_roc <-
  roc(
    visc_inj_oversample_outcome,
    PCNL_visc_inj_imp_oversample_xgboost_predict$Yes)
visc_inj_imp_oversample_auc<-auc(PCNL_visc_inj_imp_oversample_xgboost_roc)
print(visc_inj_imp_oversample_auc)
print(ci.auc(PCNL_visc_inj_imp_oversample_xgboost_roc))
varImp(PCNL_visc_inj_imp_oversample_xgboost)


### ROC
visc_inj_roc_list <-
  list(
    PCNL_visc_inj_na_omit_xgboost_roc,
    PCNL_visc_inj_oversample_xgboost_roc,
    PCNL_visc_inj_imp_xgboost_roc,
    PCNL_visc_inj_imp_oversample_xgboost_roc
  )

visc_inj_ci_list <-
  lapply(visc_inj_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_visc_inj_ci_list <- lapply(visc_inj_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))

visc_inj_roc_combined <-
  ggroc(visc_inj_roc_list, aes = c("color")) + theme_minimal() + labs(
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
  visc_inj_roc_combined <- visc_inj_roc_combined + geom_ribbon(
    data = dat_visc_inj_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

visc_inj_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_visc_inj_na_omit_xgboost_roc),
      ci.auc(PCNL_visc_inj_oversample_xgboost_roc),
      ci.auc(PCNL_visc_inj_imp_xgboost_roc),
      ci.auc(PCNL_visc_inj_imp_oversample_xgboost_roc)
    )
  ) %>% as_tibble()
colnames(visc_inj_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
visc_inj_auc_data_overall$AUC <- as.numeric(visc_inj_auc_data_overall$AUC) %>% round(digits = 2)
visc_inj_auc_data_overall$LB <- as.numeric(visc_inj_auc_data_overall$LB) %>% round(digits = 2)
visc_inj_auc_data_overall$UB <- as.numeric(visc_inj_auc_data_overall$UB) %>% round(digits = 2)
visc_inj_auc_data_overall <-visc_inj_auc_data_overall %>% relocate(AUC, .before = LB) 

visc_inj_auc_data_overall <- visc_inj_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

visc_inj_auc_table <- visc_inj_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(visc_inj_roc_combined))
visc_inj_auc_table_vp <- viewport(x = 0.7, y = 0.22, 
                                  just = c("right", "bottom"),
                                  height = 0.1, width = 0.2)
pushViewport(visc_inj_auc_table_vp)
grid.draw(visc_inj_auc_table)

popViewport()



#clearance_on_post_operative_radiological_imaging_during_a
clearance_during_admission_na_omit_outcome <-
  ifelse(PCNL_clearance_during_admission_omit_na_test$clearance_on_post_operative_radiological_imaging_during_a == "Yes",
         "1",
         "0") %>% as.numeric()

clearance_during_admission_oversample_outcome <-
  ifelse(PCNL_clearance_during_admission_omit_na_test$clearance_on_post_operative_radiological_imaging_during_a == "Yes",
         "1",
         "0") %>% as.numeric()

### Omit NA only
PCNL_clearance_during_admission_xgboost_predict <-
  predict(PCNL_clearance_during_admission_xgboost,
          PCNL_clearance_during_admission_omit_na_test,
          type = "prob")
PCNL_clearance_during_admission_xgboost_predict2 <-
  predict(PCNL_clearance_during_admission_xgboost,
          PCNL_clearance_during_admission_omit_na_test,
          type = "raw")
PCNL_clearance_during_admission_na_omit_xgboost_tb <-
  table(
    pred = factor(PCNL_clearance_during_admission_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_clearance_during_admission_omit_na_test$clearance_on_post_operative_radiological_imaging_during_a
  )
print(confusionMatrix(PCNL_clearance_during_admission_na_omit_xgboost_tb))
res_PCNL_clearance_during_admission_na_omit_xgboost <-
  evalm(PCNL_clearance_during_admission_xgboost)
PCNL_clearance_during_admission_na_omit_xgboost_roc <-
  roc(
    clearance_during_admission_na_omit_outcome,
    PCNL_clearance_during_admission_xgboost_predict$Yes)
clearance_during_admission_na_omit_auc<-auc(PCNL_clearance_during_admission_na_omit_xgboost_roc)
print(clearance_during_admission_na_omit_auc)
print(ci.auc(PCNL_clearance_during_admission_na_omit_xgboost_roc))
varImp(PCNL_clearance_during_admission_xgboost)

### Oversampled
PCNL_clearance_during_admission_oversample_xgboost_predict <-
  predict(PCNL_clearance_during_admission_oversample_xgboost,
          PCNL_clearance_during_admission_omit_na_test,
          type = "prob")
PCNL_clearance_during_admission_oversample_xgboost_predict2 <-
  predict(PCNL_clearance_during_admission_oversample_xgboost,
          PCNL_clearance_during_admission_omit_na_test,
          type = "raw")
PCNL_clearance_during_admission_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_clearance_during_admission_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_clearance_during_admission_omit_na_test$clearance_on_post_operative_radiological_imaging_during_a
  )
print(confusionMatrix(PCNL_clearance_during_admission_oversample_xgboost_tb))
res_PCNL_clearance_during_admission_oversample_xgboost <-
  evalm(PCNL_clearance_during_admission_oversample_xgboost)
PCNL_clearance_during_admission_oversample_xgboost_predict2 <-
  ifelse(PCNL_clearance_during_admission_oversample_xgboost_predict == "Yes", "1", "0") %>% as.numeric()
PCNL_clearance_during_admission_oversample_xgboost_roc <-
  roc(
    clearance_during_admission_oversample_outcome,
    PCNL_clearance_during_admission_oversample_xgboost_predict$Yes)
clearance_during_admission_oversample_auc<-auc(PCNL_clearance_during_admission_oversample_xgboost_roc)
print(clearance_during_admission_oversample_auc)
print(ci.auc(PCNL_clearance_during_admission_oversample_xgboost_roc))
varImp(PCNL_clearance_during_admission_oversample_xgboost)


### Imputed
PCNL_clearance_during_admission_imp_xgboost_predict <-
  predict(PCNL_clearance_during_admission_imp_xgboost,
          PCNL_clearance_during_admission_omit_na_test,
          type = "prob")
PCNL_clearance_during_admission_imp_xgboost_predict2 <-
  predict(PCNL_clearance_during_admission_imp_xgboost,
          PCNL_clearance_during_admission_omit_na_test,
          type = "raw")
PCNL_clearance_during_admission_imp_xgboost_tb <-
  table(
    pred = factor(PCNL_clearance_during_admission_imp_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_clearance_during_admission_omit_na_test$clearance_on_post_operative_radiological_imaging_during_a
  )
print(confusionMatrix(PCNL_clearance_during_admission_imp_xgboost_tb))
res_PCNL_clearance_during_admission_imp_xgboost <-
  evalm(PCNL_clearance_during_admission_imp_xgboost)
PCNL_clearance_during_admission_imp_xgboost_roc <-
  roc(
    clearance_during_admission_oversample_outcome,
    PCNL_clearance_during_admission_imp_xgboost_predict$Yes)
clearance_during_admission_imp_auc<-auc(PCNL_clearance_during_admission_imp_xgboost_roc)
print(clearance_during_admission_imp_auc)
print(ci.auc(PCNL_clearance_during_admission_imp_xgboost_roc))
varImp(PCNL_clearance_during_admission_imp_xgboost)

### Imputed and Oversampled
PCNL_clearance_during_admission_imp_oversample_xgboost_predict <-
  predict(PCNL_clearance_during_admission_imp_oversample_xgboost,
          PCNL_clearance_during_admission_omit_na_test,
          type = "prob")
PCNL_clearance_during_admission_imp_oversample_xgboost_predict2 <-
  predict(PCNL_clearance_during_admission_imp_oversample_xgboost,
          PCNL_clearance_during_admission_omit_na_test,
          type = "raw")
PCNL_clearance_during_admission_imp_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_clearance_during_admission_imp_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_clearance_during_admission_omit_na_test$clearance_on_post_operative_radiological_imaging_during_a
  )
print(confusionMatrix(PCNL_clearance_during_admission_imp_oversample_xgboost_tb))
res_PCNL_clearance_during_admission_imp_oversample_xgboost <-
  evalm(PCNL_clearance_during_admission_imp_oversample_xgboost)

PCNL_clearance_during_admission_imp_oversample_xgboost_roc <-
  roc(
    clearance_during_admission_oversample_outcome,
    PCNL_clearance_during_admission_imp_oversample_xgboost_predict$Yes)
clearance_during_admission_imp_oversample_auc<-auc(PCNL_clearance_during_admission_imp_oversample_xgboost_roc)
print(clearance_during_admission_imp_oversample_auc)
print(ci.auc(PCNL_clearance_during_admission_imp_oversample_xgboost_roc))
varImp(PCNL_clearance_during_admission_imp_oversample_xgboost)


### ROC
clearance_during_admission_roc_list <-
  list(
    PCNL_clearance_during_admission_na_omit_xgboost_roc,
    PCNL_clearance_during_admission_oversample_xgboost_roc,
    PCNL_clearance_during_admission_imp_xgboost_roc,
    PCNL_clearance_during_admission_imp_oversample_xgboost_roc
  )

clearance_during_admission_ci_list <-
  lapply(clearance_during_admission_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_clearance_during_admission_ci_list <- lapply(clearance_during_admission_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))

clearance_during_admission_roc_combined <-
  ggroc(clearance_during_admission_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Clearance on Inpatient, Post-Operative Imaging",
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
  clearance_during_admission_roc_combined <- clearance_during_admission_roc_combined + geom_ribbon(
    data = dat_clearance_during_admission_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

clearance_during_admission_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_clearance_during_admission_na_omit_xgboost_roc),
      ci.auc(PCNL_clearance_during_admission_oversample_xgboost_roc),
      ci.auc(PCNL_clearance_during_admission_imp_xgboost_roc),
      ci.auc(PCNL_clearance_during_admission_imp_oversample_xgboost_roc)
    )
  ) %>% as_tibble()
colnames(clearance_during_admission_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
clearance_during_admission_auc_data_overall$AUC <- as.numeric(clearance_during_admission_auc_data_overall$AUC) %>% round(digits = 2)
clearance_during_admission_auc_data_overall$LB <- as.numeric(clearance_during_admission_auc_data_overall$LB) %>% round(digits = 2)
clearance_during_admission_auc_data_overall$UB <- as.numeric(clearance_during_admission_auc_data_overall$UB) %>% round(digits = 2)
clearance_during_admission_auc_data_overall <-clearance_during_admission_auc_data_overall %>% relocate(AUC, .before = LB) 

clearance_during_admission_auc_data_overall <- clearance_during_admission_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

clearance_during_admission_auc_table <- clearance_during_admission_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(clearance_during_admission_roc_combined))
clearance_during_admission_auc_table_vp <- viewport(x = 0.7, y = 0.22, 
                                  just = c("right", "bottom"),
                                  height = 0.1, width = 0.2)
pushViewport(clearance_during_admission_auc_table_vp)
grid.draw(clearance_during_admission_auc_table)

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

### Omit NA only
PCNL_post_op_comp_xgboost_predict <-
  predict(PCNL_post_op_comp_xgboost,
          PCNL_post_op_comp_omit_na_test,
          type = "prob")
PCNL_post_op_comp_xgboost_predict2 <-
  predict(PCNL_post_op_comp_xgboost,
          PCNL_post_op_comp_omit_na_test,
          type = "raw")
PCNL_post_op_comp_na_omit_xgboost_tb <-
  table(
    pred = factor(PCNL_post_op_comp_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_post_op_comp_omit_na_test$postop_complications
  )
print(confusionMatrix(PCNL_post_op_comp_na_omit_xgboost_tb))
res_PCNL_post_op_comp_na_omit_xgboost <-
  evalm(PCNL_post_op_comp_xgboost)

PCNL_post_op_comp_na_omit_xgboost_roc <-
  roc(
    post_op_comp_na_omit_outcome,
    PCNL_post_op_comp_xgboost_predict$Yes)
post_op_comp_na_omit_auc<-auc(PCNL_post_op_comp_na_omit_xgboost_roc)
print(post_op_comp_na_omit_auc)
print(ci.auc(PCNL_post_op_comp_na_omit_xgboost_roc))
varImp(PCNL_post_op_comp_xgboost)

### Oversampled
PCNL_post_op_comp_oversample_xgboost_predict <-
  predict(PCNL_post_op_comp_oversample_xgboost,
          PCNL_post_op_comp_omit_na_test,
          type = "prob")
PCNL_post_op_comp_oversample_xgboost_predict2 <-
  predict(PCNL_post_op_comp_oversample_xgboost,
          PCNL_post_op_comp_omit_na_test,
          type = "raw")
PCNL_post_op_comp_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_post_op_comp_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_post_op_comp_omit_na_test$postop_complications
  )
print(confusionMatrix(PCNL_post_op_comp_oversample_xgboost_tb))
res_PCNL_post_op_comp_oversample_xgboost <-
  evalm(PCNL_post_op_comp_oversample_xgboost)

PCNL_post_op_comp_oversample_xgboost_roc <-
  roc(
    post_op_comp_oversample_outcome,
    PCNL_post_op_comp_oversample_xgboost_predict$Yes)
post_op_comp_oversample_auc<-auc(PCNL_post_op_comp_oversample_xgboost_roc)
print(post_op_comp_oversample_auc)
print(ci.auc(PCNL_post_op_comp_oversample_xgboost_roc))
varImp(PCNL_post_op_comp_oversample_xgboost)


### Imputed
PCNL_post_op_comp_imp_xgboost_predict <-
  predict(PCNL_post_op_comp_imp_xgboost,
          PCNL_post_op_comp_omit_na_test,
          type = "prob")
PCNL_post_op_comp_imp_xgboost_predict2 <-
  predict(PCNL_post_op_comp_imp_xgboost,
          PCNL_post_op_comp_omit_na_test,
          type = "raw")
PCNL_post_op_comp_imp_xgboost_tb <-
  table(
    pred = factor(PCNL_post_op_comp_imp_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_post_op_comp_omit_na_test$postop_complications
  )
print(confusionMatrix(PCNL_post_op_comp_imp_xgboost_tb))
res_PCNL_post_op_comp_imp_xgboost <-
  evalm(PCNL_post_op_comp_imp_xgboost)

PCNL_post_op_comp_imp_xgboost_roc <-
  roc(
    post_op_comp_oversample_outcome,
    PCNL_post_op_comp_imp_xgboost_predict$Yes)
post_op_comp_imp_auc<-auc(PCNL_post_op_comp_imp_xgboost_roc)
print(post_op_comp_imp_auc)
print(ci.auc(PCNL_post_op_comp_imp_xgboost_roc))
varImp(PCNL_post_op_comp_imp_xgboost)

### Imputed and Oversampled
PCNL_post_op_comp_imp_oversample_xgboost_predict <-
  predict(PCNL_post_op_comp_imp_oversample_xgboost,
          PCNL_post_op_comp_omit_na_test,
          type = "prob")
PCNL_post_op_comp_imp_oversample_xgboost_predict2 <-
  predict(PCNL_post_op_comp_imp_oversample_xgboost,
          PCNL_post_op_comp_omit_na_test,
          type = "raw")
PCNL_post_op_comp_imp_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_post_op_comp_imp_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_post_op_comp_omit_na_test$postop_complications
  )
print(confusionMatrix(PCNL_post_op_comp_imp_oversample_xgboost_tb))
res_PCNL_post_op_comp_imp_oversample_xgboost <-
  evalm(PCNL_post_op_comp_imp_oversample_xgboost)

PCNL_post_op_comp_imp_oversample_xgboost_roc <-
  roc(
    post_op_comp_oversample_outcome,
    PCNL_post_op_comp_imp_oversample_xgboost_predict$Yes)
post_op_comp_imp_oversample_auc<-auc(PCNL_post_op_comp_imp_oversample_xgboost_roc)
print(post_op_comp_imp_oversample_auc)
print(ci.auc(PCNL_post_op_comp_imp_oversample_xgboost_roc))
varImp(PCNL_post_op_comp_imp_oversample_xgboost)


### ROC
post_op_comp_roc_list <-
  list(
    PCNL_post_op_comp_na_omit_xgboost_roc,
    PCNL_post_op_comp_oversample_xgboost_roc,
    PCNL_post_op_comp_imp_xgboost_roc,
    PCNL_post_op_comp_imp_oversample_xgboost_roc
  )

post_op_comp_ci_list <-
  lapply(post_op_comp_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_post_op_comp_ci_list <- lapply(post_op_comp_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))

post_op_comp_roc_combined <-
  ggroc(post_op_comp_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Post-Operative Complications",
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
  post_op_comp_roc_combined <- post_op_comp_roc_combined + geom_ribbon(
    data = dat_post_op_comp_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

post_op_comp_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_post_op_comp_na_omit_xgboost_roc),
      ci.auc(PCNL_post_op_comp_oversample_xgboost_roc),
      ci.auc(PCNL_post_op_comp_imp_xgboost_roc),
      ci.auc(PCNL_post_op_comp_imp_oversample_xgboost_roc)
    )
  ) %>% as_tibble()
colnames(post_op_comp_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
post_op_comp_auc_data_overall$AUC <- as.numeric(post_op_comp_auc_data_overall$AUC) %>% round(digits = 2)
post_op_comp_auc_data_overall$LB <- as.numeric(post_op_comp_auc_data_overall$LB) %>% round(digits = 2)
post_op_comp_auc_data_overall$UB <- as.numeric(post_op_comp_auc_data_overall$UB) %>% round(digits = 2)
post_op_comp_auc_data_overall <-post_op_comp_auc_data_overall %>% relocate(AUC, .before = LB) 

post_op_comp_auc_data_overall <- post_op_comp_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

post_op_comp_auc_table <- post_op_comp_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(post_op_comp_roc_combined))
post_op_comp_auc_table_vp <- viewport(x = 0.7, y = 0.22, 
                               just = c("right", "bottom"),
                               height = 0.1, width = 0.2)
pushViewport(post_op_comp_auc_table_vp)
grid.draw(post_op_comp_auc_table)

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

### Omit NA only
PCNL_sf_at_fu_xgboost_predict <-
  predict(PCNL_sf_at_fu_xgboost,
          PCNL_sf_at_fu_omit_na_test,
          type = "prob")
PCNL_sf_at_fu_xgboost_predict2 <-
  predict(PCNL_sf_at_fu_xgboost,
          PCNL_sf_at_fu_omit_na_test,
          type = "raw")
PCNL_sf_at_fu_na_omit_xgboost_tb <-
  table(
    pred = factor(PCNL_sf_at_fu_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_sf_at_fu_omit_na_test$stone_free_at_follow_up
  )
print(confusionMatrix(PCNL_sf_at_fu_na_omit_xgboost_tb))
res_PCNL_sf_at_fu_na_omit_xgboost <-
  evalm(PCNL_sf_at_fu_xgboost)
PCNL_sf_at_fu_na_omit_xgboost_roc <-
  roc(
    sf_at_fu_na_omit_outcome,
    PCNL_sf_at_fu_xgboost_predict$Yes)
sf_at_fu_na_omit_auc<-auc(PCNL_sf_at_fu_na_omit_xgboost_roc)
print(sf_at_fu_na_omit_auc)
print(ci.auc(PCNL_sf_at_fu_na_omit_xgboost_roc))
varImp(PCNL_sf_at_fu_xgboost)

### Oversampled
PCNL_sf_at_fu_oversample_xgboost_predict <-
  predict(PCNL_sf_at_fu_oversample_xgboost,
          PCNL_sf_at_fu_omit_na_test,
          type = "prob")
PCNL_sf_at_fu_oversample_xgboost_predict2 <-
  predict(PCNL_sf_at_fu_oversample_xgboost,
          PCNL_sf_at_fu_omit_na_test,
          type = "raw")
PCNL_sf_at_fu_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_sf_at_fu_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_sf_at_fu_omit_na_test$stone_free_at_follow_up
  )
print(confusionMatrix(PCNL_sf_at_fu_oversample_xgboost_tb))
res_PCNL_sf_at_fu_oversample_xgboost <-
  evalm(PCNL_sf_at_fu_oversample_xgboost)

PCNL_sf_at_fu_oversample_xgboost_roc <-
  roc(
    sf_at_fu_oversample_outcome,
    PCNL_sf_at_fu_oversample_xgboost_predict$Yes)
sf_at_fu_oversample_auc<-auc(PCNL_sf_at_fu_oversample_xgboost_roc)
print(sf_at_fu_oversample_auc)
print(ci.auc(PCNL_sf_at_fu_oversample_xgboost_roc))
varImp(PCNL_sf_at_fu_oversample_xgboost)


### Imputed
PCNL_sf_at_fu_imp_xgboost_predict <-
  predict(PCNL_sf_at_fu_imp_xgboost,
          PCNL_sf_at_fu_omit_na_test,
          type = "prob")
PCNL_sf_at_fu_imp_xgboost_predict2 <-
  predict(PCNL_sf_at_fu_imp_xgboost,
          PCNL_sf_at_fu_omit_na_test,
          type = "raw")
PCNL_sf_at_fu_imp_xgboost_tb <-
  table(
    pred = factor(PCNL_sf_at_fu_imp_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_sf_at_fu_omit_na_test$stone_free_at_follow_up
  )
print(confusionMatrix(PCNL_sf_at_fu_imp_xgboost_tb))
res_PCNL_sf_at_fu_imp_xgboost <-
  evalm(PCNL_sf_at_fu_imp_xgboost)

PCNL_sf_at_fu_imp_xgboost_roc <-
  roc(
    sf_at_fu_oversample_outcome,
    PCNL_sf_at_fu_imp_xgboost_predict$Yes)
sf_at_fu_imp_auc<-auc(PCNL_sf_at_fu_imp_xgboost_roc)
print(sf_at_fu_imp_auc)
print(ci.auc(PCNL_sf_at_fu_imp_xgboost_roc))
varImp(PCNL_sf_at_fu_imp_xgboost)

### Imputed and Oversampled
PCNL_sf_at_fu_imp_oversample_xgboost_predict <-
  predict(PCNL_sf_at_fu_imp_oversample_xgboost,
          PCNL_sf_at_fu_omit_na_test,
          type = "prob")
PCNL_sf_at_fu_imp_oversample_xgboost_predict2 <-
  predict(PCNL_sf_at_fu_imp_oversample_xgboost,
          PCNL_sf_at_fu_omit_na_test,
          type = "raw")
PCNL_sf_at_fu_imp_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_sf_at_fu_imp_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_sf_at_fu_omit_na_test$stone_free_at_follow_up
  )
print(confusionMatrix(PCNL_sf_at_fu_imp_oversample_xgboost_tb))
res_PCNL_sf_at_fu_imp_oversample_xgboost <-
  evalm(PCNL_sf_at_fu_imp_oversample_xgboost)

PCNL_sf_at_fu_imp_oversample_xgboost_roc <-
  roc(
    sf_at_fu_oversample_outcome,
    PCNL_sf_at_fu_imp_oversample_xgboost_predict$Yes)
sf_at_fu_imp_oversample_auc<-auc(PCNL_sf_at_fu_imp_oversample_xgboost_roc)
print(sf_at_fu_imp_oversample_auc)
print(ci.auc(PCNL_sf_at_fu_imp_oversample_xgboost_roc))
varImp(PCNL_sf_at_fu_imp_oversample_xgboost)


### ROC
sf_at_fu_roc_list <-
  list(
    PCNL_sf_at_fu_na_omit_xgboost_roc,
    PCNL_sf_at_fu_oversample_xgboost_roc,
    PCNL_sf_at_fu_imp_xgboost_roc,
    PCNL_sf_at_fu_imp_oversample_xgboost_roc
  )

sf_at_fu_ci_list <-
  lapply(sf_at_fu_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_sf_at_fu_ci_list <- lapply(sf_at_fu_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))

sf_at_fu_roc_combined <-
  ggroc(sf_at_fu_roc_list, aes = c("color")) + theme_minimal() + labs(
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
  sf_at_fu_roc_combined <- sf_at_fu_roc_combined + geom_ribbon(
    data = dat_sf_at_fu_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

sf_at_fu_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_sf_at_fu_na_omit_xgboost_roc),
      ci.auc(PCNL_sf_at_fu_oversample_xgboost_roc),
      ci.auc(PCNL_sf_at_fu_imp_xgboost_roc),
      ci.auc(PCNL_sf_at_fu_imp_oversample_xgboost_roc)
    )
  ) %>% as_tibble()
colnames(sf_at_fu_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
sf_at_fu_auc_data_overall$AUC <- as.numeric(sf_at_fu_auc_data_overall$AUC) %>% round(digits = 2)
sf_at_fu_auc_data_overall$LB <- as.numeric(sf_at_fu_auc_data_overall$LB) %>% round(digits = 2)
sf_at_fu_auc_data_overall$UB <- as.numeric(sf_at_fu_auc_data_overall$UB) %>% round(digits = 2)
sf_at_fu_auc_data_overall <-sf_at_fu_auc_data_overall %>% relocate(AUC, .before = LB) 

sf_at_fu_auc_data_overall <- sf_at_fu_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

sf_at_fu_auc_table <- sf_at_fu_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(sf_at_fu_roc_combined))
sf_at_fu_auc_table_vp <- viewport(x = 0.7, y = 0.22, 
                              just = c("right", "bottom"),
                              height = 0.1, width = 0.2)
pushViewport(sf_at_fu_auc_table_vp)
grid.draw(sf_at_fu_auc_table)

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

### Omit NA only
PCNL_adj_rx_xgboost_predict <-
  predict(PCNL_adj_rx_xgboost,
          PCNL_adj_rx_omit_na_test,
          type = "prob")
PCNL_adj_rx_xgboost_predict2 <-
  predict(PCNL_adj_rx_xgboost,
          PCNL_adj_rx_omit_na_test,
          type = "raw")
PCNL_adj_rx_na_omit_xgboost_tb <-
  table(
    pred = factor(PCNL_adj_rx_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_adj_rx_omit_na_test$adjuvant_treatment
  )
print(confusionMatrix(PCNL_adj_rx_na_omit_xgboost_tb))
res_PCNL_adj_rx_na_omit_xgboost <-
  evalm(PCNL_adj_rx_xgboost)

PCNL_adj_rx_na_omit_xgboost_roc <-
  roc(
    adj_rx_na_omit_outcome,
    PCNL_adj_rx_xgboost_predict$Yes)
adj_rx_na_omit_auc<-auc(PCNL_adj_rx_na_omit_xgboost_roc)
print(adj_rx_na_omit_auc)
print(ci.auc(PCNL_adj_rx_na_omit_xgboost_roc))
varImp(PCNL_adj_rx_xgboost)

### Oversampled
PCNL_adj_rx_oversample_xgboost_predict <-
  predict(PCNL_adj_rx_oversample_xgboost,
          PCNL_adj_rx_omit_na_test,
          type = "prob")
PCNL_adj_rx_oversample_xgboost_predict2 <-
  predict(PCNL_adj_rx_oversample_xgboost,
          PCNL_adj_rx_omit_na_test,
          type = "raw")
PCNL_adj_rx_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_adj_rx_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_adj_rx_omit_na_test$adjuvant_treatment
  )
print(confusionMatrix(PCNL_adj_rx_oversample_xgboost_tb))
res_PCNL_adj_rx_oversample_xgboost <-
  evalm(PCNL_adj_rx_oversample_xgboost)

PCNL_adj_rx_oversample_xgboost_roc <-
  roc(
    adj_rx_oversample_outcome,
    PCNL_adj_rx_oversample_xgboost_predict$Yes)
adj_rx_oversample_auc<-auc(PCNL_adj_rx_oversample_xgboost_roc)
print(adj_rx_oversample_auc)
print(ci.auc(PCNL_adj_rx_oversample_xgboost_roc))
varImp(PCNL_adj_rx_oversample_xgboost)


### Imputed
PCNL_adj_rx_imp_xgboost_predict <-
  predict(PCNL_adj_rx_imp_xgboost,
          PCNL_adj_rx_omit_na_test,
          type = "prob")
PCNL_adj_rx_imp_xgboost_predict2 <-
  predict(PCNL_adj_rx_imp_xgboost,
          PCNL_adj_rx_omit_na_test,
          type = "raw")
PCNL_adj_rx_imp_xgboost_tb <-
  table(
    pred = factor(PCNL_adj_rx_imp_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_adj_rx_omit_na_test$adjuvant_treatment
  )
print(confusionMatrix(PCNL_adj_rx_imp_xgboost_tb))
res_PCNL_adj_rx_imp_xgboost <-
  evalm(PCNL_adj_rx_imp_xgboost)

PCNL_adj_rx_imp_xgboost_roc <-
  roc(
    adj_rx_oversample_outcome,
    PCNL_adj_rx_imp_xgboost_predict$Yes)
adj_rx_imp_auc<-auc(PCNL_adj_rx_imp_xgboost_roc)
print(adj_rx_imp_auc)
print(ci.auc(PCNL_adj_rx_imp_xgboost_roc))
varImp(PCNL_adj_rx_imp_xgboost)

### Imputed and Oversampled
PCNL_adj_rx_imp_oversample_xgboost_predict <-
  predict(PCNL_adj_rx_imp_oversample_xgboost,
          PCNL_adj_rx_omit_na_test,
          type = "prob")
PCNL_adj_rx_imp_oversample_xgboost_predict2 <-
  predict(PCNL_adj_rx_imp_oversample_xgboost,
          PCNL_adj_rx_omit_na_test,
          type = "raw")
PCNL_adj_rx_imp_oversample_xgboost_tb <-
  table(
    pred = factor(PCNL_adj_rx_imp_oversample_xgboost_predict2, levels = c("No", "Yes")),
    ref = PCNL_adj_rx_omit_na_test$adjuvant_treatment
  )
print(confusionMatrix(PCNL_adj_rx_imp_oversample_xgboost_tb))
res_PCNL_adj_rx_imp_oversample_xgboost <-
  evalm(PCNL_adj_rx_imp_oversample_xgboost)

PCNL_adj_rx_imp_oversample_xgboost_roc <-
  roc(
    adj_rx_oversample_outcome,
    PCNL_adj_rx_imp_oversample_xgboost_predict$Yes)
adj_rx_imp_oversample_auc<-auc(PCNL_adj_rx_imp_oversample_xgboost_roc)
print(adj_rx_imp_oversample_auc)
print(ci.auc(PCNL_adj_rx_imp_oversample_xgboost_roc))
varImp(PCNL_adj_rx_imp_oversample_xgboost)


### ROC
adj_rx_roc_list <-
  list(
    PCNL_adj_rx_na_omit_xgboost_roc,
    PCNL_adj_rx_oversample_xgboost_roc,
    PCNL_adj_rx_imp_xgboost_roc,
    PCNL_adj_rx_imp_oversample_xgboost_roc
  )

adj_rx_ci_list <-
  lapply(adj_rx_roc_list, 
         ci.se, 
         specificities = seq(0, 1, l = 25))

dat_adj_rx_ci_list <- lapply(adj_rx_ci_list, function(ciobj)
  data.frame(
    x = as.numeric(rownames(ciobj)),
    lower = ciobj[, 1],
    upper = ciobj[, 3]
  ))

adj_rx_roc_combined <-
  ggroc(adj_rx_roc_list, aes = c("color")) + theme_minimal() + labs(
    title = "Adjuvant Treatment",
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
  adj_rx_roc_combined <- adj_rx_roc_combined + geom_ribbon(
    data = dat_adj_rx_ci_list[[i]],
    aes(x = x, ymin = lower, ymax = upper),
    fill = i + 1,
    alpha = 0.2,
    inherit.aes = F) 
} 

adj_rx_auc_data_overall <-
  cbind(
    "Model" = c(
      "Original Dataset",
      "Oversampled",
      "Imputed",
      "Imputed and Oversampled"),
    "CI" = rbind(
      ci.auc(PCNL_adj_rx_na_omit_xgboost_roc),
      ci.auc(PCNL_adj_rx_oversample_xgboost_roc),
      ci.auc(PCNL_adj_rx_imp_xgboost_roc),
      ci.auc(PCNL_adj_rx_imp_oversample_xgboost_roc)
    )
  ) %>% as_tibble()
colnames(adj_rx_auc_data_overall) <- c("Model", "LB", "AUC", "UB")
adj_rx_auc_data_overall$AUC <- as.numeric(adj_rx_auc_data_overall$AUC) %>% round(digits = 2)
adj_rx_auc_data_overall$LB <- as.numeric(adj_rx_auc_data_overall$LB) %>% round(digits = 2)
adj_rx_auc_data_overall$UB <- as.numeric(adj_rx_auc_data_overall$UB) %>% round(digits = 2)
adj_rx_auc_data_overall <-adj_rx_auc_data_overall %>% relocate(AUC, .before = LB) 

adj_rx_auc_data_overall <- adj_rx_auc_data_overall %>% unite("95% CI", LB:UB, sep = "-")

adj_rx_auc_table <- adj_rx_auc_data_overall %>% tableGrob()

grid.draw(ggplotGrob(adj_rx_roc_combined))
adj_rx_auc_table_vp <- viewport(x = 0.7, y = 0.22, 
                                  just = c("right", "bottom"),
                                  height = 0.1, width = 0.2)
pushViewport(adj_rx_auc_table_vp)
grid.draw(adj_rx_auc_table)

popViewport()


#intraop_complications