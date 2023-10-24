PCNL_ctrl <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 5,
               classProbs = TRUE,
               savePredictions = TRUE,
               p = 1,
              summaryFunction = twoClassSummary)


#single_infection_outcome 

### Omit NA only
PCNL_Post_Infection_xgboost <-
  caret::train(
    single_infection_outcome ~ .,
    data = PCNL_single_infection_omit_na_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Oversampled
PCNL_Post_Infection_oversample_xgboost <-
  caret::train(
    single_infection_outcome ~ .,
    data = PCNL_single_infection_omit_na_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed
PCNL_Post_Infection_imp_xgboost <-
  caret::train(
    single_infection_outcome ~ .,
    data = PCNL_single_infection_imp_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed and Oversampled
PCNL_Post_Infection_imp_oversample_xgboost <-
  caret::train(
    single_infection_outcome ~ .,
    data = PCNL_single_infection_imp_oversample_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

#blood_transfusion

### Omit NA only
PCNL_transfusion_xgboost <-
  caret::train(
    blood_transfusion ~ .,
    data = PCNL_transfusion_omit_na_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Oversampled
PCNL_transfusion_oversample_xgboost <-
  caret::train(
    blood_transfusion ~ .,
    data = PCNL_transfusion_omit_na_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed
PCNL_transfusion_imp_xgboost <-
  caret::train(
    blood_transfusion ~ .,
    data = PCNL_transfusion_imp_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed and Oversampled
PCNL_transfusion_imp_oversample_xgboost <-
  caret::train(
    blood_transfusion ~ .,
    data = PCNL_transfusion_imp_oversample_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

#itu_hdu_admission
### Omit NA only
PCNL_itu_hdu_xgboost <-
  caret::train(
    itu_hdu_admission ~ .,
    data = PCNL_itu_hdu_omit_na_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Oversampled
PCNL_itu_hdu_oversample_xgboost <-
  caret::train(
    itu_hdu_admission ~ .,
    data = PCNL_itu_hdu_omit_na_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed
PCNL_itu_hdu_imp_xgboost <-
  caret::train(
    itu_hdu_admission ~ .,
    data = PCNL_itu_hdu_imp_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed and Oversampled
PCNL_itu_hdu_imp_oversample_xgboost <-
  caret::train(
    itu_hdu_admission ~ .,
    data = PCNL_itu_hdu_imp_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )


#complete_clearance_on_fluoroscopy
### Omit NA only
PCNL_clearance_on_fluoro_xgboost <-
  caret::train(
    complete_clearance_on_fluoroscopy ~ .,
    data = PCNL_clearance_on_fluoro_omit_na_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Oversampled
PCNL_clearance_on_fluoro_oversample_xgboost <-
  caret::train(
    complete_clearance_on_fluoroscopy ~ .,
    data = PCNL_clearance_on_fluoro_omit_na_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed
PCNL_clearance_on_fluoro_imp_xgboost <-
  caret::train(
    complete_clearance_on_fluoroscopy ~ .,
    data = PCNL_clearance_on_fluoro_imp_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed and Oversampled
PCNL_clearance_on_fluoro_imp_oversample_xgboost <-
  caret::train(
    complete_clearance_on_fluoroscopy ~ .,
    data = PCNL_clearance_on_fluoro_imp_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

#visceral_injury
### Omit NA only
PCNL_visc_inj_xgboost <-
  caret::train(
    visceral_injury ~ .,
    data = PCNL_visc_inj_omit_na_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Oversampled
PCNL_visc_inj_oversample_xgboost <-
  caret::train(
    visceral_injury ~ .,
    data = PCNL_visc_inj_omit_na_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed
PCNL_visc_inj_imp_xgboost <-
  caret::train(
    visceral_injury ~ .,
    data = PCNL_visc_inj_imp_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed and Oversampled
PCNL_visc_inj_imp_oversample_xgboost <-
  caret::train(
    visceral_injury ~ .,
    data = PCNL_visc_inj_imp_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

#clearance_on_post_operative_radiological_imaging_during_a
### Omit NA only
PCNL_clearance_during_admission_xgboost <-
  caret::train(
    clearance_on_post_operative_radiological_imaging_during_a ~ .,
    data = PCNL_clearance_during_admission_omit_na_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Oversampled
PCNL_clearance_during_admission_oversample_xgboost <-
  caret::train(
    clearance_on_post_operative_radiological_imaging_during_a ~ .,
    data = PCNL_clearance_during_admission_omit_na_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed
PCNL_clearance_during_admission_imp_xgboost <-
  caret::train(
    clearance_on_post_operative_radiological_imaging_during_a ~ .,
    data = PCNL_clearance_during_admission_imp_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed and Oversampled
PCNL_clearance_during_admission_imp_oversample_xgboost <-
  caret::train(
    clearance_on_post_operative_radiological_imaging_during_a ~ .,
    data = PCNL_clearance_during_admission_imp_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

#postop_complications
### Omit NA only
PCNL_post_op_comp_xgboost <-
  caret::train(
    postop_complications ~ .,
    data = PCNL_post_op_comp_omit_na_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Oversampled
PCNL_post_op_comp_oversample_xgboost <-
  caret::train(
    postop_complications ~ .,
    data = PCNL_post_op_comp_omit_na_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed
PCNL_post_op_comp_imp_xgboost <-
  caret::train(
    postop_complications ~ .,
    data = PCNL_post_op_comp_imp_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed and Oversampled
PCNL_post_op_comp_imp_oversample_xgboost <-
  caret::train(
    postop_complications ~ .,
    data = PCNL_post_op_comp_imp_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

#stone_free_at_follow_up
### Omit NA only
PCNL_sf_at_fu_xgboost <-
  caret::train(
    stone_free_at_follow_up ~ .,
    data = PCNL_sf_at_fu_omit_na_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Oversampled
PCNL_sf_at_fu_oversample_xgboost <-
  caret::train(
    stone_free_at_follow_up ~ .,
    data = PCNL_sf_at_fu_omit_na_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed
PCNL_sf_at_fu_imp_xgboost <-
  caret::train(
    stone_free_at_follow_up ~ .,
    data = PCNL_sf_at_fu_imp_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed and Oversampled
PCNL_sf_at_fu_imp_oversample_xgboost <-
  caret::train(
    stone_free_at_follow_up ~ .,
    data = PCNL_sf_at_fu_imp_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

#adjuvant_treatment
### Omit NA only
PCNL_adj_rx_xgboost <-
  caret::train(
    adjuvant_treatment ~ .,
    data = PCNL_adj_rx_omit_na_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Oversampled
PCNL_adj_rx_oversample_xgboost <-
  caret::train(
    adjuvant_treatment ~ .,
    data = PCNL_adj_rx_omit_na_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed
PCNL_adj_rx_imp_xgboost <-
  caret::train(
    adjuvant_treatment ~ .,
    data = PCNL_adj_rx_imp_train,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )

### Imputed and Oversampled
PCNL_adj_rx_imp_oversample_xgboost <-
  caret::train(
    adjuvant_treatment ~ .,
    data = PCNL_adj_rx_imp_train_oversample,
    method = "xgbTree",
    trControl = PCNL_ctrl,
    metric="ROC"
  )
