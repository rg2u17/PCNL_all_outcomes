#single_infection_outcome 

### Omit NA only
PCNL_Post_Infection_glm <-
  glm(
    single_infection_outcome ~ .,
    data = PCNL_single_infection_omit_na_train,
    family=binomial
  )

### Oversampled
PCNL_Post_Infection_oversample_glm <-
  glm(
    single_infection_outcome ~ .,
    data = PCNL_single_infection_omit_na_train_oversample,
    family=binomial
  )

### Imputed
PCNL_Post_Infection_imp_glm <-
  glm(
    single_infection_outcome ~ .,
    data = PCNL_single_infection_imp_train,
    family = binomial
  )

### Imputed and Oversampled
PCNL_Post_Infection_imp_oversample_glm <-
  glm(
    single_infection_outcome ~ .,
    data = PCNL_single_infection_imp_oversample_train,
    family=binomial
  )

#blood_transfusion

### Omit NA only
PCNL_transfusion_glm <-
  glm(
    blood_transfusion ~ .,
    data = PCNL_transfusion_omit_na_train,
    family = binomial
  )

### Oversampled
PCNL_transfusion_oversample_glm <-
  glm(
    blood_transfusion ~ .,
    data = PCNL_transfusion_omit_na_train_oversample,
    family = binomial
  )

### Imputed
PCNL_transfusion_imp_glm <-
  glm(
    blood_transfusion ~ .,
    data = PCNL_transfusion_imp_train,
    family = binomial
  )

### Imputed and Oversampled
PCNL_transfusion_imp_oversample_glm <-
  glm(
    blood_transfusion ~ .,
    data = PCNL_transfusion_imp_oversample_train,
    family = binomial
  )

#itu_hdu_admission
### Omit NA only
PCNL_itu_hdu_glm <-
  glm(
    itu_hdu_admission ~ .,
    data = PCNL_itu_hdu_omit_na_train,
    family = binomial
  )

### Oversampled
PCNL_itu_hdu_oversample_glm <-
  glm(
    itu_hdu_admission ~ .,
    data = PCNL_itu_hdu_omit_na_train_oversample,
    family = binomial
  )

### Imputed
PCNL_itu_hdu_imp_glm <-
  glm(
    itu_hdu_admission ~ .,
    data = PCNL_itu_hdu_imp_train,
    family = binomial
  )

### Imputed and Oversampled
PCNL_itu_hdu_imp_oversample_glm <-
  glm(
    itu_hdu_admission ~ .,
    data = PCNL_itu_hdu_imp_train_oversample,
    family = binomial
  )


#complete_clearance_on_fluoroscopy
### Omit NA only
PCNL_clearance_on_fluoro_glm <-
  glm(
    complete_clearance_on_fluoroscopy ~ .,
    data = PCNL_clearance_on_fluoro_omit_na_train,
    family = binomial
  )

### Oversampled
PCNL_clearance_on_fluoro_oversample_glm <-
  glm(
    complete_clearance_on_fluoroscopy ~ .,
    data = PCNL_clearance_on_fluoro_omit_na_train_oversample,
    family = binomial
  )

### Imputed
PCNL_clearance_on_fluoro_imp_glm <-
  glm(
    complete_clearance_on_fluoroscopy ~ .,
    data = PCNL_clearance_on_fluoro_imp_train,
    family = binomial
  )

### Imputed and Oversampled
PCNL_clearance_on_fluoro_imp_oversample_glm <-
  glm(
    complete_clearance_on_fluoroscopy ~ .,
    data = PCNL_clearance_on_fluoro_imp_train_oversample,
    family = binomial
  )

#visceral_injury
### Omit NA only
PCNL_visc_inj_glm <-
  glm(
    visceral_injury ~ .,
    data = PCNL_visc_inj_omit_na_train,
    family = binomial
  )

### Oversampled
PCNL_visc_inj_oversample_glm <-
  glm(
    visceral_injury ~ .,
    data = PCNL_visc_inj_omit_na_train_oversample,
    family = binomial
  )

### Imputed
PCNL_visc_inj_imp_glm <-
  glm(
    visceral_injury ~ .,
    data = PCNL_visc_inj_imp_train,
    family = binomial
  )

### Imputed and Oversampled
PCNL_visc_inj_imp_oversample_glm <-
  glm(
    visceral_injury ~ .,
    data = PCNL_visc_inj_imp_train_oversample,
    family = binomial
  )

#clearance_on_post_operative_radiological_imaging_during_a
### Omit NA only
PCNL_clearance_during_admission_glm <-
  glm(
    clearance_on_post_operative_radiological_imaging_during_a ~ .,
    data = PCNL_clearance_during_admission_omit_na_train,
    family = binomial
  )

### Oversampled
PCNL_clearance_during_admission_oversample_glm <-
  glm(
    clearance_on_post_operative_radiological_imaging_during_a ~ .,
    data = PCNL_clearance_during_admission_omit_na_train_oversample,
    family = binomial
  )

### Imputed
PCNL_clearance_during_admission_imp_glm <-
  glm(
    clearance_on_post_operative_radiological_imaging_during_a ~ .,
    data = PCNL_clearance_during_admission_imp_train,
    family = binomial
  )

### Imputed and Oversampled
PCNL_clearance_during_admission_imp_oversample_glm <-
  glm(
    clearance_on_post_operative_radiological_imaging_during_a ~ .,
    data = PCNL_clearance_during_admission_imp_train_oversample,
    family = binomial
  )

#postop_complications
### Omit NA only
PCNL_post_op_comp_glm <-
  glm(
    postop_complications ~ .,
    data = PCNL_post_op_comp_omit_na_train,
    family = binomial
  )

### Oversampled
PCNL_post_op_comp_oversample_glm <-
  glm(
    postop_complications ~ .,
    data = PCNL_post_op_comp_omit_na_train_oversample,
    family = binomial
  )

### Imputed
PCNL_post_op_comp_imp_glm <-
  glm(
    postop_complications ~ .,
    data = PCNL_post_op_comp_imp_train,
    family = binomial
  )

### Imputed and Oversampled
PCNL_post_op_comp_imp_oversample_glm <-
  glm(
    postop_complications ~ .,
    data = PCNL_post_op_comp_imp_train_oversample,
    family = binomial
  )

#stone_free_at_follow_up
### Omit NA only
PCNL_sf_at_fu_glm <-
  glm(
    stone_free_at_follow_up ~ .,
    data = PCNL_sf_at_fu_omit_na_train,
    family = binomial
  )

### Oversampled
PCNL_sf_at_fu_oversample_glm <-
  glm(
    stone_free_at_follow_up ~ .,
    data = PCNL_sf_at_fu_omit_na_train_oversample,
    family = binomial
  )

### Imputed
PCNL_sf_at_fu_imp_glm <-
  glm(
    stone_free_at_follow_up ~ .,
    data = PCNL_sf_at_fu_imp_train,
    family = binomial
  )

### Imputed and Oversampled
PCNL_sf_at_fu_imp_oversample_glm <-
  glm(
    stone_free_at_follow_up ~ .,
    data = PCNL_sf_at_fu_imp_train_oversample,
    family = binomial
  )

#adjuvant_treatment
### Omit NA only
PCNL_adj_rx_glm <-
  glm(
    adjuvant_treatment ~ .,
    data = PCNL_adj_rx_omit_na_train,
    family = binomial
  )

### Oversampled
PCNL_adj_rx_oversample_glm <-
  glm(
    adjuvant_treatment ~ .,
    data = PCNL_adj_rx_omit_na_train_oversample,
    family = binomial
  )

### Imputed
PCNL_adj_rx_imp_glm <-
  glm(
    adjuvant_treatment ~ .,
    data = PCNL_adj_rx_imp_train,
    family = binomial
  )

### Imputed and Oversampled
PCNL_adj_rx_imp_oversample_glm <-
  glm(
    adjuvant_treatment ~ .,
    data = PCNL_adj_rx_imp_train_oversample,
    family = binomial
  )


