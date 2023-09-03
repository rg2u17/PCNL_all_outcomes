#single_infection_outcome 

### Omit NA only
PCNL_Post_Infection_keras <- keras_model_sequential()
PCNL_Post_Infection_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_Post_Infection_keras %>% compile(loss = 'binary_crossentropy',
                                      optimizer = 'rmsprop',
                                      metrics = c('accuracy'))
PCNL_Post_Infection_keras %>% fit(
  PCNL_single_infection_omit_na_train_predictors2,
  PCNL_single_infection_omit_na_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Oversampled
PCNL_Post_Infection_oversample_keras <- keras_model_sequential()
PCNL_Post_Infection_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_Post_Infection_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                      optimizer = 'rmsprop',
                                      metrics = c('accuracy'))
PCNL_Post_Infection_oversample_keras %>% fit(
  PCNL_single_infection_oversample_train_predictors2,
  PCNL_single_infection_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed
PCNL_Post_Infection_imp_keras <- keras_model_sequential()
PCNL_Post_Infection_imp_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_Post_Infection_imp_keras %>% compile(loss = 'binary_crossentropy',
                                                 optimizer = 'rmsprop',
                                                 metrics = c('accuracy'))
PCNL_Post_Infection_imp_keras %>% fit(
  PCNL_single_infection_imp_train_predictors2,
  PCNL_single_infection_imp_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed and Oversampled
PCNL_Post_Infection_imp_oversample_keras <- keras_model_sequential()
PCNL_Post_Infection_imp_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_Post_Infection_imp_oversample_keras %>% compile(loss = 'binary_crossentropy',
                         optimizer = 'rmsprop',
                         metrics = c('accuracy'))
PCNL_Post_Infection_imp_oversample_keras %>% fit(
  PCNL_single_infection_imp_oversample_train_predictors2,
  PCNL_single_infection_imp_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

#blood_transfusion
### Omit NA only
PCNL_transfusion_keras <- keras_model_sequential()
PCNL_transfusion_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_transfusion_keras %>% compile(loss = 'binary_crossentropy',
                                      optimizer = 'rmsprop',
                                      metrics = c('accuracy'))
PCNL_transfusion_keras %>% fit(
  PCNL_transfusion_omit_na_train_predictors2,
  PCNL_transfusion_omit_na_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Oversampled
PCNL_transfusion_oversample_keras <- keras_model_sequential()
PCNL_transfusion_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_transfusion_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                                 optimizer = 'rmsprop',
                                                 metrics = c('accuracy'))
PCNL_transfusion_oversample_keras %>% fit(
  PCNL_transfusion_oversample_train_predictors2,
  PCNL_transfusion_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed
PCNL_transfusion_imp_keras <- keras_model_sequential()
PCNL_transfusion_imp_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_transfusion_imp_keras %>% compile(loss = 'binary_crossentropy',
                                          optimizer = 'rmsprop',
                                          metrics = c('accuracy'))
PCNL_transfusion_imp_keras %>% fit(
  PCNL_transfusion_imp_train_predictors2,
  PCNL_transfusion_imp_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed and Oversampled
PCNL_transfusion_imp_oversample_keras <- keras_model_sequential()
PCNL_transfusion_imp_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_transfusion_imp_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                                     optimizer = 'rmsprop',
                                                     metrics = c('accuracy'))
PCNL_transfusion_imp_oversample_keras %>% fit(
  PCNL_transfusion_imp_oversample_train_predictors2,
  PCNL_transfusion_imp_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

#itu_hdu_admission
### Omit NA only
PCNL_itu_hdu_keras <- keras_model_sequential()
PCNL_itu_hdu_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_itu_hdu_keras %>% compile(loss = 'binary_crossentropy',
                                   optimizer = 'rmsprop',
                                   metrics = c('accuracy'))
PCNL_itu_hdu_keras %>% fit(
  PCNL_itu_hdu_omit_na_train_predictors2,
  PCNL_itu_hdu_omit_na_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Oversampled
PCNL_itu_hdu_oversample_keras <- keras_model_sequential()
PCNL_itu_hdu_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_itu_hdu_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                              optimizer = 'rmsprop',
                                              metrics = c('accuracy'))
PCNL_itu_hdu_oversample_keras %>% fit(
  PCNL_itu_hdu_oversample_train_predictors2,
  PCNL_itu_hdu_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed
PCNL_itu_hdu_imp_keras <- keras_model_sequential()
PCNL_itu_hdu_imp_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_itu_hdu_imp_keras %>% compile(loss = 'binary_crossentropy',
                                       optimizer = 'rmsprop',
                                       metrics = c('accuracy'))
PCNL_itu_hdu_imp_keras %>% fit(
  PCNL_itu_hdu_imp_train_predictors2,
  PCNL_itu_hdu_imp_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed and Oversampled
PCNL_itu_hdu_imp_oversample_keras <- keras_model_sequential()
PCNL_itu_hdu_imp_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_itu_hdu_imp_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                                  optimizer = 'rmsprop',
                                                  metrics = c('accuracy'))
PCNL_itu_hdu_imp_oversample_keras %>% fit(
  PCNL_itu_hdu_imp_oversample_train_predictors2,
  PCNL_itu_hdu_imp_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

#complete_clearance_on_fluoroscopy
### Omit NA only
PCNL_clearance_on_fluoro_keras <- keras_model_sequential()
PCNL_clearance_on_fluoro_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_clearance_on_fluoro_keras %>% compile(loss = 'binary_crossentropy',
                               optimizer = 'rmsprop',
                               metrics = c('accuracy'))
PCNL_clearance_on_fluoro_keras %>% fit(
  PCNL_clearance_on_fluoro_omit_na_train_predictors2,
  PCNL_clearance_on_fluoro_omit_na_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Oversampled
PCNL_clearance_on_fluoro_oversample_keras <- keras_model_sequential()
PCNL_clearance_on_fluoro_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_clearance_on_fluoro_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                          optimizer = 'rmsprop',
                                          metrics = c('accuracy'))
PCNL_clearance_on_fluoro_oversample_keras %>% fit(
  PCNL_clearance_on_fluoro_oversample_train_predictors2,
  PCNL_clearance_on_fluoro_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed
PCNL_clearance_on_fluoro_imp_keras <- keras_model_sequential()
PCNL_clearance_on_fluoro_imp_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_clearance_on_fluoro_imp_keras %>% compile(loss = 'binary_crossentropy',
                                   optimizer = 'rmsprop',
                                   metrics = c('accuracy'))
PCNL_clearance_on_fluoro_imp_keras %>% fit(
  PCNL_clearance_on_fluoro_imp_train_predictors2,
  PCNL_clearance_on_fluoro_imp_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed and Oversampled
PCNL_clearance_on_fluoro_imp_oversample_keras <- keras_model_sequential()
PCNL_clearance_on_fluoro_imp_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_clearance_on_fluoro_imp_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                              optimizer = 'rmsprop',
                                              metrics = c('accuracy'))
PCNL_clearance_on_fluoro_imp_oversample_keras %>% fit(
  PCNL_clearance_on_fluoro_imp_oversample_train_predictors2,
  PCNL_clearance_on_fluoro_imp_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

#visceral_injury
### Omit NA only
PCNL_visc_inj_keras <- keras_model_sequential()
PCNL_visc_inj_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_visc_inj_keras %>% compile(loss = 'binary_crossentropy',
                                           optimizer = 'rmsprop',
                                           metrics = c('accuracy'))
PCNL_visc_inj_keras %>% fit(
  PCNL_visc_inj_omit_na_train_predictors2,
  PCNL_visc_inj_omit_na_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Oversampled
PCNL_visc_inj_oversample_keras <- keras_model_sequential()
PCNL_visc_inj_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_visc_inj_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                                      optimizer = 'rmsprop',
                                                      metrics = c('accuracy'))
PCNL_visc_inj_oversample_keras %>% fit(
  PCNL_visc_inj_oversample_train_predictors2,
  PCNL_visc_inj_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed
PCNL_visc_inj_imp_keras <- keras_model_sequential()
PCNL_visc_inj_imp_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_visc_inj_imp_keras %>% compile(loss = 'binary_crossentropy',
                                               optimizer = 'rmsprop',
                                               metrics = c('accuracy'))
PCNL_visc_inj_imp_keras %>% fit(
  PCNL_visc_inj_imp_train_predictors2,
  PCNL_visc_inj_imp_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed and Oversampled
PCNL_visc_inj_imp_oversample_keras <- keras_model_sequential()
PCNL_visc_inj_imp_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_visc_inj_imp_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                                          optimizer = 'rmsprop',
                                                          metrics = c('accuracy'))
PCNL_visc_inj_imp_oversample_keras %>% fit(
  PCNL_visc_inj_imp_oversample_train_predictors2,
  PCNL_visc_inj_imp_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

#postop_complications
### Omit NA only
PCNL_post_op_comp_keras <- keras_model_sequential()
PCNL_post_op_comp_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_post_op_comp_keras %>% compile(loss = 'binary_crossentropy',
                                optimizer = 'rmsprop',
                                metrics = c('accuracy'))
PCNL_post_op_comp_keras %>% fit(
  PCNL_post_op_comp_omit_na_train_predictors2,
  PCNL_post_op_comp_omit_na_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Oversampled
PCNL_post_op_comp_oversample_keras <- keras_model_sequential()
PCNL_post_op_comp_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_post_op_comp_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                           optimizer = 'rmsprop',
                                           metrics = c('accuracy'))
PCNL_post_op_comp_oversample_keras %>% fit(
  PCNL_post_op_comp_oversample_train_predictors2,
  PCNL_post_op_comp_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed
PCNL_post_op_comp_imp_keras <- keras_model_sequential()
PCNL_post_op_comp_imp_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_post_op_comp_imp_keras %>% compile(loss = 'binary_crossentropy',
                                    optimizer = 'rmsprop',
                                    metrics = c('accuracy'))
PCNL_post_op_comp_imp_keras %>% fit(
  PCNL_post_op_comp_imp_train_predictors2,
  PCNL_post_op_comp_imp_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed and Oversampled
PCNL_post_op_comp_imp_oversample_keras <- keras_model_sequential()
PCNL_post_op_comp_imp_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_post_op_comp_imp_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                               optimizer = 'rmsprop',
                                               metrics = c('accuracy'))
PCNL_post_op_comp_imp_oversample_keras %>% fit(
  PCNL_post_op_comp_imp_oversample_train_predictors2,
  PCNL_post_op_comp_imp_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

#stone_free_at_follow_up
### Omit NA only
PCNL_sf_at_fu_keras <- keras_model_sequential()
PCNL_sf_at_fu_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_sf_at_fu_keras %>% compile(loss = 'binary_crossentropy',
                                    optimizer = 'rmsprop',
                                    metrics = c('accuracy'))
PCNL_sf_at_fu_keras %>% fit(
  PCNL_sf_at_fu_omit_na_train_predictors2,
  PCNL_sf_at_fu_omit_na_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Oversampled
PCNL_sf_at_fu_oversample_keras <- keras_model_sequential()
PCNL_sf_at_fu_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_sf_at_fu_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                               optimizer = 'rmsprop',
                                               metrics = c('accuracy'))
PCNL_sf_at_fu_oversample_keras %>% fit(
  PCNL_sf_at_fu_oversample_train_predictors2,
  PCNL_sf_at_fu_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed
PCNL_sf_at_fu_imp_keras <- keras_model_sequential()
PCNL_sf_at_fu_imp_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_sf_at_fu_imp_keras %>% compile(loss = 'binary_crossentropy',
                                        optimizer = 'rmsprop',
                                        metrics = c('accuracy'))
PCNL_sf_at_fu_imp_keras %>% fit(
  PCNL_sf_at_fu_imp_train_predictors2,
  PCNL_sf_at_fu_imp_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed and Oversampled
PCNL_sf_at_fu_imp_oversample_keras <- keras_model_sequential()
PCNL_sf_at_fu_imp_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_sf_at_fu_imp_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                                   optimizer = 'rmsprop',
                                                   metrics = c('accuracy'))
PCNL_sf_at_fu_imp_oversample_keras %>% fit(
  PCNL_sf_at_fu_imp_oversample_train_predictors2,
  PCNL_sf_at_fu_imp_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

#adjuvant_treatment
### Omit NA only
PCNL_adj_rx_keras <- keras_model_sequential()
PCNL_adj_rx_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_adj_rx_keras %>% compile(loss = 'binary_crossentropy',
                                optimizer = 'rmsprop',
                                metrics = c('accuracy'))
PCNL_adj_rx_keras %>% fit(
  PCNL_adj_rx_omit_na_train_predictors2,
  PCNL_adj_rx_omit_na_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Oversampled
PCNL_adj_rx_oversample_keras <- keras_model_sequential()
PCNL_adj_rx_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_adj_rx_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                           optimizer = 'rmsprop',
                                           metrics = c('accuracy'))
PCNL_adj_rx_oversample_keras %>% fit(
  PCNL_adj_rx_oversample_train_predictors2,
  PCNL_adj_rx_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed
PCNL_adj_rx_imp_keras <- keras_model_sequential()
PCNL_adj_rx_imp_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_adj_rx_imp_keras %>% compile(loss = 'binary_crossentropy',
                                    optimizer = 'rmsprop',
                                    metrics = c('accuracy'))
PCNL_adj_rx_imp_keras %>% fit(
  PCNL_adj_rx_imp_train_predictors2,
  PCNL_adj_rx_imp_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)

### Imputed and Oversampled
PCNL_adj_rx_imp_oversample_keras <- keras_model_sequential()
PCNL_adj_rx_imp_oversample_keras %>% layer_dense(units = 41, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = 'sigmoid')
PCNL_adj_rx_imp_oversample_keras %>% compile(loss = 'binary_crossentropy',
                                               optimizer = 'rmsprop',
                                               metrics = c('accuracy'))
PCNL_adj_rx_imp_oversample_keras %>% fit(
  PCNL_adj_rx_imp_oversample_train_predictors2,
  PCNL_adj_rx_imp_oversample_train_outcome,
  epochs = 25,
  batch_size = 5,
  validation_split = 0,
  callbacks = list(callback_early_stopping(
    monitor = "loss",
    patience = 5,
    restore_best_weights = TRUE
  ))
)