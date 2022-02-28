# collect the results and sort by our model performance metric of choice
hyper_grid <- list(
  ntrees      = seq(200, 400, by = 200),
  mtries      = seq(2, 9, by = 4),
  max_depth   = seq(10, 40, by = 20),
  min_rows    = seq(5, 10, by = 5),
  nbins       = seq(10, 40, by = 20),
  sample_rate = c(.55, .632, .75, .8)
)

# Find Parameters: Use Hyper Parameter Tuning on a "Training Dataset" that sections your training data into 5-Folds. The output at Stage 1 is the parameter set.
# build grid search 
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
  x = features, 
  y = response, 
  training_frame = train,
  keep_cross_validation_predictions = TRUE,
  keep_cross_validation_models = TRUE,
  keep_cross_validation_fold_assignment = TRUE, 
  nfolds = 10,  
  categorical_encoding = "AUTO",
  hyper_params = hyper_grid,
  search_criteria = search_criteria
)

random_grid

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)


# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)
best_model

h2o.varimp(best_model)
h2o.varimp_plot(best_model)


# Save model
model_path <- h2o.saveModel(object = best_model, path = getwd(), force = TRUE)
print(model_path)
saved_model <- h2o.loadModel(model_path)

h2o.scoreHistory(best_model)

# Get the CV models from the `best_model` object
cv_models <- sapply(best_model@model$cross_validation_models, 
                    function(i) h2o.getModel(i$name))
cv_models

# model_path <- h2o.saveModel(object = cv_models, path = getwd(), force = TRUE)
# print(model_path)


# Compare and Select Best Model: Evaluate the performance on a hidden "Test Dataset". The ouput at Stage 2 is that we determine best model.
# Now let's evaluate the model performance on a test set
best_model_perf <- h2o.performance(model = best_model, newdata = test)
best_model_perf
# RMSE of best model
h2o.mse(best_model_perf) %>% sqrt()


test$h2o_rf <- predict(best_model, test)
test <- as.data.frame(test)
write.csv(test, "results/DRF/test_h2o_RF.csv")

file_shared$h2o_rf <- predict(best_model, file_shared)

# Train Final Model: Once we have selected the best model, we train on the full dataset. This model goes into production.
model_drf <- h2o.randomForest(x = features, 
                              y = response, 
                              training_frame = file_shared,
                              ntrees = 400,
                              sample_rate = 0.8,
                              max_depth = 30,
                              min_rows = 5,
                              nbins = 10,
                              mtries = 6,
                              keep_cross_validation_predictions = TRUE,
                              keep_cross_validation_models = TRUE,
                              keep_cross_validation_fold_assignment = TRUE, 
                              nfolds = 10)

model_drf


cvpreds_id <- model_drf@model$cross_validation_holdout_predictions_frame_id$name
file_shared$cvpreds <- h2o.getFrame(cvpreds_id)

h2o.varimp(model_drf)
h2o.varimp_plot(model_drf)
file_shared$h2o_rf_m <- predict(model_drf, file_shared)

model_drf_sp <- h2o.randomForest(x = features, 
                                y = response, 
                                training_frame = file_shared,
                                ntrees = 400,
                                sample_rate = 0.8,
                                max_depth = 30,
                                min_rows = 5,
                                nbins = 10,
                                mtries = 6,
                                keep_cross_validation_predictions = TRUE,
                                keep_cross_validation_models = TRUE,
                                fold_column = "Station_code")
model_drf_sp
cvpreds_id_sp <- model_drf_sp@model$cross_validation_holdout_predictions_frame_id$name
file_shared$cvpreds_sp <- h2o.getFrame(cvpreds_id_sp)
h2o.varimp(model_drf_sp)
h2o.varimp_plot(model_drf_sp)
file_shared$h2o_drf_sp <- predict(model_drf_sp, file_shared)

model_drf_temp <- h2o.randomForest(x = features, 
                                  y = response, 
                                  training_frame = file_shared,
                                  ntrees = 400,
                                  sample_rate = 0.8,
                                  max_depth = 30,
                                  min_rows = 5,
                                  nbins = 10,
                                  mtries = 6,
                                  keep_cross_validation_predictions = TRUE,
                                  keep_cross_validation_models = TRUE,
                                  fold_column = "month")
model_drf_temp
cvpreds_id_temp <- model_drf_temp@model$cross_validation_holdout_predictions_frame_id$name
file_shared$cvpreds_temp <- h2o.getFrame(cvpreds_id_temp)
h2o.varimp(model_drf_temp)
h2o.varimp_plot(model_drf_temp)
file_shared$h2o_drf_temp <- predict(model_drf_temp, file_shared)


file_shared <- as.data.frame(file_shared)
ggplot(file_shared, aes(PM2.5, h2o_rf_m)) + geom_point() + geom_smooth(method = "lm")
summary(lm(PM2.5 ~ h2o_rf_m, data = file_shared))
mean(abs((file_shared$PM2.5 - file_shared$h2o_rf_m) / file_shared$PM2.5), na.rm = TRUE) * 100

write.csv(file_shared, "results/DRF/h2o_RF.csv")
