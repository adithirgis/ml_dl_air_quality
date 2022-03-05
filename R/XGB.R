# xgboost

hyper_grid <- list(
  sample_rate = seq(0.2, 1, 0.01),
  reg_lambda = c(0, 0.0001, 0.001, 0.1, 1),
  reg_alpha = c(0, 0.0001, 0.001, 0.1, 1),
  col_sample_rate = seq(0.2, 1, 0.01),
  col_sample_rate_per_tree = seq(0.2, 1, 0.01),
  min_rows = 2 ^ seq(0, log2(nrow(train))-1, 1),
  min_split_improvement = c(0, 1e-8, 1e-6, 1e-4),
  ntrees = c(500, 1000, 1500), 
  max_depth = c(4, 6, 8, 12, 16, 20), 
  min_child_weight = c(1, 2, 3),
  eta = c(0.025, 0.05, 0.1, 0.3),
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  distribution = c("AUTO", "gaussian", "poisson", "gamma"),
  tree_method = c("auto", "exact"),
  grow_policy = c("depthwise"),
  booster = c("gbtree", "gblinear", "dart")
)

xgb_grid_m <- h2o.grid(
  hyper_params = hyper_grid,
  search_criteria = search_criteria,
  algorithm = "xgboost",
  grid_id = "xgb_grid",
  categorical_encoding = "AUTO",
  x = features,
  y = response,
  keep_cross_validation_predictions = TRUE,
  keep_cross_validation_models = TRUE,
  keep_cross_validation_fold_assignment = TRUE,
  training_frame = train,
  nfolds = 10,
  score_tree_interval = 10,
  seed = 108
)
xgb_grid_m

grid_perf <- h2o.getGrid(
  grid_id = "xgb_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)

best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)
best_model
h2o.varimp(best_model)
h2o.varimp_plot(best_model)
model_path <- h2o.saveModel(object = best_model, path = getwd(), force = TRUE)
print(model_path)
saved_model <- h2o.loadModel(model_path)

h2o.scoreHistory(best_model)

cv_models <- sapply(best_model@model$cross_validation_models, 
                    function(i) h2o.getModel(i$name))
cv_models


best_model_perf <- h2o.performance(model = best_model, newdata = test)
best_model_perf
h2o.mse(best_model_perf) %>% sqrt()


test$h2o_xgb <- predict(best_model, test)
test <- as.data.frame(test)
write.csv(test, "results/XGB/test_h2o_XGB.csv")

file_shared$h2o_xgb <- predict(best_model, file_shared)

model_xgb <- h2o.xgboost(x = features,
                         y = response,
                         training_frame = file_shared,
                         sample_rate = 0.57,
                         reg_lambda = 0.1,
                         reg_alpha = 0.0001,
                         col_sample_rate = 0.7900,
                         col_sample_rate_per_tree = 0.46000,
                         min_rows = 2,
                         min_split_improvement = 0.05,
                         ntrees = 1000, 
                         max_depth = 4, 
                         min_child_weight = 2,
                         eta = 0.1,
                         gamma = 0.05,
                         distribution = "gamma",
                         tree_method = "exact",
                         grow_policy = "depthwise",
                         categorical_encoding = "AUTO",
                         booster = "gbtree",
                         seed = 108,
                         keep_cross_validation_predictions = TRUE,
                         keep_cross_validation_models = TRUE,
                         keep_cross_validation_fold_assignment = TRUE, 
                         nfolds = 10)


model_xgb

cvpreds_id <- model_xgb@model$cross_validation_holdout_predictions_frame_id$name
file_shared$cvpreds <- h2o.getFrame(cvpreds_id)
h2o.varimp(model_xgb)
h2o.varimp_plot(model_xgb)
file_shared$h2o_xgb_m <- predict(model_xgb, file_shared)

model_xgb_sp <- h2o.xgboost(x = features,
                            y = response,
                            training_frame = file_shared,
                            sample_rate = 0.57,
                            reg_lambda = 0.1,
                            reg_alpha = 0.0001,
                            col_sample_rate = 0.7900,
                            col_sample_rate_per_tree = 0.46000,
                            min_rows = 2,
                            min_split_improvement = 0.05,
                            ntrees = 1000, 
                            max_depth = 4, 
                            min_child_weight = 2,
                            eta = 0.1,
                            gamma = 0.05,
                            distribution = "gamma",
                            tree_method = "exact",
                            grow_policy = "depthwise",
                            categorical_encoding = "AUTO",
                            booster = "gbtree",
                            seed = 108,
                            keep_cross_validation_predictions = TRUE,
                            keep_cross_validation_models = TRUE,
                            fold_column = "Station_code")
model_xgb_sp
cvpreds_id_sp <- model_xgb_sp@model$cross_validation_holdout_predictions_frame_id$name
file_shared$cvpreds_sp <- h2o.getFrame(cvpreds_id_sp)
h2o.varimp(model_xgb_sp)
h2o.varimp_plot(model_xgb_sp)
file_shared$h2o_xgb_sp <- predict(model_xgb_sp, file_shared)
xgb <- predict_daily(number_of_days, all_tables, model_xgb_sp, "xgb")

model_xgb_temp <- h2o.xgboost(x = features,
                              y = response,
                              training_frame = file_shared,
                              sample_rate = 0.57,
                              reg_lambda = 0.1,
                              reg_alpha = 0.0001,
                              col_sample_rate = 0.7900,
                              col_sample_rate_per_tree = 0.46000,
                              min_rows = 2,
                              min_split_improvement = 0.05,
                              ntrees = 1000, 
                              max_depth = 4, 
                              min_child_weight = 2,
                              eta = 0.1,
                              gamma = 0.05,
                              distribution = "gamma",
                              tree_method = "exact",
                              grow_policy = "depthwise",
                              categorical_encoding = "AUTO",
                              booster = "gbtree",
                              seed = 108,
                              keep_cross_validation_predictions = TRUE,
                              keep_cross_validation_models = TRUE,
                              fold_column = "month")
model_xgb_temp
cvpreds_id_temp <- model_xgb_temp@model$cross_validation_holdout_predictions_frame_id$name
file_shared$cvpreds_temp <- h2o.getFrame(cvpreds_id_temp)
h2o.varimp(model_xgb_temp)
h2o.varimp_plot(model_xgb_temp)
file_shared$h2o_xgb_temp <- predict(model_xgb_temp, file_shared)

file_shared <- as.data.frame(file_shared)
write.csv(file_shared, "results/XGB/h2o_XGB.csv")