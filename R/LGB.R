# lightGBM
hyper_grid <- list(
  sample_rate = seq(0.2, 1, 0.01),
  reg_lambda = c(0, 0.0001, 0.001, 0.01, 0.1, 1),
  reg_alpha = c(0, 0.0001, 0.001, 0.01, 0.1, 1),
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
  tree_method = c("hist"),
  grow_policy = c("lossguide"),
  booster = c("gbtree", "gblinear", "dart")
)

lgb_grid_m <- h2o.grid(
  hyper_params = hyper_grid,
  search_criteria = search_criteria,
  algorithm = "xgboost",
  grid_id = "lgb_grid",
  x = features,
  y = response,
  categorical_encoding = "AUTO",
  keep_cross_validation_predictions = TRUE,
  keep_cross_validation_models = TRUE,
  keep_cross_validation_fold_assignment = TRUE,
  training_frame = train,
  nfolds = 10,
  score_tree_interval = 10,
  seed = 108
)
lgb_grid_m


grid_perf <- h2o.getGrid(
  grid_id = "lgb_grid", 
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


test$h2o_lgb <- predict(best_model, test)
test <- as.data.frame(test)
write.csv(test, "results/LGB/test_h2o_LGB.csv")

file_shared$h2o_lgb <- predict(best_model, file_shared)

model_lgb <- h2o.xgboost(x = features,
                         y = response,
                         training_frame = file_shared,
                         grow_policy = "lossguide",
                         tree_method = "hist",
                         sample_rate = 0.57,
                         reg_lambda = 0,
                         categorical_encoding = "AUTO",
                         reg_alpha = 0.1,
                         col_sample_rate = 0.79,
                         col_sample_rate_per_tree = 0.46,
                         min_rows = 2,
                         min_split_improvement = 0.05,
                         ntrees = 1000, 
                         max_depth = 4, 
                         min_child_weight = 2,
                         eta = 0.1,
                         gamma = 0.05,
                         distribution = "gamma",
                         booster = "gbtree",
                         seed = 108)


model_lgb
file_shared$h2o_lgb_m <- predict(model_lgb, file_shared)
predict_daily(number_of_days, all_tables, model_lgb, "lgb")

model_lgb_10 <- h2o.xgboost(x = features,
                            y = response,
                            training_frame = file_shared,
                            grow_policy = "lossguide",
                            tree_method = "hist",
                            sample_rate = 0.57,
                            reg_lambda = 0,
                            categorical_encoding = "AUTO",
                            reg_alpha = 0.1,
                            col_sample_rate = 0.79,
                            col_sample_rate_per_tree = 0.46,
                            min_rows = 2,
                            min_split_improvement = 0.05,
                            ntrees = 1000, 
                            max_depth = 4, 
                            min_child_weight = 2,
                            eta = 0.1,
                            gamma = 0.05,
                            distribution = "gamma",
                            booster = "gbtree",
                            seed = 108,
                            keep_cross_validation_predictions = TRUE,
                            keep_cross_validation_models = TRUE,
                            keep_cross_validation_fold_assignment = TRUE, 
                            nfolds = 10)


model_lgb_10
cvpreds_id <- model_lgb_10@model$cross_validation_holdout_predictions_frame_id$name
file_shared$cvpreds_10 <- h2o.getFrame(cvpreds_id)
file_shared$h2o_lgb_m_10 <- predict(model_lgb_10, file_shared)

model_lgb_sp <- h2o.xgboost(x = features,
                            y = response,
                            training_frame = file_shared,
                            grow_policy = "lossguide",
                            tree_method = "hist",
                            sample_rate = 0.57,
                            reg_lambda = 0,
                            categorical_encoding = "AUTO",
                            reg_alpha = 0.1,
                            col_sample_rate = 0.79,
                            col_sample_rate_per_tree = 0.46,
                            min_rows = 2,
                            min_split_improvement = 0.05,
                            ntrees = 1000, 
                            max_depth = 4, 
                            min_child_weight = 2,
                            eta = 0.1,
                            gamma = 0.05,
                            distribution = "gamma",
                            booster = "gbtree",
                            seed = 108,
                            keep_cross_validation_predictions = TRUE,
                            keep_cross_validation_models = TRUE,
                            fold_column = "Station_code")
model_lgb_sp
cvpreds_id <- model_lgb_sp@model$cross_validation_holdout_predictions_frame_id$name
file_shared$cvpreds_sp <- h2o.getFrame(cvpreds_id)
file_shared$h2o_lgb_sp <- predict(model_lgb_sp, file_shared)


model_lgb_temp <- h2o.xgboost(x = features,
                              y = response,
                              training_frame = file_shared,
                              grow_policy = "lossguide",
                              tree_method = "hist",
                              sample_rate = 0.57,
                              reg_lambda = 0,
                              categorical_encoding = "AUTO",
                              reg_alpha = 0.1,
                              col_sample_rate = 0.79,
                              col_sample_rate_per_tree = 0.46,
                              min_rows = 2,
                              min_split_improvement = 0.05,
                              ntrees = 1000, 
                              max_depth = 4, 
                              min_child_weight = 2,
                              eta = 0.1,
                              gamma = 0.05,
                              distribution = "gamma",
                              booster = "gbtree",
                              seed = 108,
                              keep_cross_validation_predictions = TRUE,
                              keep_cross_validation_models = TRUE,
                              fold_column = "month")
model_lgb_temp
cvpreds_id <- model_lgb_temp@model$cross_validation_holdout_predictions_frame_id$name
file_shared$cvpreds_temp <- h2o.getFrame(cvpreds_id)
file_shared$h2o_lgb_temp <- predict(model_lgb_temp, file_shared)

file_shared <- as.data.frame(file_shared)
write.csv(file_shared, "results/LGB/h2o_LGB.csv")