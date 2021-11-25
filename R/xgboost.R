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
  max_depth = c(4, 6, 8, 12, 16, 20), min_child_weight = c(1, 2, 3),
  eta = c(0.025, 0.05, 0.1, 0.3),
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  distribution = c("AUTO", "gaussian", "poisson", "gamma"),
  tree_method = c("auto", "exact", "approx"),
  grow_policy = c("depthwise"),
  booster = c("gbtree", "gblinear", "dart")
)


grid <- h2o.grid(
  hyper_params = hyper_grid,
  search_criteria = search_criteria,
  algorithm = "xgboost",
  grid_id = "xgb_grid",
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

grid_perf <- h2o.getGrid(
  grid_id = "xgb_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)

best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

model_path <- h2o.saveModel(object = best_model, path = getwd(), force = TRUE)
print(model_path)
saved_model <- h2o.loadModel(model_path)
h2o.varimp(best_model) 
h2o.scoreHistory(best_model)
plot(best_model, 
     timestep = "epochs", 
     metric = "rmse")

cv_models <- sapply(best_model@model$cross_validation_models, 
                    function(i) h2o.getModel(i$name))
plot(cv_models[[1]], 
     timestep = "epochs", 
     metric = "rmse")

best_model_perf <- h2o.performance(model = best_model, newdata = test)

h2o.mse(best_model_perf) %>% sqrt()

file_shared$h2o_XGB <- predict(best_model, file_shared)
file_shared <- as.data.frame(file_shared)
ggplot(file_shared, aes(PM2.5, h2o_XGB)) + geom_point() + geom_smooth(method = "lm")
summary(lm(PM2.5 ~ h2o_XGB, data = file_shared))
mean(abs((file_shared$PM2.5 - file_shared$h2o_XGB) / file_shared$PM2.5), na.rm = TRUE) * 100
write.csv(file_shared, "results/h2o_XGB.csv")



# lightGBM
hyper_grid <- list(
  sample_rate = seq(0.2, 1, 0.01),
  reg_lambda = c(0, 0.0001, 0.001, 0.1, 1),
  reg_alpha = c(0, 0.0001, 0.001, 0.1, 1),
  col_sample_rate = seq(0.2, 1, 0.01),
  col_sample_rate_per_tree = seq(0.2, 1, 0.01),
  min_rows = 2 ^ seq(0, log2(nrow(train))-1, 1),
  min_split_improvement = c(0, 1e-8, 1e-6, 1e-4),
  ntrees = c(500, 1000, 1500), 
  max_depth = c(4, 6, 8, 12, 16, 20), min_child_weight = c(1, 2, 3),
  eta = c(0.025, 0.05, 0.1, 0.3),
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  distribution = c("AUTO", "gaussian", "poisson", "gamma"),
  tree_method = c("hist"),
  grow_policy = c("lossguide"),
  booster = c("gbtree", "gblinear", "dart")
)


