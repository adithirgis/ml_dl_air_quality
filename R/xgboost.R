

hyper_grid <- list(
  sample_rate = seq(0.2, 1, 0.01),
  reg_lambda = c(0, 0.0001, 0.001, 0.1, 1),
  reg_alpha = c(0, 0.0001, 0.001, 0.1, 1),
  col_sample_rate = seq(0.2, 1, 0.01),
  col_sample_rate_per_tree = seq(0.2, 1, 0.01),
  col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
  min_rows = 2 ^ seq(0, log2(nrow(train))-1, 1),
  nbins = 2 ^ seq(4, 10, 1),
  min_split_improvement = c(0, 1e-8, 1e-6, 1e-4),
  ntrees = c(500, 1000, 1500), 
  max_depth = c(4, 6, 8, 12, 16, 20), min_child_weight = c(1, 2, 3),
  learn_rate = 0.03, eta = c(0.025, 0.05, 0.1, 0.3),
  subsample = c(0.5, 0.75, 1.0),
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  distribution = c("AUTO", "gaussian", "poisson", "gamma",
                   "laplace", "quantile", "huber"),
  tree_method = c("auto", "exact", "approx", "hist"),
  grow_policy = c("depthwise", "lossguide"),
  booster = c("gbtree", "gblinear")
)


grid <- h2o.grid(
  hyper_params = hyper_grid,
  search_criteria = search_criteria,
  algorithm = "xgboost",
  grid_id = "xgb_grid",
  x = features,
  y = response,
  training_frame = train,
  nfolds = 10,
  score_tree_interval = 10,
  seed = 108
)





