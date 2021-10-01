file_shared <- as.data.frame(file_shared)
train_ind <- sample(seq_len(nrow(file_shared)), size = (nrow(file_shared) * 0.7))
train_x <- as.matrix(file_shared[train_ind, c("Station_code", "CWV", "ELV", "AOD", 
                                              "PM2.5", "Temp", "RH", "NDVI", "WD", 
                                              "WS", "BLH", "Press", "season", "day") ])
train_y <- as.matrix(file_shared[train_ind, "PM2.5" ])
test_x <- as.matrix(file_shared[-train_ind, c("Station_code", "CWV", "ELV", "AOD", 
                                              "PM2.5", "Temp", "RH", "NDVI", "WD", 
                                              "WS", "BLH", "Press", "season", "day") ])
test_y <- as.matrix(file_shared[-train_ind, "PM2.5" ])
dtrain <- lgb.Dataset(train_x, label = train_y)

lgb_grid = list(objective = "regression",
                metric = "l2",
                min_sum_hessian_in_leaf = 1,
                feature_fraction = 0.7,
                bagging_fraction = 0.5,
                bagging_freq = 5,
                min_data = 100,
                max_bin = 50,
                lambda_l1 = 8,
                lambda_l2 = 1.3,
                min_data_in_bin = 100,
                min_gain_to_split = 10,
                min_data_in_leaf = 30,
                is_unbalance = TRUE)


lgb_normalizedgini <- function(preds, dtrain){
  actual <- getinfo(dtrain, "label")
  score  <- NormalizedGini(preds, actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}

lgb_model_cv <- lgb.cv(params = lgb_grid, data = dtrain, learning_rate = 0.02, num_leaves = 25,
                       num_threads = 2 , nrounds = 7000, early_stopping_rounds = 50,
                       eval_freq = 20, eval = lgb_normalizedgini, nfold = 10, stratified = TRUE)
best_iter <- lgb_model_cv$best_iter

lgb_model <- lgb.train(params = lgb_grid, data = dtrain, learning_rate = 0.02,
                       num_leaves = 25, num_threads = 2 , nrounds = best_iter,
                       eval_freq = 20, eval = lgb_normalizedgini)

test_x <- as.matrix(file_shared[, c("Station_code", "CWV", "ELV", "AOD", 
                                    "PM2.5", "Temp", "RH", "NDVI", "WD", 
                                    "WS", "BLH", "Press", "season", "day") ])
file_shared$pred_lightgbm <- predict(lgb_model, test_x)

ggplot(file_shared, aes(PM2.5, pred_lightgbm)) + geom_point() + geom_smooth(method = "lm")
summary(lm(PM2.5 ~ pred_lightgbm, data = file_shared))
mean(abs((file_shared$PM2.5 - file_shared$pred_lightgbm) / file_shared$PM2.5)) * 100


write.csv(file_shared, "lightGBM_lightGBM.csv")