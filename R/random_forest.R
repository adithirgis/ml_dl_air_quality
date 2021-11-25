
# collect the results and sort by our model performance metric of choice
hyper_grid <- list(
  ntrees      = seq(100, 500, by = 100),
  mtries      = seq(2, 10, by = 2),
  max_depth   = seq(20, 40, by = 5),
  min_rows    = seq(1, 5, by = 2),
  nbins       = seq(10, 40, by = 5),
  sample_rate = c(.55, .632, 0.7, .75, .8)
)

# build grid search 

start <- Sys.time()
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
  hyper_params = hyper_grid,
  search_criteria = search_criteria
)
end <- Sys.time() 
beepr::beep()

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

# Save model
model_path <- h2o.saveModel(object = best_model, path = getwd(), force = TRUE)
print(model_path)
saved_model <- h2o.loadModel(model_path)
h2o.varimp(best_model) 
h2o.scoreHistory(best_model)

# Get the CV models from the `best_model` object
cv_models <- sapply(best_model@model$cross_validation_models, 
                    function(i) h2o.getModel(i$name))

# model_path <- h2o.saveModel(object = cv_models, path = getwd(), force = TRUE)
# print(model_path)

# Plot the scoring history over time
plot(cv_models[[1]], 
     timestep = "epochs", 
     metric = "rmse")

# Now let's evaluate the model performance on a test set
best_model_perf <- h2o.performance(model = best_model, newdata = test)

# RMSE of best model
h2o.mse(best_model_perf) %>% sqrt()


file_shared$h2o_DRF <- predict(best_model, file_shared)
file_shared <- as.data.frame(file_shared)
ggplot(file_shared, aes(PM2.5, h2o_DRF)) + geom_point() + geom_smooth(method = "lm")
summary(lm(PM2.5 ~ h2o_DRF, data = file_shared))
mean(abs((file_shared$PM2.5 - file_shared$h2o_DRF) / file_shared$PM2.5), na.rm = TRUE) * 100

write.csv(file_shared, "h2o_DRF.csv")

