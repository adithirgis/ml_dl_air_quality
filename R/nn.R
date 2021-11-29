
hyper_grid <- list(
  activation = c("Rectifier", "Tanh", "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"), 
  hidden = list(c(5, 5, 5, 5, 5), c(30, 30, 30, 30), c(50, 50, 50, 50), c(100, 100, 100, 100)),
  epochs = c(50, 100, 200, 300, 400, 500),
  l1 = c(0, 0.00001, 0.0001), 
  l2 = c(0, 0.00001, 0.0001),
  rate = c(0, 01, 0.005, 0.001),
  rate_annealing = c(1e-8, 1e-7, 1e-6),
  rho = c(0.9, 0.95, 0.99, 0.999),
  epsilon = c(1e-10, 1e-8, 1e-6, 1e-4),
  momentum_start = c(0, 0.5),
  momentum_stable = c(0.99, 0.5, 0),
  distribution = c("AUTO", "gaussian", "poisson", "gamma"),
  input_dropout_ratio = c(0, 0.1, 0.2),
  max_w2 = c(1, 10, 100, 1000, 3.4028235e+38)
)

dl_grid <- h2o.grid(algorithm = "deeplearning", 
                    x = features,
                    y = response,
                    grid_id = "dl_grid",
                    training_frame = train,
                    keep_cross_validation_predictions = TRUE,
                    keep_cross_validation_models = TRUE,
                    keep_cross_validation_fold_assignment = TRUE, 
                    nfolds = 10,                           
                    hyper_params = hyper_grid,
                    search_criteria = search_criteria,
                    seed = 108
)


grid_perf <- h2o.getGrid(
  grid_id = "dl_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)

best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

model_path <- h2o.saveModel(object = best_model, path = getwd(), force = TRUE)
print(model_path)
saved_model <- h2o.loadModel(model_path)
 
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

test$h2o_DL <- predict(best_model, test)
test <- as.data.frame(test)
write.csv(test, "test_h2o_DL.csv")

file_shared$h2o_dl_bm <- predict(best_model, file_shared)


model_dl <- h2o.deeplearning(x = features,
                             y = response,
                             training_frame = train,
                             distribution = "gaussian",
                             hidden =  c(50, 50, 50, 50),
                             epochs = 300,
                             l1 = 0.0001, 
                             l2 = 0.00001,
                             rho = 0.95,
                             epsilon = 1e-6,
                             momentum_start = 0,
                             input_dropout_ratio = 0.2,
                             max_w2 = 1000,
                             reproducible = TRUE,
                             activation = "Tanh",
                             seed = 108,
                             keep_cross_validation_predictions = TRUE,
                             keep_cross_validation_models = TRUE,
                             keep_cross_validation_fold_assignment = TRUE, 
                             nfolds = 10)
model_dl
h2o.varimp(model_dl)
file_shared$h2o_dl <- predict(model_dl, file_shared)
file_shared <- as.data.frame(file_shared)
ggplot(file_shared, aes(PM2.5, h2o_dl)) + geom_point() + geom_smooth(method = "lm")
summary(lm(PM2.5 ~ h2o_dl, data = file_shared))
mean(abs((file_shared$PM2.5 - file_shared$h2o_dl) / file_shared$PM2.5), na.rm = TRUE) * 100
write.csv(file_shared, "h2o_DL.csv")

