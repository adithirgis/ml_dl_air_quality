# https://shiring.github.io/machine_learning/2017/03/07/grid_search
# https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/deep-learning.html

hyper_grid <- list(
  activation = c("Rectifier", "Tanh", "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"), 
  hidden = list(c(5, 5, 5, 5, 5), c(50, 50, 50), c(100, 100, 100)),
  epochs = c(50, 100, 200),
  l1 = c(0, 0.00001, 0.0001), 
  l2 = c(0, 0.00001, 0.0001),
  rate = c(0, 01, 0.005, 0.001),
  rate_annealing = c(1e-8, 1e-7, 1e-6),
  rho = c(0.9, 0.95, 0.99, 0.999),
  epsilon = c(1e-10, 1e-8, 1e-6, 1e-4),
  momentum_start = c(0, 0.5),
  momentum_stable = c(0.99, 0.5, 0),
  input_dropout_ratio = c(0, 0.1, 0.2),
  max_w2 = c(10, 100, 1000, 3.4028235e+38)
)

system.time(dl_grid <- h2o.grid(algorithm = "deeplearning", 
                    x = features,
                    y = response,
                    grid_id = "dl_grid",
                    training_frame = train,
                    validation_frame = valid,
                    nfolds = 15,                           
                    hyper_params = hyper_grid,
                    search_criteria = search_criteria,
                    seed = 108
))

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "dl_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)


# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let's evaluate the model performance on a test set
best_model_perf <- h2o.performance(model = best_model, newdata = valid)

# RMSE of best model
h2o.mse(best_model_perf) %>% sqrt()


file_shared$h2o_nn <- predict(best_model, file_shared)
file_shared <- as.data.frame(file_shared)
ggplot(file_shared, aes(PM2.5, h2o_nn)) + geom_point() + geom_smooth(method = "lm")
summary(lm(PM2.5 ~ h2o_nn, data = file_shared))
mean(abs((file_shared$PM2.5 - file_shared$h2o_nn) / file_shared$PM2.5), na.rm = TRUE) * 100

write.csv(file_shared, "results/h2o_NN.csv")


