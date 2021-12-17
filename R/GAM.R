set.seed(108)


gam_model <- train(PM2.5 ~ CWV + ELV + AOD + Temp + RH + NDVI + WD + WS + BLH + Press, 
                   data = file_shared,
                   method = "gam",
                   trControl = trainControl(method = "cv", number = 10, 
                                            savePredictions = TRUE)
)

file_shared$model_pred <- predict(gam_model, newdata = file_shared)
write.csv(file_shared, "gam_model.csv")

write.csv(as.data.frame(gam_model$pred), "gam_cv.csv")

