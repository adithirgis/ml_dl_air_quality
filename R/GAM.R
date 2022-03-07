set.seed(108)


gam_model <- train(PM2.5 ~ CWV + ELV + AOD + Temp + RH + NDVI + WD + WS + BLH + Press, 
                   data = file_shared,
                   method = "gam",
                   trControl = trainControl(method = "cv", number = 10, 
                                            savePredictions = TRUE)
)

model_gam_sp <- train(PM2.5 ~ CWV + ELV + AOD + Temp + RH + NDVI + WD + WS + BLH + Press, 
                   data = file_shared,
                   method = "gam",
                   trControl = trainControl(method = "cv", number = 10, 
                                            savePredictions = TRUE)
)
predict_daily <- function(number_of_days, all_tables, model_input_sp, model) {
  for(i in 1:number_of_days) {
    all_tables_sub <- all_tables %>% 
      select(ends_with(paste0("_", as.character(i))))
    names(all_tables_sub) <- c("WS", "WD", "Temp", "RH", "Press", "NDVI", "ELV",
                               "CWV", "BLH", "AOD")
    all_tables_sub$PM2.5 <- predict(model_input_sp, all_tables_sub)
    write.csv(all_tables_sub, paste0(model, "_predicted", "_", as.character(i), ".csv"))
  }
}    
predict_daily(number_of_days, all_tables, model_gam_sp, "gam")

file_shared$model_pred <- predict(gam_model, newdata = file_shared)
write.csv(file_shared, "gam_model.csv")
gam_cv <- as.data.frame(gam_model$pred)
gam_cv <- gam_cv %>% 
  subset(select == "FALSE")
write.csv(gam_cv, "gam_cv.csv")

gam_model <- gam(PM2.5 ~ s(CWV) + s(ELV) + s(AOD) + s(Temp) + s(RH) + s(NDVI) + s(WD) + s(WS) + s(BLH) + s(Press), 
                   data = file_shared)
file_shared$model_pred2 <- predict(gam_model, newdata = file_shared)
plot.gam(gam_model, pages = 1, residuals = TRUE, all.terms = TRUE, 
     shade = TRUE, shade.col = 2)