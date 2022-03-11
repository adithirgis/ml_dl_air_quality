set.seed(108)

gam_model <- gam(PM2.5 ~ s(CWV) + s(ELV) + s(AOD) + s(Temp) + s(RH) + s(NDVI) + s(WD) + s(WS) + s(BLH) + s(Press), 
                 data = file_shared)
plot.gam(gam_model, pages = 1, residuals = TRUE, all.terms = TRUE, 
         shade = TRUE, shade.col = 2)
file_shared$model_pred <- predict(gam_model, newdata = file_shared)

predict_daily_gam <- function(number_of_days, all_tables, model_input_sp, model) {
  for(i in 1:number_of_days) {
    all_tables_sub <- all_tables %>% 
      select(ends_with(paste0("_", as.character(i))))
    names(all_tables_sub) <- c("WS", "WD", "Temp", "RH", "Press", "NDVI", "ELV",
                               "CWV", "BLH", "AOD")
    all_tables_sub$PM2.5 <- predict(model_input_sp, all_tables_sub)
    write.csv(all_tables_sub, paste0(model, "_predicted", "_", as.character(i), ".csv"))
  }
}    
predict_daily_gam(number_of_days, all_tables, gam_model, "gam")


gam_model_10 <- train(PM2.5 ~ CWV + ELV + AOD + Temp + RH + NDVI + WD + WS + BLH + Press, 
                      data = file_shared,
                      method = "gam",
                      trControl = trainControl(method = "cv", number = 10, 
                                               savePredictions = TRUE)
)
file_shared$model_pred_10 <- predict(gam_model_10, newdata = file_shared)
gam_cv_10 <- as.data.frame(gam_model_10$pred)
gam_cv_10 <- gam_cv_10 %>% 
  subset(select == "FALSE")
write.csv(gam_cv_10, "gam_cv_10.csv")

# LOOCV based on Station code
predicted_sp <- data.frame()
for(i in unique(file_shared$Station_code)){
  file_shared_sub <- file_shared %>% 
    subset(Station_code != i)
  file_shared_pred <- file_shared %>% 
    subset(Station_code == i)
  model_gam_sp <- gam(PM2.5 ~ s(CWV) + s(ELV) + s(AOD) + s(Temp) + s(RH) + s(NDVI) + s(WD) + s(WS) + s(BLH) + s(Press), 
                      data = file_shared_sub)
  file_shared_pred$predicted <- predict(model_gam_sp, newdata = file_shared_pred)
  predicted_sp <- rbind(predicted_sp, file_shared_pred)
}

write.csv(predicted_sp, "gam_sp.csv")

