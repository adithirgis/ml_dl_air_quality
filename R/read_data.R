library(xgboost)
library(readxl)
library(tidyverse)
library(tidymodels)
library(h2o)
library(here)
library(data.table)

set.seed(108)

file_shared <- read_excel(here("data", "Final_Delhi_2019_Data.xlsx"), sheet = 1) %>%
  select("Station_code" = StationCode_2019, "CWV" = CWVDailyMean_2019, "ELV" = Elevation_2019, 
         "AOD" = Corrected_DailyMeanAOD_2019, "PM2.5" = Corrected_PM25DailyMean_2019,
         "Temp" = TempDailyMean_2019, "RH" = RHDailyMean_2019, "NDVI" = RHDailyMean_2019,
         "WD" = WDDailyMean_2019, "WS" = WSDailyMean_2019, "BLH" = BLHDailyMean_2019,
         "Press" = PressureDailyMean_2019, "season" = Season_2019, "day" = JulianDay_2019) %>%
  mutate_at(c("CWV", "ELV", "AOD", "PM2.5", "Temp", "RH", "NDVI", "WD", "WS", "BLH", "Press"), as.numeric) %>% 
  filter(!is.nan(PM2.5)) 


file_shared$Station_code <- as.factor(file_shared$Station_code)
file_shared$season <- as.factor(file_shared$season)
file_shared$day <- as.factor(file_shared$day)

# h2o.shutdown()
h2o.no_progress()
h2o.init(max_mem_size = "5g")

file_shared <- as.h2o(file_shared)

splits <- h2o.splitFrame(
  data = file_shared,
  ratios = c(0.7, 0),   
  destination_frames = c("train_hex", "valid_hex", "test_hex"), seed = 108
)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]

response <- "PM2.5"
features <- setdiff(names(train), c(response))
h2o.describe(file_shared)

search_criteria <- list(strategy = "RandomDiscrete", 
                        stopping_metric = "mse",
                        stopping_tolerance = 1e-4,
                        stopping_rounds = 10,
                        max_models = 100,
                        seed = 108)


# save and load the model
# model_path <- h2o.saveModel(object = model, path = getwd(), force = TRUE)
# print(model_path)

# saved_model <- h2o.loadModel(model_path)
 
# my_local_model <- h2o.download_model(model, path = "/Users/UserName/Desktop")
 
# uploaded_model <- h2o.upload_model(my_local_model)
