library(xgboost)
library(readxl)
library(tidyverse)
library(tidymodels)
library(h2o)
library(here)
library(mgcv)
library(caret)
library(data.table)
library(janitor)

set.seed(108)
lat_lon <- read_excel(here("data", "Delhi_sites.xlsx"), sheet = 1) %>% 
  select("Station_code" = `Station Code`, "lon" = `Longitude (Degree E)`, "lat" = `Latitude (Degree N)`)
file_shared <- read_excel(here("data", "Final_Delhi_2019_Data.xlsx"), sheet = 1) %>%
  select("Station_code" = StationCode_2019, "CWV" = CWVDailyMean_2019, "ELV" = Elevation_2019, 
         "AOD" = Corrected_DailyMeanAOD_2019, "PM2.5" = Corrected_PM25DailyMean_2019,
         "Temp" = TempDailyMean_2019, "RH" = RHDailyMean_2019, "NDVI" = NDVI_2019,
         "WD" = WDDailyMean_2019, "WS" = WSDailyMean_2019, "BLH" = BLHDailyMean_2019,
         "Press" = PressureDailyMean_2019, "season" = Season_2019, "day" = JulianDay_2019, "month" = Month_2019) %>%
  filter(!is.nan(PM2.5)) %>% 
  left_join(., lat_lon, by = "Station_code") %>% 
  mutate_at(c("CWV", "ELV", "AOD", "PM2.5", "Temp", "RH", "NDVI", "WD", "WS", "BLH", "Press", "month", "lat", "lon"), as.numeric) %>% 
  na.omit() 


file_shared$Station_code <- as.factor(file_shared$Station_code)
file_shared$season <- as.factor(file_shared$season)
file_shared$day <- as.factor(file_shared$day)

# h2o.shutdown()
h2o.no_progress()
h2o.init(max_mem_size = "8g", min_mem_size = "3g")

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
features <- setdiff(names(train), c(response, "month", "Station_code"))
h2o.describe(file_shared)

search_criteria <- list(strategy = "RandomDiscrete", 
                        stopping_metric = "mse",
                        stopping_tolerance = 1e-4,
                        max_runtime_secs = 90 * 60,
                        seed = 108)

# This data  is avaiable in the dropbox alone, not on github 
dir <- here::here("data/Predictors_Spatial_Mapping")
all_tables <- data.frame()
list_csv <- list.files(dir, pattern = "\\.csv$")
list_csv <- sub("\\.csv.*", "", list_csv)
for(i in list_csv) {
  table <- fread(paste0(dir, "/", i, ".csv"))
  colnames(table) <- paste0(i, "_", colnames(table))
  all_tables <- cbind(table, all_tables)
}
lat <- fread(paste0(here::here("data"), "/", "NCR_LAT.csv"))[, 1] %>% 
  select("lat" = V1)
lon <- fread(paste0(here::here("data"), "/", "NCR_LON.csv"))[, 1] %>% 
  select("lon" = V1)


names(all_tables) <- gsub("_V", "_", names(all_tables))
number_of_days <- ncol(table)
all_tables <- all_tables[-1, ] 
all_tables <- cbind(all_tables, lat, lon)
file_meta <- read_excel(here::here("data", "Julian_Season.xlsx")) %>% 
  select("day" = `Julian Days`, "season" = Season) %>% 
  arrange(day)
predict_daily <- function(number_of_days, all_tables, model_input_sp, model) {
  for(i in 1:number_of_days) {
    all_tables_sub <- all_tables %>% 
      select(ends_with(paste0("_", as.character(i))), lat, lon) %>% 
      # janitor::remove_empty("rows") %>% 
      mutate(day = as.character(file_meta[i, "day"]), season = as.character(file_meta[i, "season"]))
    names(all_tables_sub) <- c("WS", "WD", "Temp", "RH", "Press", "NDVI", "ELV",
                               "CWV", "BLH", "AOD", "lat", "lon", "day", "season")
    all_tables_sub <- as.h2o(all_tables_sub)
    all_tables_sub$PM2.5 <- predict(model_input_sp, all_tables_sub)
    all_tables_sub <- as.data.frame(all_tables_sub)
    write.csv(all_tables_sub, paste0(model, "_predicted", "_", as.character(i), ".csv"))
  }
}    
