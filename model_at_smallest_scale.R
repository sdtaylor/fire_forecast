library(tidyverse)
library(magrittr)
library(lubridate)
library(raster)
source('./fire_model_tools.R')

training_years = 2001:2010
testing_years = 2011:2015

do_unit_scale_model = FALSE

results_file = 'results_amazone_fire_method1.csv'
###############################################################################
#Model fires and create rasters from the testing years at the unit scale

if(do_unit_scale_model){
  #Read in at smallest scale, fit model, make predictions, and write raster for each year/month in the testing period
  fire_data = compile_fire_precip_data(spatial_scale = 1, drop_na = FALSE)
  
  fire_data$temporal_scale = 1
  fire_data$spatial_scale = 1
  
  #Ensure these get treated as catagorical in the models
  fire_data$spatial_cell_id = as.factor(paste0('cell-',fire_data$spatial_cell_id))
  fire_data$temporal_cell_id = as.factor(fire_data$month)
  
  training_data = fire_data %>%
    filter(year %in% training_years)
  testing_data = fire_data %>%
    filter(year %in% testing_years)
  
  #The quick model for testing
  #testing_data$num_fires_predicted = testing_data$num_fires + rpois(nrow(testing_data), 200)  
  #The real model
  #NA's are kept throughout, but excluded in modes fitting and prediction, because
  #they are needed in re-building the prediction rasters
  model = glm(num_fires ~ precip*spatial_cell_id + precip:spatial_cell_id:temporal_cell_id, family='poisson', data=training_data[!is.na(training_data$num_fires),])
  pred = predict(model, newdata=testing_data[!is.na(testing_data$num_fires),], type = 'response', se.fit=TRUE)
  testing_data$num_fires_predicted=NA
  testing_data$num_fires_predicted[!is.na(testing_data$num_fires)] = pred$fit

  #################################################################
  #Create  predictions rasters so the predictions can be spatially
  #and temporally aggregated
  
  reference_raster = raster('./data/fire_rasters/fire-2001-10.tif') %>%
    crop_to_land()
  
  for(this_year in testing_years){
    for(this_month in 6:11){
      this_month_values = testing_data %>%
        filter(year==this_year, month==this_month)
      
      this_raster = setValues(reference_raster, this_month_values$num_fires_predicted)
      raster_filename = paste0('./data/fire_predictions_rasters/fire-pred-',this_year,'-',this_month,'.tif')
      writeRaster(this_raster, raster_filename, overwrite = TRUE)
    }
  }
}

#################################################################################
#################################################################################
#Aggregate the predictions of the unit scale model to large grains and
#calculate errors at each one

all_results = data.frame()

for(this_temporal_scale in c(1,2,3,6)){
  for(this_spatial_scale in c(1,2,3,4)){
   this_scale_data = compile_fire_forecast_and_observations(spatial_scale = this_spatial_scale) %>%
     apply_temporal_scale_method1(this_temporal_scale)
   
   this_scale_data$temporal_scale = this_temporal_scale
   this_scale_data$spatial_scale = this_spatial_scale
    
   this_scale_data = this_scale_data %>%
     mutate(error = (num_fires - num_fires_predicted)^2)
   
   all_results = all_results %>%
     bind_rows(this_scale_data)
  }
}

write.csv(all_results, results_file, row.names = FALSE)







