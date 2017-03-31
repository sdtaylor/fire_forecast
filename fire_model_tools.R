library(tidyverse)
library(magrittr)
library(lubridate)
library(raster)


#################################################################################
#Crop a raster to the South American continent using the NCEP land  mask
#Also insert 0 values, which is relevent for the fire data
south_american_extent=extent(276, 328, -38, 15)
land_mask = raster::crop(raster::raster('./data/precip_rasters/land_mask.tif'), south_american_extent)

crop_to_land = function(r){
  r = raster::crop(r, south_american_extent)
  if(!compareRaster(land_mask, r, stopiffalse = FALSE)){
    land_mask = raster::resample(land_mask, r, method='ngb')
  }
  r = raster::calc(r, fun = function(x) ifelse(is.na(x),0,x))
  raster::mask(r, land_mask, maskvalue=0)
}

########################################################################################
#Upscale the data temporally by aggregating months together within each year
#Method 2 scales precip and fires together, while method 1 scales fire and fire predictions
#together. 
#Predefined aggregations for the months 6-10 at the different grain sizes
temporal_groupings = read.csv('temporal_groupings.csv')

apply_temporal_scale_method1 = function(df, this_temporal_scale){
  df = df %>%
    left_join(filter(temporal_groupings, temporal_scale==this_temporal_scale), by='month') %>%
    group_by(temporal_cell_id, spatial_cell_id, year) %>%
    summarize(num_fires = sum(num_fires), num_fires_predicted = sum(num_fires_predicted)) %>%
    ungroup()
  return(df)
}

apply_temporal_scale_method2 = function(df, this_temporal_scale){
  df = df %>%
    left_join(filter(temporal_groupings, temporal_scale==this_temporal_scale), by='month') %>%
    group_by(temporal_cell_id, spatial_cell_id, year, precip, lat, lon) %>%
    summarize(num_fires = sum(num_fires)) %>%
    ungroup()
  return(df)
}


########################################################################################
#Error metrics

#Continuous ranked probability score
crps = function(df){
  obs = df$num_fires
  pred = as.matrix(df[,c('num_fires_predicted','num_fires_predicted_se')])
  
  verification::crps(obs, pred)$crps
}

#R^2 from a 1:1 line
#log transform because Marks & Muller-Landau 2007: 10.1126/science.1140190 
obs_pred_square=function(actual, predicted){
  actual=actual
  predicted=predicted
  1 - (sum((actual - predicted) ** 2) / sum((actual - mean(actual)) ** 2))
}

#########################################################################################
#Combine the fire forecast and observed fires for all the testing years. 
#Used for method 1 modeling. 
compile_fire_forecast_and_observations = function(spatial_scale){
  all_data=data.frame()
  for(this_year in testing_years){
    for(this_month in 6:11){
      forecast_raster_file = paste0('./data/fire_predictions_rasters/fire-pred-',this_year,'-',this_month,'.tif')
      forecast_raster = raster(forecast_raster_file) %>%
        crop_to_land()
      
      observed_raster_file = paste0('./data/fire_rasters/fire-',this_year,'-',this_month,'.tif')
      observed_raster = raster(observed_raster_file) %>%
        crop_to_land()
      
      if(spatial_scale > 1){
        forecast_raster = aggregate(forecast_raster, fact=spatial_scale, fun=sum)
        observed_raster = aggregate(observed_raster, fact=spatial_scale, fun=sum)
      }
      
      combined = as.data.frame(raster::stack(observed_raster, forecast_raster))
      colnames(combined) = c('num_fires', 'num_fires_predicted')
      combined$spatial_cell_id = 1:nrow(combined)
      
      combined = combined %>%
        mutate(year=this_year, month = this_month) %>%
        filter(!is.na(num_fires))
  
      all_data = all_data %>%
        bind_rows(combined)
    }
  } 
  return(all_data)
}


#########################################################################################
#Convert all the raster data into a data.frame at a particular spatial scale.
#unit scale is the original NCEP Reanalysis scale (2.5 deg). 
#Each row is a year,month,cell
compile_fire_precip_data = function(spatial_scale, drop_na=TRUE){
  all_data=data.frame()
  for(this_year in 2001:2015){
    precip_raster_file = paste0('./data/precip_rasters/precip-',this_year,'.tif')
    precip_raster = raster(precip_raster_file) %>%
      crop_to_land()
    
    if(spatial_scale > 1){
      precip_raster = aggregate(precip_raster, fact=spatial_scale, fun=mean)
    }
    
    for(this_month in 6:11){
      fire_raster_file = paste0('./data/fire_rasters/fire-',this_year,'-',this_month,'.tif')
      fire_raster = raster(fire_raster_file) %>%
        crop_to_land()
      
      if(spatial_scale > 1){
        fire_raster = aggregate(fire_raster, fact=spatial_scale, fun=sum)
      }
      
      combined = as.data.frame(raster::stack(fire_raster, precip_raster))
      colnames(combined) = c('num_fires', 'precip')
      combined$spatial_cell_id = 1:nrow(combined)
      
      combined = combined %>%
        mutate(year=this_year, month = this_month) 
      
      if(drop_na){
        combined = combined %>%
          filter(!is.na(num_fires)) 
      }
      
      all_data = all_data %>%
        bind_rows(combined)
    }
  }
  
  #Add in latitude and longitude
  reference_raster = raster('./data/precip_rasters/precip-2001.tif') %>%
    crop_to_land()
  reference_raster = setValues(reference_raster, rep(1, ncell(reference_raster)))
  lat_lons = as.data.frame(rasterToPoints(reference_raster))
  colnames(lat_lons) = c('lon','lat','foo')
  lat_lons$spatial_cell_id = 1:nrow(lat_lons)
  lat_lons = dplyr::select(lat_lons, -foo)
  all_data = all_data %>%
    left_join(lat_lons, by='spatial_cell_id')
  
  return(all_data)
}
