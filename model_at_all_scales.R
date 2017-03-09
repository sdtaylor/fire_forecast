library(tidyverse)
library(magrittr)
library(lubridate)
library(raster)


south_american_extent=extent(276, 328, -38, 15)

training_years = 2001:2010
testing_years = 2011:2015

results_file='results_amazon_fire.csv'

#########################################################################################
#Convert all the raster data into a data.frame at a particular spatial scale.
#unit scale is the original NCEP Reanalysis scale (2.5 deg). 
#Each row is a year,month,cell
compile_fire_precip_data = function(spatial_scale){
  all_data=data.frame()
  for(this_year in 2001:2015){
    precip_raster_file = paste0('./data/precip_rasters/precip-',this_year,'.tif')
    precip_raster = raster(precip_raster_file)
    precip_raster = crop(precip_raster, south_american_extent)
    
    if(spatial_scale > 1){
      precip_raster = aggregate(precip_raster, fact=spatial_scale, fun=mean)
    }
    
    for(this_month in 6:11){
      fire_raster_file = paste0('./data/fire_rasters/fire-',this_year,'-',this_month,'.tif')
      fire_raster = raster(fire_raster_file)
      
      if(spatial_scale > 1){
        fire_raster = aggregate(fire_raster, fact=spatial_scale, fun=sum)
      }
      
      combined = as.data.frame(raster::stack(fire_raster, precip_raster))
      colnames(combined) = c('num_fires', 'precip')
      combined$spatial_cell_id = 1:nrow(combined)
      
      combined = combined %>%
        mutate(year=this_year, month = this_month) %>%
        filter(!is.na(num_fires)) 
      
      all_data = all_data %>%
        bind_rows(combined)
    }
  }
  
  all_data$spatial_scale = spatial_scale
  return(all_data)
}

########################################################################################
#Scale the data temporally by aggregating months together within each year
#uses the direct output of compile_fire_precip_data()

#Predefined aggregations for the months 6-10 at the different grain sizes
temporal_groupings = read.csv('temporal_groupings.csv')

apply_temporal_scale = function(df, this_temporal_scale){
  df = df %>%
    left_join(filter(temporal_groupings, temporal_scale==this_temporal_scale), by='month') %>%
    group_by(temporal_cell_id, spatial_cell_id, year, spatial_scale, precip) %>%
    summarize(num_fires = sum(num_fires)) %>%
    ungroup()
  df$temporal_scale = this_temporal_scale
  return(df)
}


########################################################################################
#Error metrics
crps = function(df){
  obs = df$num_fires
  pred = as.matrix(df[,c('num_fires_predicted','num_fires_predicted_se')])
  
  verification::crps(obs, pred)$crps
}


########################################################################################
#Run a model given 

all_results = data.frame()

for(this_temporal_scale in c(1,2,3,6)){
  for(this_spatial_scale in c(1,2,3,4)){
    print(paste0('Spatial: ',this_spatial_scale,', Temporal: ',this_temporal_scale))
    
    fire_data = compile_fire_precip_data(spatial_scale = this_spatial_scale) %>%
      apply_temporal_scale(this_temporal_scale = this_temporal_scale)
    
    #Ensure these get treated as catagorical in the models
    fire_data$spatial_cell_id = as.factor(paste0('cell-',fire_data$spatial_cell_id))
    fire_data$temporal_cell_id = as.factor(fire_data$temporal_cell_id)
    
    training_data = fire_data %>%
      filter(year %in% training_years)
    testing_data = fire_data %>%
      filter(year %in% testing_years)
    
    #The largest temporal grain is the sum of the entire fire season, so do not include
    #a month covariate
    if(this_temporal_scale == 6){
      model = glm(num_fires ~ precip*spatial_cell_id, family='poisson', data=training_data)
    } else {
      model = glm(num_fires ~ precip*spatial_cell_id + precip:spatial_cell_id:temporal_cell_id, family='poisson', data=training_data)
    }
    
    pred = predict(model, newdata=testing_data, type = 'response', se.fit=TRUE)
    testing_data$num_fires_predicted = pred$fit
    testing_data$num_fires_predicted_se = pred$se.fit
    
    testing_data$error = crps(testing_data)
    
    results = testing_data %>%
      dplyr::select(temporal_cell_id, spatial_cell_id, year, spatial_scale, temporal_scale, error)
    
    all_results = all_results %>%
      bind_rows(results)
  }
}

write.csv(all_results, results_file, row.names = FALSE)














