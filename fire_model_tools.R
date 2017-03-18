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
  
  return(all_data)
}
