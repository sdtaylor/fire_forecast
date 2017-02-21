library(tidyverse)
library(lubridate)
library(raster)


south_american_extent=extent(276, 328, -38, 15)

#Get the latitude and longitude of all cells
reference_raster = crop(raster('./data/precip_rasters/precip-2001.tif'), south_american_extent)
lat_lons = as.data.frame(rasterToPoints(reference_raster))
colnames(lat_lons) = c('lon','lat','precip')
lat_lons$cell_id = 1:nrow(lat_lons)
lat_lons = dplyr::select(lat_lons, -precip)
#############################################################################

all_data=data.frame()

for(this_year in 2001:2015){
  precip_raster_file = paste0('./data/precip_rasters/precip-',this_year,'.tif')
  precip_raster = raster(precip_raster_file)
  precip_raster = crop(precip_raster, south_american_extent)
  
  for(this_month in 6:12){
    fire_raster_file = paste0('./data/fire_rasters/fire-',this_year,'-',this_month,'.tif')
    fire_raster = raster(fire_raster_file)
    
    combined = as.data.frame(raster::stack(fire_raster, precip_raster))
    colnames(combined) = c('num_fires', 'precip')
    combined$cell_id = 1:nrow(combined)
    
    combined = combined %>%
      mutate(year=this_year, month = this_month) %>%
      filter(!is.na(num_fires)) %>%
      left_join(lat_lons, by='cell_id')
    
    all_data = all_data %>%
      bind_rows(combined)
  }
  
}

###############################################################################

model = glm(num_fires ~ precip + month + lon + lat, family='poisson', data=all_data)



