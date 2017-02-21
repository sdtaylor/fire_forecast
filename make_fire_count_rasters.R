library(tidyverse)
library(magrittr)
library(lubridate)
library(rgeos)
library(sp)
library(raster)


south_american_extent=extent(276, 328, -38, 15)

reference_raster = raster('~/data/ncep_reanalysis/precip-2001.tif')

#A dataframe to join total_fires with cell_id's
reference_dataframe = as.data.frame(reference_raster)
reference_dataframe$cell_id = 1:nrow(reference_dataframe)

#Assign fire occurances to a grid cell, year, and month, and summarize the total fires 
#within that cell. Made to work with read_csv_chunked() call since
#the input csv is very large.
process_fire_chunk=function(df, pos){
  df = df %>%
    mutate(date=as.Date(YYYYMMDD, format='%Y%m%d')) %>%
    mutate(month=month(date), year=year(date)) %>%
    dplyr::select(-YYYYMMDD, -date)
  
  #NCEP Reanalysis longitude goes from 0-365. 
  df = df %>%
    mutate(lon = lon+365)
  
  coordinates(df) <- ~lon+lat
  proj4string(df) <- CRS("+proj=longlat +datum=WGS84")
  
  cell_fire_counts = as.data.frame(extract(reference_raster, df, cellnumbers=TRUE))
  colnames(cell_fire_counts) = c('cell_id','raster_value')
  
  cell_fire_counts = cell_fire_counts %>%
    bind_cols(df@data) %>%
    group_by(cell_id, month, year) %>%
    summarize(total_fires = n())

}



processed_fire_data_file='./proccessed_fire_data.csv'
if(file.exists(processed_fire_data_file)){
  fire_data = read_csv(processed_fire_data_file)
} else {
  fire_data=read_csv_chunked('~/data/MCD14ML/cleaned_data.csv', callback = DataFrameCallback$new(process_fire_chunk), chunk_size = 50000, 
                             col_types = list(sat = col_character(), 
                                              YYYYMMDD=col_character(), 
                                              HHMM=col_character()) )
  #Summarize again incase fires in a single month/cell were spread out over
  #different chunks
  fire_data = fire_data %>%
    group_by(cell_id, month, year) %>%
    summarise(total_fires = sum(total_fires)) %>%
    ungroup()
  write.csv(fire_data, processed_fire_data_file, row.names =FALSE)
}


#Make a raster of fire counts for every month of the fire season
for(this_year in 2001:2016){
  for(this_month in 6:12){
    monthly_data = fire_data %>%
      filter(month==this_month, year==this_year) %>%
      right_join(reference_dataframe, by='cell_id')
    
    
    monthly_raster = setValues(reference_raster, monthly_data$total_fires)
    
    monthly_raster = crop(monthly_raster, south_american_extent)
    
    raster_filename = paste0('./fire_rasters/fire-',this_year,'-',this_month,'.tif')
    writeRaster(monthly_raster, raster_filename)
  }
}
