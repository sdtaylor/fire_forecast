library(tidyverse)
library(lubridate)
library(rgeos)
library(sp)


#######################################################
#Create a continuous grid of a certain cell size across the extent of the template raster
#######################################################
create_grid=function(cellsize, minX, maxX, minY, maxY){
  lowerX=maxX-minX
  lowerY=maxY-minY
  
  numCellsX=round((lowerX+1)/cellsize)
  numCellsY=round((lowerY+1)/cellsize)
  g=GridTopology(c(minX, minY), c(cellsize,cellsize), c(numCellsX, numCellsY))
  g=SpatialGrid(g, proj4string = CRS("+proj=longlat +datum=WGS84"))
}

#######################################################
#Takes a grid cell and site spatial df.
#Returns a df of colnames('cellID','cellSize','siteID')
#######################################################
assign_sites_to_grid=function(g, sites){
  g=as(g, 'SpatialPolygons')
  sites@data$cell_id=over(sites, g)
  return(sites)
}

########################################################################
south_american_grid=create_grid(cellsize = 5, minX=-84, maxX=-32, minY=-38, maxY=15)

process_fire_chunk=function(df, pos){
  df = df %>%
    mutate(date=as.Date(YYYYMMDD, format='%Y%m%d')) %>%
    mutate(month=month(date), year=year(date)) %>%
    dplyr::select(-YYYYMMDD, -date)
  
  coordinates(df) <- ~lon+lat
  proj4string(df) <- CRS("+proj=longlat +datum=WGS84")
  
  df=assign_sites_to_grid(south_american_grid, df)
  df=df@data
  
  df = df %>%
    group_by(year, month, cell_id, sat) %>%
    summarize(n_fires=n()) %>%
    ungroup()
}

fire_data=read_csv_chunked('~/data/MCD14ML/cleaned_data.csv', callback = DataFrameCallback$new(process_fire_chunk), chunk_size = 50000, 
                           col_types = list(sat = col_character(), 
                                            YYYYMMDD=col_character(), 
                                            HHMM=col_character()) )

fire_data = fire_data %>%
  group_by(year, month, cell_id, sat) %>%
  summarize(n_fires = sum(n_fires))

#########################################################################
