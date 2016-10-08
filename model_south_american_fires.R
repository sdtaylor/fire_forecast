library(tidyverse)
library(magrittr)
library(lubridate)
library(rgeos)
library(sp)


#######################################################
#Create a continuous grid of a certain cell size 
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

#Assign fire occurances to a grid cell, year, and month, and summarize the total fires 
#within that cell. Made to work with read_csv_chunked() call since
#the input csv is very large.
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

processed_fire_data_file='./proccessed_fire_data.csv'

if(file.exists(processed_fire_data_file)){
  fire_data = read_csv(processed_fire_data_file)
} else {
  fire_data=read_csv_chunked('~/data/MCD14ML/cleaned_data.csv', callback = DataFrameCallback$new(process_fire_chunk), chunk_size = 50000, 
                             col_types = list(sat = col_character(), 
                                             YYYYMMDD=col_character(), 
                                              HHMM=col_character()) )
  write.csv(fire_data, processed_fire_data_file, row.names =FALSE)
}

#Sum up counts again in case fires close by got processed in different chunks.
fire_data = fire_data %>%
  group_by(year, month, cell_id, sat) %>%
  summarize(n_fires = sum(n_fires)) %>%
  ungroup()

#Only using Terra for now
fire_data = fire_data %>%
  filter(sat=='T') %>%
  select(-sat)

fire_data = fire_data %>%
  mutate(date=as_date(paste(year,month,'01',sep='-')))
########################################################################
#Information on the peak fire month
find_peak_fire_month=function(df){
  df$month[which.max(df$n_fires)]
}

#Find peak fire  month for each grid cell
peak_fire_month = fire_data %>%
  group_by(cell_id, month) %>%
  summarise(n_fires = mean(n_fires)) %>%
  ungroup() %>%
  group_by(cell_id) %>%
  do(peak_fire_month = find_peak_fire_month(.)) %>%
  ungroup()

#Make an entry for each cell and each year in dataset with start and end time
#to count fires and calculate FSS
#Exclude year 2000 because it only has 2 months of data.
peak_fire_month = merge(peak_fire_month, data.frame(year=unique(fire_data$year))) 

#Get season start and end for each cell/year to calculate fss for that season
#exclude any entires with a season_start before the earliest date in the data
peak_fire_month = peak_fire_month %>%
  mutate(peak_fire_month=as_date(paste(year,peak_fire_month,1,sep='-'))) %>%
  mutate(season_start = peak_fire_month - months(4),
         season_end   = peak_fire_month + months(5) - days(1)) %>%
  filter(season_start >= as_date('2000-11-01'))



#######################################################################
#calculate FSS
get_fss=function(df){
  #print(df)
  this_instance_data=fire_data %>%
    filter(cell_id==df$cell_id, date>=df$season_start, date<df$season_end)
  df$fss=sum(this_instance_data$n_fires)
  return(as.data.frame(df))
}


fss=peak_fire_month %>%
  rowwise() %>%
  do(get_fss(.))


#########################################################################
#Atlantic Multidecadal Oscillation
amo_file='./climate_data/amo.csv'
amo_data=read_csv(amo_file) %>%
  dplyr::filter(year>=2000) %>%
  dplyr::mutate(date = as_date(paste(year,month,1,sep='-'))) %>%
  dplyr::select(-month)
  


#El nino index (ONI). The raw csv was copied and pasted from http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml
ono_data = read_csv('./climate_data/oni.csv') %>%
  rename(year=Year) %>%
  gather(month, ono_value, -year) %>%
  filter(year>=2000)

month_integer=data.frame(month=c("DJF", "JFM", "FMA", "MAM", "AMJ", "MJJ", "JJA", "JAS", "ASO", "SON", "OND", "NDJ"), 
                         month_int=1:12)

ono_data = ono_data %>%
  left_join(month_integer, by='month') %>%
  mutate(date = as_date(paste(year,month_int,1,sep='-'))) %>%
  select(-month, -month_int)
rm(month_integer)
  
############################################################################
get_climate_index_data=function(peak_fire_month, min_lead_time=4, max_lead_time=10){
  months_to_include=seq(peak_fire_month-months(max_lead_time), peak_fire_month-months(min_lead_time), by='month')
  
  amo=amo_data %>%
    filter(date %in% months_to_include) %>%
    arrange(date) %>%
    dplyr::select(-year,-date, index_value=amo_value)
  amo$lead_time=max_lead_time:min_lead_time
  amo$index='amo'
  
  ono=ono_data %>%
    filter(date %in% months_to_include) %>%
    arrange(date) %>%
    dplyr::select(-year,-date, index_value=ono_value)
  ono$lead_time=max_lead_time:min_lead_time
  ono$index='ono'
  
  both=bind_rows(amo, ono)
  both$peak_fire_month=peak_fire_month
  return(both)
}

#Find the lead times with the maximum correlation between FSS and AMO/ONO for each cell
find_lead_time=function(df){
  climate_data=df %>%
    rowwise() %>%
    do(get_climate_index_data(.$peak_fire_month)) %>%
    ungroup() %>%
    left_join(select(df, peak_fire_month, fss))
  
   correlations=climate_data %>%
    group_by(index, lead_time) %>%
    summarize(corr=cor(fss, index_value)) %>%
    ungroup()

#TODO: get this to work
  lead_times=correlations %>%
    group_by(index, lead_time) %>%
    filter(abs(corr)==max(abs(corr))) %>%
    ungroup() %>%
    dplyr::select(-corr) %>%
    mutate(index=paste0(index,'_lead_time')) %>%
    spread(index, lead_time)
  
}









