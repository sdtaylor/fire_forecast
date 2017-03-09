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

#Get total yearly fires instead of monthly
#all_data = all_data %>%
#  group_by(precip, cell_id, year, lon, lat) %>%
#  summarise(num_fires = sum(num_fires))

all_data = all_data %>% filter(!cell_id %in% c(39,113,384,630))
#################################################################3
#R^2 from a 1:1 line
#log transform because Marks & Muller-Landau 2007: 10.1126/science.1140190 
obs_pred_square=function(actual, predicted){
  actual=log1p(actual)
  predicted=log1p(predicted)
  1 - (sum((actual - predicted) ** 2) / sum((actual - mean(actual)) ** 2))
} 



###############################################################################
#https://www.jaredknowles.com/journal/2013/11/25/getting-started-with-mixed-effect-models-in-r
all_data$cell_id = paste0('c-',all_data$cell_id)
all_data$month = paste0('m-', all_data$month)
train_data = all_data[all_data$year<=2010,]
test_data  = all_data[all_data$year>2010,]

#model = randomForest(num_fires ~ precip*lat*lon, data=train_data)
model_glm_poisson = glm(num_fires ~ precip*cell_id + month, data=train_data, family='poisson')
#model_glm = glm.nb(num_fires ~ precip + precip*as.factor(cell_id), data=train_data)
#model = glm.nb(num_fires ~ precip + as.factor(month) + lon + lon*lon + lat + lat*lat, data=train_data)
#model = glm(num_fires ~ precip + as.factor(month):as.factor(cell_id) + as.factor(cell_id)*precip, family='poisson', data=train_data)

#model = glmer(log(num_fires) ~ precip + (1 + precip | cell_id), family='poisson', data=train_data)


test_data$predict = predict(model_glm_poisson, newdata=test_data, type = 'response')
#test_data$predict = exp(test_data$predict)
ggplot(test_data, aes(x=log(num_fires), y=log(predict))) + geom_point(alpha=0.2) + 
  #xlim(0,10000) + ylim(0,10000) + 
  geom_abline(slope=1, intercept = 0)

cell_r2 = test_data %>% 
  group_by(cell_id) %>% 
  summarise(r2= obs_pred_square(num_fires, predict), 
            spearman=cor(num_fires, predict, method='spearman'), 
            pearson=cor(num_fires,predict, method='pearson'), 
            n=n()) %>%
  ungroup()
  
ggplot(test_data, aes(x=num_fires, y=predict)) + geom_point()+facet_wrap(~cell_id, nrow=20)


