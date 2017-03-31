library(tidyverse)
source('./fire_model_tools.R')


all_results = read.csv('results_amazon_fire_method1.csv')

#Add in error metrics
scores = all_results %>%
  group_by(temporal_scale, spatial_scale, scaling_method) %>%
  summarize(rmse = sqrt(mean((num_fires_predicted - num_fires)^2)),
            r2 = obs_pred_square(num_fires, num_fires_predicted),
            pearson = cor(num_fires, num_fires_predicted, method='pearson')) %>%
  gather(error_metric, value, rmse, r2, pearson)


ggplot(scores, aes(x=spatial_scale, y=value, color=as.factor(temporal_scale), group=as.factor(temporal_scale))) +
  geom_line() +
  geom_point() +
  facet_wrap(scaling_method~error_metric, scales='free')


#########################################################
#Grid 

