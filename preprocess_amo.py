#Do initial cleaning of atlantic multidecadal oscillation index data
#Done in python because pandas can deal with irregular whitespace delimitors
import os
import pandas as pd
from urllib.request import urlretrieve

amo_url='http://www.esrl.noaa.gov/psd/data/correlation/amon.us.data'
amo_file='./climate_data/amo.csv'

urlretrieve(amo_url, amo_file)

df=pd.read_csv(amo_file, delim_whitespace=True, header=None, skiprows=1, skipfooter=4, engine='python')

#1st column is year, the rest are monthly values for that year. 
col_names=list(range(1,13))
col_names.insert(0,'year')
df.columns=col_names

#One value per row
df=pd.melt(df, id_vars='year', value_vars=col_names[1:],
               var_name='month', value_name='amo_value')

#-99 to nan
df.replace([-99.99], [None], inplace=True)

df.to_csv(amo_file, index=False)
