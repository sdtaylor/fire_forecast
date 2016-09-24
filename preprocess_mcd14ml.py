#Do first filtering of all fires
#1. within the bounds of south america
#2. confidence >= 30
#3. type==0 (everything not a vegetation fire)
#Also put everything in one file
#Done in python because pandas can deal with irregular whitespace delimitors


import os
import pandas as pd

data_dir='/home/shawn/data/MCD14ML/'
final_csv='/home/shawn/data/MCD14ML/cleaned_data.csv'

lat_high=15
lat_low=-38
lon_high=-32
lon_low=-84

all_files=os.listdir(data_dir)

for i,f in enumerate(all_files):
    df=pd.read_csv(data_dir+f, delim_whitespace=True)
    df=df[(df.lat<=lat_high) & (df.lat>=lat_low) & (df.lon<=lon_high) & (df.lon>=lon_low) & (df.type==0) & (df.conf>=30)]
    df=df[['YYYYMMDD','HHMM','sat','lat','lon','conf']]

    if i ==0:
        final=df
    else:
        final=final.append(df)


final.to_csv(final_csv, index=False)

