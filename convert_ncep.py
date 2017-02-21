from osgeo import gdal, gdalnumeric, osr, gdal_array
import xarray as xr
import numpy as np

def get_netCDF_info(nc_filename):
    gdal_object   = gdal.Open(nc_filename)
    no_data_value = gdal_object.GetRasterBand(1).GetNoDataValue()
    x_size        = gdal_object.RasterXSize
    y_size        = gdal_object.RasterYSize
    datatype      = gdal_object.GetRasterBand(1).DataType
    geo_transform = gdal_object.GetGeoTransform()
    projection    = osr.SpatialReference()
    projection.ImportFromWkt(gdal_object.GetProjectionRef())

    return no_data_value, x_size, y_size, datatype, geo_transform, projection


#Create a 1 band tiff from numpy array
def create_geotiff(filename, data, no_data_value, x_size, y_size, datatype, geo_transform, projection):
    driver = gdal.GetDriverByName('GTiff')

    data[np.isnan(data)] = no_data_value

    dataset = driver.Create(filename, x_size, y_size, 1, 6)
    dataset.SetGeoTransform(geo_transform)
    dataset.SetProjection(projection.ExportToWkt())

    band_out = dataset.GetRasterBand(1)
    gdalnumeric.BandWriteArray(band_out, data)
    band_out.SetNoDataValue(no_data_value)

#Open the file and do a little preprocessing
def open_ncep_file(filename):
    try:
        xr_object=xr.open_dataset(filename)
    except:
        xr_object=xr.open_dataset(filename, decode_cf=False)
        xr_object.prate.attrs.pop('_FillValue')
        xr_object = xr.conventions.decode_cf(xr_object)

    #Mean mm/s to total precip over 6 hours
    xr_object['prate'] = xr_object.prate*6*60*60

    #Convert each 6 hour measurement to just the month
    xr_object['time'] = xr_object.time.values.astype('datetime64[M]')

    return xr_object

#Read in a netcdf file with xarray and get X*Y*month array of total precip
def jan_apr_precip(filename):
    xr_object = open_ncep_file(filename)

    monthly_precip = np.zeros((4, xr_object.dims['lat'], xr_object.dims['lon']))
    months = np.unique(xr_object.time.values)
    months.sort()

    for i, this_month in enumerate(months[0:4]):
        monthly_precip[i] = np.sum(xr_object.sel(time=this_month).prate.values, axis=0)

    total_precip = np.sum(monthly_precip, axis=0)
    return total_precip

def dec_precip(filename):
    xr_object = open_ncep_file(filename)

    monthly_precip = np.zeros((4, xr_object.dims['lat'], xr_object.dims['lon']))
    months = np.unique(xr_object.time.values)
    months.sort()
    december = months[-1]

    dec_precip = np.sum(xr_object.sel(time=december).prate.values, axis=0)
    return dec_precip

def ncep_filename(year):
    return data_dir+'prate.sfc.gauss.'+str(year)+'.nc'

############################
raw_data_dir='/home/shawn/data/ncep_reanalysis/netcdf/'
processed_data_dir='/home/shawn/projects/fire_prediction/data/precip_rasters/'

years = list(range(2001,2017))

if __name__ == '__main__':
    for year in years:
        netcdf_filename = ncep_filename(year)
        raster_info = get_netCDF_info(netcdf_filename)

        total_precip=jan_apr_precip(ncep_filename(year)) + dec_precip(ncep_filename(year-1))

        tif_filename = processed_data_dir+'precip-'+str(year)+'.tif'
        create_geotiff(tif_filename, total_precip, *raster_info)
