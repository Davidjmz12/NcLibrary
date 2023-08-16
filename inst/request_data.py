import os
import netCDF4 as nc
import cdsapi
from datetime import timedelta, datetime


def get_days_request(start_date, end_date):
    start_date = datetime.strptime(start_date, "%Y/%m/%d")
    end_date = datetime.strptime(end_date, "%Y/%m/%d")
    delta = end_date - start_date  # returns timedelta
    _days = [(start_date + timedelta(days=_i)).strftime('%Y-%m-%d') for _i in range(delta.days + 1)]
    _days = "/".join(_days)
    return _days


def set_level(file_name, level_value):
    existing_nc = nc.Dataset(file_name, 'r')
    if "level" not in existing_nc.dimensions:
        # Create a new netCDF4 file for writing
        new_nc = nc.Dataset('temp.nc', 'w')

        # Copy dimensions from the existing file to the new filez
        for dim_name, dim in existing_nc.dimensions.items():
            new_nc.createDimension(dim_name, len(dim) if not dim.isunlimited() else None)
            if dim_name == "latitude":
                new_nc.createDimension("level", size=1)


        # Copy variables from the existing file to the new file with the extended dimension
        for var_name, var in reversed(existing_nc.variables.items()):
            dims = tuple(var.dimensions if var_name in new_nc.dimensions.keys() else (
                "longitude", "latitude", "level", "time"))
            new_var = new_nc.createVariable(var_name, var.dtype, dims)
            new_var.setncatts(existing_nc.variables[var_name].__dict__)
            if var_name in new_nc.dimensions.keys():
                new_var[:] = var[:]
            else:
                new_var[:, :, 0, :] = var[:]
            if var_name == "latitude":
                level_var = new_nc.createVariable("level", 'int32', ("level",))
                level_var[0] = level_value

        # Close both the existing and new netCDF4 files
        existing_nc.close()
        new_nc.close()
        os.remove(file_name)
        os.rename("temp.nc", file_name)
        print("\n --- FILE CHANGED")
    else:
        existing_nc.close()

def make_request(levels, hours, grid, target, start_date, end_date, area, _variables):
    grid = grid.replace("x", "/")
    param = _variables.replace(",", "/")
    dates = get_days_request(start_date, end_date)
    levels = levels.replace(",", "/")
    hours = hours.replace(",", "/")
    c = cdsapi.Client()
    request = {  # Requests follow MARS syntax
        # Keywords 'expver' and 'class' can be dropped. They are obsolete
        # since their values are imposed by 'reanalysis-era5-complete'
        'date': dates,
        'levelist': levels,  # 1 is top level, 137 the lowest model level in ERA5. Use '/' to separate values.
        'levtype': 'ml',
        'param': param,  # Full information at https://apps.ecmwf.int/codes/grib/param-db/
        # The native representation for temperature is spherical harmonics
        'stream': 'oper',  # Denotes ERA5. Ensemble members are selected by 'enda'
        'time': hours,  # You can drop :00:00 and use MARS shorthand notation, instead of '00/06/12/18'
        'type': 'an',
        'area': area,  # North, West, South, East. Default: global
        'grid': grid,  # Latitude/longitude. Default: spherical harmonics or reduced Gaussian grid
        'format': 'netcdf',  # Output needs to be regular lat-lon, so only works in combination with 'grid'!
    }
    c.retrieve('reanalysis-era5-complete', request, target)  # Output file. Adapt as you wish.
    set_level(target, levels)
