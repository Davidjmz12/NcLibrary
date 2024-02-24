import os
import netCDF4 as nc
import cdsapi
import pandas as pd
from datetime import timedelta, datetime


def get_days_request(start_date, end_date):
    start_date = datetime.strptime(start_date, "%Y/%m/%d")
    end_date = datetime.strptime(end_date, "%Y/%m/%d")
    delta = end_date - start_date  # returns timedelta
    _days = [(start_date + timedelta(days=_i)).strftime('%Y-%m-%d') for _i in range(delta.days + 1)]
    _days = "/".join(_days)
    return _days


def make_request(levels, hours, grid, target, start_date, end_date, area, _variables, index_level, df_lvl):
    grid = grid.replace("x", "/")
    param = _variables.replace(",", "/")
    dates = get_days_request(start_date, end_date)
    levels = levels.replace(",", "/")
    hours = hours.replace(",", "/")

    levels_row = [int(item)-1 for item in levels.split("/")]
    levels_values = df_lvl.iloc[levels_row, index_level].values

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

