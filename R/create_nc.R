
#' Create python environment
#' @importFrom reticulate use_condaenv
#' @importFrom reticulate conda_create
#' @importFrom reticulate conda_list
#' @export
set_reticulate <- function()
{
  list <- conda_list()$name
  if("ncLib-reticulate" %in% list)
  {
    warning("Reticulate already set.")
  }
  else
  {
    lib <- c("cdsapi","openpyxl","tk","pandas","netCDF4")
    conda_create("ncLib-reticulate",packages=lib)
    warning("Remember to close the python enviroment if you won't use it anymore.")
  }

}

#' Close python environment
#' @importFrom reticulate conda_remove
#' @export
close_reticulate <- function()
{
  conda_remove("ncLib-reticulate")
}


#' @title Request ERA5 file using an UI.
#'
#' @description
#' Create a request to ERA5 complete reanalysis database with a user interface.
#' @seealso \code{\link{create_nc_file_data}}
#' @importFrom reticulate import_from_path
#' @importFrom reticulate source_python
#' @importFrom reticulate conda_list
#' @importFrom utils read.csv
#' @export
create_nc_file_app <- function()
{
	list <- conda_list()$name
	if("ncLib-reticulate" %in% list)
	{
    use_condaenv("ncLib-reticulate")
    file_var <- system.file("var_names.csv",package="NcLibrary")
    df_var <- read.csv(file_var, sep=",")
    file_lvl <- system.file("geo_names.csv", package="NcLibrary")
    df_lvl <- read.csv(file_lvl,sep=",")

    source_python(system.file("app_era5.py",package="NcLibrary"))
    app <- import_from_path("app_era5", file.path(system.file(package = "NcLibrary")))$app
    app(df_var,df_lvl)
	}
	else{
	  stop("It is needed to execute set_reticulate() before using this function.")
	}
}

#' @title Request ERA5 file.
#'
#' @description
#' Create a request to ERA5 complete reanalysis database and return a .nc
#' file with the data requested
#'
#' @param levels levels id requested joined by comma ",". The range is from 1 to 127 both included
#' @param hours requested in HH:MM:SS format joined by comma ",".
#' @param grid of the longitude-latitude net in RxR format where R stands for a real number
#' @param target name of the .nc file. Must end with ".nc" extension.
#' @param start_date first day to request in format YYYY/MM/DD (included).
#' @param end_date last day to request in format YYYY/MM/DD (included).
#' @param coordinates string with the four coordinates joined by slash "/" (N/W/E/S)
#' @param variables integer representation of the variables to request joined by comma ",".
#' @param units_num number column of the unit of the level. 3 for half-level level, 5 for Geopotential altitude and 7 for Temperature
#' @seealso \url{https://apps.ecmwf.int/codes/grib/param-db/}
#' @seealso \url{https://confluence.ecmwf.int/display/UDOC/L137+model+level+definitions}
#' @importFrom reticulate source_python
#' @importFrom reticulate conda_list
#' @importFrom reticulate import_from_path
#' @keywords internal
#' @export
create_nc_file_data <- function(levels, hours, grid, target, start_date, end_date, coordinates, variables, units_num)
{
	list <- conda_list()$name
	if("ncLib-reticulate" %in% list)
	{
	  if(units_num %in% c(3,5,7))
	  {
	    use_condaenv("ncLib-reticulate")
	    source_python(system.file("request_data.py",package="NcLibrary"))
	    make_request <- import_from_path("request_data", file.path(system.file(package = "NcLibrary")))$make_request

	    file_lvl <- system.file("geo_names.csv", package="NcLibrary")
	    df_lvl <- read.csv(file_lvl,sep=",")

	    make_request(levels, hours, grid, target, start_date, end_date, coordinates, variables, df_lvl)
	  }
	  else
	  {
	    stop("units_num must be 3,5 or 7")
	  }

	}
	else{
	  stop("It is needed to execute set_reticulate() before using this function.")
	}
}
