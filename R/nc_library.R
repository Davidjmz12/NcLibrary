
utils::globalVariables(c("DATE", "aux", "x"))

#' Get times information
#'
#' @param hourly_column TRUE if the hours are meant to be in the columns.
#' @param extended_hours TRUE to display the hours with minutes and seconds.
#' @param private_var variable with the useful information of the .nc file
#'
#' @return a list with the information of the times stored in .nc file
#' @noRd
.get_times<-function(hourly_column=FALSE, extended_hours=TRUE, private_var)
{
  date <- as.POSIXct(private_var$nc_data$dim$time$vals*3600, origin = "1900-01-01 00:00:00.0", tz="GMT", format = "%Y-%m-%d %H:%M:%S")
  date <- format(date, format = "%Y-%m-%d %H:%M:%S")
  if(hourly_column) date <- date[seq(1,length(date),by=.get_num_hours(private_var))]

  days <- day(date)
	months <- month(date)
  years <- year(date)
	if(hourly_column)
  {
    date <- strftime(date, format = "%Y-%m-%d")
		return(data.frame(DATE=date, DAY=days, MONTH=months, YEAR=years))
  }
	else{
		hours <- hour(date)
		if(!extended_hours)
		{
		  date <- strftime(date, format = "%Y-%m-%d %H")
			return(data.frame(DATE=date, HOUR=hours, DAY=days, MONTH=months, YEAR=years))
		}
		else{
			minutes <- minute(date)
			seconds <- second(date)
			hour_ext <- strftime(date,format = "%H:%M:%S")
			return(data.frame(DATE=date, SECOND=seconds, MINUTE=minutes, HOUR=hours, DAY=days, MONTH=months, YEAR=years,HOUR_EXT=hour_ext))
		}
	}
}

#' Return the number of hours of a .nc file
#'
#' @param private_var variable with the useful information of the .nc file
#'
#' @return a integer with the number of hours
#' @noRd
.get_num_hours <- function(private_var)
{
    times <- .get_times(private_var=private_var)$HOUR
    return(length(unique(times)))
}

#' Get the hours of the .nc file
#'
#' @param hourly_col TRUE if the hours are meant to be in the columns.
#' @param private_var variable with the useful information of the .nc file
#' @param extended_hours TRUE to display the hours with minutes and seconds.
#'
#' @return a list with the hours.
#' @noRd
.get_hours <- function(hourly_col, private_var, extended_hours)
{
    if(hourly_col)
	{
		if(extended_hours)
		{
			return(paste0(unique(.get_times(private_var=private_var, extended_hours = extended_hours)$HOUR_EXT), "h"))
		}
		else{
			return(paste0(sprintf("%02d", unique(.get_times(private_var=private_var, extended_hours = extended_hours)$HOUR)), "h"))
		}

	}
    else return("nh")
}

#' Get the data of a variable in the wanted order.
#'
#' @param nc_item item returned by ncdf4::nc_open
#' @param var_name variable name
#'
#' @return a multi-dimensional matrix with the data of the variable
#' @importFrom ncdf4 ncvar_get
#' @noRd
.ncvar_get_ <- function(nc_item,var_name)
{
	old_order <- nc_item$var[[var_name]]$dimids
	new_order <- c(0,1,2,3) # Example matrix data
	mat <- array(ncvar_get(nc_item,var_name),dim=nc_item$var[[var_name]]$size)

	# Create a mapping of old dimensions to their positions in new dimensions
	dim_mapping <- match(new_order, old_order)

	# Permute dimensions of the matrix using the mapping
	reordered_matrix <- aperm(mat, perm = dim_mapping)
	return(reordered_matrix)
}

#' Return a 2-D matrix with the data of the variable arranged in the proper way.
#'
#' @param var_name name of the variable
#' @param level_value geo potential level
#' @param hourly_column TRUE if the hours are meant to be in the columns.
#' @param private_var variable with the useful information of the .nc file
#'
#' @return a 2-D matrix with the data.
#' @noRd
.data_values <- function(var_name, level_value, hourly_column, private_var)
{
    level_index <- which(.geo_levels(private_var$nc_data)==level_value)
    data <- .ncvar_get_(private_var$nc_data, var_name)[,, level_index,]
    data <- data[seq(1,dim(data)[1],by=private_var$red_x),seq(1,dim(data)[2],by=private_var$red_y),]

    num_hours <- 1
    if(hourly_column) num_hours <- .get_num_hours(private_var)

    data_val <- apply(data,MARGIN=3,FUN=as.vector)
    final_matrix <- matrix(data_val, nrow=dim(data_val)[1]*num_hours, ncol=dim(data_val)[2]/num_hours)
    return(t(final_matrix))
}

#' Column names of the data frame wanted.
#'
#' @param var_name variable name.
#' @param sep separator.
#' @param level_value geo potential level.
#' @param hourly_column TRUE if the hours are meant to be in the columns.
#' @param private_var variable with the useful information of the .nc file.
#' @param extended_hours TRUE to display the hours with minutes and seconds.
#'
#' @return an array with the column names.
#' @noRd
.col_names <- function(var_name, sep, level_value, hourly_column, private_var, extended_hours)
{

    aux <- .get_coordinates(private_var)
    longitude <- aux$longitude
    latitude <- aux$latitude
    
    level_units <- private_var$nc_data$dim$level$units
    level_value <- paste0(round(as.double(level_value),2),level_units)

    hours <- .get_hours(hourly_column, private_var, extended_hours)

    all_comb <- expand.grid(longitude,latitude,hours)

    fun <- function(x)
    {
        aux <- paste(x,collapse=sep)
        aux <- paste(var_name,level_value,aux,sep=sep)
        return(aux)
    }
    return(apply(all_comb,MARGIN=1,FUN=fun))
}

#' Get information of the latitude and longitude variables of the .nc file
#'
#' @param private_var with the useful information of the .nc file
#'
#' @return a list with the information
#' @noRd
.get_coordinates <- function(private_var)
{
    longitude <- private_var$nc_data$dim$longitude$vals
    latitude <- private_var$nc_data$dim$latitude$vals
    longitude <- longitude[seq(1,length(longitude),by=private_var$red_x)]
    latitude <- latitude[seq(1,length(latitude),by=private_var$red_y)]
    grid <- (paste0(abs(longitude[1] - longitude[2]), "x", abs(latitude[1] - latitude[2])))

    lat_mm <- list(min=lat_to_str(min(latitude)), max=lat_to_str(max(latitude)))
    long_mm <- list(min=lon_to_str(min(longitude)), max=lon_to_str(max(longitude)))

    longitude <- sapply(longitude, FUN=lon_to_str)
    latitude <- sapply(latitude, FUN=lat_to_str)

    return(list(longitude=longitude,latitude=latitude,grid=grid,lat_mm=lat_mm,long_mm=long_mm))
}

#' File name of the .csv file
#'
#' @param root root of the file
#' @param sep separator
#' @param prefix prefix of the column names
#' @param variables name of the variables
#' @param levels geo-potential levels
#' @param hourly_column TRUE if the hours are meant to be in the columns.
#' @param private_var with the useful information of the .nc file
#'
#' @return the file name
#' @noRd
.file_name <- function(root, sep, prefix, variables, levels, hourly_col, private_var)
{
    aux <- .get_coordinates(private_var)
    res_ll <- paste(c(aux$lat_mm,aux$long_mm,aux$grid),collapse=sep)
    res_var <- paste(variables,collapse = sep)
    res_lev <- paste(levels,collapse = sep)

    if(hourly_col) res_hour <- "daily"
    else  res_hour <- "ndaily"

    root <- paste0(root,"/",prefix)
    if(prefix!="")  root <- paste0(root,sep)
    str_f <- paste0(root,res_hour)
    str_f <- paste(str_f,res_var,res_lev,res_ll,sep=sep)
    str_f <- paste0(str_f,".csv")
    return(str_f)
}

#' Get a grid of a specified depth and a coordinate point.
#'
#' @param x coordinate points.
#' @param depth depth of the grid.
#' @param sup coordinate point to grid of.
#'
#' @return a subset of x with the points close to sup.
#' @noRd
#' @importFrom dplyr arrange
#' @importFrom utils head
#' @examples
#' .get_grid_1D(c(1,2,3,4,5),2,3.5)
.get_grid_1D <- function(x,depth,sup)
{
	df <- as.data.frame(cbind(x,aux=x))
	df$aux <- abs(df$aux-sup)
	df <- arrange(df,aux)
	return(sort(head(df$x,2*depth)))
}

#' Return a grid of a specified depth surrounding a city.
#'
#' @param data_era data from ERA5 list.
#' @param coordinates coordinate point
#' @param depth of the grid
#'
#' @return a grid of coordinate points surrounding the city.
#' @noRd
.get_grid_coordinate <- function(data_era, coordinates, depth=1)
{
	lat <- data_era$latitude$vals
	lon <- data_era$longitude$vals
	if(!coord_is_inside(coordinates, data_era$longitude$vals, data_era$latitude$vals))
	{
		stop("Coordinate of range.\n")
	}
	xgrid <- .get_grid_1D(lon,depth,coordinates[1])
	ygrid <- .get_grid_1D(lat,depth,coordinates[2])
	
	mx <- min(xgrid)
	my <- min(ygrid)
	Mx <- max(xgrid)
	My <- max(ygrid)
	
	if(!.all_coordinates_inside_era(data_era,matrix(data=c(Mx,Mx,mx,mx,My,my,My,my),ncol=2,nrow=4)))
	{
	  warning("Some points are out of range.\n")
	}
	return(list(x=xgrid,y=ygrid))
}



#' Create the variable with information of the .nc file
#'
#' @param nc_file name of the file
#' @param reduction_factor reduction factor
#'
#' @return the private variable with the information
#' @importFrom ncdf4 nc_open
#' @noRd
.set_private_var <- function(nc_file, reduction_factor)
{
	private_var <- list(
					 nc_data = nc_open(nc_file),
					 red_x = 1L,
					 red_y=  1L
				   )
	private_var$nc_data$dim$longitude$vals <- .transform_longitude(private_var$nc_data$dim$longitude$vals)
  long_l <- private_var$nc_data$dim$longitude$len-1
  lat_l <- private_var$nc_data$dim$latitude$len-1
  if(is.integer(reduction_factor) & all(reduction_factor)>=1)
  {
  	if(long_l%%reduction_factor[1] != 0 || lat_l%%reduction_factor[2] != 0)
  	{
  		warning("Reduction factor does not cover all the grid.\n")
  	}
  	private_var$red_x <- reduction_factor[1]
  	private_var$red_y <- reduction_factor[2]
  	return(private_var)
  }
  else{
    stop("The reduction_factor must be a positive integer.")
    stop("Use L (e.g, 5L) to explicitly create an integer object.")
  }
}

#' Return the variable names.
#'
#' @param nc_data nc object
#'
#' @return a list with the names.
#' @noRd
.var_names <- function(nc_data)
{
	return(names(nc_data$var))
}

#' Return the geo-potential levels.
#'
#' @param nc_data nc object
#'
#' @return a list with the levels.
#' @noRd
.geo_levels <- function(nc_data)
{
	return(nc_data$dim$level$vals)
}


#' @title Information of a .nc.
#' @description 
#' Fetch the most important information about a .nc file.
#'
#' @param nc_file .nc file.
#' @param reduction.x integer reduction factor of the longitude grid.
#' @param reduction.y integer reduction factor of the latitude grid.
#' 
#' @return a list with the information of the .nc file.
#' 
#' @export
#' @seealso \code{\link{read_nc_file}}
fetch_information <- function(nc_file, reduction.x=1L, reduction.y=1L)
{
	nc_data <- nc_open(nc_file)
	var_names <- .var_names(nc_data)
	geo_levels <- .geo_levels(nc_data)
	private_var <- .set_private_var(nc_file,c(reduction.x,reduction.y))
	coordinates <- .get_coordinates(private_var)
	return(list(variables = var_names,geo_levels=geo_levels,coordinates=coordinates))
}


#' @title Read data from .nc
#' @description
#' Create a "ERA5" list containing the data frame with the information stored in a .nc file.
#'
#' @param nc_file name of the nc file
#' @param reduction.x integer reduction factor of the longitude grid.
#' @param reduction.y integer reduction factor of the latitude grid.
#' @param sep character to separate fields of the result .csv
#' @param hourly.col TRUE if the hours are meant to be in the columns.
#' @param var.names Name of the variables to save. Every variable as default.
#' @param geo.levels Levels to save. Every level as default.
#' @param extended.hours TRUE to display the hours with minutes and seconds.
#' @param display.times TRUE to display a time-report of the function.
#'
#' @return a list with the information in the .nc file
#' @importFrom ncdf4 nc_close
#' @export
read_nc_file <- function(nc_file, reduction.x=1L,reduction.y=1L,sep="_", hourly.col=TRUE,
						 var.names=.var_names(nc_open(nc_file)), geo.levels=.geo_levels(nc_open(nc_file)),
						 extended.hours=FALSE,display.times=FALSE)
{

  if(display.times)
  {
      start <- Sys.time()
      print("-- Starting function get_df ")
  }
	private_var <- .set_private_var(nc_file, c(reduction.x, reduction.y))

  var_geo <-expand.grid(geo.levels,var.names)

  fun_col <- function(x,sep,hourly.col, private_var, extended.hours)
  {
    x <- unname(x)
    geo_level <-gsub(" ", "", x[1])
    var <- x[2]
    col_names <- .col_names(var, sep, geo_level, hourly.col, private_var, extended.hours)
    return(col_names)
  }
  column_names <- apply(var_geo, MARGIN=1, FUN=fun_col, sep=sep, hourly.col=hourly.col,
					  private_var=private_var, extended.hours=extended.hours)

  fun_val <- function(x,hourly.col)
  {
      geo_level <- x$Var1
      var <- as.character(x$Var2)
      mat_values <- .data_values(var, geo_level, hourly.col, private_var)
      return(mat_values)
  }
  row_names <- .get_times(hourly.col, extended.hours, private_var)
  mat <- matrix(NA, nrow=dim(row_names)[1],0)
  for(i in seq_len(nrow(var_geo)))
  {
      mat_aux <- fun_val(var_geo[i,],hourly.col)
      mat <- cbind(mat,mat_aux)
  }

  df <- as.data.frame(mat)
  colnames(df) <- column_names
  df <- cbind(row_names,df)

  if(display.times)
  {
      end <- Sys.time()-start
      print(sprintf("** Time elapsed function get_df : %f s",end))
      print(sprintf("   With dataframe dimension: %d x %d",dim(df)[1],dim(df)[2]))
  }

  aux <- .get_coordinates(private_var)
  l <- list(temporal_info=list(daily=hourly.col, first=min(row_names$DATE),last=max(row_names$DATE),vals=row_names$DATE),
            df=df,vars=var.names,geo_levels=geo.levels,geo.units = private_var$nc_data$dim$level$units, creation_time=as.character(Sys.time()),
            extended_hours=extended.hours,hours=.get_hours(TRUE, private_var,extended.hours),grid=aux$grid,
            latitude = list(range=list(min=str_to_lat(aux$lat_mm$min),max=str_to_lat(aux$lat_mm$max)),vals=str_to_lat(aux$latitude)),
            longitude= list(range=list(min=str_to_lon(aux$long_mm$min),max=str_to_lon(aux$long_mm$max)),vals=str_to_lon(aux$longitude)),
            file=nc_file,sep=sep,obs_values= NULL)
  nc_close(private_var$nc_data)
	return(l)
}

#' @title Writing .csv of .nc file
#' 
#' @description  
#' Create a .csv file with the information stored in a .nc file.
#'
#' @param nc_file name of the .nc file
#' @param reduction.x integer reduction factor of the longitude grid.
#' @param reduction.y integer reduction factor of the latitude grid.
#' @param prefix of the column names in the data frame.
#' @param sep character to separate fields of the result .csv
#' @param root to set the create directory.
#' @param hourly.col TRUE if the hours are meant to be in the columns.
#' @param var.names Name of the variables to save. Every variable as default.
#' @param geo.levels Levels to save. Every level as default.
#' @param extended.hours TRUE to display the hours with minutes and seconds.
#' @param display.times TRUE to display a time-report of the function.
#'
#' @importFrom ncdf4 nc_open
#' @importFrom data.table fwrite
#' 
#' @export
write_csv <- function(nc_file, reduction.x=1L, reduction.y=1L, prefix="", sep="_", root=".", hourly.col=TRUE,
					  var.names=.var_names(nc_open(nc_file)), geo.levels=.geo_levels(nc_open(nc_file)),
					  extended.hours=FALSE, display.times=FALSE)
{

    if(display.times)
    {
        start <- Sys.time()
        print("-- Starting function csv_write ")
    }
    df <- read_nc_file(nc_file,reduction.x,reduction.y,sep, hourly.col, var.names, geo.levels, extended.hours,display.times)
    private_var <- .set_private_var(nc_file, c(reduction.x, reduction.y))
	  file_name <- .file_name(root, sep, prefix, var.names, geo.levels, hourly.col, private_var)
    fwrite(df$df,file_name)
	  print("Write done.")
    if(display.times)
    {
        end <- Sys.time()-start
        print(sprintf("** Time elapsed function csv_write : %f %s",end,attr(x,"units")))
        file_size <- file.size(file_name,"MB")[1]
        print(sprintf("   With file size : %d MB",file_size))
    }
}


#' @title Merge with observed values
#' 
#' @description
#' Add to a "ERA5" daily file an observed "ECA" variable. 
#' The observed time set could be reduced in order to fit the "ERA5" time set.
#'  
#' @param data_era "ERA5" list to add.
#' @param data_obs Observed variable to add.
#' @param name_col name of the column in the data frame.
#'
#' @return the new "ERA5" list with the observed value added.
#' @importFrom dplyr inner_join
#' @export
get_merge_observed <- function(data_era, data_obs,
							   name_col=paste(data_obs$station_info$name, data_obs$var_info$name, sep=data_era$sep))
{
    if(!data_era$temporal_info$daily)
    {
        stop("Data from ERAS5 must be daily.")
    }
    else
	{
		dates_era <- data_era$temporal_info$vals
        dates_eca <- data_obs$temporal_info$vals
		new_time_set <- intersect(dates_era,dates_eca)
		if((newlength <- length(new_time_set))==0)
		{
			stop("The intersection of both times data sets is 0. ")
		}
		else
		{
			if(newlength != length(dates_era)) warning("The observed data does not cover all the ERA5 data.")
			auxy <- subset(data_obs$data, select=c("DATE", data_obs$var_info$name))
			colnames(auxy) <- c("DATE",paste("OBS",name_col,sep=data_era$sep))
			data_era$df <- inner_join(data_era$df,auxy,by= "DATE")
			data_era$obs_values <- append(data_era$obs_values,name_col)
			data_era$temporal_info$vals <- new_time_set
			data_era$temporal_info$first <- min(new_time_set)
			data_era$temporal_info$last <- max(new_time_set)

			return(data_era)
		}

    }
}

#' @title Subset grid city
#' 
#' @description
#' Function that returns a subset of a "ERA5" list around a location.
#'
#' @param data_era "ERA5" list to modify.
#' @param city_name name of the city.
#' @param depth of the net to subset.
#'
#' @return the "ERA5" subset.
#' @seealso \code{\link{read_nc_file}}
#' 
#' @importFrom tmaptools geocode_OSM
#' @export
get_subset_city <- function(data_era, city_name, depth=1)
{
  coord_city <- unname(geocode_OSM(city_name)$coords)
  return(get_subset_coordinates(data_era,coord_city,depth))
}

#' @title Subset grid by coordinate point
#' 
#' @description
#' Function that returns a subset of a "ERA5" list around a coordinate.
#'
#' @param data_era "ERA5" list to modify.
#' @param coordinate array with the longitude and latitude.
#' @param depth of the net to subset.
#'
#' @return the "ERA5" subset.
#' @seealso \code{\link{read_nc_file}}
#' @export
get_subset_coordinates <- function(data_era, coordinate, depth=1)
{
  cord_grid <- .get_grid_coordinate(data_era,coordinate,depth)
  grid <- expand.grid(cord_grid$x, cord_grid$y)
  grid[,1] <- lon_to_str(grid[,1])
  grid[,2] <- lat_to_str(grid[,2])
  names <- apply(grid,MARGIN=1,FUN=function(x) paste0(data_era$sep,paste(x,collapse=data_era$sep),data_era$sep))
  names <- c(names,paste0("OBS",data_era$sep),"YEAR","MONTH","DAY","DATE","HOUR","MINUTE","SECOND","HOUR_EXT")
  reg <- paste(names, collapse="|")
  bools <- sapply(colnames(data_era$df), function(item1) any(grepl(reg, item1)))
  data_era$df <- data_era$df[,bools]
  data_era$latitude$vals <- cord_grid[2]
  data_era$longitude$vals <- cord_grid[1]
  data_era <- c(data_era,list(coordinate_subset=coordinate))
  return(data_era)
}

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
