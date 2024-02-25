
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

#' All coordinates
#'
#' @param data_era data from ERA5 list.
#' @param coordinates_m Matrix where each row is a coordinate (size nx2)
#'
#' @return true if all the coordinates are in the list
#' @noRd
.all_coordinates_inside_era <- function(lat,lon,coordinates_m)
{
  fun <- function(x){coord_is_inside(x, lon, lat)}
  bools <- apply(coordinates_m,MARGIN=1,FUN=fun)
  return(all(bools))
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
	lat <- data_era$extra$lat
	lon <- data_era$extra$lon

	if(!coord_is_inside(coordinates, lon, lat))
	{
		stop("Coordinate of range.\n")
	}
	xgrid <- .get_grid_1D(lon,depth,coordinates[1])
	ygrid <- .get_grid_1D(lat,depth,coordinates[2])

	mx <- min(xgrid)
	my <- min(ygrid)
	Mx <- max(xgrid)
	My <- max(ygrid)

	if(!.all_coordinates_inside_era(lat,lon,matrix(data=c(Mx,Mx,mx,mx,My,my,My,my),ncol=2,nrow=4)))
	{
	  warning("Some points are out of range.\n")
	}
	return(list(x=xgrid,y=ygrid))
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
							   name_col=paste(data_obs$station_info$name, data_obs$var_info$name, sep=data_era$configuration$sep))
{
	dates_era <- data_era$df$DATE
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
		colnames(auxy) <- c("DATE",paste("OBS",name_col,sep=data_era$configuration$sep))
		data_era$df <- inner_join(data_era$df,auxy,by= "DATE")

		return(data_era)
	}
}

cxc <- get_merge_observed(b,a$vars$CC_STAID000238)

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
	if(!all(c("lat","lon") %in% data_era$extra)){
		stop("There is not a two-coordinate dimension in the nc file.")
	}

	cord_grid <- .get_grid_coordinate(data_era,coordinate,depth)
	grid <- expand.grid(cord_grid$x, cord_grid$y)
	grid[,1] <- lon_to_str(grid[,1])
	grid[,2] <- lat_to_str(grid[,2])
	names <- apply(grid,MARGIN=1,FUN=function(x) paste0(data_era$configuration$sep,paste(x,collapse=data_era$configuration$sep),data_era$configuration$sep))
	names <- c(names,paste0("OBS",data_era$configuration$sep),"YEAR","MONTH","DAY","DATE","HOUR","MINUTE","SECOND")
	reg <- paste(names, collapse="|")
	bools <- sapply(colnames(data_era$df), function(item1) any(grepl(reg, item1)))
	data_era$df <- data_era$df[,bools]
	data_era$extra$lat <- cord_grid[2]
	data_era$extra$lon <- cord_grid[1]
	return(data_era)
}
