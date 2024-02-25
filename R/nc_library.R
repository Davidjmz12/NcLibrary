
utils::globalVariables(c("DATE", "aux", "x"))

#' Get times data frame
#'
#' @param configuration of the request
#'
#' @return A times data frame with the information
#' @noRd
.get_times<-function(configuration)
{
  	vals <- configuration$read_nc$dim[[configuration$time$name]]$vals
	date_no_format <- as.POSIXct(vals*3600, origin = "1900-01-01 00:00:00.0", tz="GMT", format = "%Y-%m-%d %H:%M:%S")

	format <- ifelse(configuration$time$extended,"%Y-%m-%d %H:%M:%S","%Y-%m-%d %H")
	date_format <- data.frame(DATE=format(date_no_format, format = format))

	if(configuration$time$time_div){
		date_format <- cbind(data.frame(HOUR=hour(date_no_format),DAY=day(date_no_format),MONTH= month(date_no_format), YEAR= year(date_no_format)),date_format)
		if(configuration$time$extended){
			date_format <- cbind(data.frame(MINUTE=minute(date_no_format),SECOND=second(date_no_format)),date_format)
		}
	}

	return (date_format)
}

#' Get Variables with same ordering dimension
#'
#' @param nc_item nc_list result of ncvar_get
#' @param var_name name of the variable
#'
#' @return a matrix with the values of nc_item
#' @noRd
.ncvar_get_ <- function(nc_item,var_name)
{
	old_order <- nc_item$var[[var_name]]$dimids
	new_order <- 0:(nc_item$ndims-1) # Example matrix data
	mat <- array(ncvar_get(nc_item,var_name),dim=nc_item$var[[var_name]]$size)

	# Create a mapping of old dimensions to their positions in new dimensions
	dim_mapping <- match(new_order, old_order)

	# Permute dimensions of the matrix using the mapping
	reordered_matrix <- aperm(mat, perm = dim_mapping)
	return(reordered_matrix)
}

#' Values of data-frame
#'
#' @param configuration of the request
#'
#' @return a 2-D matrix with the information of the nc file.
#' @noRd
.data_values <- function (configuration)
{
	.get_one_variable_values <- function (variable)
	{
		data <- .ncvar_get_(configuration$read_nc,var_name=variable)
		time_dim <- which(sapply(configuration$read_nc$dim, function(x) x$name == configuration$time$name))
		data_formated <-t(apply(data,MARGIN=time_dim,FUN=c))
		return(data_formated)
	}
	vars <- NULL
	for(var in configuration$read_nc$var){
		vars <- cbind(vars,.get_one_variable_values(var$name))
	}
	return (vars)
}


#' Index of the configuration
#'
#' @param configuration of the request.
#' @param dim dimension of the .nc file
#'
#' @return the index of the dimension or 0 if not found.
#' @noRd
.get_index_config <- function (configuration, dim)
{
	names_x <- sapply(configuration$dim,function(x) x$name)
	if(dim$name %in% names_x){
		return(unname(which(names_x==dim$name)))
	} else{
		return(0)
	}
}


#' Modifier of dimension values of type "lat"
#'
#' @param configuration_dim of the dimension
#' @param dim dimension of the nc file
#'
#' @return a formatted version of the column names for a dimension of type "lat".
#' @noRd
.transform_latitude <- function (configuration_dim,dim) {

	values <- if(configuration_dim$coord_360) transform_coordinate(dim$vals) else{dim$vals}
	if(configuration_dim$format == 0){
		return(paste0(values,"N"))
	} else if(configuration_dim$format == 1){
		return(lat_to_str(values))
	}
}

#' Modifier of dimension values of type "lon"
#'
#' @param configuration_dim of the dimension
#' @param dim dimension of the nc file
#'
#' @return a formatted version of the column names for a dimension of type "lon".
#' @noRd
.transform_longitude <- function (configuration_dim,dim) {

	values <- if(configuration_dim$coord_360) transform_coordinate(dim$vals) else{dim$vals}
	if(configuration_dim$format == 0){
		return(paste0(values,"N"))
	} else if(configuration_dim$format == 1){
		return(lat_to_str(values))
	}
}


#' Modifier of dimension values of type "any"
#'
#' @param configuration_dim of the dimension
#' @param dim dimension of the nc file
#'
#' @return a formatted version of the column names for a dimension of type "any".
#' @noRd
.transform_any_dim <- function (configuration_dim,dim){
	units <- if(is.null(configuration_dim$units)) dim$units else configuration_dim$units
	values <- if(is.null(configuration_dim$round)) dim$vals else round(dim$vals,configuration_dim$round)
	return(paste0(values,units))
}

#' Modifier of dimension values
#'
#' @param configuration of the request
#' @param dim dimension of the nc file
#'
#' @return a  formatted version of the column names.
#' @noRd
.modify_dim_values <- function (configuration,dim)
{
	ind_dim <- .get_index_config(configuration,dim)
	if(ind_dim == 0){
		return(paste0(dim$vals,dim$units))
	} else{
		configuration_dim <- configuration$dim[[ind_dim]]

		if(configuration_dim$type=="lat"){
			return(.transform_latitude(configuration_dim,dim))
		} else if(configuration_dim$type=="lon"){
			return(.transform_longitude(configuration_dim,dim))
		} else if(configuration_dim$type=="any"){
			return(.transform_any_dim(configuration_dim,dim))
		}
	}


}

#' Column names
#'
#' @param configuration of the request
#'
#' @return an array with the column names
#' @noRd
.create_names_col <- function(configuration)
{
	read_nc <- configuration$read_nc
	vars <- read_nc$var
	dims <- read_nc$dim
	all_variables <- NULL

	for(var in vars){
		aux_s <- var$name
		for(dim in dims){
			if(dim$name != configuration$time$name){
				dim_values <- .modify_dim_values(configuration,dim)

				expanded <- expand.grid(aux_s,dim_values)

				aux_f <- function(x){
					paste0(x,collapse=configuration$sep)
				}
				aux_s <- apply(expanded,MARGIN=1,aux_f)
			}
		}
		all_variables <- c(all_variables,aux_s)
	}
	return(all_variables)
}



#' Check configuration
#'
#' @param lst configuration
#'
#' @return True if it is a correct configuration
#' @noRd
.check_configuration <- function(lst)
{
	if (!is.list(lst)) {
		stop("Input must be a list.")
	}

	# Check "time" field
	if (!all(c("extended", "time_div", "name") %in% names(lst$time))) {
		stop("'time' list must have 'extended', 'time_div' and 'name'.")
	}

  	if (!is.logical(lst$time$extended) || !is.logical(lst$time$time_div)) {
    	stop("'extended', 'hourly', and 'time_div' in 'time' must be booleans.")
  	}
  	if (!is.character(lst$time$name)) {
    	stop("'name' in 'time' must be a string.")
	}

	# Check "sep" field
	if (!is.character(lst$sep)) {
		stop("'sep' must be a string.")
	}

	# Check "nc_file" field
	if (!is.character(lst$nc_file)) {
		stop("'nc_file' must be a string.")
	}

	# Check "dim" field
	if (!is.list(lst$dim)) {
		stop("'dim' field must be a list.")
	}

	for (dim in lst$dim) {
		if (!all(c("name", "type") %in% names(dim))) {
			stop("'dim' must have 'name' and 'type'.")
		}

		if (dim$type == "any") {
			if (!any(c("units", "round") %in% names(dim))) {
				stop("'any' dimension must have at least 'units' or 'round'.")
			}
		} else if (dim$type %in% c("lon", "lat")) {
			if (!all(c("coord_360", "format") %in% names(dim))) {
				stop("'dim' field is incorrect. No coord_360 or format field")
			}
		  	if (!is.logical(dim$coord_360)) {
        		stop("'coord_360' must be a boolean.")
      		}
      		if (!dim$format %in% c(0, 1)) {
        		stop("'format' must be 0 or 1.")
      		}
		} else {
			stop("'type' of a dimension field is incorrect.")
		}
	}

	tryCatch(
	{
		read_nc <- nc_open(lst$nc_file)
		for(dim in lst$dim){
			if(is.null(read_nc$dim[[dim$name]])){
				stop("Configuration dimensions do not match with the .nc file.")
			}
		}

		if(is.null(read_nc$dim[[lst$time$name]])){
			stop("Configuration time do not match with the .nc file.")
		}

		return(TRUE)
	}, error = function(cond){
			stop("No such file.")
		}
	)
}

#' Read a .nc file
#'
#' @param configuration A list containing the following fields:
#'   \describe{
#'     \item{sep}{A string specifying the column name separator.}
#'     \item{time}{A list with information about the "y" variable in the data frame. It must contain:
#'       \describe{
#'         \item{name}{The name of the time dimension in the .nc file.}
#'         \item{extended}{A boolean (TRUE if you want to include MINUTES and SECONDS in your time format).}
#'         \item{time_div}{A boolean (TRUE if you want extra columns with individual values of YEAR, DAY, MONTH, HOUR, etc.).}
#'       }
#'     }
#'     \item{nc_file}{A string with the name of the ".nc" file to read.}
#'     \item{dim}{A list of dimensions to include metadata. It can be empty but it must be included in configuration. Each element is a list with:
#'       \describe{
#'         \item{type}{"lon" for longitude dimensions, "lat" for latitude dimensions, or "any" for other dimensions.}
#'         \item{For type "lon" or "lat":}{
#'           \describe{
#'             \item{coord_360}{A boolean indicating if the coordinate is given in the set [0,360].}
#'             \item{name}{The name of the corresponding dimension in the .nc file.}
#'             \item{format}{0 if the value format is "+-xN" for latitude or "+-xE" for longitude, or 1 if the format is "+xN or +xS" for latitude or "+xE or +xW".}
#'           }
#'         }
#'         \item{For type "any":}{
#'           \describe{
#'             \item{units}{A string specifying the measurement units.}
#'             \item{round}{An integer specifying the number of decimals for dimension values.}
#'           }
#'         }
#'       }
#'     }
#'   }
#'
#' @return A data frame with the requested information.
#' @export
read_nc_file <- function(configuration)
{
	if(!.check_configuration(configuration)){
		print("Not a good configuration")
		return(FALSE)
	}

	configuration[["read_nc"]] <- nc_open(configuration$nc_file)

	col_names <- .create_names_col(configuration)

	row_names <- .get_times(configuration)

	data <- .data_values(configuration)

	df <- as.data.frame(data)
	colnames(df) <- col_names
	df <- cbind(row_names,df)

	return(df)
}