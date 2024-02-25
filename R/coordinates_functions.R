
#' Transform (0,360) coordinate format to (-180,180)
#'
#' @param v array of latitudes or longitudes
#'
#' @return array with the transformed longitudes or latitudes
#' @export
#' 
#' @examples
#' transform_coordinate(c(360,0))
transform_coordinate <- function(v)
{
  fun <- function(v)
  {
    return ((v+180)%%360-180)
  }
  return(sapply(v,fun))
}


#' @title Coordinate point is inside the grid.
#' @description
#' Check if a coordinate limit is outside a grid of points
#' 
#'
#' @param coordinate coordinate point
#' @param longitude_v longitude array
#' @param latitude_v latitude array
#'
#' @return TRUE if the coordinate point is inside the grid.
#' @export
#'
#' @examples
#' coord_is_inside(c(2,3),c(0,10),c(0,10))
coord_is_inside <- function(coordinate, longitude_v, latitude_v)
{
    return(coordinate[1] %in% longitude_v && coordinate[2] %in% latitude_v)

}

#' Latitude integer to string representation
#'
#' @param latitude latitude array in integer representation
#'
#' @return An array of strings with the string representation of latitude.
#' @export
#'
#' @examples
#' lat_to_str(c(1,-1))
lat_to_str <- function(latitude)
{
    aux <- function(latitude)
    {
        latitude <- as.character(latitude)
        if(substr(latitude,1,1) == "-") return(paste0(substr(latitude, 2, nchar(latitude)), "S"))
        else return(paste0(latitude, "N"))
    }
    return(sapply(latitude,FUN=aux))
}


#' String latitude representation to integer value
#'
#' @param lat_str string array representing the latitudes.
#'
#' @return an integer array with the integer representation
#' @export
#'
#' @examples
#' str_to_lat(c("1N","1S"))
str_to_lat <- function(lat_str)
{
    aux <- function(lat_str)
    {
        lat_numeric <- as.numeric(substr(lat_str, 1, nchar(lat_str) - 1))
        # Check the last character for 'N' or 'S' and assign the sign accordingly
        last_char <- toupper(substr(lat_str, nchar(lat_str), nchar(lat_str)))
        if (last_char == "N") return(lat_numeric)
        else if (last_char == "S") return(-lat_numeric)
        else stop("Invalid longitude format. Use 'N' or 'S' at the end of the string.")
    }
    return(unname(sapply(lat_str,FUN=aux)))
}

#' Longitude integer to string representation
#'
#' @param longitude longitude array in integer representation
#'
#' @return an array of strings with the string representation of longitude
#' @export
#'
#' @examples
#' lon_to_str(c(1,-1))
lon_to_str <- function(longitude)
{
    aux <- function(longitude)
    {
        longitude <- as.character(longitude)
        if(substr(longitude, 1, 1) == "-") return(paste0(substr(longitude, 2, nchar(longitude)), "W"))
        else return(paste0(longitude, "E"))
    }
    return(sapply(longitude,FUN=aux))
}

#' String longitude representation to integer value
#'
#' @param lon_str string array representing the longitudes.
#'
#' @return an integer array with the integer representation
#' @export
#'
#' @examples
#' str_to_lon(c("1E","1W"))
str_to_lon <- function(lon_str) {

    aux <- function(lon_str)
    {
        lon_numeric <- as.numeric(substr(lon_str, 1, nchar(lon_str) - 1))
        last_char <- toupper(substr(lon_str, nchar(lon_str), nchar(lon_str)))
        if (last_char == "E") return(lon_numeric)
        else if (last_char == "W") return(-lon_numeric)
        else stop("Invalid longitude format. Use 'E' or 'W' at the end of the string.")
    }
    return(unname(sapply(lon_str,FUN=aux)))
}
