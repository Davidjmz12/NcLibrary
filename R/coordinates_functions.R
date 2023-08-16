
#' @title Coordinate point is outside,
#' @description
#' Check if a coordinate limit is outside a 2D interval.
#' 
#'
#' @param coordinate_str coordinate point
#' @param limit_long longitude interval.
#' @param limit_lat_str latitude interval.
#'
#' @return TRUE if the coordinate point is outside the 2D interval
#' @noRd
#'
#' @examples
#' coord_is_not_inside(c(2,3),c(0,10),c(0,10))
coord_is_not_inside <- function(coordinate_str, limit_long, limit_lat_str)
{
    return(coordinate_str[1]<limit_long[1] || coordinate_str[1]>limit_long[2] ||
             coordinate_str[2]<limit_lat_str[1] || coordinate_str[2]>limit_lat_str[2])

}

#' Latitude integer to string representation
#'
#' @param latitude latitude array in integer representation
#'
#' @return an array of strings with the string representation of latitude.
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
