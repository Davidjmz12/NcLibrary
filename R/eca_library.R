
#' Delete all the unused files of a ECA zip.
#'
#' @noRd
.delete_files <- function()
{
	unlink("temp/elements.txt")
	unlink("temp/metadata.txt")
	unlink("temp/sources.txt")
	unlink("temp/stations.txt")
}

#' Extract the station of a variable
#'
#' @param file_name file name of the variable
#' @param blended TRUE if it is blended.
#'
#' @return a list with the station info of a variable
#' @importFrom stringr str_match
#' @importFrom stringr str_detect
#' @noRd
.get_station_variable <- function(file_name,blended)
{
	pattern <- ifelse(blended,"station\\s(.*)\\s\\("
							,"of\\s(.*)\\s\\(")

	lines <- readLines(file_name,n=20)
	wanted_line <- lines[str_detect(lines, "This is the")]
	aux <- str_match(wanted_line,pattern)
	name <- aux[,2]

	souid <- as.numeric(str_match(wanted_line,"SOUID:\\s([0-9]+)")[,2])
	staid <- as.numeric(str_match(wanted_line,"STAID:\\s([0-9]+)")[,2])

	return(list(name=name,staid=staid,souid=souid))
}

#' Variable information
#'
#' @param file_name file name of the variable
#'
#' @return list with the information of the variable
#' @importFrom stringr str_match
#' @importFrom stringr str_detect
#' @noRd
.get_variable_name <- function(file_name)
{
	lines <- readLines(file_name,n=20)
	wanted_line <- lines[which(str_detect(lines, "YYYYMMDD"))[1]+1]
	aux <- str_match(wanted_line,"[0-9]+[-][0-9]+\\s+([:alnum:]+)\\s+:\\s+(.*)")
	name <- aux[,2]
	description <- aux[,3]
	return(list(name=name,description=description))
}

#' Get all data from an array of dates.
#'
#' @param dates. array of the dates
#'
#' @return a list with the information of the dates
#' @import lubridate
#' @noRd
.get_dates_info <- function(dates.)
{
	days <-  day(dates.)
	months <- month(dates.)
	years <- year(dates.)
	date <- format(dates.,"%Y-%m-%d")
	return(list(DAY=days,MONTH=months,YEAR=years,DATE=date))
}

#' Get the data and the temporal data of a variable
#'
#' @param file_name file name of the variable
#'
#' @return a list with the information
#' @importFrom utils read.table
#' @importFrom stringr str_detect
#' @noRd
.get_data <- function(file_name)
{
	skip <- which(str_detect(readLines(file_name,n=23), "DATE,"))[1]-1

	data <- read.table(file_name, header=TRUE, skip=skip,sep=",")
	values <- dim(data)[2]-1
	data[data[,values]==-9999,values] <- NA
	dates. <- as.Date(as.character(data$DATE),format="%Y%m%d")
	all_date_info <- .get_dates_info(dates.)
	data <- subset(data,select=-DATE)
	data <- cbind(all_date_info,data)


	is_daily <- all(diff(dates.) == 1)
	min_date <- .get_dates_info(min(dates.))$DATE
	max_date <- .get_dates_info(max(dates.))$DATE
	temporal_data <- list(first=min_date,last=max_date,daily=is_daily,vals=as.character(dates.))

	return(list(data=data,temporal_data=temporal_data))
}

#' Return all the data from a variable.
#'
#' @param file_name file name of the variable.
#' @param blended TRUE if it is blended.
#'
#' @return a list with all the information.
#' @noRd
.get_variable <- function(file_name,blended)
{
	station <- .get_station_variable(file_name,blended)
	variable_name <- .get_variable_name(file_name)
	temp <- .get_data(file_name)

	data <- temp$data
	temporal_data <- temp$temporal_data

	return(list(temporal_info=temporal_data,station_info=station,var_info=variable_name,data=data))
}

#' Return all the data from the zip file.
#'
#' @param blended TRUE if it is blended.
#'
#' @return a list with all the information of every variable and the general information.
#' @importFrom stringr str_match
#' @importFrom stats setNames
#' @noRd
.get_all_variables <- function(blended)
{
	list_files <- list.files(path="temp/")
	list_files <- paste0("temp/",list_files)
	vars <- sapply(list_files,FUN=.get_variable,blended=blended)
	names <- str_match(colnames(vars),"/(.*)\\.")[,2]
	list_of_lists <- setNames(as.list(data.frame(vars)), names)
	return(list_of_lists)
}

#' Delete temporal directory
#' @noRd
.delete_temporal_dir <- function()
{
	unlink("temp",recursive=TRUE)
}

#' Create a temporal directory with the data of a directory
#'
#' @param zip_file zip file to extract
#' @importFrom utils unzip
#' @noRd
.create_temporal_dir <- function(zip_file)
{
	.delete_temporal_dir()
	dir.create("temp")
	unzip(zip_file,exdir="temp/")
}


#' Extract the information of the stations.
#'
#' @return the information of the station file in the temp directory
#' @noRd
.get_all_stations_info <- function()
{
	data <- read.table("temp/stations.txt", header=TRUE, skip=17,sep=",")
	return(data)
}


#' @title Extract data from ECA&D database
#' @description
#' Function that extract all the data downloaded from the ECA&D database.
#' 
#' @param zip_file name of the zip extracted from the ECA&D database.
#' @param blended TRUE if the data is blended, FALSE otherwise.
#'
#' @return a list with all the variables contained in the zip_file
#' @export
#' @seealso \url{https://www.ecad.eu/dailydata/customquery.php}
read_eca_zip <- function(zip_file,blended)
{
	.create_temporal_dir(zip_file)

	if(blended) stations_info <- .get_all_stations_info()

	.delete_files()

	vars <- .get_all_variables(blended)

	.delete_temporal_dir()

	aux <- list(vars=vars,nvars=length(vars),creation_time=as.character(Sys.time()),
				origin_file=zip_file,blended=blended)
	if(blended) return(c(aux,list(stations_info=stations_info)))
	else return(aux)
}
