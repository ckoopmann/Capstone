#' Clean Earthquake data
#'
#' Clean up the Earthquake Dataframe
#'
#' @param df Raw data frame with columns YEAR, MONTH, DAY, LATITUDE and LONGITUDE
#' 
#' @details  This function cleans up the data frame in the sense that it creates a new DATE Column and converting LATITUDE and LONGITUDE columns to numeric
#'
#' @return Data frame with additional column DATE
#' 
#' @examples eq_clean_data(df)
#' 
#' @importFrom lubridate as_date
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate date
#' 
#' @export
eq_clean_data <- function(df){
      df$DATE = lubridate::as_date("01-01-1970",format = "%d-%m-%Y")
      lubridate::year(df$DATE) = df$YEAR
      lubridate::month(df$DATE) = df$MONTH
      lubridate::day(df$DATE) = df$DAY
      df$LATITUDE = as.numeric(df$LATITUDE)
      df$LONGITUDE = as.numeric(df$LONGITUDE)
      return(df)
}

#' Clean Earthquake Location
#'
#' Clean up the Earthquake Location column
#'
#' @param df Raw data frame with column LOCATION_NAME
#' 
#' @details This function removes the country name from the location by splitting the location column at the first colon
#' 
#' @return Data frame with transformed LOCATION_NAME column
#' 
#' @examples eq_clean_location(df)
#' 
#' @importFrom stringr str_split_fixed
#' 
#' @export
eq_location_clean <- function(df){
      df$LOCATION_NAME <- stringr::str_split_fixed(df$LOCATION_NAME, ": ", n = 2)[,2]
      return(df)
}
