#'clean the RAW data, output clean dataframe
#'@param data Raw data need to be cleaned
#'@value cleaned data
#'@examples
#'\dontrun{
#'cleaned <- eq_clean_data(raw_data)
#'}
#'@importFrom dplyr %>%
#'@importFrom rlang .data
#'@export

eq_clean_data <- function(data){

  #Combine year, month, and day into date
  dat <- data %>% tidyr::unite("DATE_", .data$YEAR, .data$MONTH, .data$DAY, sep = "-", remove = F) %>%
    dplyr::mutate(DATE = dplyr::case_when(
      .data$YEAR <= -1000 | .data$YEAR >= 1000 ~ lubridate::ymd(.data$DATE_, truncated = 2, quiet = TRUE),
      .data$YEAR > -1000 & .data$YEAR < 0 ~ as.Date(ISOdate(year = -.data$YEAR,
                                                            month = dplyr::if_else(is.na(.data$MONTH), 1, as.numeric(.data$MONTH)),
                                                            day = dplyr::if_else(is.na(.data$DAY), 1,as.numeric(.data$DAY)))),
      .data$YEAR < 1000 & .data$YEAR > 0 ~ as.Date(ISOdate(year = .data$YEAR,
                                                           month = dplyr::if_else(is.na(.data$MONTH), 1, as.numeric(.data$MONTH) ),
                                                           day = dplyr::if_else(is.na(.data$DAY), 1,as.numeric(.data$DAY) ) )) ) ) %>%
    dplyr::mutate(DATE = dplyr::if_else(.data$YEAR<0, .data$DATE - 2*difftime(.data$DATE, lubridate::ymd("0000-1-1")), .data$DATE)) %>%
    dplyr::select(-.data$DATE_)

  # convert lat and lon to numeric
  dat <- dat %>% dplyr::mutate(LATITUDE = as.numeric(.data$LATITUDE)) %>%
    dplyr::mutate(LONGITUDE = as.numeric(.data$LONGITUDE))

  # strip and convert location_name
  dat <- dat %>% tidyr::separate(.data$LOCATION_NAME, c("COUNTRY_", "loc"), sep = ":", extra = "drop", fill = "left", remove = FALSE) %>%
    dplyr::mutate(LOCATION_NAME_ = stringr::str_to_title(.data$loc)) %>% dplyr::select(-.data$COUNTRY_, -.data$loc)

  dat
}

#'
#'@export

dplyr::`%>%`


#'
#'earthquake dataset use for vignette.
#'
#'@format A dataframe with 49 variables and 5998 obs.
#'@source \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
"noaa"
