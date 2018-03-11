#'clean the RAW data, output clean dataframe
#'@param data Raw data need to be cleaned
#'@value cleaned data
#'@examples
#'\dontrun{
#'cleaned <- eq_clean_data(raw_data)
#'}
#'@importFrom dplyr %>%
#'@export

eq_clean_data <- function(data){

  #Combine year, month, and day into date
  dat <- data %>% tidyr::unite(DATE_, YEAR, MONTH, DAY, sep = "-", remove = F) %>%
    dplyr::mutate(DATE = dplyr::case_when(
      YEAR <= -1000 | YEAR >= 1000 ~ lubridate::ymd(DATE_, truncated = 2, quiet = TRUE),
      YEAR > -1000 & YEAR < 0 ~ as.Date(ISOdate(year = -YEAR, month = dplyr::if_else(is.na(MONTH), 1, as.numeric(MONTH)), day = dplyr::if_else(is.na(DAY), 1,as.numeric(DAY)))),
      YEAR < 1000 & YEAR > 0 ~ as.Date(ISOdate(year = YEAR, month = dplyr::if_else(is.na(MONTH), 1, as.numeric(MONTH)), day = dplyr::if_else(is.na(DAY), 1,as.numeric(DAY)))))) %>%
    dplyr::mutate(DATE = dplyr::if_else(YEAR<0, DATE - 2*difftime(DATE, lubridate::ymd("0000-1-1")), DATE)) %>%
    dplyr::select(-DATE_)

  # convert lat and lon to numeric
  dat <- dat %>% dplyr::mutate(LATITUDE = as.numeric(LATITUDE)) %>%
    dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE))

  # strip and convert location_name
  dat <- dat %>% tidyr::separate(LOCATION_NAME, c("COUNTRY_", "loc"), sep = ":", extra = "drop", fill = "left", remove = FALSE) %>%
    dplyr::mutate(LOCATION_NAME_ = stringr::str_to_title(loc)) %>% dplyr::select(-COUNTRY_, -loc)

  dat
}
