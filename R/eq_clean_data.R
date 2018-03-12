#'clean the RAW data, output clean dataframe
#'@param data Raw data need to be cleaned
#'@value cleaned data
#'@examples
#'\dontrun{
#'cleaned <- eq_clean_data(raw_data)
#'}
#'@importFrom dplyr %>%
#'@importFrom dplyr quo
#'@export

eq_clean_data <- function(data){

  #Combine year, month, and day into date
  dat <- data %>% tidyr::unite(!!quo(DATE_), !!quo(YEAR), !!quo(MONTH), !!quo(DAY), sep = "-", remove = F) %>%
    dplyr::mutate(DATE = dplyr::case_when(
      !!quo(YEAR <= -1000 | YEAR >= 1000 ~ lubridate::ymd(!!quo(DATE_), truncated = 2, quiet = TRUE)),
      !!quo(YEAR > -1000 & YEAR < 0 ~ as.Date(ISOdate(year = !!quo(-YEAR), month = dplyr::if_else(is.na(!!quo(MONTH)), 1, as.numeric(!!quo(MONTH))), day = dplyr::if_else(is.na(!!quo(DAY)), 1,as.numeric(!!quo(DAY)))))),
      !!quo(YEAR < 1000 & YEAR > 0 ~ as.Date(ISOdate(year = !!quo(YEAR), month = dplyr::if_else(is.na(!!quo(MONTH)), 1, as.numeric(!!quo(MONTH))), day = dplyr::if_else(is.na(!!quo(DAY)), 1,as.numeric(!!quo(DAY)))))))) %>%
    dplyr::mutate(DATE = dplyr::if_else(!!quo(YEAR<0), !!quo(DATE - 2*difftime(!!quo(DATE), lubridate::ymd("0000-1-1"))), !!quo(DATE))) %>%
    dplyr::select(!!quo(-DATE_))

  # convert lat and lon to numeric
  dat <- dat %>% dplyr::mutate(LATITUDE = as.numeric(!!quo(LATITUDE))) %>%
    dplyr::mutate(LONGITUDE = as.numeric(!!quo(LONGITUDE)))

  # strip and convert location_name
  dat <- dat %>% tidyr::separate(!!quo(LOCATION_NAME), c("COUNTRY_", "loc"), sep = ":", extra = "drop", fill = "left", remove = FALSE) %>%
    dplyr::mutate(LOCATION_NAME_ = stringr::str_to_title(!!quo(loc))) %>% dplyr::select(!!quo(-COUNTRY_), !!quo(-loc))

  dat
}
