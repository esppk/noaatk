#'This functions use to plot earthquakes on relavent map.
#'
#'@param annot_col columns used to annotate
#'@value This function will generate interactive map with dots shows the earthquake locations.
#'@examples
#'\dontrun{
#'labeled_map_dat <- map_dat %>% mutate(pop_text = eq_create_label(.))
#'labeled_map_dat %>% eq_map(DATE)
#'}
#'@importFrom dplyr enquo
#'@export
eq_map <- function(map_dat, annot_col){

  annot_col <- enquo(annot_col)
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = map_dat, radius = ~ EQ_MAG_MS,
                     lng = ~ LONGITUDE, lat = ~ LATITUDE,
                     popup = annot_col)
}


#'function to generate popup label for eq_map
#'@param dat data used to add generate label
#'@value text that can be used as in popup labels
#'@examples
#'\dontrun{
#'labeled_map_dat <- map_dat %>% mutate(pop_text = eq_create_label(.))
#'}
#'@export
eq_create_label <- function(dat){
  loc <- dat$LOCATION_NAME_
  mag <- dat$EQ_MAG_MS
  dea <- dat$DEATHS
  paste("<b>Location: </b>", loc,"<br>",
        "<b>Magnitude: </b>", mag,"<br>",
        "<b>Total deaths: </b>", dea)
}

