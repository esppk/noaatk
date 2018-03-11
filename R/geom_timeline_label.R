#'Geom Object:Timeline with labels
#'@param x A date data that used to draw the timeline
#'@param loc location of the earthquake, used in the labels
#'@param n_max show how many largest earthquake you want show
#'@param y denote each country
#'@param size same as in ggplot2, used to show magnitude of earth quake
#'@return This function use along ggplot2 to draw timeline geom
#'@export
GeomTimelinelabel <- ggplot2::ggproto("GeomTimelinelabel", ggplot2::Geom,
                                 required_aes = c("x", "loc"),
                                 default_aes = ggplot2::aes(n_max = 3,y = 0.2, fill = "gray", size = 1, alpha = 0.5),
                                 draw_key = function(data, params, size){
                                   alpha <- (data$alpha - min(data$alpha, na.rm = T))/(max(data$alpha, na.rm =  T) - min(data$alpha, na.rm = T))
                                   alpha[is.na(alpha)] <- 0.5
                                   grid::circleGrob(0.5,0.5,r = data$size/20,
                                              gp = grid::gpar(
                                                fill = "gray"
                                              ))
                                 },
                                 draw_panel = function(data, panel_scales, coord) {

                                   #data <- data %>% dplyr::filter((x >= xmin) & (x <= xmax))
                                   coords <- coord$transform(data, panel_scales)


                                   xmin <- min(coords$x)-0.1
                                   xmax <- max(coords$x)+0.1
                                   #standardize the alpha
                                   alpha <- (coords$alpha - min(coords$alpha, na.rm = T))/(max(coords$alpha, na.rm =  T) - min(coords$alpha, na.rm = T))
                                   alpha[is.na(alpha)] <- 0.5

                                   #construct a circle grob
                                   c <- grid::circleGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     r = coords$size/200,
                                     gp = grid::gpar(alpha = alpha, fontsize = 4,
                                                     fill = coords$fill)
                                   )
                                   s <- grid::segmentsGrob(x0 = xmin, y0 = coords$y,
                                                           x1 = xmax, y1 = coords$y,
                                                           gp = grid::gpar(lwd = 2, col = "#c8cacf"))

                                   thresh <- sort(coords$size, decreasing = T)[coords$n_max[1]]
                                   #cat(thresh)
                                   # cat(str(data))
                                   coords <- coords %>% filter(!!quo(size >=thresh))
                                  #cat(str(coords))
                                   t <- grid::segmentsGrob(x0 = coords$x, y0 = coords$y,
                                                           x1 = coords$x, y = coords$y+0.1,
                                                           gp = grid::gpar(lwd = 2, col = "#c8cacf"))
                                   te <- grid::textGrob(coords$loc, x = coords$x, y = coords$y+0.11,
                                                        just = c("bottom"), rot = 15)

                                   grid::gList(s,t,c, te)
                                 }

)

#'
#'plot Timeline with labels
#'
#'@examples
#'\dontrun{
#'ggplot(plot_dat, aes(x = DATE,y = COUNTRY)) + geom_timelinelabel(aes(fill = DEATHS, size = EQ_MAG_MS, loc = LOCATION_NAME_))
#'}
#'@export
geom_timelinelabel <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelinelabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

