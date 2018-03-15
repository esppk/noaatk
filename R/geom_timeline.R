#'Geom obj: Timeline
#'
#'@return This function use along ggplot2 to draw timeline geom
#'@export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(y = 0.2, fill = "gray", size = 1, alpha = 0.5),
                                 draw_key = function(data, params, size){
                                   grid::circleGrob(0.5,0.5,r = data$size/20,
                                                    gp = grid::gpar(
                                                      fill = "gray"
                                                    ))
                                 },
                                 draw_panel = function(data, panel_scales, coord) {

                                   #data <- data %>% dplyr::filter((x >= xmin) & (x <= xmax))
                                   coords <- coord$transform(data, panel_scales)
                                   cat(str(coords$size))

                                   r = coords$inten
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
                                     gp = grid::gpar(alpha = alpha,
                                                     fill = coords$fill)
                                   )
                                   s <- grid::segmentsGrob(x0 = xmin, y0 = coords$y,
                                                           x1 = xmax, y1 = coords$y,
                                                           gp = grid::gpar(lwd = 2, col = "#c8cacf"))

                                   grid::gList(s,c)
                                 }
)

#'plot Timeline without labels
#'
#'@details
#' x A date data that used to draw the timeline
#'   y denote each country
#'   size same as in ggplot2, used to show magnitude of earth quake
#'@param mapping mapping
#'@param data data used to plot
#'@param stat calculation that used to transform the data
#'@param position relative position of the geoms
#'@param na.rm NA remove
#'@param show.legend whether show legend
#'@param inherit.aes aes that inherit
#'@param ... other params to pass in
#'@examples
#'\dontrun{
#'ggplot(plot_dat, aes(x = DATE,y = COUNTRY))+ geom_timeline(aes(fill = DEATHS, size = EQ_MAG_MS))
#'}
#'@export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


