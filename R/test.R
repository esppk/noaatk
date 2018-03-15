#
#
# noaa_raw <- readr::read_delim("signif.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
# noaa <- eq_clean_data(noaa_raw)
# plot_dat <- noaa %>% filter(DATE< ymd("2009-1-1") & DATE > ymd("2005-1-1")) %>% filter(COUNTRY %in% c("CHINA", "JAPAN", "USA"))
#
# ggplot(plot_dat, aes(x = DATE,y = 0.3, inten = INTENSITY))+ geom_timeline()
#
# ggplot(plot_dat, aes(x = DATE,y = COUNTRY))+ geom_timelinelabel(aes(fill = DEATHS, size = EQ_MAG_MS, loc = LOCATION_NAME_))
#
# ggplot(plot_dat, aes(x = DATE,y = COUNTRY))+ geom_timeline(aes(alpha = ))
#
# grid.text("SOMETHING NICE AND BIG", gp=gpar(fontsize=5))
#
#
# library(lubridate)
# library(grid)
#
#
# df <- data.frame(
#   trt = factor(c(1, 1, 2, 2)),
#   resp = c(1, 5, 3, 4),
#   group = factor(c(1, 2, 1, 2)),
#   upper = c(1.1, 5.3, 3.3, 4.2),
#   lower = c(0.8, 4.6, 2.4, 3.6)
# )
#
# p <- ggplot(df, aes(trt, resp, colour = group))
# p + geom_linerange(aes(ymin = lower, ymax = upper))
# p + geom_pointrange(aes(ymin = lower, ymax = upper))
# p + geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2)
# p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
# p + geom_point(aes(size = upper))
#
#
#
#
#
#
#































