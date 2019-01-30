
## Trying to build plotting function or script to run in the example script
library(ggplot2)
library(viridis)


test_data <- as.data.frame(plotResults) %>%
    filter(
        # year %in% c(1980, 1990, 2000, 2010),
           metricType == "s") %>%
    group_by(dirID, year, analysis) %>%
    mutate(scaledMetricValue.bycol = base::scale(metricValue, center =T, scale = T)) %>%
    ungroup() %>%
    na.omit(metricValue)

ggplot(test_data) + geom_raster(aes(x = long, y = lat, fill = scaledMetricValue.bycol)) +
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = -1) +
    facet_wrap(~year, strip.position = "top", nrow=3) +
    theme(legend.position = "none")


## plot the change in values over time
test_data2<-test_data %>%
    na.omit(metricValue) %>%
    group_by(metricType, cellID) %>%
    arrange(year) %>%
    mutate(dScaledMetricValue.bycol = scaledMetricValue.bycol - lag(scaledMetricValue.bycol))


ggplot() + geom_raster(data = test_data2, aes(x = long, y = lat, fill = dScaledMetricValue.bycol)) +
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = -1) +
    facet_wrap(~year, ncol = 3, strip.position = "top") +
    theme(legend.position = "none")




