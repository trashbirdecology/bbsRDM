
## Trying to build plotting function or script to run in the example script
library(ggplot2)
library(viridis)

test_data <- full_join(
    coords_grd,
    results_dist %>% dplyr::select(metricType, metricValue, cellID, year, rowID)
) %>%  na.omit(metricType) %>%
    filter(year %in% c(1980, 1990, 2000, 2010),
           metricType == "s") %>%
    group_by(rowID, year) %>%
    mutate(scaledMetricValue.byrow = base::scale(metricValue, center =T)) %>%
    ungroup() %>%
    na.omit(metricValue)

ggplot() + geom_raster(data = test_data, aes(x = long, y = lat, fill = scaledMetricValue.byrow)) +
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = -1) +
    facet_wrap(~year, ncol = 2, strip.position = "top") +
    theme(legend.position = "top")


## plot the change in values over time
test_data2<-test_data %>%
    group_by(metricType, cellID) %>%
    arrange(year) %>%
    mutate(dScaledMetricValue.byrow = scaledMetricValue.byrow - lag(scaledMetricValue.byrow)) %>%
    na.omit(metricValue)


ggplot() + geom_raster(data = test_data2, aes(x = long, y = lat, fill = dScaledMetricValue.byrow)) +
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = -1) +
    facet_wrap(~year, ncol = 2, strip.position = "right") +
    theme(legend.position = "none")

