

# Single line plot of one transect, multiple years, one metric ------------
sort.year.line <- function(plotResults, metric.ind, year.ind) {
    sortVar.lab <-
        ifelse(unique(plotResults@data$direction) == "South-North",
               "latitude",
               "longitude")

    p =
        ggplot(plotResults@data %>%
                   filter(metricType == metric.ind,
                          year %in% year.ind)) +
        geom_line(aes(
            x = sortVar,
            y = metricValue,
            group = year,
            color = year
        )) +
        ggtitle(paste0(unique(direction), " # ", unique(dirID))) +
        xlab(sortVar.lab) +
        ylab(metric.ind) +
        myTheme()

    if (length(metric.ind) > 1) {
        if (length(metric.ind) < 4) {
            p.facet = p +
                facet_wrap( ~ metricType, scales = "free_y", ncol = 1)
            return(p.facet)
        }

        p.facet = p +
            facet_wrap( ~ metricType, scales = "free_y")



        return(p.facet)
    } else
        (return(p))

}
