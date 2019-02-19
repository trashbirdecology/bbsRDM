
#' @export sort.year.line
#' @title Plot a single transect over multiple years, with one metricTYpe.
#' @export
# Single line plot of one transect, multiple years, one metric ------------
sort.year.line <-
    function(df,
             metric.ind,
             year.ind,
             dirID.ind,
             direction,
             scale = T,
             center = T) {
        sortVar.lab <-
            ifelse( direction == "South-North",
                   "latitude",
                   "longitude")
        data <-
            df@data %>%
            filter(metricType %in% metric.ind,
                   year %in% year.ind,
                   dirID %in% dirID.ind ,
                   direction == direction)

        if (scale == T | center == T) {
            data <-
                data %>% group_by(metricType, dirID, year) %>%
                mutate(metricValue = base::scale(metricValue, center = center, scale = scale)) %>%
                ungroup()
            if (scale == T & center == T) {
                print("Data were z-scored.")
            }
            if (scale == T & center == F)
                print("Data were mean-centered.")
            if (scale == F & center == T)
                print("Data were 0-1 scaled.")

        }

        p =
            ggplot(data) +
            geom_line(aes(
                x = sortVar,
                y = metricValue,
                group = year,
                color = year
            )) +
            ggtitle(paste0(unique(data$direction), " # ", unique(data$dirID))) +
            xlab(sortVar.lab) +
            ylab(metric.ind) +
            myTheme()

        if (length(metric.ind) > 1) {
            if (length(metric.ind) < 4) {
                p.facet = p +
                    facet_wrap(~ metricType, scales = "free_y", ncol = 1) +
                    ylab("value")
                return(p.facet)
            }

            p.facet = p +
                facet_wrap(~ metricType, scales = "free_y")


            return(p.facet)
        }

        return(p)

    }



