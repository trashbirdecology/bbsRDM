#' @title Calculate chosen regime detection metrics
#' @description
#' @param dataIn A data frame containing columns c(variable, time, value).
#'
#' @param metrics.to.calc One or more of c("distances", "ews")
#' @export calculateMetrics

calculateMetrics <- function(dataIn = dataIn, metrics.to.calc = c("distances", "ews")
){

if("distances" %in% metrics.to.calc) {
    metricInd = "distances"
    # keep only the relevant columns
    dataInDist  <- dataIn %>%
        dplyr::select(
            time, variable, value)

    # Calc distances
    results <- NULL
    results <- calculate_distanceTravelled(dataInDist, derivs = T) %>%
        gather(key = 'metricType', value ='metricValue', -time)

    saveMyResults(results ,resultsDir =resultsDir, analySpatTemp =analySpatTemp, metricInd = metricInd)

    rm(dataInDist)

}

# f.ii.a Calculate the EWSs
if ("ews" %in% metrics.to.calc) {
    metricInd = "ews"

    # keep only the relevant columns
    dataInRDM  <- dataIn %>%
        dplyr::select(
            time, variable, value) %>%
        # add up any species having multiple observations -- this happens esp. for hybrids! is common.
        group_by(variable, time) %>%
        summarise(value = sum(value)) %>%
        ungroup()

    if(!length(unique(dataInRDM$time)) < min.samp.sites){

    results <- NULL
    results <-
        rdm_window_analysis(
            dataIn = dataInRDM %>%
                # arrange the data in temporal (spatial) order
                dplyr::group_by(variable) %>%
                arrange(variable, time) %>%
                ungroup(),
            winMove = winMove,
            overrideSiteErr = F,
            fi.equation = fi.equation,
            min.window.dat = min.window.dat,
            fill = fill,
            to.calc = to.calc)

    saveMyResults(results , resultsDir =resultsDir, analySpatTemp =analySpatTemp, metricInd = metricInd)


    rm(dataInRDM)


    }else(print(paste0("# data points < min.samp.sites... skipping loop ", i)))

} # END EWS calcs

}
