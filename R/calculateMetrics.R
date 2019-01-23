#' @title Calculate chosen regime detection metrics
#' @description
#' @param dataIn A data frame containing columns c(variable, time, value).
#'
#' @param metrics.to.calc One or more of c("distances", "ews")
#' @export calculateMetrics

calculateMetrics <- function(dataIn = dataIn, metrics.to.calc = c("distances", "ews", min.samp.sites = 8)
){

     # keep only the relevant columns
     dataIn  <- dataIn %>%
     dplyr::select(
         time, variable, value) %>%
     # add up any species having multiple observations -- this happens esp. for hybrids! is common.
     group_by(variable, time) %>%
     summarise(value = sum(value)) %>%
     ungroup()


flag <- ifelse(length(unique(dataIn$time)) < min.samp.sites, 'stop', 'go')


if("distances" %in% metrics.to.calc & flag == "go") {
    metricInd = "distances"
    # keep only the relevant columns
    dataIn  <- dataIn %>%
        dplyr::select(
            time, variable, value)
    if(!length(unique(dataIn$time)) < min.samp.sites){

    # Calc distances
    results <- NULL
    results <- calculate_distanceTravelled(dataIn, derivs = T) %>%
        gather(key = 'metricType', value ='metricValue', -time)


    saveMyResults(results ,resultsDir =resultsDir, analySpatTemp =analySpatTemp, metricInd = metricInd)

    }
}

# f.ii.a Calculate the EWSs
if ("ews" %in% metrics.to.calc & flag == "go") {
    metricInd = "ews"

    # keep only the relevant columns
    dataIn  <- dataIn %>%
        dplyr::select(
            time, variable, value) %>%
        # add up any species having multiple observations -- this happens esp. for hybrids! is common.
        group_by(variable, time) %>%
        summarise(value = sum(value)) %>%
        ungroup()

    results <- NULL
    results <-
        rdm_window_analysis(
            dataInRDM = dataIn %>%
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

    } # leave EWS calculations

} # leave function

