

# a. Create an empty list for storing the results
resultsList <- list()

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

    resultsList$distance <- results

    rm(dataInDist)

    saveMyResults(resultsList$distance ,resultsDir =resultsDir, analySpatTemp =analySpatTemp, metricInd = metricInd)

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

    resultsList$ews <- results

    rm(dataInRDM)

    saveMyResults(resultsList$ews , resultsDir =resultsDir, analySpatTemp =analySpatTemp, metricInd = metricInd)

    }else(print(paste0("# data points < min.samp.sites... skipping loop ", i)))

} # END EWS calcs

