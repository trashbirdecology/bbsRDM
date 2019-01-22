
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
            time, variable, value)

    if(length(unique(dataInRDM$time)) < min.samp.sites){stop("data has less observations than min.samp.sites")}

    results <- NULL
    results <-
        rdm_window_analysis(
            dataIn = dataInRDM,
            winMove = winMove,
            overrideSiteErr = F,
            fi.equation = fi.equation,
            min.window.dat = min.window.dat,
            fill = fill,
            to.calc = to.calc)

    resultsList$ews <- results

    rm(dataInRDM)

    saveMyResults(resultsList$ews ,resultsDir =resultsDir, analySpatTemp =analySpatTemp, metricInd = metricInd)

} # END EWS calcs

