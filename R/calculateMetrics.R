#' @title Calculate regime detection metrics
#' @description Calculates regime detection metrics across space or time. Calculates distance travelled, Fisher Information, Variance Index, Coefficient of Variation, mean, standard deviation, variance,skewness, and kurtosis. #' @param dataIn A data frame containing columns c(variable, time, value).
#' @param metrics.to.calc One or more of c("distances", "ews")
#' @param min.samp.sites Minimum number of unique sites in the transect (or unique times along the time series) required to analyze the data. Most metrics can be calculated using three data points, although we do not nrecommend this.
#' @export calculateMetrics
#' @example

calculateMetrics <-
    function(dataIn = dataIn,
             metrics.to.calc = c("distances", "ews"),
             min.samp.sites = 8) {
        ## Munge the input data a little --this is a failsafe!
        # keep only the relevant columns
        dataIn  <- dataIn %>%
            dplyr::select(time, variable, value) %>%
            # add up any species having multiple observations -- this happens esp. for hybrids! is common.
            group_by(variable, time) %>%
            summarise(value = sum(value)) %>%
            ungroup()


        if(length(unique(dataIn$time)) < min.samp.sites){
             flag = "# dataIn$time points < min.samp.sites. Not calculating metrics. "
            return(flag)
}

        ## Calculate distance traveled
        if ("distances" %in% metrics.to.calc) {
            metricInd = "distances"
            # keep only the relevant columns
            dataIn  <- dataIn %>%
                dplyr::select(time, variable, value)
            if (!length(unique(dataIn$time)) < min.samp.sites) {
                # Calc distances
                results <- NULL
                results <- calculate_distanceTravelled(dataIn, derivs = T) %>%
                    gather(key = 'metricType', value = 'metricValue',-time)

                # Save the results, if exist
                if(!exists("results") | !is.null(results)){
                saveMyResults(
                    results ,
                    resultsDir = resultsDir,
                    analySpatTemp = analySpatTemp,
                    metricInd = metricInd
                )}

            }
        }


        ## Calculate early-warning signals
        if ("ews" %in% metrics.to.calc) {
            metricInd = "ews"

            # keep only the relevant columns
            dataIn  <- dataIn %>%
                dplyr::select(time, variable, value) %>%
                # add up any species having multiple observations -- this happens esp. for hybrids! is common.
                group_by(variable, time) %>%
                summarise(value = sum(value)) %>%
                ungroup()

            results <- NULL
            results <-
                rdm_window_analysisTEMP(
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
                    to.calc = to.calc
                )

            # Save the results, if exist
            if(!exists("results") | !is.null(results)){
            saveMyResults(
                results ,
                resultsDir = resultsDir,
                analySpatTemp = analySpatTemp,
                metricInd = metricInd
            )}

        } # leave EWS calculations


    } # leave function
