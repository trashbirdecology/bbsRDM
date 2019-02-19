#' @title Calculate regime detection metrics
#' @description Calculates regime detection metrics across space or time. Calculates distance travelled, Fisher Information, Variance Index, Coefficient of Variation, mean, standard deviation, variance,skewness, and kurtosis. #' @param dataIn A data frame containing columns c(variable, time, value).
#' @param metrics.to.calc One or more of c("distances", "ews")
#' @param dataIn data frame with columns: sortVar (the sorting variable; latitude or longitude),  cellID (cell ID for the spatial grid),  variable (species),  value (count data).
#' @param min.samp.sites Minimum number of unique sites in the transect (or unique times along the time series) required to analyze the data. Most metrics can be calculated using three data points, although we do not nrecommend this.
#' @param direction Direction of the analysis (South-North or East-West)
#' @export calculateMetrics
#'
#' @example

calculateMetrics <-
    function(dataIn,
             metrics.to.calc = c("distances", "ews"),
             min.samp.sites = 8,
             direction,
             yearInd,
             to.calc = c("EWS", "FI", "VI")) {
        # Create an id for joining the results with cell ID.
        id <- dataIn %>% dplyr::select(sortVar, cellID, direction, dirID, year) %>% distinct()

        # Abandon calcs if not enough data
        if (length(unique(dataIn$sortVar)) < min.samp.sites) {
            flag = "# dataIn$time points < min.samp.sites. Not calculating metrics. "
            return(flag)
        }

        # Calculate distance traveled
        if ("distances" %in% metrics.to.calc) {
            metricInd = "distances"

            # keep only the relevant columns
            dataInDist  <- dataIn %>%
                dplyr::select(sortVar, cellID, variable, value)
            if (!length(unique(dataIn$sortVar)) < min.samp.sites) {
                # Calc distances
                results <- NULL
                results <-
                    calculate_distanceTravelled(dataInDist, derivs = T) %>%
                    gather(key = 'metricType', value = 'metricValue', -sortVar, -cellID)


                # Save the results, if exist
                if (exists("results") & !is.null(results)) {

                    # Add the identifiers back onto the results
                    results <- left_join(results, id)

                     saveMyResults(
                        results ,
                        resultsDir = resultsDir,
                        metricInd = metricInd
                    )
                rm(results)
                }

            }
        }


        ## Calculate early-warning signals
        if ("ews" %in% metrics.to.calc) {
            metricInd = "ews"

            dataInRDM  <- dataIn %>%
                dplyr::select(sortVar, cellID, variable, value)

                    # create empty df for results
            results <- NULL
            results <-
                rdm_window_analysis(
                    dataInRDM = dataInRDM,
                    winMove = winMove,
                    overrideSiteErr = F,
                    fi.equation = fi.equation,
                    min.window.dat = min.window.dat,
                    fill = fill,
                    to.calc = to.calc
                )



            # Save the results, if exist
            if (exists("results") & !is.null(results)) {

                # Add the identifiers back onto the results
                results <- results %>%
                    mutate(direction =direction,
                           dirID = unique(id$dirID),
                           year = yearInd)

                saveMyResults(
                    results ,
                    resultsDir = resultsDir,
                    metricInd = metricInd
                )

                rm(results)
            }
        } # leave EWS calculations

    } # leave function
