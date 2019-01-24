#' @title Save results from distance travelled
#' @description Writes the results of distance travelled to file as .feather.
#' @param results A data frame or list element with columns 'time', 'metricType', and 'metricValue' for either the EWS or Distance results.
#' @param resultsDir Where to save the feather.
#' @param analySpatTemp One of 'temporal', 'East'West', or 'South-North' to specify the type of analysis. Will be used in filename.
#' @param metricInd One of 'distances' or 'ews'. Used in outfile name.

saveMyResults <- function(results,
                          resultsDir,
                          analySpatTemp,
                          metricInd,
                          timeVar,
                          rowInd,
                          yearInd,
                          colInd
                          ) {
    ## FOR SPATIAL ANALYSES ##
    # Fix the time column names and add columns
    if (timeVar == "long") {
        results <- results %>%
            mutate(rowID = rowInd,
                   year  = yearInd)
        fileNameInd = paste0("_row", rowInd, "_year", yearInd)
    }

    if (timeVar == "lat") {
        results <- results %>%
            mutate(colID = colInd,
                   year  = yearInd)
        fileNameInd = paste0("_col", colInd, "_year", yearInd)
    }

    if (metricInd == "distances") {
        results <- results %>%
            dplyr::rename(long = time)
        resultsDir <- paste0(resultsDir, "distances" )
    }else(resultsDir <- paste0(resultsDir, "ews" ))


    ## WRITE TO FILE ##
    # Save results as .feather
    write_feather(
        x = results,
        path = paste0(
            resultsDir,
            metricInd,
            "_" ,
            analySpatTemp,
            fileNameInd,
            ".feather"
        )
    )
}
