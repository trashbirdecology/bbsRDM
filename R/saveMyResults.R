#' @title Save results from distance travelled
#' @description Writes the results of distance travelled to file as .feather.
#' @param results A data frame or list element with columns 'time', 'metricType', and 'metricValue' for either the EWS or Distance results.
#' @param resultsDir Where to save the feather.
#' @param metricInd One of 'distances' or 'ews'. Used in outfile name.
#' @export


saveMyResults <- function(results,
                          resultsDir,
                          metricInd
                          ) {


    fileNameInd = paste0(Sys.Date(),"_year", unique(results$year),  "_", direction,  "_transect", unique(results$dirID))

    ## WRITE TO FILE ##
    # Save results as .feather
    write_feather(
        x = results,
        path = paste0(
            resultsDir,
            "/",
            metricInd,
            "/",
            fileNameInd,
            ".feather")
    )
}
