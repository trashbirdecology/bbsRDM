# importResults #########################################################
#' @title Load the regime detection metric results (.feathers)
#' @param resultsDir Where the results are stored.
#' @param myPattern Pattern for loading results files. One of "distance", "ews".
#' @details Used after running calculate_distanceTravelled()? to make results available
#'   for visualization?
#' @return A dataframe
#' @export

importResults <- function(resultsDir, myPattern) {
    results <- NULL
    files = list.files(resultsDir, pattern = myPattern)
    print(paste0("I am importing " , length(files) , " files."))

    for(i in 1:length(files) )

        feather <-read_feather(path = paste0(resultsDir, files[i]))

        results = rbind(feather, results)

    return(results)
}
