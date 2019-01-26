# importResults #########################################################
#' @title Load the regime detection metric results (.feathers)
#' @param resultsDir Where the results are stored.
#' @param myPattern Pattern for loading results files. Name of the subdirectory  ("distances", "ews").
#' @details Used after running calculate_distanceTravelled()? to make results available
#'   for visualization?
#' @return A dataframe
#' @export

importResults <- function(resultsDir, myPattern, subset.by = NULL) {
    results <- NULL
    files = list.files(paste0(resultsDir,myPattern))

            if(!is.null(subset.by)){
                files <- files[str_detect(files, subset.by)]
            }

    print(paste0("I am importing " , length(files) , " files. Does this sound right?!"))

    for(i in 1:length(files) ){

        feather = NULL

        feather <-read_feather(path = paste0(resultsDir, myPattern,"/", files[i]))

        results = rbind(feather, results)

            }


    results <- results %>% mutate(analysis = as.factor(myPattern)) %>%
        mutate(metricType = as.factor(metricType),
               direction = as.factor(direction),
               year = as.factor(year)
        )

    return(results)
}
