# importResults #########################################################
#' @title Load the regime detection metric results (.feathers)
#' @param resultsDir Where the results are stored.
#' @param myPattern Pattern for loading results files. Name of the subdirectory  ("distances", "ews").
#' @details Used after running calculate_distanceTravelled()? to make results available
#'   for visualization?
#' @param subset.by One or more  patterns by which to filter file names for import the data. Can be used to import South-North transects only (subset.by = "South-North"), or to import all within a single year (subset.by == "year1987"). Default = NULL will import all files in the directory. Multiple example = c("1979","South-North")
#' @param metrics.keep If specified will keep only the metrics specified.
#' @return A dataframe
#' @export importResults

importResults <-
    function(resultsDir,
             myPattern,
             subset.by = NULL,
             metrics.keep = NULL) {
        results <- NULL

        files = list.files(paste0(resultsDir, "/", myPattern))

        if (!is.null(subset.by)) {
            for (i in 1:length(subset.by))
                files <-
                    files[str_detect(files, subset.by[i])]
        }

        print(paste0(
            "I am importing " ,
            length(files) ,
            " files. Does this sound right?!"
        ))
        feather = list()
        for (i in 1:length(files)) {
            feather[[i]] <-
                read_feather(path = paste0(resultsDir, "/", myPattern, "/", files[i]))

            # Subset the data by metric type if specified.
            if (!is.null(metrics.keep)) {
                if ("FI" %in%  metrics.keep) { # make sure all the FIs are identified
                    metrics.keep <- c(metrics.keep, "FI", "FI_Eqn7.12") %>% unique()
                }


                feather[[i]] <- feather[[i]] %>%
                    filter(metricType %in% metrics.keep)
            }
        } # end import loop

        # Bind rows of list into a single df to export from function
        results <- bind_rows(feather) %>%
            mutate(analysis = as.factor(myPattern)) %>%
            mutate(
                metricType = as.factor(metricType),
                direction = as.factor(direction),
                year = as.factor(year)
            ) %>%
            distinct(dirID, year, metricType, metricValue, analysis, .keep_all = TRUE)


        return(results)
    }
