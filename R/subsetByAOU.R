#' @param myData A data frame including the column "aou".
#' @param subset.by One or more of  c("keep.diurnal", 'remove.fowl', "remove.shorebirds", "keep.passerines", 'remove.shoreWaderFowl'). This function will
#' @param mass Logical. Retrieves body mass information (Dunning reference).
#' @export subsetByAOU


subsetByAOU <- function(myData, subset.by = c("keep.diurnal", 'remove.fowl', "remove.shorebirds", "keep.passerines", 'remove.shoreWaderFowl')){
    # subset.by = c("diurnal", "no ducks/geese", "no shore/wader/fowl", "passerines")

    gc()
    if(subset.by == "keep.diurnal"){
        myData <- myData %>%
    }

    if(subset.by == "remove.fowl"){
        myData <- myData %>%
            filter(!aou %in% c(1290:1780))
    }

    if(subset.by == "remove.shorebirds"){
        myData <- myData %>%
            filter(!aou %in% c(1290:1780))
    }

    if(subset.by == "remove.shoreWaderFowl"){
        myData <- myData %>%
            filter(!aou %in% c(0000:2880))
    }

    if(subset.by == "keep.passerines"){
        myData <- myData %>%
            filter(order %in% c("Passeriformes"))
    }

    return(myData)


}
