#' @param myData A data frame including the column "aou".
#' @title Subset the BBS data by species, functional traits, and/or body mass.
#' @param subset.by One or more of 'remove.fowl' (removes waterfowl), "remove.shorebirds" (removes shorebirds and waders),  'remove.shoreWaderFowl' (removes shorebirds, waders, and fowl).
#' @param mass Logical. Retrieves body mass information (Dunning reference).
#' @param aou.codes Numeric or vector of numeric values of the AOU codes. These are the species you want to REMOVE from analysis.
#' @export subsetByAOU

subsetByAOU <- function(myData, subset.by = c( 'remove.fowl', "remove.shorebirds",  'remove.shoreWaderFowl'), 
                       aou = NULL
                       ){
    # subset.by =c("keep.diurnal", 'remove.fowl', "remove.shorebirds", "  'remove.shoreWaderFowl')

## The AOU species codes can be found at [the BBS FTP site]("ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt").

 #    if("keep.diurnal" %in% subset.by){
 #        myData <- myData %>%
 # ## HERE-- need to add species codes for owls. nightjars, etc.
 #    }

    if("remove.fowl" %in% subset.by){
        myData <- myData %>%
            filter(!aou %in% c(1290:1780))
    }

    if( "remove.shorebirds" %in% subset.by){
        myData <- myData %>%
            filter(!aou %in% c(1290:1780))
    }

    if("remove.shoreWaderFowl" %in% subset.by){
        myData <- myData %>%
            filter(!aou %in% c(0000:2880))
    }

    
    ## Subset by AOU specific numbers. 
     if(!is.null(aou.codes)){
        myData <- myData %>%
            filter(!aou %in% aou.codes)
    }

    return(myData)


}
