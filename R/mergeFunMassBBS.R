#' @name mergeFunMassBBS
#' @title Merge functional group and mass data for species with BBS counts
#' @param bbsData The input bbsData.
#' @param funMass The funMass list (from `funcMass()`).
#' @param printMissing Logical. Prints to screen the missing species.
#' @export funcMass

mergeFunMassBBS <- function(bbsData, funMass, printMissing = T){

    # Which species are in our analysis
    b = bbsData %>% distinct(aou)
    message("...there are ", max(length(b), nrow(b)), " unique species in the BBS data...")

    # unique species to funMass datasets
    f = funMass$fxn %>% distinct(aou, commonName, scientificName)
    m = funMass$mass %>% distinct(common, scientificName)

    # A. Print missing species
        # which are in BBS but are NOT in funMass$fxn
        {if (printMissing == T & !is.null(funMass$fxn)) {
           missF =  b %>%
                filter(!aou %in% f)

           print("the following species are NOT in funMass$fxn: ")
           (missF = GetSpNames() %>% dplyr::select(aou, commonName, scientificName) %>% filter(aou %in% missF$aou))

                }

        # which are in BBS but are NOT in funMass$mass
        if (printMissing == T & !is.null(funMass$mass)) {
            missM =  b %>%
                    filter(!aou %in% m)

            print("the following species are NOT in funMass$mass: ")
            (missM = GetSpNames() %>% dplyr::select(aou, commonName, scientificName) %>% right_join(missM)
                  )

        }}

    # B. Merge all the data
       dataOut <- left_join(bbsData, funMass$fxn) %>% left_join(funMass$mass)

       return(dataOut)
}
