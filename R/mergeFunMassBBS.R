#' @name mergeFunMassBBS
#' @title Merge functional group and mass data for species with BBS counts
#' @param bbsData The input bbsData.
#' @param funMass The funMass list (from `funcMass()`).
#' @param printMissing Logical. Prints to screen the missing species.
#' @export mergeFunMassBBS

mergeFunMassBBS <- function(bbsData, funMass, printMissing = TRUE){

    # Which species are in our analysis
    b = bbsData %>% distinct(aou)
    message("...there are ", max(length(b), nrow(b)), " unique species in the BBS data...")

    # unique species to funMass datasets
    if(!is.null(funMass$fxn))  f <- funMass$fxn %>% distinct(aou, commonName, scientificName)else(fxn = NULL)
    if(!is.null(funMass$mass)) m <- funMass$mass %>% distinct(common, scientificName) else(m = NULL)

    # A. Print missing species
        # which are in BBS but are NOT in funMass$fxn
        {if (printMissing == TRUE & !is.null(funMass$fxn)) {
           missF =  b %>%
                filter(!aou %in% f)

           print("the following species are NOT in funMass$fxn: ")
           (missF = GetSpNames() %>% dplyr::select(aou, commonName, scientificName) %>% filter(aou %in% missF$aou))

                }

        # which are in BBS but are NOT in funMass$mass
        if (printMissing == TRUE & !is.null(funMass$mass)) {
            missM =  b %>%
                    filter(!aou %in% m)

            print("the following species are NOT in funMass$mass: ")
            (missM = GetSpNames() %>% dplyr::select(aou, commonName, scientificName) %>% right_join(missM)
                  )

        }}

        # B. Merge all the data
    if(!is.null(f)) bird.fxn <- left_join(bbsData, f) else(bird.fxn = NULL)

    if(!is.null(m)){
        tmp <-GetSpNames() %>% dplyr::select(aou, scientificName)
        m <- left_join(m, tmp)
        bird.mass <- left_join(bbsData, m)

    } else(bird.mass = NULL)

    if(!is.null(bird.mass) & is.null(bird.fxn)) return(bird.mass)
    if(!is.null(bird.fxn) & is.null(bird.mass)) return(bird.fxn)

    if(!is.null(bird.fxn) & !is.null(bird.mass)) return(full_join(bird.mass, bird.fxn))


    }
