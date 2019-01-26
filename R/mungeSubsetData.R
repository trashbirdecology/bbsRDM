mungeSubsetData <- function(birdData){

if(direction == "East-West"){
    birdData <- birdData %>%
        dplyr::rename(sortVar = long)


}
if(direction == "South-North"){
    birdData <- birdData %>%
        dplyr::rename(sortVar = lat)
}


birdData <- birdData %>%
   # sum over species/sites to account for hybrid and UNID races
    dplyr::group_by(sortVar, cellID, direction, dirID, variable,year) %>%
    summarise(value = sum(value)) %>%
    dplyr::group_by(variable) %>%
    # need to arrange by time to make sure the distances are calculated correctly!
    arrange(variable, sortVar) %>%
    ungroup()

}
