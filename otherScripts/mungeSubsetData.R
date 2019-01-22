# A. For temporal analyses:
if(analySpatTemp == "temporal"){
    dataIn <- birdData %>%
        dplyr::rename(time = year) %>%
        dplyr::select(-long, -lat)
    timeVar = 'year'
    stateInd = unique(birdData$statenum)
    routeInd = unique(birdData$route)
    if((length(stateInd) + length(routeInd))!=2){stop("dataIn is incorrect: check filtering.")}
}

# B. For spatial analyses:
if(analySpatTemp == "East-West"){
    dataIn <- birdData %>%
        dplyr::rename(time = long) %>%
        dplyr::select(-year, -lat)
    timeVar = 'long'
    rowInd = unique(birdData$rowID)
    yearInd = unique(birdData$year)
    if(length(rowInd)!=1){stop("dataIn is incorrect: check filtering.")}
}
if(analySpatTemp == "South-North"){
    dataIn <- birdData %>%
        dplyr::rename(time = lat) %>%
        dplyr::select(-year, -long)
    timeVar = 'lat'
    colInd = unique(birdData$colID)
    yearInd = unique(birdData$year)
    if(length(colInd)!=1){stop("dataIn is incorrect: check filtering.")}
}

