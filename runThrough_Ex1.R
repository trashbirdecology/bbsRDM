rm(list = ls())
# 0: About this script ------------------------------------------------------
#######################################
##
##
##
###PART I: Setup ##########################################
# 1: Load packages -------------------------------------------------------


## Re-install often as this package is under major development.
devtools::install_github("trashbirdecology/regimedetectionmeasures", force = T)
library(regimeDetectionMeasures)
library(sp)
library(raster)
library(feather)
library(bbsRDM)


## Optional: for benchmarking but recommended due to large file sizes
# devtools::install_github("collectivemedia/tictoc", force = F)
library(tictoc)

# 0: Directories   ---------------------------------------------------

# a. Create a directory to store and/or load the BBS data as feathers
dir.create(file.path(getwd(), paste0("/bbs_raw_data")))
bbsDir <- paste0(getwd(), paste0("/bbs_raw_data/"))
if(length(list.files(bbsDir))!=0){warning("The directory ", resultsDir, " already exists.. /nProceed with caution--files in this directory will likely be overwritten.")}

# b. Create a directory to store and/or load the BBS data as feathers
dir.create(file.path(getwd(), paste0("/myResults")))
resultsDir <- paste0(getwd(), paste0("/myResults/"))
    if(length(list.files(resultsDir))!=0){warning("The directory ", resultsDir, " already exists.. /nProceed with caution--files in this directory will likely be overwritten.")}


# # I: OPTIONAL: Import BBS and save to disk as feathers -----------------------------------------------
# #FYI:  ~10-15 MINUTES TO DOWNLOAD ALL STATE FILES FOR ALL YEARS!

# # o. Load the regional .txt file from Patuxent
# regions <- GetRegions()
# # Create a series or one filenames for states, regions
# regionFileName <- regions$zipFileName %>% na.omit()
# # a.  Download and unzip the BBS data.
# {tic("Import and save bbs data from ftp")
# for(i in 1:length(regionFileName)){
#         bbsData <-  importDataBBS(
#             # arguments for getDataBBS()
#             file = regionFileName[i],
#             dir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/",
#             year = NULL,
#             aou = NULL,
#             countrynum = NULL,
#             states = NULL,
#             #  arguments for getRouteInfo():
#             routesFile = "routes.zip",
#             routesDir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/",
#             RouteTypeID = 1,
#             # one or more of c(1,2,3)
#             Stratum = NULL,
#             BCR = NULL
#         )
#
#
# # b. Save the file to disk
# birdsToFeathers(dataIn  = bbsData,
#                 newDir  = bbsDir,
#                 filename = regionFileName[i])
#
# rm(bbsData)
#
# } # end section I. loop
# toc()}
#

# # II: OPTIONAL: Subset by the functional traits and body mass data ----------------------
#
# ## a. Functional traits and body mass data (unfortunately these data should be  saved to disk prior to analysis)
# feather <- loadBirdFeathers(newDir  = bbsDir,
#                                  filename = regionFileName)
#
# funMass <- funcMass(dataWD = paste0(getwd(), "/data"), fxn = T, mass = F)
#
# funMassBirds <- mergeFunMassBBS(bbsData = feather, funMass = funMass); rm(funMass)
#

# III: Build sampling grid --------------------------------------------------------
# Define the grid's cell size (lat, long; unit:degrees)
# 1 deg latitude ~= 69 miles
# 1 deg longitude ~= 55 miles
cs <-
    c(0.5, 2)  # default is cell size 0.5 deg lat x 0.5 deg long

# Create the grid
routes_gridList <- createSamplingGrid(cs = cs)
routes_grid <- routes_gridList$routes_grid
sp_grd <- routes_gridList$sp_grd

rm(cs)

# IV: Load BBS data and overlay the sampling grid ----------------------------------

## Load all the feathers in bbsDir, munge, and rbind
# FYI: ~ 1/2 minutes to upload ALL BBS DATA; ~350MB as feather object
{feathers <- NULL
tic("Import feathers (BBS data)")
for (i in 1:length(list.files(bbsDir))) {
    feather <- NULL
    feather <- loadBirdFeathers(newDir  = bbsDir,
                                filename = list.files(bbsDir)[i]) %>%
        dplyr::rename(lat = latitude,
               long = longitude) %>%
        left_join(routes_grid)

    feathers <- rbind(feathers, feather)
    rm(feather)
}
print(object.size(feathers), units = "auto")
toc()}


### PART II: CALCULATE THE METRICS ##########################################

# I: Define parameters for calculating the metrics  -------------------------

# # Option 1; Interactive inputs for parameters
    source(paste0(getwd(), '/interactCalcRDM.R'))
  #
# # Option 2: Manually define parameters for regimeDetectionMeasures functions.
#     metrics.to.calc <- c("distances", "ews")
#     analySpatTemp <- "South-North" # choose one of : 'South-North', 'East-West', or 'temporal'
#     fill = 0
#     min.samp.sites = 15
#     min.window.dat = 3
#     fi.equation = "7.12"
#     winMove = 0.25
#     to.calc = c("EWS","FI","VI")


# II: Subset the data -----------------------------------------------------

birdData <- feathers %>%
    filter(
           year == 2015,
           colID == 81
           # rowID == 14
           # statenum == 2,
           # route == 14
           ) %>%
    dplyr::rename(variable = aou,
                  value = stoptotal)


# III: Munge the subsetted data var names ---------------------------------

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

# Create empty list to store results
resultsList <- list()

# III: Calculate the distance travelled for a single transect -------------------------------------------------

if("distances" %in% metrics.to.calc) {
    metricInd = "distances"
     # keep only the relevant columns
    dataInDist  <- dataIn %>%
       dplyr::select(
            time, variable, value)

    # Calc distances
    results <- NULL

    results <- calculate_distanceTravelled(dataInDist, derivs = T) %>%
        gather(key = 'metricType', value ='metricValue', -time)

    resultsList$distance <- results

    rm(dataInDist)

    saveMyResults(resultsList$distance ,resultsDir =resultsDir, analySpatTemp =analySpatTemp, metricInd = metricInd)

    }

# VI:: Calculate other EWSs ----------------------------------------------------


# f.ii.a Calculate the EWSs
if ("ews" %in% metrics.to.calc) {
    metricInd = "ews"

    # keep only the relevant columns
    dataInRDM  <- dataIn %>%
        dplyr::select(
            time, variable, value)

    if(length(unique(dataInRDM$time)) < min.samp.sites){stop("data has less observations than min.samp.sites")}

    results <- NULL
    results <-
        rdm_window_analysis(
            dataIn = dataInRDM,
            winMove = winMove,
            overrideSiteErr = F,
            fi.equation = fi.equation,
            min.window.dat = min.window.dat,
            fill = fill,
            to.calc = to.calc)

    resultsList$ews <- results

    rm(dataInRDM)

    saveMyResults(resultsList$ews ,resultsDir =resultsDir, analySpatTemp =analySpatTemp, metricInd = metricInd)

} # END EWS calcs
