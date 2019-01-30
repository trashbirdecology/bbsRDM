rm(list = ls())

#################PART I: SETUP #########################################################################
# I:    Load packages -------------------------------------------------------


## Re-install `regimDetectionMeasures` often as this package is under major development.
devtools::install_github("trashbirdecology/regimedetectionmeasures",
                         force = T,
                         dep = F)

library(regimeDetectionMeasures)
library(sp)
library(raster)
library(feather)
library(bbsRDM)
library(feather)


# II:   Directories   ---------------------------------------------------

# a. Create a directory to store and/or load the BBS data as feathers
dir.create(file.path(getwd(), paste0("/bbs_raw_data")))
bbsDir <- paste0(getwd(), paste0("/bbs_raw_data/"))
if (length(list.files(bbsDir)) != 0) {
    warning(
        "The directory ",
        bbsDir,
        " already exists.. /nProceed with caution--files in this directory will likely be overwritten."
    )
}

# b. Create directories to store and/or load the BBS data as feathers
resultsDir <- paste0(getwd(), paste0("/myResults/"))
if (length(list.files(resultsDir)) != 0) {
    warning("The directory ",
            resultsDir,
            " already exists. Files will be overwritten.")
}
dir.create(file.path(getwd(), paste0("/myResults")))
dir.create(file.path(getwd(), paste0("/myResults/distances")))
dir.create(file.path(getwd(), paste0("/myResults/ews")))


# III:  OPTIONAL IF DATA ALREADY DOWNLOADED: Import BBS and save to disk as feathers -----------------------------------------------
# ##FYI:  ~10-15 MINUTES TO DOWNLOAD ALL STATE FILES FOR ALL YEARS!
#
# # a. Load the regional (list of states, regions) .txt file from Patuxent
# regions <- getRegions()
#
# # b. Create a series or one filenames for states, regions
# regionFileName <- regions$zipFileName %>% na.omit()
#
# # c.  Download and unzip the BBS data.
# tic("Import and save bbs data from ftp")
# for (i in 1:length(regionFileName)) {
#     bbsData <-  importDataBBS(
#         # arguments for getDataBBS()
#         file = regionFileName[i],
#         dir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/",
#         year = NULL,
#         aou = NULL,
#         countrynum = NULL,
#         states = NULL,
#         #  arguments for getRouteInfo():
#         routesFile = "routes.zip",
#         routesDir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/",
#         RouteTypeID = 1,
#         # one or more of c(1,2,3)
#         Stratum = NULL,
#         BCR = NULL
#     )
#
#
#
#     # c. Save the file to disk
#     birdsToFeathers(dataIn  = bbsData,
#                     newDir  = bbsDir,
#                     filename = regionFileName[i])
#
#     rm(bbsData)
#
# } # end section BBS unzip and save loop
# toc()
#
#
#
################# PART II: CREATE SAMPLING GRID USING BBS ROUTES ########################################
# IV:   Build a spatial sampling grid --------------------------------------------------------
# Define the grid's cell size (lat, long; unit:degrees)
# 1 deg latitude ~= 69 miles
# 1 deg longitude ~= 55 miles
cs <-
    c(1, 1)  # default is cell size 0.5 deg lat x 0.5 deg long

# Create the sampling grid
routes_gridList <- createSamplingGrid(cs)

cellFromRowCol(raster(routes_gridList$routes_grid))
gridded(routes_gridList$sp_grd)

# V:    Load BBS data and overlay the sampling grid ----------------------------------

### Load all the feathers in bbsDir, munge, and rbind
### FYI: ~ 1/2 minutes to upload ALL BBS DATA; ~350MB as feather object

feathers <- NULL
suppressMessages(for (i in 1:length(list.files(bbsDir))) {
    feather <- NULL
    feather <- loadBirdFeathers(newDir  = bbsDir,
                                filename = list.files(bbsDir)[i]) %>%
        dplyr::rename(lat = latitude,
                      long = longitude) %>%
        left_join(routes_gridList$routes_grid) # join bbs data with the spatial samling grid

    feathers <- rbind(feathers, feather)
    rm(feather)
})
print(object.size(feathers), units = "auto")


################ PART III: SUBSET THE SPECIES DATA ######################################################
### This section is optional, however, we do not recommend using all raw bbs data.
### Two choices here:
### Section VI:  Subsets by specific functional traits and/or body masses. More tedious than VII.
### Section VII: Subset by larger groups of species according to AOU code #s.

print("This is the data we will work with hereafter.")
head(feathers)

# # VI: OPTIONAL Subset by the functional traits and body mass data ----------------------
#
#
# # Load the functional trait and mass data, and munge/merge
# funMass <-
#     funcMass(dataWD = paste0(getwd(), "/data"),
#              fxn = T,
#              mass = F)
#
# # Combine the two datasets
# bbsData <-
#     mergeFunMassBBS(bbsData = feather, funMass = funMass)
# rm(funMass)
# rm(funMass, feather)
#
#
# VII:  Subset by AOU codes ------------------------------------------------

feathers <- subsetByAOU(myData = feathers, 'remove.shoreWaderFowl')


################ PART IV: CALCULATE THE METRICS #########################################################
# VIII: Define parameters for subsetting the data and for calculating the metrics  -------------------------

# # Option 1; Interactive inputs for parameters
# source(paste0(getwd(), '/otherScripts/interactCalcRDM.R'))

# Option 2: Manually define parameters for regimeDetectionMeasures functions.
metrics.to.calc <- c("ews", "distances")
direction <-
    "East-West"
# "East-West" # choose one of : 'South-North', 'East-West'
fill = 0 # Fills in missing species counts with ZERO.
min.samp.sites = 8
min.window.dat = 3
fi.equation = "7.12"
winMove = 0.25
to.calc = c("EWS", "FI", "VI")


# Get all possible years
years.use = unique(feathers$year)
# keep only the years divisible by 5
years.use  <- years.use[which(years.use %% 5 == 0 & years.use > 1975)] %>% sort()

# Define some filtering and labeling parameters based on direction of spatial analysis (if applicable)
if (direction == "South-North") {
    dir.use =  unique(feathers$colID) %>% na.omit(colID) %>% sort()

}
if (direction == "East-West") {
    dir.use = unique(feathers$rowID) %>% na.omit(rowID) %>% sort()
}


# VIX:  Conduct analyses  -----------------------------------------------------
## About 5 minutes run time for all transects in one direction!
for (j in 1:length(dir.use)) {
    # For east-west analysis
        if (direction == "East-West"){
            birdsData <- feathers %>%
                filter(rowID == dir.use[j]) %>%
                mutate(direction = direction,
                       dirID = dir.use[j])
    }
    # For south-north analysis
        if (direction == "South-North"){
            birdsData <- feathers %>%
            filter(colID == dir.use[j]) %>%
            mutate(direction = direction,
                   dirID = dir.use[j])
    }



    if (nrow(birdsData) < min.samp.sites) {
        next(print(paste0("Not enough data to analyze. Skipping j-loop ", dir.use[j])))
    }


    # VX.  Analyze the data ---------------------------------------------------

    for (i in 1:length(years.use)){
        # a. Subset the data according to year, colID, rowID, state, country, etc.x
        birdData <- birdsData %>%
            filter(year == years.use[i]) %>%
            dplyr::rename(variable = aou,
                          value = stoptotal)



        if (nrow(birdData) == 0){
            next
        }

        # b. Munge the data further
        birdData <- mungeSubsetData(birdData)


        # X.   Calculate the metrics ---------------------------------------------------
        ## This function analyzes the data and writes results to file (in subdirectory 'myResults') as .feather files.

        suppressMessages(calculateMetrics(dataIn = birdData, metrics.to.calc = metrics.to.calc, yearInd = years.use[i]))

        print(paste0("End i-loop (years) ", i, " of ",  length(years.use)))

    } # end i-loop

    print(paste0("End j-loop (transects) ", j, " of ",  length(dir.use)))
} # end j-loop



################ PART V: VISUALIZE THE METRICS  #########################################################
# XI.   Import results --------------------------------------------------------

# a. Import EWS results
results_ews <-
    importResults(resultsDir = resultsDir, myPattern = 'ews',
                  subset.by = direction) %>%
    # assign the end of the window as the cellID
    mutate(cellID = cellID_max)

## FYI: varibles will liekly be missing (NA) for metricTypes FI and VI, because these are calculated across ALL variables at a time...

# b. Import distance results
results_dist <-
    importResults(resultsDir = resultsDir, myPattern = 'distances', subset.by = direction)


# X.    Create spatial results data -------------------------

# a. Get the spatial sampling grid coordinates
coords_grd <-
    cbind(routes_gridList$sp_grd@data,
          coordinates(routes_gridList$sp_grd)) %>%
    rename(lat = s2,
           long = s1,
           cellID  = id)

# b. Join coords_grd with results
# note: a full join will likely produce many cells with NO results data..
# but NO lat or long should == NA!
distResults <-
    full_join(coords_grd,
              results_dist) %>%
    na.omit(metricType)

ewsResults <-
    full_join(coords_grd,
              results_ews) %>%
    na.omit(metricType) %>%
    dplyr::select(-cellID_min,-cellID_max, -winStart  , -winStop)

# d. Set coordinate system and projection
coordinates(distResults) <-
    coordinates(ewsResults) <- c("long", "lat")
sp::proj4string(distResults) <-
    sp::proj4string(ewsResults) <-
    sp::CRS("+proj=longlat +datum=WGS84")


# XI.  Parameter specification --------------------------------------------

plotResults <- distResults
metric.ind <- "s"
    # c('dsdt',"s") # the metrics to print
year.ind <- unique(plotResults@data$year) %>% sort()
sortVar.lab <-
    ifelse(unique(plotResults@data$direction) == "South-North",
           "latitude",
           "longitude")

# XII.  2D PLOTS  ------------------------------------------------------
# Plot indiviudal transects..
dirID.ind <- 22
sort.year.line(plotResults, metric.ind, year.ind, dirID.ind, scale = T, center = T)+
    geom_vline(aes(xintercept=-85.2), color = "grey", linetype = 2)


# XII.  Spatially explicit plots ------------------------------------------

# get u.s. state data
us <- getData('GADM', country = 'US', level = 1)

# check the CRS to know which map units are used
proj4string(us)
proj4string(plotResults)

# plot US
plot(us)

raster(plotResults)
str(plotResults)

plot(grid, pch = ".", add = T)

# X. EXTRA PLOTS ----------------------------------------------------

# years.use = c( 2015)

# a. Plot distances
results.to.plot <- allResults %>%
    filter(year %in% years.use,
           analysisInd = "distances") %>%
    left_join(routes_gridList$routes_grid)


ggplot(results.to.plot) +
    geom_line(aes(x = long,
                  y = metricValue, color = as.factor(year))) +
    facet_wrap(~ metricType, scales = "free_y")


# b. Plot ews
results.to.plot <- results_ews %>%
    filter(year %in% years.use)
metrics.to.plot <- c("VI", "FI_Eqn7.12")

ggplot(results.to.plot %>%
           filter(metricType %in% metrics.to.plot)) +
    geom_line(aes(x = winStart,
                  y = metricValue, color = as.factor(year))) +
    facet_wrap(~ metricType , scales = "free_y", ncol = 1)
