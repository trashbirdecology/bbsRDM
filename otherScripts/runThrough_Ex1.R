rm(list = ls())


#################PART I: SETUP #########################################################################
# I:   Load packages -------------------------------------------------------


## Re-install `regimDetectionMeasures` often as this package is under major development.
devtools::install_github("trashbirdecology/regimedetectionmeasures", force = T)
library(regimeDetectionMeasures)
library(sp)
library(raster)
library(feather)
library(bbsRDM)
library(feather)
# devtools::install_github("collectivemedia/tictoc", force = F)  # Optional but must silence tic()s and toc()s in following lines
library(tictoc)

# II:  Directories   ---------------------------------------------------

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

# b. Create a directory to store and/or load the BBS data as feathers
dir.create(file.path(getwd(), paste0("/myResults")))
resultsDir <- paste0(getwd(), paste0("/myResults/"))
if (length(list.files(resultsDir)) != 0) {
    warning(
        "The directory ",
        resultsDir,
        " already exists.. /nProceed with caution--files in this directory will likely be overwritten."
    )
}


# # III: OPTIONAL IF DATA ALREADY DOWNLOADED: Import BBS and save to disk as feathers -----------------------------------------------
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
# IV:  Build a spatial sampling grid --------------------------------------------------------
# Define the grid's cell size (lat, long; unit:degrees)
# 1 deg latitude ~= 69 miles
# 1 deg longitude ~= 55 miles
cs <-
    c(1, 1)  # default is cell size 0.5 deg lat x 0.5 deg long

# Create the grid
routes_gridList <- createSamplingGrid(cs = cs)
routes_grid <- routes_gridList$routes_grid
sp_grd <- routes_gridList$sp_grd
rm(cs)


# V:   Load BBS data and overlay the sampling grid ----------------------------------

### Load all the feathers in bbsDir, munge, and rbind
### FYI: ~ 1/2 minutes to upload ALL BBS DATA; ~350MB as feather object

feathers <- NULL
tic("Import feathers (BBS data)")
for (i in 1:length(list.files(bbsDir))) {
    feather <- NULL
    feather <- loadBirdFeathers(newDir  = bbsDir,
                                filename = list.files(bbsDir)[i]) %>%
        dplyr::rename(lat = latitude,
                      long = longitude) %>%
        left_join(routes_grid) # join bbs data with the spatial samling grid

    feathers <- rbind(feathers, feather)
    rm(feather)
}
print(object.size(feathers), units = "auto")
toc()


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

feathers <- subsetByAOU(myData = feathers)
str(feathers)

################ PART IV: CALCULATE THE METRICS #########################################################
# VIII: Define parameters for calculating the metrics  -------------------------

# # Option 1; Interactive inputs for parameters
# source(paste0(getwd(), '/otherScripts/interactCalcRDM.R'))


# Option 2: Manually define parameters for regimeDetectionMeasures functions.
metrics.to.calc <- c("distances", "ews")
analySpatTemp <-
    # "South-North"
"East-West" # choose one of : 'South-North', 'East-West', or 'temporal'
fill = 0
min.samp.sites = 3
min.window.dat = 3
fi.equation = "7.12"
winMove = 0.1
to.calc = c("EWS", "FI", "VI")

# VIX:  Create a dataset for analysis -----------------------------------------------------
# Create loop indices
years.use = unique(feathers$year)
# keep only the years divisible by 5
years.use  <- years.use[which(years.use %% 5 == 0)] %>% sort()

rows.use <-
    unique(feathers$rowID)
rows.use <- rows.use[!is.na(rows.use)] %>% sort()
cols.use <-
    unique(feathers$colID)
cols.use <- cols.use[!is.na(cols.use)] %>% sort()

for (j in 1:length(rows.use)) {
    for (i in 1:length(years.use)) {
        # a. Subset the data according to year, colID, rowID, state, country, etc.x
        birdData <- feathers %>%
            filter(year == years.use[i],
                   # colID == 42) %>%
                   rowID == rows.use[j]) %>%
                   # colID == cols.use[j]) %>%
            # statenum == 2,
            # route == 14) %>%
            dplyr::rename(variable = aou,
                          value = stoptotal)

        if (nrow(birdData) == 0) {
            next
        }

        # b. Munge the data further
        source(paste0(getwd(), "/otherScripts/mungeSubsetData.R"))
        # This script will produce a dataset called `dataIn`, and will fill paramters for labeling.


        # X.   Calculate the metrics ---------------------------------------------------

        ## This function analyzes the data and writes results to file (in subdirectory 'myResults') as .feather files.
        calculateMetrics(dataIn = dataIn, metrics.to.calc = metrics.to.calc)

    }
}
# XI. Load the results --------------------------------------------------------

# a. Import EWS results
results_ews <-
    importResults(resultsDir = resultsDir, myPattern = 'ews')
## FYI: varible should be missing (NA) for metricTypes fi and VI



# b. Import distance results
results_dist <-
    importResults(resultsDir = resultsDir, myPattern = 'distance')


################ PART V: VISUALIZE THE METRICS  #########################################################
# X. Visualize results ----------------------------------------------------

# years.use = c( 2015)

# a. Plot distances
results.to.plot <- results_dist %>%
    filter(year %in% years.use) %>%
    left_join(routes_grid)


ggplot(results.to.plot) +
    geom_line(aes(x = long,
                  y = metricValue, color = as.factor(year))) +
    facet_wrap( ~ metricType, scales = "free_y")


# b. Plot ews
results.to.plot <- results_ews %>%
    filter(year %in% years.use)
metrics.to.plot <- c("VI", "FI_Eqn7.12")

ggplot(results.to.plot %>%
           filter(metricType %in% metrics.to.plot)) +
    geom_line(aes(x = winStart,
                  y = metricValue, color = as.factor(year))) +
    facet_wrap( ~ metricType , scales = "free_y", ncol = 1)



head(results.to.plot)

# Spatial visualization ---------------------------------------------------


coordinates(sp_grd)
spplot(sp_grd)

coordinates(results.to.plot) <-

    View(sp_grd)
