rm(list = ls())
# 0: About this script ------------------------------------------------------
#######################################
##
##
##
#######################################
# 1: Load packages -------------------------------------------------------

## Re-install often as this package is under major development.
devtools::install_github("trashbirdecology/regimedetectionmeasures")
library(regimeDetectionMeasures)
library(tidyverse)
library(sp)
library(raster)
library(feather)


## Optional: for benchmarking but recommended due to large file sizes
# devtools::install_github("collectivemedia/tictoc", force = F)
library(tictoc)

# 0: Preliminary stuff  ---------------------------------------------------

# a. Create a directory to store and/or load the BBS data as feathers
dir.create(file.path(getwd(), paste0("/bbs_raw_data")))
bbsDir <-paste0(getwd(), paste0("/bbs_raw_data/"))

# b.  Load the regional .txt file from Patuxent
regions <- GetRegions()
  # Create a series or one filenames for states, regions
  regionFileName <- regions$zipFileName %>% na.omit()



# I: Import BBS and save to disk as feathers -----------------------------------------------
#fyi:  ~8 MINUTES TO DOWNLOAD ALL STATE FILES

# a.  Download and unzip the BBS data.
{tic("Import and save bbs data from ftp")
for(i in 1:length(regionFileName)){
        bbsData <-  importDataBBS(
            # arguments for getDataBBS()
            file = regionFileName[i],
            dir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/",
            year = NULL,
            aou = NULL,
            countrynum = NULL,
            states = NULL,
            #  arguments for getRouteInfo():
            routesFile = "routes.zip",
            routesDir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/",
            RouteTypeID = 1,
            # one or more of c(1,2,3)
            Stratum = NULL,
            BCR = NULL
        )


# b. Save the file to disk
birdsToFeathers(dataIn  = bbsData,
                newDir  = bbsDir,
                filename = regionFileName[i])

rm(bbsData)

} # end section I. loop
toc()}


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


# IV: Load BBS data and overlay the sampling grid ----------------------------------


## Load all the feathers in bbsDir, munge, and rbind
for(i in 1:length(list.files(bbsDir))){
feather <- NULL
feather <- loadBirdFeathers(newDir  = bbsDir,
                            filename = list.files(bbsDir)[i]) %>%
            rename(lat = latitude,
                   long = longitude) %>%
    left_join(routes_grid)

feathers <- rbind(feathers, feather)
}



# V: Calculate the regime detection metrics -------------------------------

# Option 1; Interactive inputs for parameters
source(paste0(getwd(), '/R/interactCalcRDM.R'))

# Option 2: Manually fill them in
