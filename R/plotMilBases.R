
# get military bases shapefile
milBases <- getMilBases(shploc = "http://www.acq.osd.mil/eie/Downloads/DISDI/installations_ranges.zip", shpfile = "MIRTA_Points")


# Set projection to WGS84 lat long friendly
sp::proj4string(milBases) <-
    sp::CRS("+proj=longlat +datum=WGS84")



# Extract the CELL ID in which each military base falls.
temp <- merge(plotResults, milBases)
str(temp)


CRS(milBases) =proj4string(plotResults)

# Merge the cellID with the route DF
routeDF$cellID <- temp$id

