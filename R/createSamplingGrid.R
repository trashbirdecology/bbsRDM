#' @title Generate a sampling grid (rectangular) for regions in North America.
#' @description  Creates a sampling grid across the continental united states and assign BBS routes to specific a row and column ID.
#' @param cs Cell size (in degree lat, long). Default is 0.5 degree long by 0.5 degree lat. In this region, 1 deg latitude ~= 69 miles & 1 deg longitude ~= 55 miles. The total length of a BBS route is ~50 miles. Caution when using degrees < 1 by 1 degree as a single route could fal into multiple cells...
#' @param bbLat Min and max (in any order) latitude coordinates for the bounding box. The function removes routes (lat,long) falling outside these coordinates.  Default = c(23, 51). See also 'bbLong'.
#' @param bbLong Min and max (in any order) longitude for the bounding box. The function removes routes (lat,long) falling outside these coordinates. Default = c(23, 51). See also 'bbLat'.
#' @param country One or more of c("CA","USA"). If not specified, will keep grid based on both CA and USA.
#' @usage routes_gridList <- createSamplingGrid(cs = c(1,1))

#' @keywords bbs, routes
#' @export createSamplingGrid

createSamplingGrid <-
    function(cs = c(0.5, 0.5),
             bbLat = c(53, 26) ,
             bbLong =  c(-62, -128),
             country = c("CA", "USA")) {
        # A: BBS Routes ------------------------------------------------------------

        # Load the BBS routes information and location
        routes <- getRouteInfo()

        # B: Create a bounding box -----------------------------------------------------

        # Set a bounding box for the route data (here = continental United States)
        routes <- routes %>%
            filter(
                # countrynum %in% c(840, 124),!(countrynum == 840 & statenum == 03),
                # restrict analysis to within continental U.S.
                latitude < max(bbLat) ,
                latitude > min(bbLat),
                longitude < max(bbLong),
                longitude > min(bbLong)
            )

        # If user wants to analyse more CA and USA, they will need to specify in country arg
        routes.temp = NULL
        if ("USA" %in% country) {
            routes.temp <-
                routes %>% filter(countrynum == 840 | countrynum == 124)
        }
        if ("CA" %in% country) {
            routes.temp2 <- routes %>% filter(countrynum == 124)
            if (exists("routes.temp1")) {
                routes.temp = rbind(routes.temp2, routes.temp)
            }

        }

        if(exists("routes.temp")){routes<-routes.temp}

        # Set coordinates for route locations
        sp::coordinates(routes) <- ~ longitude + latitude

        # Set projection to WGS84 lat long friendly
        sp::proj4string(routes) <-
            sp::CRS("+proj=longlat +datum=WGS84")


        # Create a bounding box around the BBS route lines
        bb <- sp::bbox(raster::extent(routes))


        if (!exists("cs")) {
            # Define the SpatialGRid cell size based on degrees lat/long
            cs <- c(1, 1)  # cell size 0.5 deg lat x 0.5 deg long
            # 1 deg latitude ~= 69 miles
            # 1 deg longitude ~= 55 miles
        }

        # Define the cell offset
        cc <- bb[, 1] + (cs / 2)  # cell offset

        # Number of cells per direction based on cell size and bb
        cd <- ceiling(diff(t(bb)) / cs)

        # Create grid topology object
        grd <- sp::GridTopology(
            cellcentre.offset = cc,
            cellsize = cs,
            cells.dim = cd
        )

        # Create the SpatialGridDataFrame object
        sp_grd <- sp::SpatialGridDataFrame(
            grd,
            data = data.frame(id = 1:prod(cd)),
            proj4string = sp::CRS(sp::proj4string(routes))
        )



        # C: Manipulate the  (sp_grd) SpatialGridDataFrame ----------------------------------

        # Create unique row and column IDs
        rep.row <- function(x, n) {
            matrix(rep(x, each = n), ncol = n, byrow = TRUE)
        }
        rep.col <- function(x, n) {
            matrix(rep(x, each = n), ncol = n, byrow = TRUE)
        }

        col_m <- rep.row(1:grd@cells.dim[1], grd@cells.dim[2])
        row_m <- t(rep.col(1:grd@cells.dim[2], grd@cells.dim[1]))

        # This looks a little messy (columnnames) but for now leave it to match sp_grd
        rowcol_ID <-
            as.data.frame(cbind(
                ID = tidyr::gather(as_tibble(row_m), value = "rowID")$rowID,
                colID = tidyr::gather(as_tibble(col_m), value = "colID")$colID
            ))



        # Append the row and column IDs to the sp_grd
        rowColID_sp_grid_ID <- cbind(sp_grd@data, rowcol_ID) %>%
            dplyr::rename(cellID = id,
                          rowID = ID) %>%
            as.data.frame()


        # D: Spatial join the BBS routes onto the grid ----------------------------

        # Convert the SLDF to SPDF if you want to...
        route_spdf <- as(routes, "SpatialPointsDataFrame")

        # Extract the CELL ID in which each point along the poly line falls.
        ## This will give you MULTIPLE CELL IDs for each RTENO,
        ## and if the route falls into >1 grid cell, you will get >1 CELL ID.
        temp <- route_spdf %over% sp_grd

        # Need to force the SPDF to a data frame in order to merge them
        routeDF <- as_tibble(route_spdf) %>%
            dplyr::rename(long = longitude,
                          lat = latitude)


        # Merge the cellID with the route DF
        routeDF$cellID <- temp$id



        # E: Create the exported route/grid object --------------------------------

        # Join the row and col ID indexes to the cellID
        routeDF_final <-
            suppressMessages(
                left_join(routeDF, rowColID_sp_grid_ID) %>%
                    distinct(long, lat, cellID , rowID, colID, countrynum, statenum, route)
            )


        # F: Return object(s) -----------------------------------------------------
        listOut <- list(routeDF_final, sp_grd)
        names(listOut) <- c('routes_grid', "sp_grd")

        return(listOut)

        # # Detach package to avoid conflicts
        # detach("package:raster", unload = TRUE) # see bug: https://github.com/tidyverse/dplyr/issues/643

        # End Function ------------------------------------------------------------
    }
