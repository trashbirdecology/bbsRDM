# getDataBBS ##########################################################
#' @title Download USGS Breeding Bird Survey data
#' @description This function was adapted from **oharar/rBBS** package.
#' @param file One file names ("stateX.zip"). Preferably download a single state at a time, otherwise run time will take >1 minutes.
#' @param dir URL to the StatesFiles.
#' @param year Vector of years. Default = NULL (all years).
#' @param aou Vector of AOU #s Default = NULL (all species).
#' @param countrynum Vector of country ID #'s. Default = NULL (all countryNums).
#' @param states Vector of state names Default = NULL (all states).
#' @exports
#'
#' @return If download successful, a dataframe with the results.
#'
#' @examples
#' # download all species and years from Nebraska.
#'
#' \dontrun{
#' NE <- getDataBBS(file = "Nebrask.zip")
#' }
#'
getDataBBS <- function(file = file,
                       dir =  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/",
                       year = NULL,
                       aou = NULL,
                       countrynum = NULL,
                       states = NULL) {
  # Unzip the the state file(s)
  dat <-
    GetUnzip(ZipName = paste0(dir, file),
             FileName = gsub("^Fifty", "fifty", gsub("zip", "csv", file)))

  names(dat) <- tolower(names(dat))

  ## Define subsetting parameters
  {
    if (is.null(countrynum)) {
      UseCountry <-
        TRUE
    } else {
      UseCountry <- dat$countrynum %in% countrynum
    }
    if (is.null(year)) {
      UseYear <- TRUE
    } else {
      UseYear <- dat$year %in% year
    }
    if (is.null(aou)) {
      UseAOU <- TRUE
    } else {
      UseAOU <- dat$aou %in% aou
    }
    if (is.null(states)) {
      UseState <- TRUE
    } else {
      UseState <- dat$statenum %in% states
    }
  }

  ## Create a vector based on this
  Use <-
    UseYear & UseAOU & UseCountry & UseState

  if (sum(Use) > 0) {
    dat$routeID <-
      paste(dat$statenum, dat[, grep("^[Rr]oute$", names(dat))])

  dat <- subset(dat, subset = Use)

    return(dat)
  } else
    warning("no data downloaded")
  return(NULL)
}
