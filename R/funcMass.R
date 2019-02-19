#' Load functional trait and mass data
#'
#' @param dataWD Where the functional trait and mass dataframes are stored.
#' @param fxn Logical. Retrieves functional trait data (referece).
#' @param mass Logical. Retrieves body mass information (Dunning reference).
#' @export

funcMass <- function(dataWD = paste0(getwd(), "/data"), fxn = T, mass = F){

# Load  data -----------------------------------------------------------


# Functional groups (from LCTA):
if(fxn == T){
    func <- suppressMessages(suppressWarnings(readr::read_csv(paste(dataWD, "/foraging_data_LCTA.csv", sep = ""))))}else(fxn= NULL)

if(mass==T){
# Body mass (Dunning 2008):
mass <- suppressMessages(suppressWarnings(readr::read_csv(paste(dataWD, "/bird.mass.dunning4.csv", sep = "")))) %>%
  dplyr::select(-X13)

mass$scientificName <-
  str_extract(mass$spp, "[A-Z][a-z]+\\ [a-z]+")
}else(mass = NULL)

funcMassList <- list(func, mass);names(funcMassList) <- c("fxn", "mass")

return(funcMassList)

}
