
# GetUnzip #########################################################
#' @details
#' @description
#' @param ZipName
#' @param FileName
#' @exports GetUnzip

GetUnzip <- function(ZipName, FileName) {
    if (grepl('^[hf]t+p', ZipName)) {
        temp <- tempfile()
        download.file(ZipName, temp, quiet = FALSE)
        data <- read.csv(unz(temp, FileName))
        unlink(temp)
    } else {
        data <- read.csv(unz(ZipName, FileName))
    }
    data
}
