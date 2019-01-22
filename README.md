# bbsRDM

The goal of bbsRDM is to download BBS data from USGS's FTP server, and calculate various regime detection metrics

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TrashBirdEcology/bbsRDM")
```
## Example

This is a basic example which shows you how to solve a common problem:

``` r
NE <- getDataBBS("Nebrask.zip") # downloads a zip file and returns it as a data.frame
```

