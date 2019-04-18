# bbsRDM

The goal of bbsRDM is to download BBS data from the USGS's FTP server, and calculate various regime detection metrics. 

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TrashBirdEcology/bbsRDM")
```
## Example (in dev) PLEASE SEE VIGNETTES

This is a basic example using Nebraska data. 

``` r
NE <- getDataBBS("Nebrask.zip") # downloads a zip file and returns it as a data.frame. This is NOT a typo.
```
Use `r GetRegions()` to find the zip file names of a particular state (USA) or province (Canada). 
