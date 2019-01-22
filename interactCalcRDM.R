# Interactive prompt to fill required parameters.


# PART I ------------------------------------------------------------------

# a. If the session is not interactive, do not proceed.
if (interactive() == F) {
    stop(
        "...r session needs to be interactive in order to proceed...alternatively, manually enter parameter values for the following:
        ENTER PARAM AND DESCRIPTIONS HERE"
    )
}

list = NULL

# b. Some simple ones
# Spatial or temporal analysis
{
    analySpatTemp <-
        readline(prompt = "Do you want to run a spatial or temporal analysis?
                 Type '1' for spatial
                 Type '2' for temporal")
    if (analySpatTemp == 1) {
        analySpatTemp <-
            readline(prompt = "In which direction do you want the analysis to run?
                     Type '1' for East-West
                     Type '2' for South-North")
    }
    if(analySpatTemp ==2){analySpatTemp= "temporal"}
    }

# c. Fill value. What value to fill in NA/missing data with
{fill <- readline(prompt = "What value should replace missing time series or spatial observations (NA)?
Ex: if using count data and missing data are absences, use 0 (zero; this fill does not include missing covariate data)
    Type '1' for 0
    Type '2' for NA
    Type '3' for other") %>% as.integer()

        if(fill == 3){readline(prompt = "Please enter the fill value you would like to use for missing data:
                               ...can be numeric or character...")}}

# d. Parameters for 'regimeDetectionMeasures()'
metrics.to.calc <-  readline(prompt = "Which metrics do you want to calculate?
        Type '1' for early-warning signals
        Type '2' for distance travelled
        Type '3' for all
                             ") %>% as.integer()
while(metrics.to.calc != 1 &
      metrics.to.calc != 2 &
      metrics.to.calc != 3){readline('Please enter 1, 2 or 3 only!!!
                                     ')}

        if(metrics.to.calc ==1){metrics.to.calc = c("ews")}
        if(metrics.to.calc ==2){metrics.to.calc = c( "distances")}
        if(metrics.to.calc ==3){metrics.to.calc = c("ews", "distances")}


# for calculating early-warning signals only
if("ews" %in% metrics.to.calc){

{print('The following are decisions controlling `regimeDetectionMetrics()` pkg functions')}
    ## i. for "rdm_window_analysis()"
    min.samp.sites <-
        readline(prompt = "Minimum # (integer) of observations within a moving window?
                 **If NA or NULL, will default to 2**
                 ") %>% as.numeric()
    ## ii. winMove: % of data by which window moves
    winMove <-
        readline(prompt = "Proportion of the entire dataset by which each window will move?
                 Enter a number between 0 and 1 (I like to start with 0.25...s)
                 ") %>% as.numeric()
    if(is.null(winMove)){winMove <- 0.25}
    if(!is.null(winMove) & (winMove > 1 | winMove <0) ){readline(prompt = "Incorrect parameter. Please enter a NUMBER BETWEEN ZERO AND ONE!?
                 Enter a number between 0 and 1") %>% as.numeric()}

# d1. Calc rdm
 to.calc <-
     readline(prompt = "Which RDMs do you want to calculate? Enter 1,2,3 or 4:
           1: Fisher Information (FI)
           2: Variance Index (VI)
           3: Early-warning signals (EWS)
           4: All
              ") %>% as.integer()
    suppressWarnings(while(to.calc!=1 & to.calc!=2 & to.calc!=3 & to.calc!=4 ) {
        fi.equation <- readline("Please enter a number 1-4!") %>% as.integer
    })
            if(to.calc == 4){to.calc=c(c("EWS","FI","VI"))}
            if(to.calc == 3){to.calc=c("EWS")}
            if(to.calc == 2){to.calc=c("VI")}
            if(to.calc == 1){to.calc=c("FI")}


            ## calculate_FI()
        if("FI" %in% to.calc){
            fi.equation <- readline(prompt = "Which equations would you like to calculate?
    Enter 1,2,3 or 4:
        1: Eq. 7.12 (default)
        2: Eq. 7.13a
        3: Eq. 7.13b
        4: All") %>% as.integer()
    suppressWarnings(while(!c('1','2','3','4') %in% fi.equation) {
                fi.equation <- readline("Please enter a number between 1 and 4!
                                        ") %>% as.integer
                        })

        if(!is.null(fi.equation)){
            if(fi.equation == 4){fi.equation=c("7.12", "7.13a",'7.13b')}
            if(fi.equation == 3){fi.equation=c('7.13b')}
            if(fi.equation == 2){fi.equation=c("7.13a")}
            if(fi.equation == 1){fi.equation=c("7.12")} }
        }


    } # end EWS calcualtions all



