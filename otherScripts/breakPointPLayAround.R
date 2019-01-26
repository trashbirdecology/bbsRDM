#
install.packages("breakpoint")

test = plotResults@data %>%
    filter(
        # year == 2015,
           metricType == "s",
           dirID == 12
           ) %>%
    arrange(sortVar)


list  = breakpoint::CE.Normal.Mean(test %>% dplyr::select(metricValue), Nmax = 5, eps = 0.01, rho = 0.05, M = 200, h = 5, a = 0.8, b = 0.8,
               distyp = 1, penalty = "mBIC", parallel = FALSE)

list$No.BPs
list$BP.Loc


bkPts <- test[list$BP.Loc,]

ggplot(test)+
    geom_line(aes(x=sortVar, y = metricValue))+
    geom_point(data = bkPts, aes(x=sortVar, y = metricValue), color = "red")


#okit#
# install.packages("strucchange")
library(strucchange)


## F statistics indicate one breakpoint
library(dplyr)
library(tidyr)
library(purrr)
library(strucchange)
# install.packages("segmented")
library(segmented)
library(ggplot2)


# Calculate breakpoints
nestDat <- plotResults@data %>%
    group_by(metricType, direction, dirID, year, analysis) %>%
    nest() %>%
    mutate(mod_obj   = map(data, ~ lm(metricValue ~ sortVar, data = .x)),
           summaries = map(mod_obj, broom::glance)) %>%
    # dplyr::select(metricType, direction, dirID, year, analysis, summaries) %>%
    unnest(summaries)


segf <- function(df){
    require(segmented)
    segmented(lm(metricValue ~ sortVar, data=df))
}

models <- nestDat$mod_obj %>%
    mutate(segs = data %>% map(segf))

models$mod_obj[[1]]

out.lm <- lm(metricValue ~ sortVar, data = dati)
str(out.lm)

o <- segmented(out.lm)#, seg.Z = ~sortVar, psi = list(x = c(30,60)),
#                control = seg.control(display = FALSE)
# )

dat2 = data.frame(x = dati$sortVar, y = broken.line(o)$fit)

ggplot(data = dati, aes(x = sortVar, y = metricValue)) +
    geom_point(color = "grey") +
    geom_line(data = dat2, aes(x, y), color = 'blue')+
    ylim(0,1e4)


















