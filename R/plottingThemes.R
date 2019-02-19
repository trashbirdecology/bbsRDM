#' @description sets a plotting theme for plots
#' @export
myTheme <- function() {

# Define cb-friendly ramp -------------------------------------------------

cbbPalette <-
    c(
        "#000000",
        "#009E73",
        "#e79f00",
        "#9ad0f3",
        "#0072B2",
        "#D55E00",
        "#CC79A7",
        "#F0E442"

    )


# Define custom plotting themes and functions for plotting  ---------------


    # edit the faceting box aes
    #eliminates background, gridlines, and chart border
    theme(
        plot.background = element_blank()
        ,
        panel.grid.major = element_blank()
        ,
        panel.grid.minor = element_blank()
        ,
        panel.border = element_blank()
        #draws x and y axis line
        ,
        axis.line = element_line(color = 'black')
        ,
        strip.background = element_rect(fill = "white", color = "white")
    )

    scale_color_manual(values = cbbPalette)


}


