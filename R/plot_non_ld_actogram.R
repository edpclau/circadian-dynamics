#' Internal Function to plot Non LD Actograms
#'
#' @param df The output of internal formatting in the plot_actogram2 function.
#'
#'
#' @export
#' @return A set of ggplot2 actograms
#' @description An internal helper function not meant to be called by the user.
#'
#' @importFrom future sequential plan
#' @importFrom furrr future_map2
#' @import ggplot2
plot_non_ld_actogram <- function(df,id) {

plan(sequential)
#Data for Actogram
return(
  future_map2(
  .x = df,
  .y = id,
  .f = ~ {ggplot(.x, aes(x = dur, y = raw_values/2, height = raw_values)) +
      geom_tile() +
      geom_vline(aes(xintercept = 24 - 0.025), col = 'blue') +
      facet_grid(window ~ ., switch = 'y') +
      labs(x = 'Days', y = '', title = .y, fill = 'Dark/Light') +
      theme(
        panel.spacing = unit(0, "cm", data = NULL),
        axis.title = element_text(face = "bold", size = 12),
        axis.ticks = element_blank(),
        axis.line = element_line(),
        axis.line.x = element_line(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, size = 12, vjust = 0),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"),
        panel.border= element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust =  - 0.5)
      )
  }
)

)
}
