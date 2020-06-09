#' Export/Plot Actogram
#'
#' @details Exports actogram plots for each column of a data.frame of tibble with a datetime object.
#' @usage plot_actogram(df = NULL, datetime_column = 1, filename = "actogram.pdf",
#' export = FALSE, width = 12, height = 12, dpi = 800, nrow = 5, ncol = 5)
#' @param df A data.frame or tibble which contains a datetime column and measurement values.
#' @param datetime_column An integer which indicates the column that contains the datetime.
#' @param filename A charater string  ending with ".pdf" which customizes the name of the file to be exported. "actogram.pdf" (default)
#' @param export Logical. If FALSE (default), it will open a window with the plots. If TRUE, saves the plots on the current directory in pdf format.
#' @param width a numeric indicating the width of the page of the pdf file. default = 12
#' @param height a numeric indicating height of the page of the pdf file. default = 12
#' @param nrow a numeric indicating how many rows of plots to put in a page. default = 5
#' @param ncol a numeric indicating how many columns of plots to put in a page. default = 5
#'
#' @examples
#' plot_actogram(df = monitor,
#' datetime_column = 1, filename = "actogram.pdf",
#' export = FALSE)
#'
#' @import ggplot2
#' @importFrom gridExtra marrangeGrob
#' @importFrom purrr map
#' @import dplyr
#' @importFrom magrittr '%>%'
#' @import tidyr
#' @import lubridate
plot_actogram <- function(df = NULL, datetime_column = 1, filename = "actogram.pdf", export = FALSE,
                          width = 12, height = 12, dpi = 800, nrow = 5, ncol = 5) {

  ##### Flow Control #####
  if (!is.null(df)) {
    names(df)[datetime_column] <- "datetime"
    df <- df %>% select(datetime, everything())
  } else (
    stop("Must provide a data.frame or tibble.")
  )



#Wrangle Data

data <- df %>% make_time_windows(window_size_in_days = 2, window_step_in_days = 1) %>% gather("ind", "value", -c(window,datetime)) %>%
  group_by(ind, window) %>%
  mutate(time = format(datetime, "%H:%M:%S") %>% strptime("%H:%M:%S"),
         date = case_when(
           format(min(datetime), "%d") == format(datetime, "%d") ~ 1,
           format(max(datetime) - lubridate::ddays(1), "%d") == format(datetime, "%d") ~ 2,
           TRUE ~ 3
         )
         )  %>%
  ungroup() %>%
  mutate(ind = as.numeric(str_remove(ind, "IND "))) %>%
  arrange(ind)


#### Parameters for how the plots will look on the page



pl <- map(.x = unique(data$ind),
           .f = ~ data %>%
    filter(date != 3, ind == .x) %>%

  ggplot(aes(x = time, y = value/2, height = value)) +
  geom_tile(fill = "black") +

  labs(title = paste(.x), y = "Days", x = "Clock Time (Hr)") +
  geom_hline(yintercept = 0, lty = "solid") +
  geom_vline(xintercept = min(data$time) - lubridate::dminutes(30)) +
  facet_grid(window ~ date,  switch  = "y") +


  scale_x_datetime(date_labels = "%k", expand = c(0,0)) +
  scale_y_discrete(breaks = c(0), labels = c(0)) +

  theme(panel.spacing.y = unit(0.05, "lines"),
        panel.spacing.x = unit(0, "lines"),
        axis.title = element_text(face = "bold", size = 12),
        axis.ticks = element_blank(),
        axis.line = element_line(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, size = 12, vjust = 0),
        strip.text.x = element_blank(),
        plot.margin = unit(c(0,0.5,0,0), "cm"),
        panel.border= element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust =  - 0.5),
        )
)

ml <- marrangeGrob(pl,  ncol = ncol, nrow = nrow)

if (export) {
ggsave(filename, ml, dpi = dpi, width = width, height = height)
} else {ml}

}
