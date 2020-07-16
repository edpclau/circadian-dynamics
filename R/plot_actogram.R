#' Export/Plot Actogram
#' @export
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
#' @importFrom ggplot2 ggplot geom_tile aes geom_hline geom_vline labs facet_grid scale_x_datetime scale_y_discrete theme element_blank element_text element_line ggsave unit
#' @importFrom gridExtra marrangeGrob
#' @importFrom purrr map
#' @importFrom dplyr group_by mutate case_when arrange ungroup filter
#' @importFrom stringr str_remove
#' @importFrom magrittr '%>%'
#' @importFrom tidyr gather
#' @importFrom lubridate ddays dminutes
#' @importFrom forcats fct_reorder
plot_actogram <- function(df = NULL, datetime_column = 1, filename = "actogram.pdf", export = FALSE,
                          width = 12, height = 12, dpi = 800, nrow = 5, ncol = 5) {

  ##### Flow Control #####
  if (!is.null(df)) {
    names(df)[datetime_column] <- "datetime"
    df <- df %>% select(datetime, everything())
  } else (
    stop("Must provide a data.frame or tibble.")
  )

# Make date start at midnight
  if (lubridate::hour(min(df$datetime)) != 0) {

    df <- dplyr::filter(df, datetime >= lubridate::ceiling_date(min(df$datetime), unit = "1 day"))
  }

#Wrangle Data

data <- df %>%
  make_time_windows(window_size_in_days = 2, window_step_in_days = 1) %>%
  tidyr::gather("ind", "value", -c(window,datetime)) %>%
  dplyr::group_by(ind, window) %>%
  dplyr::mutate(
         time = format(datetime, "%H:%M:%S") %>%
                strptime("%H:%M:%S"),
         date = dplyr::case_when(
           format(min(datetime), "%d") == format(datetime, "%d") ~ 1,
           format(max(datetime) - lubridate::ddays(1), "%d") == format(datetime, "%d") ~ 2,
           TRUE ~ 3
         )
         )  %>%
  dplyr::ungroup() %>%
  dplyr::mutate(window = as.numeric(window)) %>%
  dplyr::arrange(ind, window, datetime)

data <- dplyr::group_by(data, window, ind) %>%
  dplyr::mutate(value = scale(value, center = FALSE)) %>%
  dplyr::ungroup()
#### Parameters for how the plots will look on the page



pl <- map(.x = unique(data$ind),
           .f = ~ data %>%
    filter(date != 3, ind == .x) %>%

  ggplot(aes(x = time, y = value/2, height = value)) +
  geom_tile(fill = "black", na.rm = TRUE) +

  labs(title = paste(.x), y = "Days", x = "Clock Time (Hr)") +
  geom_hline(yintercept = 0, lty = "solid") +
  geom_vline(xintercept = min(data$time) - lubridate::dminutes(30)) +
  facet_grid(forcats::fct_reorder(factor(window), as.numeric(window)) ~ date,  switch  = "y", drop = FALSE) +


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
