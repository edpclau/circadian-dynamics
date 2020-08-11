#' Plot the Autocorrelation by windows
#'
#' @usage plot_autocorrelation(df, path = NULL, filename = "actogram.pdf", dir_choose_gui = TRUE,
#' export = TRUE, window_of_interest = NULL)
#'
#' @param df a data.frame containing the output from acf_window.
#' @param path a string containing a path in which to save the file. It is not necesssary if dir_choose_gui = TRUE.
#' @param filename a string containing a name for the file. It must end in ".pdf". Default = "actogram.pdf".
#' @param dir_choose_gui A gui for choosing the directory/folder in which to save the file. If, false, a path
#' is needed. Default = TRUE.
#' @param export If FALSE, will plot the figures in the console. If TRUE, will export the figures as a .pdf file.
#' @param window_of_interest optional. a number or list of numbers which indicate the windows to be plotted.
#'
#' @return
#' @export
#'re
#'
plot_autocorrelation <- function(df, path = NULL, filename = "actogram.pdf", dir_choose_gui = TRUE,
                                 export = TRUE, window_of_interest = NULL) {
##### Flow control #####
#Change name of df internally
acf_results <- df

# Restrict the windows to be plotted
if (!is.null(window_of_interest)) {
  acf_results <- dplyr::filter(acf_results, window %in% window_of_interest)
}





####### Plot autocorrelation ######



# 1. First:  Run the cross-correlation
autocor <-  purrr::map2(.x = acf_results$period, .y = acf_results$acf_input_values,
         .f = ~ if(is.na(.x)) {NA} else {  ccf(x = .y,
                                                  y = .y,
                                                  na.action = na.pass,
                                                  type = "correlation",
                                                  lag.max = length(unlist(.y)),
                                                  plot = FALSE
                                                  )})

names(autocor) <- acf_results$window

# transform the results of the cross-correlation so that we can join it to our acf_results df
autocor_df <- purrr::map_if(names(autocor),
       .p = ~ !is.na(autocor[.]),
       .f = ~ tibble::tibble(window = as.numeric(.), lag = autocor[[.]][[4]], acf = autocor[[.]][[1]]),
       .else = ~ tibble::tibble(window = as.numeric(.), lag = NA, acf = NA))

#join the autocor_df to acf_results
acf_df <- dplyr::bind_rows(autocor_df) %>% tidyr::nest(acf = c("lag", "acf")) %>% right_join(acf_results, by = "window")

acf_df <- map(acf_df$window, ~ filter(acf_df, window == .))
names(acf_df) <- acf_results$window




## Publish the figures
if (export) {

  #Directory Management
  if (dir_choose_gui) {
    dir <- selectDirectory()
    filename <- paste0(dir, "/", filename)
  }  else {
    filename <- paste0(path, "/", filename)
  }

# Assign a file name
pdf(file = filename)


}

#Map_if runs the iterations
test <- purrr::map_if(names(acf_df),

    # if there is a period, plot the auto-correlation
      .p = ~ !is.na(acf_df[[.]][["period"]]),

    #plotting the auto-correlation
    .f =  ~ {

      lag <- acf_df[[.]][[2]][[1]][["lag"]]
      acf_coeff <- acf_df[[.]][[2]][[1]][["acf"]]

    plot(x = lag, #x = lag
         y = acf_coeff, # y = acf coefficients
         type = "l",
         col = "blue",
         main = paste("Window", acf_df[[.]][["window"]]),
         xlab = "Lag",
         ylab = "ACF Coeffient",
         xaxt = "n")

    #Drawing the window where we searched for peaks
    abline(v = acf_df[[.]][["from_acf"]])
    abline(v = acf_df[[.]][["to_acf"]])

    #Handling the x axis ticks
    axis(1, at = seq(min(lag), max(lag), by = 4), labels = FALSE)
    axis(1, at = seq(min(lag), max(lag), by = 12), labels = seq(min(lag), max(lag), by = 12))

    #Calculate and add confidence intervals
    error <- 2/sqrt(length(acf_coeff))
    abline(h = mean(acf_coeff) - error, lty=2, col = "red")
    abline(h = mean(acf_coeff) + error, lty=2, col = "red")


    # Draw the peak we used to identify the period
    points(
      x= acf_df[[.]][["peak_lags"]],
      y= acf_df[[.]][["peaks"]],
      pch=4,
      cex=3,
      col = 'red')

    # print additional info. Period in hours and the correlation coefficient.
    legend("topright",
           legend = c(paste("Period = ", acf_df[[1]][["period_hours"]]),
                      paste("C.C. =",round(acf_df[[1]][["autocorrelation_power"]], digits = 3)),
                      paste("R.S. = ",round(acf_df[[.]][["rythm_strength"]], digits = 3))),
           bty = "n",
           cex = 1)

  },

  # If the period is NA, print an empty figure
  .else = ~ {

    acf_input_values <- acf_df[[.]][["acf_input_values"]][[1]][["values"]]

    plot(x = seq(-length(acf_input_values), length(acf_input_values), by = 1),
         y = rep(0, (length(acf_input_values)*2) + 1),
         type = "l",
         col = "blue",
         main = paste("Window", acf_df[[.]][["window"]]),
         xlab = "Lag",
         ylab = "ACF Coeffiecient")

    abline(v = acf_df[[.]][["from_acf"]])
    abline(v = acf_df[[.]][["to_acf"]])

    legend("topright",
           legend = c(paste("Period = ", acf_df[[.]][["period_hours"]]),
                      paste("C.C. =",round(acf_df[[.]][["autocorrelation_power"]], digits = 3)),
                      paste("R.S. = ",round(acf_df[[.]][["rythm_strength"]], digits = 3))),
           bty = "n",
           cex = 1)


  }

  )

if (export) {
dev.off()
}

}

