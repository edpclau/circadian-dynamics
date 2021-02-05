#' Phase plots for all individuals
#'
#' @param path directory for where to save the plots
#' @param analysis output from the analysis made with Circadian Dynamics
#'
#' @return a pdf with plots
#' @export
#'
#' @examples
#' plot_phase(path = choose.dir(), analysis = analysis_data)
#'
plot_phase <- function(path = getwd(), analysis = NULL) {

samples <- names(analysis)


### Setting up the pdf dimensions
pdf(file = paste0(path,"/phase_plots.pdf"))
par(mar=c(2,4,1.75,2), mfrow = c(4,4))



## For loop to plot all samples
for (sample in samples) {

#Selecting the data for plotting
to_plot <- filter(analysis[[sample]], method == "lomb_scargle") %>%
  select(window, window_starts, window_ends, phase_in_seconds, phase_se_seconds)


# Plot the phase using steps
plot(x = to_plot$window, y = to_plot$phase_in_seconds, type = "s",
     main = paste(sample), xlab = "Window", ylab = "Phase in Hours", xaxt = "n")
# X-axis specifications. Jump every 2 days
x_labs = seq(min(to_plot$window), max(to_plot$window), by = 2)
axis(1, at = x_labs)

}
#Build the PDF
dev.off()

}
