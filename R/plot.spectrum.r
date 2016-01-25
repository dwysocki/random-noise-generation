#' Plots a power spectrum and time series side-by-side, saving as a
#' PDF.
#'
#' @param power.spectrum Data frame containing freq and power.
#' @param time.series Data frame containing time and signal.
#' @param output Name of PDF output file.
#'
#' @export
plot.spectrum <- function(power.spectrum, time.series,
                          output = "spectrum_plot.pdf") {
    pdf(output)

    par(mfrow = c(1, 2))

    plot(power.spectrum$freq, Mod(power.spectrum$power)^2,
         type = "l", color = "blue")
    plot(time.series$time, time.series$signal,
         type = "l", color = "blue")

    dev.off()
}
