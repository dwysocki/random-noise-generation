#' Transform a power spectrum into a time series, using an inverse
#' fast Fourier transform.
#'
#' @param freq The frequency.
#' @param power The power spectral distribution.
#'
#' @return Time series.
#'
#' @export
time.series <- function(freq, power) {
    time   <- as.time(Re(freq))
    signal <- fft(power, inverse = TRUE)

    data.frame(time, signal)
}
