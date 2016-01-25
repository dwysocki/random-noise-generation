#' Generate a noisy power law power spectrum.
#'
#' \deqn{PSD(f) = f^{-\beta}}
#'
#' @param N Number of frequencies (must be even).
#' @param freq.max Value of largest frequency.
#' @param beta Value of power.
#' @param sd Standard deviation.
#'
#' @return Frequencies and power spectral density.
#'
#' @examples
#' power.law(100)                # 100 samples with default options
#' power.law(100, freq.max = 50) # provide maximum frequency
#' power.law(100, sd = 2.0)      # provide standard deviation
#' power.law(100, beta = 0.0)    # white noise
#' power.law(100, beta = 1.0)    # 1/f noise
#' power.law(100, beta = 2.0)    # brown noise
#'
#' @export
power.law <- function(N, freq.max = 1/(2*pi),
                      beta = 0.0, sd = 1.0) {
    f <- freq(N, freq.max = freq.max)
    freq.pos <- f$freq.pos
    freq     <- f$freq

    # compute the mean values of the power spectrum's absolute value
    mean.power <- freq.pos^(0.5*beta)

    # generate a noisy power spectrum in complex form
    power <- noisy.power.spectrum(mean.power, sd = sd)
    # include negative frequencies in the final spectrum, with opposite phase
    power <- mirror.complex(power)

    data.frame(freq, power)
}
