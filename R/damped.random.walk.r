#' Generate a damped random walk power spectrum.
#'
#' \deqn{PSD(f) = \frac{\tau^2 SF_\infty^2}{1 + (2 \pi f \tau)^2}}
#'
#' @param N Number of frequencies (must be even).
#' @param freq.max Value of largest frequency.
#' @param tau Characteristic timescale.
#' @param SF Value of structure function over long time scales.
#' @param sd Standard deviation.
#'
#' @return Frequencies and power spectral density.
#'
#' @examples
#' damped.random.walk(100)                # 100 samples with default options
#' damped.random.walk(100, freq.max = 50) # provide maximum frequency
#' damped.random.walk(100, sd = 2.0)      # provide standard deviation
#' damped.random.walk(100, tau = 2.0)     # provide characteristic timescale
#' damped.random.walk(100, SF = 2.0)      # provide value of structure function
#'
#' @export
damped.random.walk <- function(N, freq.max = 1/(2*pi),
                           tau = 1.0, SF = 1.0, sd = 1.0) {
    f <- freq(N, freq.max = freq.max)
    freq.pos <- f$freq.pos
    freq     <- f$freq

    # compute the mean values of the power spectrum's absolute value
    mean.power <- tau * SF * (1 + (2*pi * freq.pos*tau)^2)^-0.5

    # generate a noisy power spectrum in complex form
    z <- noisy.power.spectrum(mean.power, sd = sd)
    # include negative frequencies in the final spectrum, with opposite phase
    z <- mirror.complex(z)

    # return the power spectrum along with the frequencies
    rbind(freq, z)
}
