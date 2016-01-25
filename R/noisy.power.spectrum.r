#' Takes a power spectrum and makes it noisy, by adding white noise to
#' its amplitude, and introducing random, uniformly distributed
#' phases.
#'
#' \deqn{A(f) \sim \mathcal{N}(PSD(f), \sigma^2)}
#' \deqn{\phi(f) \sim \mathcal{U}(0, 2\pi)}
#' \deqn{z(f) = A(f) \exp(i \phi(f))}
#'
#' @param power Power spectral density.
#' @param sd Standard deviation of white noise.
#'
#' @return Power with noise added.
#'
#' @export
noisy.power.spectrum <- function(power, sd = 1.0) {
    A   <- rnorm(power, mean = power, sd = sd)
    phi <- runif(power, min = 0, max = 2*pi)

    A * exp(1i * phi)
}
