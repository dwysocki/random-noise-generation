#!/usr/bin/env Rscript

suppressWarnings(suppressMessages(library(argparse)))


as.time <- function(freq) {
    freq <- Re(freq)

    N <- length(freq)

    freq.max <- max(freq)

    time.step <- 1 / (2*freq.max)
    time.max <- N * time.step

    time <- seq(time.step, time.max, time.step)
}

power.spectrum <- function(N, freq.max = 1/(2*pi), beta = 1.0, sd = 1.0) {
    # generate the positive frequencies
    freq.step <- freq.max / (N%/%2)
    freq.pos  <- seq(from = freq.step, to = freq.max, length.out = N%/%2)
    # generate both positive and negative frequencies
    freq <- c(-rev(freq.pos), freq.pos)

    # compute the mean values of the power spectrum's absolute value
    mean.power <- freq.pos^(beta/2)

    # generate a noisy power spectrum in complex form
    A   <- rnorm(mean.power, mean = mean.power, sd = sd)
    phi <- runif(mean.power, min = 0, max = 2*pi)
    z   <- A * exp(1i * phi)

    # include negative frequencies in the final spectrum, with opposite phase
    z <- c(rev(z)*-1i, z)

    # return the power spectrum along with the frequencies
    rbind(freq, z)
}


time.series <- function(power.spectrum) {
    freq  <- power.spectrum[1,]
    power <- power.spectrum[2,]

    time   <- as.time(Re(freq))
    signal <- fft(power, inverse = TRUE)

    rbind(time, signal)
}


plot.spectrum <- function(power.spectrum, time.series) {
    pdf("power_law_noise.pdf")

    par(mfrow = c(1, 2))

    freq  <- power.spectrum[1,]
    power <- power.spectrum[2,]

    time   <- time.series[1,]
    signal <- time.series[2,]

    plot(freq, Mod(power)^2,
         type = "l", color = "blue")
    plot(time, signal,
         type = "l", color = "blue")

    dev.off()
}


main <- function() {
    # hard code values for now, fix later
    freq.max <- 100
    N <- 200

    beta <- 2 # brown noise
    sd <- 1.0

    power.spectrum <- power.spectrum(N, freq.max = freq.max,
                                     beta = beta, sd = sd)
    time.series <- time.series(power.spectrum)

    plot.spectrum(power.spectrum, time.series)
}


main()
