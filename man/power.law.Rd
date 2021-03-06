% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/power.law.r
\name{power.law}
\alias{power.law}
\title{Generate a noisy power law power spectrum.}
\usage{
power.law(N, freq.max = 1/(2 * pi), beta = 0, sd = 1)
}
\arguments{
\item{N}{Number of frequencies (must be even).}

\item{freq.max}{Value of largest frequency.}

\item{beta}{Value of power.}

\item{sd}{Standard deviation.}
}
\value{
Frequencies and power spectral density.
}
\description{
\deqn{PSD(f) = f^{-\beta}}
}
\examples{
power.law(100)                # 100 samples with default options
power.law(100, freq.max = 50) # provide maximum frequency
power.law(100, sd = 2.0)      # provide standard deviation
power.law(100, beta = 0.0)    # white noise
power.law(100, beta = 1.0)    # 1/f noise
power.law(100, beta = 2.0)    # brown noise

}

