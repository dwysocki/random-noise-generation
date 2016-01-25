#' Convert a vector of frequencies into a vector of times.
#'
#' @param freq Vector of frequencies.
#'
#' @return Vector of times.
#'
#' @examples
#' as.time(c(-3, -2, -1, +1, +2, +3))
#'
#' @export
as.time <- function(freq) {
    freq <- Re(freq)

    N <- length(freq)

    freq.max <- max(freq)

    time.step <- 1 / (2*freq.max)
    time.max <- N * time.step

    seq(time.step, time.max, time.step)
}
