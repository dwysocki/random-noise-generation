#' Generate a vector of frequencies.
#'
#' @param N Number of elements in vector (must be even).
#' @param freq.max Value of largest frequency.
#'
#' @return Vector of frequencies.
#'
#' @examples
#' freq(10, freq.max = 5) # => c(-5, -4, -3, -2, -1, 1, 2, 3, 4, 5)
#'
#' @export
freq <- function(N, freq.max = 1/(2*pi)) {
    ## Input Validation ##

    if (N %% 2) {
        stop("'N' must be even")
    }


    ## Setup ##

    # Precompute N/2, which we know to be an integer.
    half.N <- N / 2
    # Increment between freq[i] and freq[i+1].
    freq.step <- freq.max / half.N


    ## Frequency Vectors ##

    # Obtain the positive frequencies by counting from the smallest
    # one to the maximum. Start counting from 'freq.step', which is
    # the increment between frequencies, as zero frequency is
    # undefined.
    freq.pos <- seq(from = freq.step,
                    to = freq.max,
                    length.out = half.N)
    # Obtain the negative frequencies by reversing the order on the
    # positive ones, and negating them.
    freq.neg <- -rev(freq.pos)
    # Concatenate the negative and positive frequencies to get the
    # full vector of frequencies.
    freq <- c(freq.neg, freq.pos)


    ## Return ##

    list(
      freq     = freq,
      freq.pos = freq.pos,
      freq.neg = freq.neg
    )
}
