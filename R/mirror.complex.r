#' Takes a complex valued vector and returns a new vector, which is
#' mirrored at index 0, but with the mirrored values having opposite
#' imaginary parts.
#'
#' TODO: Explain this better.
#'
#' @param z Complex valued vector.
#'
#' @return Mirrored vector.
#'
#' @examples
#' mirror.complex(c(1+1i, 2+2i)) # => c(2-2i, 1-1i, 1+1i, 2+2i)
#'
#' @export
mirror.complex <- function(z) {
    c(rev(z)*-1i, z)
}
