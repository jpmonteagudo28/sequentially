# NOT READY YET
# find_common_diff <- function(.x,
#                              type = "linear") {
#
#   stopifnot(is.sequence(.x) ||is.numeric(.x))
#
#   type <- match.arg(type,c("linear","quad","cubic","quart",
#                            "quint","exp","circle","back",
#                            "elastic","sine","bounce","step"))
#   if(type == "linear"){
#     m <- diff(.x)
#     dif <- .x[2] - .x[1]
#
#     # Check if all elements are equal w/i given tolerance limits
#     if(all(abs(m - dif)) < 1e-10){
#       m <- m[1]
#     } else {
#       m <- get_mode(m)  # Mode of the differences if the sequence isn't regular
#     }
#
#     seq_diff <- switch(
#       type,
#       quad = iterate_diff(x),
#       cubic = iterate_diff(x),
#       quartic = iterate_diff(x),
#       quint = iterate_diff(x),
#
#     )
#     return(m)
#   }
#
# }

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
#' Find the Polynomial Degree of a Sequence
#'
#' This function determines the polynomial degree of a numeric sequence. The degree is determined by the number of times you must take differences of the sequence until you obtain a constant sequence (or a sequence that can no longer be differentiated).
#'
#' @param x A numeric vector or an object of class \code{Sequence}.
#'   The sequence for which the polynomial degree is determined.
#'
#' @return An integer representing the degree of the polynomial. If the sequence is constant, the degree will be 0.
#'
#' @details
#' The function repeatedly takes the differences of the input sequence until it becomes constant. The degree is incremented each time a new set of differences is computed.
#'
#' @examples
#' find_polynomial_degree(c(1, 4, 9, 16))  # Output will be 2 (quadratic)
#' find_polynomial_degree(c(2, 4, 6, 8))   # Output will be 1 (linear)
#' find_polynomial_degree(c(5, 5, 5, 5))   # Output will be 0 (constant)
#'
#' @seealso \code{\link{iterate_diff}}
#' @export
#'
find_polynomial_degree <- function(x) {

  if(!is.numeric(x))
    stop("Vector must be an object of class 'Sequence' or 'numeric'")

  # Check if the input sequence is valid
  if (length(x) < 2) {
    stop("Sequence must have at least two elements.")
  }

  if (all_the_same(x)) {
    return(0)  # Constant sequence
  }

  differences <- x
  degree <- 1

  while (length(differences) > 1) {
    differences <- diff(differences)

    # Check if the differences are constant
    if (all_the_same(differences)) {
      return(degree)
    }
    degree <- degree + 1
  }

  return(degree) # If all differences reduce to a single constant value
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
#' Compute the Differences of a Sequence Based on Its Polynomial Degree
#'
#' This function computes the differences of a sequence iteratively based on the polynomial degree of the sequence. It first determines the degree of the sequence and then computes differences for that number of times.
#'
#' @param x A numeric vector or object of class \code{Sequence}.
#'   The sequence for which the differences will be computed.
#'
#' @return A numeric vector representing the sequence after applying the differences iteratively according to its polynomial degree.
#'
#' @details
#' The function first determines the polynomial degree of the input sequence using \code{\link{find_polynomial_degree}}. It then applies the \code{diff} function iteratively, the number of times corresponding to the degree of the sequence.
#'
#' @examples
#' iterate_diff(c(1, 4, 9, 16))  # Output will be a sequence of differences after two iterations
#' iterate_diff(c(2, 4, 6, 8))   # Output will be a sequence of differences after one iteration
#' iterate_diff(c(5, 5, 5, 5))   # Output will be a constant sequence (no change)
#'
#' @seealso \code{\link{find_polynomial_degree}}
#' @export
iterate_diff <- function(x) {

  stopifnot(is.numeric(x))

  if(length(x) < 2)
    stop("Sequence must have at least two elements.")

  if (all_the_same(x)) {
    return(x)  # Constant sequence
  }

  degree <- find_polynomial_degree(x)

  # Compute differences 'degree' number of times
  current_sequence <- x
  for (i in seq_len(degree)) {
    current_sequence <- diff(current_sequence)
  }

  return(current_sequence)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
