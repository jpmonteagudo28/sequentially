#' Interpolate a sequence of values with Easing or Stepping Given Data Points
#'
#' This function generates a sequence of values based on a specified easing or stepping function.
#' It supports linear, polynomial, exponential, and other smooth transitions, as well as stepped transitions.
#'
#' @param data Numeric vector, matrix, data frame, or list. The input data to be used for generating the sequence.
#' @param type Character string specifying the type of sequence. Supported types include:
#'   \itemize{
#'     \item `"linear"`: Linear interpolation.
#'     \item `"quad"`: Quadratic easing.
#'     \item `"cubic"`: Cubic easing.
#'     \item `"quart"`: Quartic easing.
#'     \item `"quint"`: Quintic easing.
#'     \item `"exp"`: Exponential easing.
#'     \item `"circle"`: Circular easing.
#'     \item `"back"`: Back easing with overshoot.
#'     \item `"elastic"`: Elastic easing with oscillation.
#'     \item `"sine"`: Sine wave easing.
#'     \item `"bounce"`: Bouncing easing.
#'     \item `"step"`: Stepped transitions.
#'   }
#'   Defaults to `"linear"`.
#' @param step_count Integer specifying the number of steps for the `"step"` type. Must be between 1 and the length of `data`. Defaults to `NULL`.
#' @param ease Character string specifying the direction of easing. Supported values are:
#'   \itemize{
#'     \item `"in"`: Easing starts slow and accelerates.
#'     \item `"out"`: Easing starts fast and decelerates.
#'     \item `"in_out"`: Easing combines both behaviors.
#'   }
#'   Applicable only for non-linear types. Defaults to `NULL`.
#'
#' @return A numeric vector containing the generated sequence.
#'   \itemize{
#'     \item For `"linear"`, a smoothly interpolated sequence is returned.
#'     \item For `"step"`, a sequence with distinct steps is generated.
#'     \item For other easing types, the sequence follows the specified smooth transition curve.
#'   }
#'
#' @details
#' The `seq_data` function calculates a sequence of values based on the specified `type` and `ease`.
#' The `data` input is used to determine the range (minimum and maximum) of the sequence to then be interpolated, and the resulting
#' sequence is normalized between 0 and 1 before applying the specified easing or stepping function.
#'
#' For `"step"` type, the number of steps can be controlled using `step_count`. The `ease` parameter has no effect
#' when `type` is `"linear"` or `"step"`.
#'
#' @examples
#' # Generate a linear sequence
#' interpolate(1:10, type = "linear")
#'
#' # Generate a quadratic easing sequence
#' interpolate(rnorm(100,14,5), type = "quad", ease = "in_out")
#'
#' # Generate a stepped sequence with 5 steps
#' interpolate(rpois(100,3), type = "step", step_count = 5)
#'
#' @note
#' This function supports various easing functions commonly used in animations and graphics, as well as
#' stepped sequences for discrete transitions. Invalid or unsupported inputs will result in informative
#' error messages or warnings.
#'
#' @seealso [func(approx)]
#' @export

interpolate <- function(data,
                            type = "linear",
                            step_count = NULL,
                            ease = NULL){

  stopifnot(is.character(type),
            is.numeric(data) || is.matrix(data) || is.data.frame(data) || is.list(data) || is.sequence(data))

  if(!is.null(ease))
    if(!is.character(ease))
      stop("Ease must be a characer string of length 1")

  type <- match.arg(type,c("linear","quad","cubic","quart",
                           "quint","exp","circle","back",
                           "elastic","sine","bounce","step"))

  # Compute normalized time (t) as the y-component
  # Time could be any range, but it complicates comparison if
  # time range is not bounded. However, you can always
  # normalize it to be bounded from [0,1]
  if (is.numeric(data)|| is.sequence(data)) {
    n <- length(data)
  } else if (is.data.frame(data) || is.matrix(data)) {
    n <- nrow(data)
  } else if (is.list(data)) {
    n <- unique(lengths(data))
  } else {
    stop("Unsupported data type: data must be numeric, a data.frame, a matrix, or a list.")
  }

  from <- min(data)
  to <- max(data)

  t <- seq(0,1,length.out = n)

  # Default sequence
  if(type == "linear") {
    seq <- from + t*(to-from)
    return(seq)
  }

  # `in` curves it at the start
  #`out` will curve the line at the end
  #`in_out` will curve the line at both ends
  # Keep in mind there are n - 1 critical points
  # as polynomials of size n increases.

  # Issue warning if 'ease' not set to NULL when type is linear
  if (type != "linear" && type != "step") {
    ease <- match.arg(ease, c("in", "out", "in_out"))
  }

  # Compute normalized time
  t <- seq(0, 1, length.out = n)

  # Linear sequence
  if (type == "linear") {
    seq <- from + t * (to - from)
    return(seq)
  }

  # Step sequence
  if (type == "step") {

    # Handle null or invalid step_count
    if (is.null(step_count)) {
      warning("Step count is 'NULL'. Using default 'step_count' = 4.")
      step_count <- 4
    }

    # Check step_count limits
    if (step_count < 1) {
      stop("Invalid 'step_count': Minimum number of steps is 1. Provided: ", step_count)
    }
    if (step_count > n) {
      stop("Invalid 'step_count': Number of steps (", step_count,
           ") cannot exceed the length of the numeric vector (n = ", n, ").")
    }

    # Warn if 'ease' is provided (not applicable for steps)
    if (!is.null(ease) && !is.na(ease)) {
      warning("'ease' has no effect on step functions. Step function is not continuous.")
    }

    # Compute step sequence
    smooth_seq <- from + (to - from) * round(step_count * t) / step_count

    return(smooth_seq)
  }


  # What type of sequence and direction to compute
  smooth_fashion <- join_char(type,"_",ease)


  smooth_fashion <- switch(
    smooth_fashion,
    #---- --- ---- --- ---- --- ---- --- ----- --- ----#
    # Notice how we use base '2' and exponentiate as
    # polynomial increases by n. You can use any base you
    # like, I chose 2 because that's what I've seen others
    # do, and it's the standard as far as I know.
    quad_in = t^2,
    quad_out = 1-(1 - t)^2,
    quad_in_out = ifelse(t < 0.5,
                         2*t^2,
                         1 - 0.5*(-2*t+2)^2),
    #---- --- ---- --- ---- --- ---- --- ----- --- ----#
    cubic_in = t^3,
    cubic_out = 1 - (1-t)^3,
    cubic_in_out = ifelse(t < 0.5,
                          4*t^3,
                          1- 0.5*(-2*t+2)^3),
    #---- --- ---- --- ---- --- ---- --- ----- --- ----#
    quart_in = t^4,
    quart_out = 1 - (1-t)^4,
    quart_in_out = ifelse(t < 0.5,
                          8*t^4,
                          1 - 0.5*(-2*t+2)^4),
    #---- --- ---- --- ---- --- ---- --- ----- --- ----#
    quint_in = t^5,
    quint_out = 1 - (1-t)^5,
    quint_in_out = ifelse(t < 0.5,
                          16*t^5,
                          1 - 0.5*(-2*t+2)^5),
    #---- --- ---- --- ---- --- ---- --- ----- --- ----#
    exp_in = 2^(10*t - 10),
    exp_out = 1 - 2^(-10*t),
    exp_in_out = ifelse(t == 0,0,
                        ifelse(t == 1,1,
                               ifelse(t < 0.5,
                                      2^(20*t-10)/2,
                                      (2 - 2^(-20*t+10))/2
                               ))),
    #---- --- ---- --- ---- --- ---- --- ----- --- ----#
    circle_in = 1 - sqrt(1-t^2),
    circle_out = sqrt(1 - (t - 1)^2),
    # Supress warnings about NA's
    circle_in_out = supressWarnings({ifelse(t < 0.5,
                           (1 - sqrt(1 - (2 * t)^2)) / 2,
                           0.5 * (sqrt(1 - (-2 * t + 2)^2) + 1))}),
    #---- --- ---- --- ---- --- ---- --- ----- --- ----#
    back_in = 2.70158*t^3 - 1.70158*t^2,
    back_out = 1 + 2.70158* (t-1)^3 + 1.70158*(t-1)^2,
    back_in_out = {
      k <- 1.70158
      k2 <- k * 1.525
      ifelse(t < 0.5,
             (2*t)^2 * ((k2 + 1) * 2 * t - k2) / 2,
             ((2*t-2)^2 * ((k2 + 1) * (t * 2 - 2) + k2) + 2) / 2
      )
    },
    #---- --- ---- --- ---- --- ---- --- ----- --- ----#
    elastic_in = ifelse(t == 0, 0,
                        ifelse(t == 1,1,
                               -(2 ^ (10 * t - 10)) * sin((t * 10 - 10.75) * 2 * pi / 3)
                        )
    ),
    elastic_out = ifelse(t == 0,0,
                         ifelse(t ==1,1,
                                2^(-10*t)*sin((t*10-0.75)*2*pi/3)+1)
    ),
    elastic_in_out = ifelse(
      t == 0, 0,
      ifelse(t == 1, 1,
             ifelse(t < 0.5,
                    -(2^( 20*t - 10) * sin((20 * t - 11.125) * 2 * pi/4.5)) / 2,
                    (2^(-20*t + 10) * sin((20 * t - 11.125) * 2 * pi/4.5)) / 2 + 1
             ))
    ),
    #---- --- ---- --- ---- --- ---- --- ----- --- ----#
    sine_in = 1 - cos((t*pi)/2),
    sine_out = sin((t*pi)/2),
    sine_in_out = -(cos(t*pi)-1)/2,
    #---- --- ---- --- ---- --- ---- --- ----- --- ----#
    bounce_in = 1 - ifelse((1 - t) < 0.3636,
                           7.5625 * (1 - t)^2,
                           ifelse((1 - t) < 0.7273,
                                  7.5625 * ((1 - t) - 1.5 / 2.75)^2 + 0.75,
                                  ifelse((1 - t) < 0.9091,
                                         7.5625 * ((1 - t) - 2.25 / 2.75)^2 + 0.9375,
                                         7.5625 * ((1 - t) - 2.625 / 2.75)^2 + 0.984375))
    ),
    bounce_out = ifelse(t < 0.3636,
                        7.5625 * t^2,
                        ifelse(t < 0.7273,
                               7.5625 * (t - 1.5 / 2.75)^2 + 0.75,
                               ifelse(t < 0.9091,
                                      7.5625 * (t - 2.25 / 2.75)^2 + 0.9375,
                                      7.5625 * (t - 2.625 / 2.75)^2 + 0.984375))
    )
    ,
    bounce_in_out = ifelse(t < 0.5,
                           0.5 * ifelse(t * 2 < 0.3636,
                                        7.5625 * (2 * t)^2,
                                        ifelse(t * 2 < 0.7273,
                                               7.5625 * ((2 * t) - 1.5 / 2.75)^2 + 0.75,
                                               ifelse(t * 2 < 0.9091,
                                                      7.5625 * ((2 * t) - 2.25 / 2.75)^2 + 0.9375,
                                                      7.5625 * ((2 * t) - 2.625 / 2.75)^2 + 0.984375))),
                           0.5 * ifelse((2 * t - 1) < 0.3636,
                                        7.5625 * (2 * t - 1)^2,
                                        ifelse((2 * t - 1) < 0.7273,
                                               7.5625 * ((2 * t - 1) - 1.5 / 2.75)^2 + 0.75,
                                               ifelse((2 * t - 1) < 0.9091,
                                                      7.5625 * ((2 * t - 1) - 2.25 / 2.75)^2 + 0.9375,
                                                      7.5625 * ((2 * t - 1) - 2.625 / 2.75)^2 + 0.984375))) + 0.5
    )
  )

  smooth_seq <- from + smooth_fashion * (to-from)

  return(smooth_seq)
}
