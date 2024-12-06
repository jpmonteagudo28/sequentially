#' Generate Non-linear Smooth Sequences with Custom Easing
#'
#' The `seq_smooth` function generates a sequence of numbers between a specified range (`from` to `to`) using different smoothing techniques and easing types. It supports various interpolation methods such as linear, quadratic, cubic, and more. Additionally, easing options (`in`, `out`, and `in_out`) allow for customization of how the transition occurs across the range.
#'
#' @param from A numeric value specifying the starting value of the sequence. Default is 1.
#' @param to A numeric value specifying the ending value of the sequence. Default is 1.
#' @param n An integer specifying the number of points in the sequence. Default is 100.
#' @param type A character string indicating the type of smoothing to apply. Available options include `"linear"`, `"quad"`, `"cubic"`, `"quart"`, `"quint"`, `"exp"`, `"circle"`, `"back"`, `"elastic"`, `"sine"`, `"bounce"`, and `"step"`. Default is `"linear"`.
#' @param step_count An integer specifying the number of discrete steps for the `"step"` type. If `NULL`, defaults to 4.The number of steps cannot exceed 'n'.
#' @param ease A character string indicating the easing direction to apply. Available options are `"in"` (smooth transition at the start), `"out"` (smooth transition at the end), and `"in_out"` (smooth transition at both start and end). Required for non-linear types. Default is `NULL`.
#'
#' @details
#' The function calculates a sequence based on the specified `type` and applies the easing (`ease`) to modify how values progress. For `"linear"` types, no easing is applied, and the sequence is uniformly spaced. For other types, the function supports easing that modifies the progression curve.
#'
#' - **Linear**: A straight-line interpolation.
#' - **Quadratic to Quintic**: Higher-degree polynomial interpolations.
#' - **Exponential**: Exponential interpolation.
#' - **Elastic and Bounce**: Nonlinear interpolations with oscillations.
#' - **Step**: A step-wise progression with discrete levels.
#'
#' The `ease` parameter controls how values are distributed along the sequence:
#' - `"in"`: Starts slow and accelerates.
#' - `"out"`: Starts fast and decelerates.
#' - `"in_out"`: Combines both for a smooth start and end.
#'
#' For `"step"` type, `step_count` specifies the number of steps in the sequence.
#'
#' @return A numeric vector of length `n`, representing the non-linear, smoothed sequence.
#'
#' @examples
#' # Linear sequence from 0 to 10
#' t <- seq(0,1,length.out = 100)
#' lin_seq <- seq_smooth(0, 10, n = 100, type = "linear")
#' plot.new()
#' plot.window(range(t),range(lin_seq))
#' points(t,lin_seq,pch = 16, cex = .75,col = "red")
#' axis(1,tcl = 0.75,lwd = 0, family = "serif")
#' axis(2,lwd = 0, family = "serif", las = 1)
#' grid(2,3,col = "gray80",lty = "dotted", lwd = 0.50)
#' mtext("Linear Sequence",3,cex = 1.3, family = "serif")
#'
#' # Quadratic easing in sequence
#' quad_seq <- seq_smooth(0, 10, n = 100, type = "quad", ease = "in")
#' plot.new()
#' plot.window(range(t),range(quad_seq))
#' points(t,quad_seq,pch = 16, cex = .75,col = "red")
#' axis(1,tcl = 0.75,lwd = 0, family = "serif")
#' axis(2,lwd = 0, family = "serif", las = 1)
#' grid(2,3,col = "gray80",lty = "dotted", lwd = 0.50)
#' mtext("Ease-in Quadratic Sequence",3,cex = 1.3, family = "serif")
#'
#' # Step sequence with 5 steps
#' step_seq <- seq_smooth(0, 10, n = 100, type = "step", step_count = 5)
#' plot.new()
#' plot.window(range(t),range(step_seq))
#' lines(t,step_seq,pch = 16, cex = .75,col = "red")
#' axis(1,tcl = 0.75,lwd = 0, family = "serif")
#' axis(2,lwd = 0, family = "serif", las = 1)
#' grid(2,3,col = "gray80",lty = "dotted", lwd = 0.50)
#' mtext("Step Sequence",3,cex = 1.3, family = "serif")
#'
#' # Elastic easing out sequence
#' elastic_seq <- seq_smooth(0, 10, n = 100, type = "elastic", ease = "out")
#' plot.new()
#' plot.window(range(t),range(elastic_seq))
#' points(t,elastic_seq,pch = 16, cex = .75,col = "red")
#' axis(1,tcl = 0.75,lwd = 0, family = "serif")
#' axis(2,lwd = 0, family = "serif", las = 1)
#' grid(2,3,col = "gray80",lty = "dotted", lwd = 0.50)
#' mtext("Ease-out Elastic Sequence",3,cex = 1.3, family = "serif")
#'
#' @export


seq_smooth <- function(from = 1, to = 1,
                     n = 100,
                     type = "linear",
                     step_count = NULL,
                     ease = NULL){

stopifnot(is.character(type))

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
    circle_in_out = ifelse(t < 0.5,
                           (1 - sqrt(1 - (2 * t)^2)) / 2,
                           0.5 * (sqrt(1 - (-2 * t + 2)^2) + 1)),
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
    elastic_in =  ifelse(t == 0, 0,
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
