% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seq_data_nl.R
\name{interpolate}
\alias{interpolate}
\title{Interpolate a sequence of values with Easing or Stepping Given Data Points}
\usage{
interpolate(data, type = "linear", step_count = NULL, ease = NULL)
}
\arguments{
\item{data}{Numeric vector, matrix, data frame, or list. The input data to be used for generating the sequence.}

\item{type}{Character string specifying the type of sequence. Supported types include:
\itemize{
\item \code{"linear"}: Linear interpolation.
\item \code{"quad"}: Quadratic easing.
\item \code{"cubic"}: Cubic easing.
\item \code{"quart"}: Quartic easing.
\item \code{"quint"}: Quintic easing.
\item \code{"exp"}: Exponential easing.
\item \code{"circle"}: Circular easing.
\item \code{"back"}: Back easing with overshoot.
\item \code{"elastic"}: Elastic easing with oscillation.
\item \code{"sine"}: Sine wave easing.
\item \code{"bounce"}: Bouncing easing.
\item \code{"step"}: Stepped transitions.
}
Defaults to \code{"linear"}.}

\item{step_count}{Integer specifying the number of steps for the \code{"step"} type. Must be between 1 and the length of \code{data}. Defaults to \code{NULL}.}

\item{ease}{Character string specifying the direction of easing. Supported values are:
\itemize{
\item \code{"in"}: Easing starts slow and accelerates.
\item \code{"out"}: Easing starts fast and decelerates.
\item \code{"in_out"}: Easing combines both behaviors.
}
Applicable only for non-linear types. Defaults to \code{NULL}.}
}
\value{
A numeric vector containing the generated sequence.
\itemize{
\item For \code{"linear"}, a smoothly interpolated sequence is returned.
\item For \code{"step"}, a sequence with distinct steps is generated.
\item For other easing types, the sequence follows the specified smooth transition curve.
}
}
\description{
This function generates a sequence of values based on a specified easing or stepping function.
It supports linear, polynomial, exponential, and other smooth transitions, as well as stepped transitions.
}
\details{
The \code{seq_data} function calculates a sequence of values based on the specified \code{type} and \code{ease}.
The \code{data} input is used to determine the range (minimum and maximum) of the sequence to then be interpolated, and the resulting
sequence is normalized between 0 and 1 before applying the specified easing or stepping function.

For \code{"step"} type, the number of steps can be controlled using \code{step_count}. The \code{ease} parameter has no effect
when \code{type} is \code{"linear"} or \code{"step"}.
}
\note{
This function supports various easing functions commonly used in animations and graphics, as well as
stepped sequences for discrete transitions. Invalid or unsupported inputs will result in informative
error messages or warnings.
}
\examples{
# Generate a linear sequence
interpolate(1:10, type = "linear")

# Generate a quadratic easing sequence
interpolate(rnorm(100,14,5), type = "quad", ease = "in_out")

# Generate a stepped sequence with 5 steps
interpolate(rpois(100,3), type = "step", step_count = 5)

}
\seealso{
\link{func(approx)}
}
