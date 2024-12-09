#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Initially working on making function output an S4 object of class 'sequence'
# but it limited the number of operations and type of manipulation offered for
# S3 objects. If I want to implement the same for an S4 object, it requires a
# whole lot of typing, and some obscure code that it's not so easy to understand.
# I'm leaving this here so I can continue working on it when time permits, and
# I feel in the mood for torture.

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Create a class for non-linear sequence
methods::setClass("Sequence",
                  slots = c(
                    values = "numeric",
                    type = "character",
                    ease = "character",
                    steps = "numeric"
                  ),
                  prototype = list(
                    values = numeric(),
                    type = character(),
                    ease = character(),
                    steps = numeric()
                )
              )

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Set setter and getter for `type` slot
methods::setGeneric("type", function(x) methods::standardGeneric("type"))
methods::setGeneric("type<-", function(x,value) methods::standardGeneric("type<-"))

# Set setter and getter for `ease` slot
methods::setGeneric("ease", function(x) methods::standardGeneric("ease"))
methods::setGeneric("ease<-", function(x,value) methods::standardGeneric("ease<-"))

# Set setter and getter fr `values` slot
methods::setGeneric("values", function(x) methods::standardGeneric("values"))
methods::setGeneric("values<-", function(x,value) methods::standardGeneric("values<-"))

# Set setter and getter fr `steps` slot
methods::setGeneric("steps", function(x) methods::standardGeneric("steps"))
methods::setGeneric("steps<-", function(x,value) methods::standardGeneric("steps<-"))
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Type
methods::setMethod("type","Sequence",function(x) x@type)
methods::setMethod("type<-","Sequence",function(x,value){
  x@type <- value
  x
})
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Ease
methods::setMethod("ease","Sequence",function(x) x@ease)
methods::setMethod("ease<-","Sequence",function(x,value){
  x@ease <- value
  x
})
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Steps
methods::setMethod("steps","Sequence",function(x) x@steps)
methods::setMethod("steps<-","Sequence",function(x,value){
  x@steps <- value
  x
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Define method for 'min'
methods::setMethod("min", "Sequence", function(x) {
  min(x@values)
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
#Define method for 'max'
methods::setMethod("max", "Sequence",
                   function(x, ..., na.rm = FALSE) {
                      max(x@values, ..., na.rm = na.rm)
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Define method for 'range'
methods::setMethod("range","Sequence",
  function(x, ...,na.rm = TRUE) {
    range(x@values, ...,na.rm = TRUE)
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Define method for 'IQR'
methods::setGeneric("IQR", function(x) standardGeneric("IQR"))
methods::setMethod("IQR","Sequence",
  function(x) {
    stats::IQR(x@values)
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Define method for sum
methods::setMethod("sum", "Sequence", function(x, ...) {
  sum(x@values, ...)
})
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Method for subtraction between sequence and integer vector
methods::setMethod("-", c("Sequence","integer"),
                   function(e1,e2) {
                     e1@values <- e1@values - e2
                     validObject(e1) # Validate the modified object
                     e1
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Method for subtraction between two sequences
methods::setMethod("-",c(e1 = "Sequence", e2 = "Sequence"),
  function(e1, e2) {
    if (length(e1@values) != length(e2@values)) {
      stop("The two Sequence objects must have the same length to perform subtraction")
    }
    e1@values <- e1@values - e2@values
    validObject(e1) # Validate the modified object
    e1
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Define method for diff
methods::setMethod("diff", "Sequence", function(x, ...) {
  diff(x@values, ...)
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Method for multiplication
methods::setMethod("*", c("Sequence", "numeric"),
  function(e1, e2) {
    e1@values <- e1@values * e2
    validObject(e1) # Validate the object before returning
    e1
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Method for division
methods::setMethod("/",c("Sequence", "numeric"),
  function(e1, e2) {
    if (any(e2 == 0))
      stop("Division by zero is not allowed")
    e1@values <- e1@values / e2
    validObject(e1) # Validate the object before returning
    e1
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Method for exponentiation
methods::setMethod("^",c("Sequence", "numeric"),
  function(e1, e2) {
    e1@values <- e1@values ^ e2
    validObject(e1) # Validate the object before returning
    e1
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Method for logarithm
methods::setMethod("log",("Sequence"),
  function(x, base = exp(1)) {
    if (any(x@values <= 0)) stop("Logarithm is undefined for non-positive values")
    x@values <- log(x@values, base = base)
    validObject(x) # Validate the object before returning
    x
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
methods::setMethod( "[",c("Sequence", "integer", "missing", "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    # Access the `values` slot of the Sequence object
    value <- x@values[i]
    value
  })

methods::setMethod("[[",c("Sequence", "integer", "missing"),
  function(x, i, j, ...) {
    # Access the `values` slot of the Sequence object
    value <- x@values[[i]]
    value
  })
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
methods::setMethod("summary", "Sequence", function(object, ...) {
  # Calculate summary statistics for the `values` slot
  stats <- summary(object@values)

  # Combine with additional information from the object
  result <- list(
    Summary = stats,
    Type = object@type,
    Ease = object@ease
  )

  # Set a class for the output to customize print behavior
  class(result) <- "summary.Sequence"
  result
})

# Define a print method for the custom summary output
print.summary.Sequence <- function(x, ...) {
  cat("Summary of Sequence Object:\n")
  cat("\nType:", x$Type, "\n")
  cat("Ease:", x$Ease, "\n\n")
  print(x$Summary)
}
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Define method for plotting using 'xy.coords'
methods::setMethod("xy.coords",c(x = "Sequence", y = "missing", setLab = "ANY"),
  function(x, y, setLab = TRUE) {
    # Define x and y
    x_coords <- seq_along(x@values)
    y_coords <- x@values

    # Set custom labels if provided
    xlab <- if (is.character(setLab)) paste(setLab, "X") else NULL
    ylab <- if (is.character(setLab)) paste(setLab, "Y") else NULL

    # Return xy.coords
    xy.coords(x = x_coords, y = y_coords, xlab = xlab, ylab = ylab)
  })

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Define methods
methods::setMethod("show", "Sequence", function(object) {
                     # Display only the 'values' slot
                     cat("Values:\n", object@values, "\n")

                     # Return 'type','ease', and 'steps' invisibly
                     invisible(
                       list(type = object@type,
                            ease = object@ease,
                            steps = object@steps)
                       )
                   }
)

Sequence <- function(values = numeric(),
                     type = character(),
                     ease = character(),
                     steps = numeric()) {
  new("Sequence", values = values, type = type, ease = ease, steps = steps)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
methods::setValidity("Sequence", function(object){
  if(length(object@type) != length(object@ease)){
    "@type and @ease must be the same length"
  }
  if (!is.numeric(object@values)) {
    return("@values must be a numeric vector")
  }
  if(!is.numeric(object@steps)){
    return("@steps must be a numeric vector of length 1")
  }
  TRUE
})
