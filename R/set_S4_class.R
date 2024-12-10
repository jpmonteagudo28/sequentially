# Create a class for non-linear sequence
methods::setClass( "Sequence",
                   contains = "numeric",  # Inherit from numeric
                   slots = c(
                     type = "character",
                     ease = "character"
                   ),
                   prototype = methods::prototype(
                     type = character(),    # Default: empty character vector
                     ease = character()    # Default: empty numeric vector
                   )
)
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Set setter and getter for `type` slot
methods::setGeneric("type", function(x) methods::standardGeneric("type"))
methods::setGeneric("type<-", function(x,value) methods::standardGeneric("type<-"))

# Set setter and getter for `ease` slot
methods::setGeneric("ease", function(x) methods::standardGeneric("ease"))
methods::setGeneric("ease<-", function(x,value) methods::standardGeneric("ease<-"))
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
methods::setMethod("Arith",c("Sequence", "numeric"),
  function(e1, e2) {
    result <- new("Sequence",.Data = callGeneric(as.numeric(e1), e2),  # Perform operation on numeric part
      type = e1@type,
      ease = e1@ease
    )
    validObject(result)
    result
  })

methods::setMethod("Arith",c("numeric", "Sequence"),
  function(e1, e2) {
    result <- new("Sequence",.Data = callGeneric(e1, as.numeric(e2)),  # Perform operation on numeric part
      type = e2@type,
      ease = e2@ease
    )
    validObject(result)
    result
  })

methods::setMethod("Arith",c("Sequence", "Sequence"),
  function(e1, e2) {
    result <- new("Sequence",.Data = callGeneric(as.numeric(e1), as.numeric(e2)),  # Perform operation on numeric parts
      type = e1@type,  # Inherit 'type' from first object
      ease = e1@ease
    )
    validObject(result)
    result
  })


methods::setMethod("Math","Sequence",
  function(x) {
    result <- new("Sequence",.Data = callGeneric(as.numeric(x)),  # Perform operation on numeric part
      type = x@type,
      ease = x@ease
    )
    validObject(result)
    result
  })

methods::setMethod("Summary","Sequence",
  function(x, ..., na.rm = FALSE) {
    callGeneric(as.numeric(x@.Data), ..., na.rm = na.rm)
  })

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
methods::setMethod("show","Sequence",
                   function(object) {
                     # Display the numeric values (stored in the .Data slot)
                     cat("Values:\n", object@.Data, "\n\n")

                     # Return 'type' and 'ease' invisibly
                     invisible(
                       list(
                         type = slot(object, "type"),
                         ease = slot(object, "ease")
                       )
                     )
                   })

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
# Define constructor function
Sequence <- function(values, type = NA, ease = NA) {
  new("Sequence", .Data = values, type = type, ease = ease)
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
methods::setValidity("Sequence", function(object){
  if(length(object@type) != length(object@ease)){
    "@type and @ease must be the same length"
  }
  if (!is.numeric(object@.Data)) {
    return("@values must be a numeric vector")
  }
  TRUE
})
