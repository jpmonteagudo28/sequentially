#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
join_char <- function(first,
                      join_with = "_",
                      second){

  stopifnot(is.character(first),
            is.character(second))

  new_string <- paste0(first,join_with,second)

  return(new_string)
}
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
get_mode <- function(.x) {
  .x <- unique(.x)
  x_not_na <- .x[which(!is.na(.x))]
  if(length(x_not_na) > 0) {
    tab <- tabulate(match(.x, x_not_na))
    candidates <- x_not_na[tab == max(tab)]
    if (is.logical(.x)) {
      any(candidates) # return TRUE if any true. max returns an integer
    } else {
      max(candidates) # return highest (ie max) value
    }
  } else {
    .x[NA_integer_]
  }
}

#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
is.sequence <- function(x,...){
  if(!inherits(x,"Sequence"))FALSE
  else {
    TRUE
    }
}
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
all_different <- function(x){
  length(unique(x)) == length(x)
}
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
all_the_same <- function(x, tol = 1e-10){
  if(is.numeric(x)){
    return(max(x,na.rm = TRUE) - min(x,na.rm = TRUE) < tol  && !anyNA(x))
  }
  if (!is.list(x)) {
    return(length(unique(x)) == 1)
  }
}
#---- --- ---- --- ---- --- ---- --- ---- --- ---- --- ----#
are_equal = function(x, y,
                     check.names = TRUE,
                     check.attributes = TRUE,
                     ...) {

  test = all.equal(target = x,
                         current = y,
                         check.names = check.names,
                         check.attributes = check.attributes,
                         ...)

  if (is.logical(test)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
