join_char <- function(first,
                      join_with = "_",
                      second){

  stopifnot(is.character(first),
            is.character(second))

  new_string <- paste0(first,join_with,second)

  return(new_string)
}

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

is.sequence <- function(x,...){

  if(!inherits(x,"Sequence"))
    FALSE
  else {
    TRUE
  }
}
