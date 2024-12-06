replace_char <- function(.input_string,
                         old_char,
                         new_char,
                         use_regex = FALSE) {

  if (old_char == "" || is.null(old_char)) {
    stop("Error: The character to replace cannot be empty.")
  }

  gsub(old_char,
       new_char,
       .input_string,
       fixed = !use_regex)

}

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
