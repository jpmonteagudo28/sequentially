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
