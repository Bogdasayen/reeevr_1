#' Test if x is na or null
#' Tests if missing value and overcomes logical(0) errors when using is.na
#' Shared by Backlin 29-Oct-2013
#' https://stackoverflow.com/questions/19655579/a-function-that-returns-true-on-na-null-nan-in-r
#' @param x The scalar to test
#' @param false_triggers extra conditions
#' @return Boolean indicating if x is blank 
#' @examples 
#' temp1 <- is_blank(x = NA)
#' temp2 <- is_blank(x = 2)
#' @export

is_blank <- function(x, false_triggers = FALSE){
  if(is.function(x)) return(FALSE) # Some of the tests below trigger
  # warnings when used on functions
  return(
    is.null(x) ||                # Actually this line is unnecessary since
      length(x) == 0 ||            # length(NULL) = 0, but I like to be clear
      all(is.na(x)) ||
      all(x=="") ||
      (false_triggers && all(!x))
  )
}