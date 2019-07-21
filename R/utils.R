#' Convert character string to numbers
#'
#' @param string
#'
#' @return character vector
#' @export
#'
#' @examples
#' str_to_num("AEHD")
str_to_num <- function(string) {
  vec <- unlist(strsplit(gsub("(.)", "\\1 ", string), " "))
  match_vec <- LETTERS[1:8]
  paste(as.character(unlist(lapply(vec, function(x) which(x == match_vec))), ""), collapse = "")
}
