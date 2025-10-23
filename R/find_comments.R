#' Find comment lines
#'
#' Scans raw lines for comments (lines beginning with "!") and returns them
#' along with their line numbers.
#'
#' @param raw A character vector where each element is a line of text.
#'
#' @return A data.frame with two columns: `line_number` (the original index)
#'   and `comment_text` (the full text of the comment line).
#'
#' @export
#'
find_comments <- function(raw) {

  comment_indices <- grep("^!", raw)

  if (length(comment_indices) == 0) {
    return(data.frame(line_number = integer(0), comment_text = character(0)))
  }
  comment_text <- raw[comment_indices]

  return(data.frame(line_number = comment_indices, comment_text = comment_text))
}
