#' @title       correctLPJmLInputs_new
#' @description correct LPJmLInputs_new content
#'
#' @param x magpie object provided by the read function
#'
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' readSource("LPJmLInputs_new", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace
#'

correctLPJmLInputs_new <- function(x) { # nolint : object_name_linter.

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)

  return(x)
}
