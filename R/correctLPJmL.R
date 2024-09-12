#' @title correctLPJmL
#' @description corrects data that was read in from LPJmL
#' @param x magpie object provided by the read function
#'
#' @author Kristine Karstens, Felicitas Beier
#' @seealso
#' [readLPJmL()]
#' @examples
#' \dontrun{
#' readSource("LPJmL", convert = "onlycorrect")
#' }
#'
#' @importFrom madrat toolConditionalReplace
#'

correctLPJmL <- function(x) { # nolint: object_name_linter.

  x <- toolConditionalReplace(x, conditions = "<0", replaceby = 0)

  return(x)
}
