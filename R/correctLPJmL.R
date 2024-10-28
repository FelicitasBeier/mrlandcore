#' @title correctLPJmL
#' @description corrects data that was read in from LPJmL
#' @param x magpie object provided by the read function
#'
#' @author Kristine Karstens, Felicitas Beier
#' @seealso
#' [readLPJmL()]
#' @examples
#' \dontrun{
#' readSource("LPJmL", convert = TRUE)
#' }
#'

correctLPJmL <- function(x) {

  ### check whether there are negatives (and if so give warning)
  x <- madrat::toolConditionalReplace(x, conditions = "<0", replaceby = 0)

  ### do we want to check and correct N/As?
  # Jens?, Kristine?, Mike?

  unit <- madrat::getFromComment(x, "unit")

  return(list(x = x,
              unit = unit))
}
