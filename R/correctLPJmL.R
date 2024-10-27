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
#' @importFrom madrat toolConditionalReplace
#'

correctLPJmL <- function(x) {

  ### check whether there are negatives (and if so give warning)
  x <- toolConditionalReplace(x, conditions = "<0", replaceby = 0)

  ### do we want to check and correct for N/As?
  # Jens?, Kristine?, Mike?

  description <- "description" # To do: replace once getComment() works
  unit <- "unit" # To do: replace once getComment() works

  return(list(x = x,
              description = description,
              unit = unit))
}
