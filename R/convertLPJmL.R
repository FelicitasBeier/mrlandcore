#' @title convertLPJmL
#' @description converts units of data that was read in from LPJmL
#'              to units used in madrat and MAgPIE
#' @param x magpie object provided by the read function
#'
#' @author Felicitas Beier, Jan Philipp Dietrich
#' @seealso
#' [readLPJmL()]
#' @examples
#' \dontrun{
#' readSource("LPJmL", convert = TRUE)
#' }
#'
#' @importFrom madrat toolConditionalReplace
#'

convertLPJmL <- function(x) {

  # check if unit using getComment() or getFromComment(x, "unit")

  # convert gC/m^2 to tC/ha
  conversionFactor <- 0.01

  # convert mm to m^3/ha
  conversionFactor <- 10

  conversionFactor <- 1 # To do: delete once if-conditions activated

  # conversion
  x <- x * conversionFactor

  unit <- "unit" # To do: replace once getComment() works
  description <- "description" # To do: replace once getComment() works

  return(list(x = x,
              description = description,
              unit = unit))
}
