#' @title correctLPJmL
#' @description corrects data that was read in from LPJmL and converts units
#'              to MAgPIE standard
#' @param x list of magpie object and unit provided by the read function
#'
#' @author Felicitas Beier, Kristine Karstens
#' @seealso
#' [readLPJmL()]
#' @examples
#' \dontrun{
#' readSource("LPJmL", convert = "onlycorrect")
#' }
#'

correctLPJmL <- function(x) {

  # check and replace negative values
  toolExpectTrue(all(x >= -1e-10), "Data provided by LPJmL is not negative", falseStatus = "warn")
  x <- madrat::toolConditionalReplace(x, conditions = "<0", replaceby = 0)

  ### To Do (discuss with Mike, Kristine): Do we want warning from these?
  ### Do we want to replace N/A's and <0 with 0?

  # check and replace N/A's
  toolExpectTrue(all(!is.na(x)), "Data provided by LPJmL doesn't contain N/A's", falseStatus = "warn")
  x <- toolConditionalReplace(x, conditions = c("is.na()"), replaceby = 0)

  # extract unit of data
  unit <- madrat::getFromComment(x, "unit")

  ### To Do (Kristine): please double-check unit conversion below

  # unit conversion
  if (grepl("gC/m2", unit)) {
    # convert gC/m2 to tC/ha
    # Conversion note:
    # Transformation factor (numerator): 1 g = 1e-6 t
    # Transformation factor (denominator): 1 m^2 = 1e-4 ha
    x <- x * 0.01
    unit <- "tC/ha"
  } else if (grepl("mm", unit)) {
    # convert mm to m^3/ha
    # Conversion note:
    # Def.: mm is equal to liter/m^2
    # Transformation factor (numerator): 1 liter = 1e-3 m^3
    # Transformation factor (denominator): 1 m^2   = 1e-4 ha
    x <- x * 10
    unit <- "m^3/ha"
  } else if (identical(unit, "")) {
    # empty units are replaced with 1 in magclass
    unit <- NULL
  } else if (unit == "m2") {
    # convert m^2 to ha
    # Transformation factor (numerator): 1 m^2 = 1e-4 ha
    x <- x * 1e-4
    unit <- "ha"
    ### Question (Jan, Kristine): should we transform to ha
    ### or does this include unnecessary rounding imprecision?
    ### when we transform lake evap that also has to be multiplied with 1e-6
    ### so that it ends up being to mio. m^3?
  } else if (unit == "hm3/month") {
    # Transformation factor (numerator): 1 cubic hectometer = 1 mio. cubic meter
    ### Question (Jens, Kristine): Is this correct? What's hm3?
    unit <- "mio. m^3/month"
  }

  return(list(x = x,
              unit = unit))
}
