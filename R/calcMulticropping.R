#' @title calcMulticropping
#' @description Multicropping factor, telling how many harvests are produced
#' on one unit of phyiscal area. Fallow lands are not counted in, so the factor
#' is always larger than 1.
#'
#' @param irrigation report irrigated and non-irrigated areas separately
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcCroparea()]
#' @examples
#' \dontrun{
#' calcOutput("Multicropping")
#' }
#'
calcMulticropping <- function(irrigation = TRUE, cellular = FALSE) {   # nolint

  phys   <- collapseNames(dimSums(calcOutput("Croparea", physical = TRUE, cellular = cellular,
                                             aggregate = FALSE, irrigation = irrigation),
                                  dim = 3.1))
  harv   <- collapseNames(dimSums(calcOutput("Croparea", physical = FALSE, cellular = cellular,
                                             aggregate = FALSE, irrigation = irrigation),
                                  dim = 3.1))
  out <- harv / phys

  out[is.na(out)] <- 1

  out  <- toolHoldConstantBeyondEnd(out)
  phys <- toolHoldConstantBeyondEnd(phys)

  return(list(x           = out,
              weight      = phys,
              unit        = "ratio",
              description = "Ratio of area harvested to phyiscal area, excluding fallow land"))
}
