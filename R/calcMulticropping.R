#' @title calcMulticropping
#' @description Multicropping factor, telling how many harvests are produced
#' on one unit of phyiscal area. Fallow lands are not counted in, so the factor
#' is always larger than 1.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcCroparea()]
#' @examples
#' \dontrun{
#' calcOutput("Multicropping")
#' }
#'
calcMulticropping <- function() {   # nolint

  phys   <- collapseNames(dimSums(calcOutput("Croparea", physical = TRUE,
                                             aggregate = FALSE, sectoral = "kcr"),
                                  dim = 3.1))
  harv   <- collapseNames(dimSums(calcOutput("Croparea", physical = FALSE,
                                             aggregate = FALSE, sectoral = "kcr"),
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
