#' @title calcFallow
#' @description
#' Returns fallow land of our default datasource.
#' The estimates are scaled to match our Land classes in the Croparea function.
#' Due to multiple cropping, harvested cropland area can be greater than non-fallow land area
#' and even greater than physical cropland area.
#' @param cellular TRUE for cellular outputs.
#' @param physical    if TRUE it returns the physical area, with fallow of multicropping systems being
#' scaled to match physical areas of the rotation, if FALSE the area harvested
#' being fallow is returned
#' @return MAgPIE object containing fallow land in Mha
#' @author David Hoetten, Felicitas Beier
#' @seealso
#' \code{\link{readLandInG}}
#' @examples
#' \dontrun{
#' calcOutput("Fallow")
#' }
#' @importFrom magclass dimSums mbind
#' @importFrom madrat toolConditionalReplace
#'
calcFallow <- function(cellular = FALSE, physical = TRUE) {

  fallow <- calcOutput("Croparea", cellular = cellular,
                       physical = physical, fallow = TRUE,
                       irrigation = FALSE, aggregate = FALSE)
  fallow <- fallow[, , "fallow"]

  return(list(x = fallow,
              weight = NULL,
              description = "Fallow land",
              unit = "Mha",
              isocountries = FALSE))

}
