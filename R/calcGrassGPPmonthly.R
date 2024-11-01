#' @title calcGrassGPPmonthly
#'
#' @description Calculates monthly gross primary production (GPP) of grassland
#'              under irrigated and rainfed conditions based on LPJmL inputs.
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("GrassGPP", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums getItems new.magpie getSets add_dimension

calcGrassGPPmonthly <- function(selectyears, lpjml, climatetype) {

  ### Question (Kristine): What's the difference between "harmonizedHistorical"
  ### and "harmonizedScenario"? When to use what? And do I even need the distinction here?
  ### Preferrably, I would just like to select the climatetype (and harmonized)
  ### and not have to care for the stage...
  if (grepl("historical", climatetype)) {
    stage <- "smoothed" # maybe: harmonizedHistorical
  } else {
    stage <- "harmonizedHistorical" # maybe: harmonizedScenario
  }

  ####################
  ### Read in data ###
  ####################
  # To Do: update once new LPJmL data is there (with rainfed run and irrigated run)

  # Jens: is monthly grass GPP the same for cropsIr and cropsRf runs? (i.e., is separation not necessary?)
  # maybe this function is not necessary then... (only calcGrassGPPyearly)

  # monthly irrigated grass GPP (in tDM/ha)
  # To Do (Feli): adjust runfolder once new runs ready
  # Jens: this would be from irrigated run (cropsIr) in the future, right?
  monthlyIrrigated <- calcOutput("LPJmLharmonize", subtype = "crops:gpp_grass_ir",
                                 years = selectyears, stage = stage,
                                 version = lpjml, climatetype = climatetype,
                                 aggregate = FALSE)

  # monthly rainfed grass GPP (in tDM/ha)
  # To Do (Feli): adjust runfolder once new runs ready
  # Jens: this would be from rainfed run (cropsRf) in the future, right?
  monthlyRainfed <- calcOutput("LPJmLharmonize", subtype = "crops:gpp_grass_rf",
                               years = selectyears, stage = stage,
                               version = lpjml, climatetype = climatetype,
                               aggregate = FALSE)

  ####################
  ### Calculations ###
  ####################
  # Monthly grass GPP
  monthlyRainfed   <- add_dimension(monthlyRainfed,
                                    add = "irrigation", nm = "rainfed")
  monthlyIrrigated <- add_dimension(monthlyIrrigated,
                                    add = "irrigation", nm = "irrigated")

  ##############
  ### Return ###
  ##############
  unit        <- "tDM per ha"
  description <- "irrigated and rainfed gross primary production (GPP) of grass"

  out                  <- mbind(monthlyRainfed, monthlyIrrigated)
  getSets(out)["d3.2"] <- "month"
  description          <- paste0(description, " per month")

  ##############
  ### Checks ###
  ##############
  if (any(is.na(out))) {
    stop("calcGrassGPPmonthly produced NA values")
  }
  if (any(out < 0)) {
    stop("calcGrassGPPmonthly produced negative values")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
