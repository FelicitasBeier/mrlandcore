#' @title calcCroparea
#' @description returns croparea
#'
#' @param sectoral    "kcr" MAgPIE items, and "lpj" LPJmL items
#' @param physical    if TRUE the sum over all crops agrees with the cropland area per country
#' @param cellular    if TRUE: calculates cellular MAgPIE crop area for all magpie croptypes.
#'                    Crop area from LUH3 crop types (c3ann, c4ann, c3per, c4per, cnfx)
#'                    are mapped to MAgpIE crop types using "FAO2LUH2MAG_croptypes" and doing
#'                    an intermediate step via harvested areas of FAO weight area within a
#'                    specific LUH crop type to divide into MAgPIE crop types.
#' @param irrigation  If true: cellular areas are returned separated
#'                    into irrigated and rainfed (see setup in calcLUH3)
#' @param selectyears Vector of years to be returned
#' @param datasource  Croparea data source (LandInG or FAOLUH).
#'                    Datasource FAOLUH is deprecated and is only kept for
#'                    backwards compatibility.
#'
#' @return areas of individual crops from FAOSTAT and weight
#'
#' @author Felicitas Beier, Benjamin Leon Bodirsky
#'
#' @importFrom magpiesets findset

calcCroparea <- function(sectoral = "kcr", physical = TRUE, cellular = FALSE,
                         irrigation = FALSE, selectyears = "past", datasource = "LandInG") {

  if (selectyears == "past") {
    selectyears <- findset("past")
  }

  if (datasource == "LandInG") {
    # read in croparea
    croparea <- calcOutput("CropareaLandInG", sectoral = sectoral, physical = physical,
                           cellular = TRUE, irrigation = irrigation,
                           selectyears = selectyears, aggregate = FALSE)

    # description of data to be returned
    description <- paste0(ifelse(physical, "physical ", "harvested "),
                          "croparea from LandInG data set")

    # correction to match LUH cropland area
    physCroparea <- dimSums(calcOutput("CropareaLandInG", sectoral = sectoral, physical = TRUE,
                                       cellular = TRUE, irrigation = FALSE,
                                       selectyears = selectyears, aggregate = FALSE), dim = "crop")
    fallowLand   <- calcOutput("FallowLand", cellular = TRUE, aggregate = FALSE)[, selectyears, ]
    luh3 <- calcOutput("LUH3", landuseTypes = "magpie", irrigation = FALSE,
                       cellular = TRUE, yrs = selectyears, aggregate = FALSE)
    luh3Croparea <- collapseNames(luh3[, , "crop"])
    luh3Total    <- dimSums(luh3, dim = "landuse")

    #### BENNI: Please correct the correction!
    correctionFactor <- ifelse((physCroparea + fallowLand) > 0 & luh3Croparea > 0,
                               luh3Croparea / (physCroparea + fallowLand),
                               1)

    # apply correction to croparea from above
    croparea <- croparea * correctionFactor

    # Sanity check (physical croparea + fallow land should not exceed total land area in LUH3)
    if (any((physCroparea + fallowLand) - luh3Total > 0)) {
      stop("Croparea is larger than total land area in LUH3.")
    }
    # Sanity check (physical croparea + fallow land should not exceed cropland area in LUH3)
    if (any((physCroparea + fallowLand) - luh3Croparea > 0)) {
      stop("Croparea + fallow land is larger than cropland area in LUH3.
            This should no longer be the case after the correction.")
    }

    # Aggregation to iso-level
    if (!cellular) {
      # aggregate to countries
      croparea <- dimSums(croparea, dim = c("x", "y"))
      # fill missing countries with 0
      croparea <- toolConditionalReplace(x = toolCountryFill(croparea),
                                         conditions = "is.na()", replaceby = 0)
    }

  } else if (datasource == "FAOLUH") {

    ### For backwards compatibility only ###
    # This chunk can be deleted when croparea update is completed.

    # read in croparea
    croparea <- calcOutput("CropareaFAOLUH", sectoral = sectoral, physical = physical,
                           cellular = cellular, irrigation = irrigation,
                           aggregate = FALSE)
    croparea <- croparea[, selectyears, ]

    # description of data to be returned
    description <- paste0(ifelse(physical, "physical ", "harvested "),
                          "croparea from LUH and FAOSTAT")
  } else {
    stop("Selected data source in calcCroparea unknown.")
  }

  return(list(x            = croparea,
              weight       = NULL,
              unit         = "million ha",
              description  = description,
              isocountries = !cellular))
}
