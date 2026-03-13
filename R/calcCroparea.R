#' @title calcCroparea
#' @description returns croparea
#'
#' @param sectoral    "kcr" MAgPIE items, and "lpj" LPJmL items
#' @param physical    if TRUE the sum over all crops agrees with the cropland area per country
#' @param fallow      if TRUE fallow land is returned as element in crop set
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

calcCroparea <- function(sectoral = "kcr", physical = TRUE, fallow = FALSE, cellular = FALSE,
                         irrigation = FALSE, selectyears = "all", datasource = "LandInG") {

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

    ############################################################
    ########## Correction to match LUH cropland area  ##########
    ############################################################
    # Note: Correction necessary until LandInG data set is updated to LUH3
    physCroparea <- dimSums(calcOutput("CropareaLandInG", sectoral = sectoral, physical = TRUE,
                                       cellular = TRUE, irrigation = FALSE,
                                       selectyears = selectyears, aggregate = FALSE), dim = "crop")
    fallowLand   <- calcOutput("FallowLand", cellular = TRUE, aggregate = FALSE)[, selectyears, ]
    luh3 <- calcOutput("LUH3", landuseTypes = "magpie", irrigation = FALSE,
                       cellular = TRUE, yrs = selectyears, aggregate = FALSE)
    luh3Cropland <- collapseNames(luh3[, , "crop"])
    landingCropland <- physCroparea + fallowLand
    luh3Total    <- dimSums(luh3, dim = "landuse")

    # cell-specific scaling factor from LandInG cropland to LUH3 cropland
    scalingFactor <- luh3Cropland / landingCropland

    # fallback option for grid cells without LandInG cropland, but LUH3 cropland
    fallbackCells <- landingCropland == 0 & luh3Cropland > 0
    zeroCells     <- landingCropland == 0 & luh3Cropland == 0

    # country and global aggregates
    physCropareaISO      <- dimSums(physCroparea, dim = c("x", "y"))
    fallowLandISO        <- dimSums(fallowLand, dim = c("x", "y"))
    landingCroplandISO   <- dimSums(landingCropland, dim = c("x", "y"))
    cropareaISO          <- dimSums(croparea, dim = c("x", "y"))

    physCropareaGLO      <- dimSums(physCroparea, dim = 1)
    fallowLandGLO        <- dimSums(fallowLand, dim = 1)
    landingCroplandGLO   <- dimSums(landingCropland, dim = 1)
    cropareaGLO          <- dimSums(croparea, dim = 1)

    # country and global shares
    fallowShareCountry         <- fallowLandISO / landingCroplandISO
    physShareCountry           <- physCropareaISO / landingCroplandISO
    cropareaToPhysRatioCountry <- cropareaISO / physCropareaISO

    fallowShareGLO         <- fallowLandGLO / landingCroplandGLO
    physShareGLO           <- physCropareaGLO / landingCroplandGLO
    cropareaToPhysRatioGLO <- cropareaGLO / physCropareaGLO

    fallowShareCountry[is.nan(fallowShareCountry)] <- fallowShareGLO
    physShareCountry[is.nan(physShareCountry)] <- physShareGLO
    cropareaToPhysRatioCountry[is.nan(cropareaToPhysRatioCountry)] <- cropareaToPhysRatioGLO

    # expand to grid level
    fallowShareCountryGrid         <- magpie_expand(fallowShareCountry, ref = fallowLand)
    physShareCountryGrid           <- magpie_expand(physShareCountry, ref = fallowLand)
    cropareaToPhysRatioCountryGrid <- magpie_expand(cropareaToPhysRatioCountry, ref = croparea)

    # Apply scaling factor to croparea and fallow land
    cropareaCalibrated   <- croparea * scalingFactor
    fallowLandCalibrated <- fallowLand * scalingFactor

    # replace NaNs from 0/0 by zero
    cropareaCalibrated[is.nan(cropareaCalibrated)]     <- 0
    fallowLandCalibrated[is.nan(fallowLandCalibrated)] <- 0

    # Overwrite where fallback option needed
    physCropareaFallback <- luh3Cropland * physShareCountryGrid
    fallowLandFallback   <- luh3Cropland * fallowShareCountryGrid
    cropareaFallback     <- physCropareaFallback * cropareaToPhysRatioCountryGrid

    fallowLandCalibrated[fallbackCells] <- fallowLandFallback[fallbackCells]
    cropareaCalibrated[fallbackCells]   <- cropareaFallback[fallbackCells]

    # ensure exact zero where both LUH3 and LandInG are zero
    fallowLandCalibrated[zeroCells] <- 0
    cropareaCalibrated[zeroCells]   <- 0

    # return calibrated croparea with calibrated fallow land
    fallowLandCalibrated <- setNames(object = fallowLandCalibrated, "fallow")
    getSets(fallowLandCalibrated) <- c("x", "y", "iso", "year", "crop")

    ### Check physical cropland matching ###
    if (physical) {
      # Sanity check (physical croparea + fallow land should not exceed total land area in LUH3)
      if (any((dimSums(cropareaCalibrated, dim = "crop") + fallowLandCalibrated) - luh3Total > 0)) {
        stop("Croparea is larger than total land area in LUH3.")
      }
      # Sanity check (physical croparea + fallow land should match total cropland in LUH3 after calibration)
      if (any(round(dimSums(cropareaCalibrated, dim = "crop") + fallowLandCalibrated - luh3Cropland, 6) != 0)) {
        stop("Calibrated physical croparea + fallow land does not match LUH3 cropland.")
      }
    }

    # return croparea (and optionally fallow land)
    if (fallow) {
      croparea <- mbind(cropareaCalibrated, fallowLandCalibrated)
      description <- paste0(description, " including fallow land.")
    } else {
      croparea <- cropareaCalibrated
      description <- paste0(description, " excluding fallow land.")
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

    # return croparea (and optionally fallow land)
    if (fallow) {
      # LUH3/FAO doesn't provide distinction of fallow land
      fallowLand  <- new.magpie(cells_and_regions = getItems(croparea, dim = 1),
                                years = getItems(croparea, dim = 2),
                                names = "fallow",
                                sets = getSets(croparea)[["d3.1"]],
                                fill = 0)
      croparea    <- mbind(croparea, fallowLand)
      description <- paste0(description, " including fallow land.")
    } else {
      description <- paste0(description, " excluding fallow land.")
    }

  } else {
    stop("Selected data source in calcCroparea unknown.")
  }

  return(list(x            = croparea,
              weight       = NULL,
              unit         = "million ha",
              description  = description,
              isocountries = !cellular))
}
