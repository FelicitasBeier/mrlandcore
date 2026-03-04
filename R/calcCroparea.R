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
#' @author Felicitas Beier
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
    physCroparea <- calcOutput("CropareaLandInG", sectoral = sectoral, physical = TRUE,
                               cellular = TRUE, irrigation = FALSE,
                               selectyears = selectyears, aggregate = FALSE)
    fallowLand   <- calcOutput("FallowLand", cellular = TRUE, aggregate = FALSE)
    luh3Croparea <- calcOutput("LUH3", landuseTypes = "magpie", irrigation = FALSE,
                               cellular = TRUE, yrs = selectyears, aggregate = FALSE)

    mismatch <- dimSums(physCroparea, dim = 3) - luh3Croparea
    ratio    <- luh3Croparea / physCroparea



    # fallow land + crop area = LUH cropland
    # do this at cellular, then aggregate

    # check whether physical croparea matches LUH total land area
    # LUH3 landarea must >= physical crop area

    # Check for landmass mismatch: Total physical croparea should be smaller than landmass
    # Total physical croparea
    physTotal <- dimSums(physCroparea, dim = 3)
    # Total land area according to LUH3 in iniyear (constant over time)
    landarea <- dimSums(luh3Croparea, dim = 3)

    if (any(round(landarea - physTotal, digits = 6) < 0)) {
      # Note: Due to mismatches in the land masks used in LandInG
      #       and LUH, croparea may exceed total landarea.
      stop("This should no longer be the case after the LandInG update.
         Please check where mismatch is coming from starting from calcCropareaAdjusted.")
      vcat(verbosity = 0,
           paste0("In calcCropareaAdjusted: There is a mismatch in the landmask underlying
                the ", dataset, " croparea dataset and LUH.
                Croparea is cut here to fit into the landmass as reported by LUH,
                but a more generic solution should be found to make the data consistent!"))

      ratio <- ifelse(landarea - physTotal < 0,
                      landarea / physTotal,
                      1)
      # Scale down crop-specific croparea by mismatch area
      phys <- phys * ratio

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
