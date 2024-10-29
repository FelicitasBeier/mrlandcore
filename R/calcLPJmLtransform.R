#' @title calcLPJmLtransform
#' @description Transform LPJmL data in units and variables used by magpie
#'
#' @param version Switch between LPJmL versions (including addons for further version specification)
#' @param climatetype Switch between different climate scenarios (climatemodel:climatescenario)
#' @param subtype Switch between different lpjml input (runtype:filename)
#' @param subdata Switch between data dimension subitems
#' @param stage Degree of processing: raw:cut, smoothed:cut           - raw or smoothed data from 1930|1951
#'                                    raw:fullhist, smoothed:fullhist - raw or smoothed data with full history
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#'
#' @author Kristine Karstens, Felicitas Beier
#'
#' @importFrom madrat calcOutput readSource toolSubtypeSelect toolSplitSubtype
#' @importFrom magclass dimSums getYears setYears
#'
#' @seealso
#' [readLPJmL()]
#' @examples
#' \dontrun{
#' calcOutput("LPJmLtransform", subtype = "pnv:soilc", aggregate = FALSE)
#' }
#'
calcLPJmLtransform <- function(version     = "lpjml5.9.5-m1", # nolint
                               climatetype = "MRI-ESM2-0:ssp370",
                               subtype     = "pnv:soilc", subdata = NULL,
                               stage       = "smoothed:cut") {
  ########## CONFIGURE READ START ##########

  ### Drafted please revise
  ### especially think if the year cutting can be done differently

  cfg        <- toolLPJmLScenario(version = version, climatetype = climatetype, subtype = subtype)
  readinName <- paste(cfg$version, cfg$climatetype, cfg$subtype, sep = ":")
  readinHist <- gsub("ssp[0-9]{3}", "historical", readinName)

  if (!grepl("historical", cfg$climatetype)) {

    x     <- mbind(readSource("LPJmL", subtype = readinHist, convert = "onlycorrect"),
                   readSource("LPJmL", subtype = readinName, convert = "onlycorrect"))
    if (grepl("cut", stage)) {
      years <- getYears(x, as.integer = TRUE)
      x     <- x[, years[years >= 1951], ]
    }

  } else {

    x     <- readSource("LPJmL", subtype = readinName, convert = "onlycorrect")
    if (grepl("cut", stage)) {
      years <- getYears(x, as.integer = TRUE)
      x     <- x[, years[years >= 1930], ]
    }
  }

  if (!is.null(subdata)) {
    if (!all(subdata %in% getNames(x))) {
      stop(paste0("Subdata items '", subdata, "' are not part of selected LPJmL subtype!"))
    }
    x <- x[, , subdata]
  }
  ########## CONFIGURE READ END ##########

  ########## DATA TRANSFORMATION START ###############
  # The standard unit conversion (e.g., from gC/m^2 to tC/ha, from mm to m^3/ha)
  # takes place in correctLPJmL
  # The following transformations are for selected inputs (e.g., those that
  # need to be transformed to tDM)

  # To Do (Kristine): Please double-check the units of new inputs.
  # (1) There are some that get converted to tC/ha, but not furhter to tDM/ha:
  # (1.1) tC/ha: firec, litburnc, litc, litfallc, npb
  # If any of these is supposed to be in tDM/ha, please add in the if below!
  # (1.2) m^3/ha: pet is transformed as all other water inputs (from mm to m^3/ha).
  # If you want a different unit, please add if below, but start with m^3/ha for transformation
  # (2) Jan was against units like "ha/ha", "days of the year", "shr"
  # For now all "" units in LPJmL got renamed to "1".
  # If we need a different unit, we can change it either here (after discussion with Jan)
  # or wherever the respective function is called.

  if (grepl("gpp", "npp", "harvestc")) {
    # Transformation from carbon to dry matter content
    # (from tC/ha to tDM/ha)
    x    <- x / 0.45
    unit <- "tDM/ha"
  } else if (grepl("runoff")) {
    # To Do (Feli): Decide whether this can be moved to mrwater
    # and if so: move it.

    ### Transformation to flow on land ###
    # Transformation factor: 1 m^3/ha = 1e-6 mio. m^3/ha
    # Transformation factor: 1 Mha    = 1e+6 ha
    # landarea (in Mha)
    landArea <- setYears(collapseNames(dimSums(readSource("LUH2v2",
                                                          subtype = "states",
                                                          convert = "onlycorrect")[, "y1995", ],
                                               dim = 3)), NULL)
    x <- x * landArea
    # new unit
    unit <- "mio. m^3"
  } else if (grepl("evap_lake")) {
    # To Do (Feli): Decide whether this can be moved to mrwater
    # and if so: move it.
    # Note also: input_lake is now calculated from prec
    # Probably better to do all of it in mrwater (calcRiverNaturalFlows)

    ### Transformation to flow on water bodies ###
    # lake area from LPJmL (in m^2 already transformed to ha)
    # To Do (Feli): replace with LUH2v2 lake area
    # Question (Jens): looks like LUH2v2 lake area includes ice?
    # Question (Kristine, Pascal): how would I extract lake area from LUH2v2?
    # should we do it during the LUH update?
    lakeArea <- readSource("LPJmL", subtype = "lpjml5.9.5-m1:GSWP3-W5E5:historical:pnv:lake_area")
    # transform from m^3/ha to mio. m^3
    x <- x * lakeArea * 1e-6
    # transform to yearly value
    x <- dimSums(x, dim = "month")
    ### Question (Jens): I don't think that we need this input at a monthly scale.
    ### Do I overlook something? If not: can we get it as yearly output instead?
    unit <- "mio. m^3"
  } else if (grepl("*date*", subtype)) {
    # To Do (Kristine and Jan): Discuss whether we want such renaming here
    unit <- "day of the year"
  } else if (grepl("cshift", subtype)) {
    # To Do (Kristine and Jan): Discuss whether we want such renaming here
    unit <- "C/C"
  } else if (grepl("fpc", subtype)) {
    # To Do (Kristine and Jan): Discuss whether we want such renaming here
    unit <- "ha/ha"
  } else {
    stop(paste0("subtype ", subtype, " does not exist"))
  }
  ########## DATA TRANSFORMATION STOP ###############

  ########## STAGE HANDELING START  ###############
  # To Do carify which calculation steps or test have to been done for the different stage arguments
  if (grepl("smoothed", stage)) {
    x <- toolSmooth(x)
  } else if (grepl("raw", stage)) {
    # unclear if checks are needed? I don't think so: should be covered in correctLPJmL already, right?
    # To Do (discuss with Kristine, Mike, Feli, maybe Jan)
  } else {
    stop("Stage argument not supported!")
  }
  ########## STAGE HANDELING STOP  ###############

  return(list(x            = x,
              weight       = NULL,
              unit         = unit,
              min          = 0,
              description  = paste0("Output from LPJmL (", subtype, ") for ",
                                    version, " and ", climatetype, " at stage: ", stage, "."),
              isocountries = FALSE))
}
