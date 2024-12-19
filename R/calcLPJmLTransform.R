#' @title calcLPJmLTransform
#' @description Transform LPJmL data in units and variables used by magpie
#'
#' @param lpjmlversion Switch between LPJmL versions (including addons for further version specification)
#' @param climatetype  Switch between different climate scenarios (climatemodel:climatescenario)
#' @param subtype      Switch between different lpjml input (runtype:filename)
#' @param subdata Selection of subitems of object.
#'                This argument can be used to split up the data in smaller objects
#'                where only a sub-set of the data is needed or
#'                for better handling where otherwise memory issues would occur due to the object size.
#' @param stage Degree of processing: raw:cut, smoothed:cut           - raw or smoothed data from 1930|1951
#'                                    raw:fullhist, smoothed:fullhist - raw or smoothed data with full history
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#'
#' @author Kristine Karstens, Felicitas Beier
#'
#' @seealso
#' [readLPJmL()]
#' @examples
#' \dontrun{
#' calcOutput("LPJmLTransform", subtype = "pnv:soilc", aggregate = FALSE)
#' }
#'
calcLPJmLTransform <- function(lpjmlversion = "lpjml5.9.5-m1",
                               climatetype = "MRI-ESM2-0:ssp370",
                               subtype     = "pnv:soilc", subdata = NULL,
                               stage       = "smoothed:cut") {

  ########## CONFIGURE READ START ##########
  cfg        <- toolLPJmLScenario(lpjmlversion = lpjmlversion,
                                  climatetype = climatetype,
                                  subtype = subtype)
  readinName <- paste(cfg$version, cfg$climatetype, cfg$subtype, sep = ":")
  readinHist <- gsub("ssp[0-9]{3}", "historical", readinName)

  # read in LPJmL data from selected run (historical or scenario)
  x    <- readSource("LPJmL", subtype = readinName, convert = "onlycorrect")
  unit <- madrat::getFromComment(x, "unit")

  if (!is.null(getItems(x, dim = 2))) {
    if (!grepl("historical", cfg$climatetype)) {
      # select year for cutting of the time series
      cutYr <- 1951
      # read in LPJmL data from historical run and combine with scenario run
      x <- mbind(readSource("LPJmL", subtype = readinHist, convert = "onlycorrect"),
                 x)
    } else {
      # select year for cutting of the time series
      cutYr <- 1930
    }
  }

  # shorten the time series
  if (grepl("cut", stage)) {
    years <- getYears(x, as.integer = TRUE)
    x <- x[, years[years >= cutYr], ]
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

  if (grepl("gpp|npp|harvestc", subtype)) {
    # Transformation from carbon to dry matter content
    # (from tC/ha to tDM/ha)
    x    <- x / 0.45
    unit <- "tDM/ha"
  }
  ########## DATA TRANSFORMATION STOP ###############

  ########## STAGE HANDELING START  ###############
  if (grepl("smoothed", stage)) {
    x <- toolSmooth(x)
  } else if (grepl("raw", stage)) {
    # nothing needs to be done
  } else {
    stop("Stage argument not supported!")
  }
  ########## STAGE HANDELING STOP  ###############

  return(list(x            = x,
              weight       = NULL,
              unit         = unit,
              min          = 0,
              description  = paste0("Output from LPJmL (", subtype, ") for ",
                                    lpjmlversion, " and ", climatetype, " at stage: ", stage, "."),
              isocountries = FALSE))
}
