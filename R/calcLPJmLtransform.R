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
  # Thanks, Kristine! Works fine already!
  # the if-condition with the "cut" is basically repeated code with a different year
  # We could assign the year to an object instead in the if-condition (cutYr <- 1951)
  # and then have the code outside the if. I drafted below as an example.
  # Related to this: Why don't we cut historical and scenario in the same year?
  # Alternatively, we could also leave it fully flexible
  # (e.g. instead of cut selecting a specific start year there: smoothed:1951)
  # That would make the code easier and give the user more flexibility.
  # It could be then set to the default 1951, but then it would be the same for historical and scenario


  ### Question (Kristine): Where will the subdata end up?
  cfg        <- toolLPJmLScenario(version = version, climatetype = climatetype, subtype = subtype)
  readinName <- paste(cfg$version, cfg$climatetype, cfg$subtype, sep = ":")
  readinHist <- gsub("ssp[0-9]{3}", "historical", readinName)

  # read in LPJmL data from selected run (historical or scenario)
  x <- readSource("LPJmL", subtype = readinName, convert = "onlycorrect")
  unit <- madrat::getFromComment(x, "unit")
  ### Jan, Kristine: I had to pull this out of the if because mbind() was dropping the unit information
  # Also it avoids one repeated line. If you know a better solution also fine with me.

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

  ####### SUBTYPE - RUN MAPPING START #######
  # To Do (discuss: Kristine, Mike, Feli, Jan):
  # If I understood Jan correctly, he would like for the user only to select the LPJmL output subtype,
  # not the respective run (pnv, crop, grass)
  # I am a bit unsure since it gives us less flexibility and especially because of our special case cropsRf and cropsIr
  ####### SUBTYPE - RUN MAPPING START #######

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

  if (grepl("gpp|npp|harvestc", subtype)) {
    # Transformation from carbon to dry matter content
    # (from tC/ha to tDM/ha)
    x    <- x / 0.45
    unit <- "tDM/ha"
  } else if (grepl("*date*", subtype)) {
    # To Do (Kristine and Jan): Discuss whether we want such renaming here
    unit <- "day of the year"
  } else if (grepl("cshift", subtype)) {
    # To Do (Kristine and Jan): Discuss whether we want such renaming here
    unit <- "C/C"
  } else if (grepl("fpc", subtype)) {
    # To Do (Kristine and Jan): Discuss whether we want such renaming here
    unit <- "ha/ha"
  }
  ########## DATA TRANSFORMATION STOP ###############

  ########## STAGE HANDELING START  ###############
  # To Do carify which calculation steps or tests have to been done for the different stage arguments
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
