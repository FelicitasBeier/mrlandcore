#' @title calcLPJmLharmonize
#' @description Handle harmonization of LPJmL data
#'
#' @param version Switch between LPJmL versions (including addons for further version specification)
#' @param climatetype Switch between different climate scenarios
#' @param subtype Switch between different LPJmL output variables as specified in readLPJmL
#' @param subdata Selection of subitems of object.
#'                This argument can be used to split up the data in smaller objects
#'                where only a sub-set of the data is needed or
#'                for better handling where otherwise memory issues would occur due to the object size.
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#'
#' @author Kristine Karstens, Felicitas Beier
#'
#' @importFrom madrat calcOutput readSource toolSubtypeSelect toolSplitSubtype
#' @importFrom magclass dimSums getYears setYears
#'
#' @seealso
#' [toolLPJmLHarmonization()]
#' @examples
#' \dontrun{
#' calcOutput("LPJmLharmonize", subtype = "pnv:soilc", aggregate = FALSE)
#' }
calcLPJmLharmonize <- function(version     = "lpjml5.9.5-m1", # nolint
                               climatetype = "MRI-ESM2-0:ssp370",
                               subtype     = "pnv:soilc", subdata = NULL) {
  ### Question (Jan): calcLPJmLharmonize is very slow. Can we do anything to improve the performance?

  # Extract settings for LPJmL from version and climatetype argument
  cfg <- toolLPJmLHarmonization(version = version, climatetype = climatetype)

  if (grepl("historical", climatetype)) {
    # return smoothed LPJmL data for historical baseline chosen
    x <- calcOutput("LPJmLtransform", version = cfg$readinVersion,
                    climatetype = cfg$climatetype, subtype = subtype,
                    subdata = subdata, stage = "smoothed",
                    aggregate = FALSE, supplementary = TRUE)
    # extract unit from LPJmL data
    unit <- x$unit
    # extract data
    x <- x$x
  } else {
    # The harmonization consists of two steps:
    # (1) the reference scenario is harmonized to reference baseline
    # based on the default settings of toolLPJmLHarmonization

    # read in historical data for subtype
    baseline <- calcOutput("LPJmLtransform", version = cfg$baselineVersion,
                           climatetype = cfg$baselineHist, subtype = subtype,
                           subdata = subdata, stage = "smoothed",
                           aggregate = FALSE, supplementary = TRUE)
    unit <- baseline$unit
    baseline <- baseline$x

    # read in future scenario data for subtype
    x <- calcOutput("LPJmLtransform", version = cfg$readinVersion,
                    climatetype = cfg$baselineGcm, subtype = subtype,
                    subdata = subdata, stage = "smoothed", aggregate = FALSE)

    # harmonize default future scenario to default baseline until
    # default historical year
    harmonizedScen <- toolHarmonize2Baseline(x, baseline, ref_year = cfg$refYearHist)

    # (2) the chosen climate scenario is harmonized to the reference scenario
    if (cfg$climatetype == cfg$baselineGcm &&
          cfg$readinVersion == cfg$baselineVersion) {
      # If climatetype and baseline identical to the default climatetype
      # and default baseline from toolLPJmLHarmonization
      # no additional harmonization is required
      out <- harmonizedScen
    } else {
      # Otherwise the scenario from the chosen climatetype is harmonized
      # to the default climatescenario for the reference year where the
      # GCM future starts

      # read in future scenario data for subtype
      x <- calcOutput("LPJmLtransform", version = cfg$readinVersion,
                      climatetype = cfg$climatetype, subtype = subtype,
                      subdata = subdata, stage = "smoothed", aggregate = FALSE)
      # harmonize chosen climate scenario to default baseline scenario
      out <- toolHarmonize2Baseline(x, harmonizedScen, ref_year = cfg$refYearGcm)
    }
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              min          = 0,
              description  = paste0("Output from LPJmL (", subtype, ") for ",
                                    version, " and ", climatetype, "."),
              isocountries = FALSE))
}
