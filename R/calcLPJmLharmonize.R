#' @title calcLPJmLharmonize
#' @description Handle harmonization of LPJmL data
#'
#' @param version Switch between LPJmL versions (including addons for further version specification)
#' @param climatetype Switch between different climate scenarios
#' @param subtype Switch between different LPJmL output variables as specified in readLPJmL
#' @param subdata Switch between data dimension subitems
#' #### Kristine (To Do): explain switch better (maybe give examples). For me the use case wasn't clear.
#' @param stage degree of processing: harmonizedHistorical (until reference year of historical data)
#'                                    harmonizedScenario (until reference year for harmonized Scenarios)
#'              See toolLPJmLHarmonization for current settings.
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
                               subtype     = "pnv:soilc", subdata = NULL,
                               stage       = "harmonizedScenario") {
  # Create settings for LPJmL from version and climatetype argument
  cfg <- toolLPJmLHarmonization(version = version, climatetype = climatetype)

  if (stage == "harmonizedHistorical") {
    # read in historical data for subtype
    baseline        <- calcOutput("LPJmLtransform", version = cfg$baselineVersion,
                                  climatetype = cfg$baselineHist, subtype = subtype,
                                  subdata = subdata, stage = "smoothed",
                                  aggregate = FALSE, supplementary = TRUE)

    unit            <- baseline$unit
    baseline        <- baseline$x

    # read in future scenario data for subtype
    x   <- calcOutput("LPJmLtransform", version = cfg$readinVersion,
                      climatetype = cfg$climatetype, subtype = subtype,
                      subdata = subdata, stage = "smoothed", aggregate = FALSE)
    out <- toolHarmonize2Baseline(x, baseline, ref_year = cfg$refYearHist)

  } else if (stage == "harmonizedScenario") {
    # read in historical data for subtype
    #### Kristine: something is wrong here: should this be a LPJmLharmonize call
    ###            or should the stage not be harmonized?
    baselineScen    <- calcOutput("LPJmLtransform", version = cfg$baselineVersion,
                                  climatetype = cfg$baselineGcm, subtype = subtype,
                                  subdata = subdata, stage = "harmonized",
                                  aggregate = FALSE, supplementary = TRUE)

    unit            <- baselineScen$unit
    baselineScen    <- baselineScen$x

    ### Kristine: What is this if-condition asking? Please comment
    ### Is this the case where e.g. the climate scenario is GSWP3 (the same as the historical baseline)?
    ### In this case it just returns the smoothed data, right? Does that mean that I don't need a
    ### distinction such as in https://github.com/FelicitasBeier/mrwater/blob/HackathonLPJmL2MAgPIE/R/calcGrassET.R#L27
    ### because the calcLPJmLharmonize handles it?
    if (cfg$climatetype   == cfg$baselineGcm &&
          cfg$readinVersion == cfg$baselineVersion) {

      out <- baselineScen

    } else {
      #### Kristine: What's the difference to the case above ("harmonizedHistorical")?
      ####           Why is the distinction necessary?

      # read in future scenario data for subtype
      x   <- calcOutput("LPJmLtransform", version = cfg$readinVersion,
                        climatetype = cfg$climatetype, subtype = subtype,
                        subdata = subdata, stage = "smoothed", aggregate = FALSE)
      out <- toolHarmonize2Baseline(x, baselineScen, ref_year = cfg$refYearGcm)
    }

  } else {
    stop("Stage argument not supported!")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              min          = 0,
              description  = paste0("Output from LPJmL (", subtype, ") for ",
                                    version, " and ", climatetype, " at stage: ", stage, "."),
              isocountries = FALSE))
}
