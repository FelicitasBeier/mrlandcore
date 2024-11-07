#' @title toolLPJmLHarmonization
#'
#' @description Specify default settings for LPJmL version and baseline settings
#'
#' @param lpjmlversion Switch between LPJmL versions (including add-ons (+*) for further version specification)
#' @param climatetype  Switch between different climate scenarios
#'
#' @return configuration as list
#' @author Kristine Karstens
#'
#' @importFrom stringr str_split
#'
#' @export

toolLPJmLHarmonization <- function(lpjmlversion, climatetype) {

  cfg <- NULL

  ##### DEFAULT CONFIG #####
  cfg$baselineHist    <- "GSWP3-W5E5:historical"
  cfg$refYearHist     <- "y2010"
  cfg$baselineGcm     <- "MRI-ESM2-0:ssp370"
  cfg$refYearGcm      <- "y2025"
  cfg$readinVersion   <- lpjmlversion
  cfg$baselineVersion <- lpjmlversion
  cfg$climatetype     <- climatetype
  ##### DEFAULT CONFIG #####


  ##### ADDON CONFIG #####
  # overwrite default settings and LPJmL version for
  # (1) add-on tag in version argument - implemented add-ons:
  #  * `+baselineGcm<GCM:RCP>`         - use another baseline for 2010--2020
  #  * `+scen_<scenname>(_<runtype>)` - implemented scenario will be handled toolLPJmLScenario

  ### version addon
  if (grepl("\\+", lpjmlversion)) {
    tmp <- unlist(str_split(lpjmlversion, "\\+"))

    if (any(grepl("baselineGcm", tmp))) {
      i <- grep("baselineGcm", tmp)
      cfg$baselineGcm   <- gsub("baselineGcm", "", tmp[i])
      cfg$readinVersion <- tmp[1]
    }
  }
  ##### ADDON CONFIG #####

  return(cfg)
}
