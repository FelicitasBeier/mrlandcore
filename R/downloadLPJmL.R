#' @title downloadLPJmL
#' @description Download LPJmL content by version, climate model and scenario
#'
#' @param subtype Switch between different input
#' It consists of LPJmL version, climate model, scenario and variable.
#' For pasture lpjml runs, the scenario variable is used to navigate the output folder structure
#' (e.g. 'LPJmL4_for_MAgPIE_3dda0615:GSWP3-W5E5:historical:soilc' or
#' "LPJmL5.2_Pasture:IPSL_CM6A_LR:ssp126_co2_limN_00:soilc_past_hist")
#' @return metadata entry
#' @author Kristine Karstens, Marcos Alves, Felicitas Beier
#' @examples
#' \dontrun{
#' readSource("LPJmL", convert = FALSE)
#' }
#' @importFrom utils download.file untar
#' @importFrom madrat toolSplitSubtype

downloadLPJmL <- function(subtype = "lpjml5.9.5-m1:pnv:GSWP3-W5E5:historical:soilc") { # nolint

  x     <- toolSplitSubtype(subtype,
                            list(version      = NULL,
                                 runtype      = NULL,
                                 climatemodel = NULL,
                                 scenario     = NULL,
                                 variable     = NULL))

  map      <- toolGetMapping("lpjmlSubtype2Filename.csv", where = "mrlandcore")
  filename <- map$filename[map$subtype == x$variable]

  # build a zenodo download
  # check zenodor or ZenodoManager for easy downloading,
  # or simply use
  #     download.file(url, destfile)                            # nolint: commented_code_linter.
  #     untar(destfile)                                         # nolint: commented_code_linter.
  #     unlink/file.remove(destfile) -> should be rmoved???     # nolint: commented_code_linter.
  # if zenodo download is not available
  zenodo <- FALSE

  if (zenodo == FALSE) {
    path      <- paste0("/p/projects/landuse/LPJmL_for_MAgPIE/",  # nolint: absolute_path_linter.
                        "runs_", x$version, "/output/",
                        paste("run", tolower(x$climatemodel), x$scenario, x$runtype, sep = "_"),
                        "/", filename, ".tgz")
    if (file.exists(path)) {
      utils::untar(path, exdir = ".")
    } else {
      stop("Data is not available. Please check, if the variable was created
           for this version, runtype and climatescenario")
    }
  }

  # read metadata from somewhere maybe using #' @importFrom jsonlite fromJSON

  # Compose meta data
  return(list(url           = NULL,   # UPDATE
              doi           = NULL,    # UPDATE
              title         = subtype,    # UPDATE
              author        = list(person("Jens",      "Heinke",  email = "heinke@pik-potsdam.de"),
                                   person("Christoph", "Mueller", email = "cmueller@pik-potsdam.de")),
              version       = x$version,    # UPDATE
              release_date  = NULL,    # UPDATE
              description   = NULL,    # UPDATE
              license       = "Creative Commons Attribution 4.0 International (CC BY 4.0) License",
              reference     = NULL)
  )
}
