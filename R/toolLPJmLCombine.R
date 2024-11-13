#' @title toolLPJmLCombine
#'
#' @description Reads in and combines LPJmL data from LPJmL crops run.
#'              This function is used to select the correct runfolder specification
#'              and bind the objects from these runs together.
#'              Application example: rainfed and irrigated information comes from
#'              two different `crops` runs, since their growing periods differ.
#'              They have to be combined, e.g. for yields.
#'
#' @param lpjmlversion Switch between LPJmL versions (including add-ons (+*) for further version specification)
#' @param climatetype  Switch between different climate scenarios
#' @param subtype      Switch between runtype and variables in LPJmL
#' @param selectyears  Subset of years
#'
#' @return configuration as list
#' @author Felicitas Beier, Kristine Karstens
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom magclass mbind
#'
#' @export

toolLPJmLCombine <- function(lpjmlversion, climatetype, subtype, selectyears) {
  # LPJmL crop types
  lpj2mag     <- toolGetMapping("MAgPIE_LPJmL.csv", type = "sectoral", where = "mrlandcore")
  cropsLPJmL  <- unique(lpj2mag$LPJmL5)

  # Combine irrigated and rainfed information for all crops
  irX <- list()
  rfX <- list()
  out <- list()

  # Read in by crop because of memory issues
  for (crop in cropsLPJmL) {
    # read in irrigated band for irrigated growing period
    irX[[crop]] <- calcOutput("LPJmLharmonize", subtype = paste0("cropsIr:", subtype),
                              subdata = paste("irrigated", crop, sep = "."),
                              lpjmlversion = lpjmlversion, climatetype = climatetype,
                              years = selectyears,
                              aggregate = FALSE)
    # read in rainfed band for rainfed growing period
    rfX[[crop]] <- calcOutput("LPJmLharmonize", subtype = paste0("cropsRf:", subtype),
                              subdata = paste("rainfed", crop, sep = "."),
                              lpjmlversion = lpjmlversion, climatetype = climatetype,
                              years = selectyears,
                              aggregate = FALSE)
    # combined irrigated and rainfed information
    out[[crop]] <- mbind(rfX[[crop]], irX[[crop]])
  }
  out  <- mbind(out)

  return(out)
}
