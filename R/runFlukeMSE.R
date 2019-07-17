# This script contains the run function which loops over all sub-model components to run the fluke MSE

#' @title Run through entire MSE loop for fluke
#'
#' @description
#'
#'
#'
#' @param
#' @param
#'
#' @return
#'
#'
#' @examples
#'
runFlukeMSE <- function(){
  # Set up storage
  StoreCommercialQuota <- rep(NA, Nproj)
  StoreAreaQuotas # ??? figure out how to specify size to match mngmt

  # Need something like the last 2 lines to save quota from mngmtprocedure() output
  StoreCommercialQuota[iYear] <- quotaOutput$commercialQuota
  StoreAreaQuotas[iYear,] <- quotaOutput$areaQuotas

}
