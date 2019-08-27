# This script contains the operating model function: dosurveys

#' @title Generate an index of biomass at age
#'
#' @description This function uses stock abundance at age to generate an index of biomass at age for use as the stock assessment survey data, assuming that catchability at age for the survey remains constant over time.
#'
#' @param N_abund A vector of abundance at age at the start of the year (t), no default.
#'
#' @return A list containing survey biomass in year the most recent year and survey standard deviation.
#'
#' @family operating model functions
#'
#' @examples
#'

dosurveys <- function(N_abund = NULL){
  # Fixed/locally calculated inputs
  q_age <- c(0.1,0.15,0.3,0.65,1.0,1.0,1.0,1.0) # Catchability at age for survey index, ??? in future may want to update with more accurate data (perhaps from NEFSC Fall bottom-trawl survey since Kleisner et al. 2017 found strong fall environmental changes/population shifts)
    # see Data_formatting.R for details

  # Calculate OM survey index (index of biomass at age) in year t
  Index_age <- q_age*N_abund
  OM_Survey <- sum(Index_age) # Sum over ages

  # Generate OM survey standard deviation ??? check done correctly
  OM_surveyStdDev = rnorm(1, mean = 17.77922, sd = 0.2) # ??? assume same dist. as data or simulate observation process (human error impact std dev), probably log-normal
    # avg variance over time, see line 177 Data_formatting.R

  return(list(OM_Survey=OM_Survey, # Number for survey biomass in year t
              OM_surveyStdDev = OM_surveyStdDev)) # Number for standard deviation of survey biomass in year t
}
