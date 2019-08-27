# This script contains the operating model function: flukeOM

#' @title Generate operating model data
#'
#' @description
#'
#'
#' @param Catch A vector of catches for each of 3 fleets in the order: recreational catch, recreational discards, combined commercial catch & discards, no default.
#' @param Catch_tminusone A vector of catches for each of 3 fleets in the order: recreational catch, recreational discards, combined commercial catch & discards, in the year prior to Catch (i.e. year t-1), no default.
#' @param N_abund A vector of abundance at age at the start of the year (t), no default.
#' @param N_tminusone A vector of abundance at age at the start of year (t-1), the year prior to N_abund values, no default.
#' @param Recruitment A vector of historic recruitments which are updated by the operating model throughout each simulation
#' @param R_devs A vector of historic recruitment deviations which are updated by the operating model throughout each simulation from the operating model
#' # ??? may be missing parameters for thermalhabitat()
#'
#' @return A list containing the following:
#' Biomass projection for year t+1,
#' Updated abundance in year t,
#' Updated vector of operating model recruitment deviations
#' Updated vector of operating model recruitments
#' Number for survey biomass in year t
#' Number for standard deviation of survey biomass in year t
#' Proportion for habitat availability by state
#'
#' @family operating model functions
#'
#' @examples
#'

flukeOM <- function(Catch = NULL,
                    Catch_tminusone = NULL,
                    N_abund = NULL,
                    N_tminusone = NULL,
                    Recruitment = NULL,
                    R_devs = NULL){

  # Generate biomass, abundance, recruitment projections to year t+1
  OMstockoutput <- truestocksize(Catch = Catch,
                                 Catch_tminusone = Catch_tminusone,
                                 N_abund = N_abund,
                                 N_tminusone = N_tminusone,
                                 OM_R = Recruitment,
                                 OMR_dev = R_devs)

  # Generate survey biomass in year t
  OMsurvey <- dosurveys(N_abund = N_abund)

  # Calculate thermal habitat availability and OM biomass availability by state
  habitatOutput <- thermalhabitat() # ??? needs updating


  return(list(OM_Biomass = OMstockoutput$OM_Bio, # Biomass projection for year t+1
              OM_Abundance = OMstockoutput$new_Nabund, # Updated abundance in year t
              OM_Rdevs = OMstockoutput$OM_Rdevs, # Updated vector of operating model recruitment deviations (input with newly calculated deviation for most recent year appended)
              OM_R = OMstockoutput$OM_Rs, # Updated vector of operating model recruitments (input with new calculation appended)
              OM_Survey = OMsurvey$OM_Survey, # Number for survey biomass in year t
              OM_SurveyStdDev = OMsurvey$OM_surveyStdDev, # Number for standard deviation of survey biomass in year t
              HabitatAvailable = habitatOutput$thermalhabitat))
}





