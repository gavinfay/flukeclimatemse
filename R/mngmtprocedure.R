# This script contains the management procedure function: mngmtprocedure
# ToDo:
  # search ??? and address questions/issues
  # search placeholder for variables/structures where info still needed

#' @title Determine management procedure settings
#'
#' @description This function takes inputs for recreational summer flounder management and translates these inputs to produce and adjust input control settings.
#'
#' @param DecisionArea A string to specify the management decision areas for the simulation: specify "CoastWide", "StatesIndependent", "CTandNYGroup", "CTandNYandNJGroup", no default.
#'     "CoastWide" specifies one set of management regulations for all states in which summer flounder are caught.
#'     "StatesIndependent" specifies separate management regulations for each state in which summer flounder are caught.
#'     "CTandNYGroup" specifies separate management regulations for Massachusetts/Rhode Island/combined Connecticut and New York/New Jersey/combined Delaware, Maryland, and Virginia/North Carolina
#'     "CTandNYandNJGroup" specifies separate management regulations for Massachusetts/Rhode Island/combined Connecticut, New York, and New Jersey/combined Delaware, Maryland, and Virginia/North Carolina
#' @param QuotaMethod A string specifying which method should be used to allocate quota to decision areas: "Historic" or "BioAvailability", no default.
#'      "Historic" allocates quota based on historic participation in the recreational summer flounder fishery from 1980-1988 as is done in the current (2018) stock assessment. ??? double check dates
#'      "BioAvailability" allocates quota based on biomass availability by state as determined ??? update once availability determined correctly (not based on OM, use OM+error "survey")
#' @param Availability A matrix of proportional availability by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default. ??? (divide biomass by state in proportion with habitat availability = proportion of total habitat which is in that state's fishing waters) from OM, probably need to make a "SurveyAvailability" = OM availability + error for this piece
#' @param TotalQuota A number specifying the total allowable quota based on the most recent stock assessment predicted stock size, no default.
#' @param iYear A number specifying the simulation year.
#' @param CatchObs A matrix of catch observations by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
#' @param BagSize A matrix of management settings for bag size by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
#' @param MinSize A matrix of management settings for minimum landing size by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
#' @param SeasonLength A matrix of management settings for season length (days) by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
#' @param InputMngmtMethod A string specifying what input controls should be adjusted selected from the following options: "AdjustBagSize", "AdjustMinSize", "AdjustSeason", "AdjustAll", "AdjustMixed", and "AdjustSpecific". No default.
#'      "AdjustBagSize" Specifies that only bag size should be adjusted to alter recreational catch, minimum landing size and season length remain unchanged throughout simulated projection.
#'      "AdjustMinSize" Specifies that only minimum landing size should be adjusted to alter recreational catch, bag size and season length remain unchanged throughout simulated projection.
#'      "AdjustSeason" Specifies that only season length should be adjusted to alter recreational catch, bag size and minimum landing size remain unchanged throughout simulated projection.
#'      "AdjustAll" Specifies that bag size, minimum landing size, and season length should all be be adjusted to alter recreational catch.
#'      "AdjustMixed" Randomly select to adjust between 0 and 3 input controls for each state & then randomly select that number of input controls from: bag size, minimum size, and season length to implement together in each state, this setting only functions if DecisionArea == "StatesIndependent".
#'      "AdjustSpecific" Specifies that bag size, minimum landing size, and season length be fixed at specified settings for entire projection to test the settings of interest, requires an additional argument: adjustspecific.
#' @param adjustspecific Optional matrix required by InputMngmtMethod = "AdjustSpecific" setting, contains specific settings for bag size, minimum landing size, and season length (rows labeled: "bagsize" "minsize" "seasonlength") by state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
#'
#' @return A list containing BagSize, MinSize, and SeasonLength matrices with updated management settings appended in the final row, a number for ComercialQuota and a vector of recreational quota named AreaQuota.
#'
#' @family management procedure functions
#'
#' @examples
#'

mngmtprocedure <- function(DecisionArea = NULL,
                           QuotaMethod = NULL,
                           Availability = NULL,
                           TotalQuota = NULL,
                           iYear = NULL,
                           CatchObs = NULL,
                           BagSize = NULL,
                           MinSize = NULL,
                           SeasonLength = NULL,
                           InputMngmtMethod = NULL, ...){
  # Set decision area indexing
  decisionareaindex <- decisionarea(DecisionArea = DecisionArea, NStates = ncol(BagSize)) # decision area indexing remains consistent throughout simulation


  # Allocate quota to decision areas
  quotaOutput <- allocatequota(QuotaMethod = QuotaMethod,
                               Availability = Availability,
                               areaindex = decisionareaindex,
                               TotalQuota = TotalQuota)

  # Adjust input controls
  controlOutput <- inputcontrols(areaindex = decisionareaindex,
                                CatchObs = CatchObs,
                                areaquotas = quotaOutput$areaQuotas,
                                iYear = iYear,
                                BagSize = BagSize,
                                MinSize = MinSize,
                                SeasonLength = MinSize,
                                InputMngmtMethod = InputMngmtMethod, ...)

  return(list(BagSize = controlOutput$BagSize, MinSize = controlOutput$MinSize, SeasonLength=controlOutput$SeasonLength, AreaQuota = quotaOutput$areaQuotas, CommercialQuota = quotaOutput$commercialQuota))
}


# ##### Test Management #####
# # Read in test data
# test_Bag <- read.csv("/Users/ahart2/Research/temp_Summer_Flounder_MSE/Data/RecBagSize.csv", header = TRUE, row.names = 1) # ??? correct path
# test_Min <- read.csv("/Users/ahart2/Research/temp_Summer_Flounder_MSE/Data/RecMinSize.csv", header = TRUE, row.names = 1) # ??? correct path
# test_Season <- read.csv("/Users/ahart2/Research/temp_Summer_Flounder_MSE/Data/RecSeasonLength.csv", header = TRUE, row.names = 1) # ??? correct path
# test_Catch <- read.csv("/Users/ahart2/Research/temp_Summer_Flounder_MSE/Data/RecreationalCatch.csv", header = TRUE, row.names = 1) # ??? correct path
# test_Quota <- 6384158 # this is the 2018 total commercial & recreational quota
# test_Availability <- matrix(c(0.1,0.1,0.1,0.2,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.4,0.5,0.5,0.5,0.6,0.6,0.6,0.7,0.7,0.7,0.8,0.8,0.8,0.9,0.9,0.9), ncol=9, nrow=3, byrow = TRUE)
#
# # ??? test function
# mngmtprocedure(DecisionArea = "StatesIndependent",
#                     QuotaMethod = "BioAvailability",
#                     Availability = test_Availability,
#                     TotalQuota = test_Quota,
#                     iYear = 2, # iYear=1 tests random selection of starting values, iYear=2 tests adjustment of only 1 setting
#                     CatchObs = test_Catch,
#                     BagSize = test_Bag,
#                     MinSize = test_Min,
#                     SeasonLength = test_Season,
#                     InputMngmtMethod = "AdjustBagSize") # ??? finish testing, tested already: "AdjustBagSize", "AdjustMinSize", "AdjustSeason"
#
# # ??? test function
# AreaIndex <- decisionarea(DecisionArea = "StatesIndependent", NStates = 9)
#
# # ??? test function
# store <- allocatequota(QuotaMethod = "BioAvailability", Availability = test_Availability, areaindex = AreaIndex, TotalQuota = test_Quota)
#
# # ??? test function
# inputcontrols(areaindex = AreaIndex, CatchObs = test_Catch, areaquotas = store$areaQuotas, iYear=2, BagSize = test_Bag,
#               MinSize = test_Min, SeasonLength = test_Season, InputMngmtMethod = "AdjustSeason")
#
#
# ########## example data sets #####################
#
# # Data must have same dimensions
# BagSize_example_matrix <- matrix(nrow=5,ncol=9,c(1,2,3,4,5,6,7,8,9,10,rep(1,35))) #FullMSEInput
# colnames(BagSize_example_matrix) <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
# rownames(BagSize_example_matrix) <- c("year1", "year2", "year3", "year4", "year5")
#
# adjustspecific_example <- matrix(ncol=9, nrow=3, byrow = TRUE, data = c(1,1,1,2,2,2,3,3,3, 4,4,4,5,5,5,6,6,6, 7,7,7,8,8,8,9,9,9))
# rownames(adjustspecific_example) <- c("bagsize", "minsize", "seasonlength")
# # adjustspecific is optional
