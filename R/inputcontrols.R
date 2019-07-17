# This script contains the management procedure function: inputcontrols

# To do:
  # search ???
  # build option so management not adjusted proportional to change in fishing, but how should it be adjusted?
  # fix so bag and season round to nearest whole number, min size round to nearest 0.5

#' @title Specify settings for input controls: minimum size, bag size, and season length
#'
#' @description This function compares catch observations and target area quotas (also called Recreational Harvest Limits or RHL) to determine and implement necessary adjustments to specified input controls.
#'
#' @param areaindex A list which specifies column indexing for different management decision areas, no default. Produced by the decisionarea()
#' @param CatchObs A matrix of catch observations by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
#' @param areaquotas A vector of quota allocations by decision area, no default.
#' @param iYear A number specifying the simulation year.
#' @param BagSize A matrix of management settings for bag size by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
#' @param MinSize A matrix of management settings for minimum landing size by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
#' @param SeasonLength A matrix of management settings for season length (days) by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
#' @param InputMngmt A string specifying what input controls should be adjusted selected from the following options: "AdjustBagSize", "AdjustMinSize", "AdjustSeason", "AdjustAll", "AdjustMixed", and "AdjustSpecific". No default.
#'      "AdjustBagSize" Specifies that only bag size should be adjusted to alter recreational catch, minimum landing size and season length remain unchanged throughout simulated projection.
#'      "AdjustMinSize" Specifies that only minimum landing size should be adjusted to alter recreational catch, bag size and season length remain unchanged throughout simulated projection.
#'      "AdjustSeason" Specifies that only season length should be adjusted to alter recreational catch, bag size and minimum landing size remain unchanged throughout simulated projection.
#'      "AdjustAll" Specifies that bag size, minimum landing size, and season length should all be be adjusted to alter recreational catch.
#'      "AdjustMixed" Randomly select to adjust between 0 and 3 input controls for each state & then randomly select that number of input controls from: bag size, minimum size, and season length to implement together in each state, this setting only functions if DecisionArea == "StatesIndependent".
#'      "AdjustSpecific" Specifies that bag size, minimum landing size, and season length be fixed at specified settings for entire projection to test the settings of interest, requires an additional argument: adjustspecific.
#' @param adjustspecific A matrix containing specific settings for bag size, minimum landing size, and season length (rows labeled: "bagsize" "minsize" "seasonlength") by state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
#'
#' @return A list containing BagSize, MinSize, and SeasonLength matrices with updated management settings appended in the final row.
#'
#' @family management procedure functions
#'
#' @examples
#'


inputcontrols <- function(areaindex = NULL,
                          CatchObs = NULL,
                          areaquotas = NULL,
                          iYear = NULL,
                          BagSize = NULL,
                          MinSize = NULL,
                          SeasonLength = NULL,
                          InputMngmt = NULL, ...){
  ##### Determine what adjustments are necessary ###########################################################################
  # temp storage
  recentAreaCatch <- rep(NA, length(areaindex))
  adjustments <- rep(NA, ncol(BagSize))

  for(iarea in 1:length(areaindex)){
    recentAreaCatch <- CatchObs[nrow(CatchObs), areaindex[[iarea]]]
    adjustmentRatio <- recentAreaCatch/areaquotas[iarea] # Compare recent catch to area quota ??? is this the comparison I want to make?
    # ??? this is where I would need to change from proportional to not, currently proportional, this adjustment could be be calculated in other ways

    # Currently adjustments change all specified input controls by the same proportion ??? is this a problem???
    if(adjustmentRatio < 1){
      adjustments[areaindex[[iarea]]] <- -1*adjustmentRatio # Decreasing adjustment
    } else{
      adjustments[areaindex[[iarea]]] <- adjustmentRatio - 1 # No adjustments made (= 0) or increasing adjustment
    }
  }

  ##### Specify settings for management input controls #####################################################################
  if(iYear == 1){ # In the first year of the projection select starting management setting from the historic distribution
    BagSize <- rbind(BagSize, rep(NA, ncol(BagSize)))
    MinSize <- rbind(MinSize, rep(NA, ncol(MinSize)))
    SeasonLength <- rbind(SeasonLength, rep(NA, ncol(SeasonLength)))
    for(istate in 1:ncol(BagSize)){
      # Bag size
      pickbag <- sample(1:(nrow(BagSize)-1),1)
      BagSize[nrow(BagSize), istate] <- BagSize[pickbag, istate]

      # Minimum size
      pickmin <- sample(1:(nrow(MinSize)-1),1)
      MinSize[nrow(MinSize), istate] <- MinSize[pickmin, istate]

      # Season length
      picklength <- sample(1:(nrow(SeasonLength)-1),1)
      SeasonLength[nrow(SeasonLength), istate] <- SeasonLength[picklength, istate]
    }
  } else{ # Adjust management ###################################################################################
    BagSize <- rbind(BagSize, rep(NA, ncol(BagSize)))
    MinSize <- rbind(MinSize, rep(NA, ncol(MinSize)))
    SeasonLength <- rbind(SeasonLength, rep(NA, ncol(SeasonLength)))
    if(InputMngmt == "AdjustSeason"){ # Fix bag and min size
      BagSize[nrow(BagSize),] <- BagSize[nrow(BagSize)-1,]
      MinSize[nrow(MinSize),] <- MinSize[nrow(MinSize)-1,]
      SeasonLength[nrow(SeasonLength),] <- SeasonLength[nrow(SeasonLength)-1,] + adjustments*SeasonLength[nrow(SeasonLength)-1,]
    } else if(InputMngmt == "AdjustMinSize"){
      BagSize[nrow(BagSize),] <- BagSize[nrow(BagSize)-1,]
      MinSize[nrow(MinSize),] <- MinSize[nrow(MinSize)-1,] + adjustments*MinSize[nrow(MinSize)-1,]
      SeasonLength[nrow(SeasonLength),] <- SeasonLength[nrow(SeasonLength)-1,]
    } else if(InputMngmt == "AdjustBagSize"){
      BagSize[nrow(BagSize),] <- BagSize[nrow(BagSize)-1,] + adjustments*BagSize[nrow(BagSize)-1,]
      MinSize[nrow(MinSize),] <- MinSize[nrow(MinSize)-1,]
      SeasonLength[nrow(SeasonLength),] <- SeasonLength[nrow(SeasonLength)-1,]
    } else if(InputMngmt == "AdjustAll"){
      BagSize[nrow(BagSize)-1,] + adjustments*BagSize[nrow(BagSize)-1,]
      MinSize[nrow(MinSize),] <- MinSize[nrow(MinSize)-1,] + adjustments*MinSize[nrow(MinSize)-1,]
      SeasonLength[nrow(SeasonLength),] <- SeasonLength[nrow(SeasonLength)-1,] + adjustments*SeasonLength[nrow(SeasonLength)-1,]
    } else if(InputMngmt == "AdjustSpecific"){ # require optional argument adjustspecific which is a matrix with the columns by state/rows by bag, min size, season (if similar mngmt tested across multi-state decision area then these states should have the same) rows must be labeled "bagsize" "minsize" "seasonlength" fixes mngmt settings at specified settings for entire projection to test specifics of interest
      if(iYear == 2){ # Replace randomly selected starting management settings with the specified settings in iYear == 1
        BagSize[nrow(BagSize)-1,] <- adjustspecific["bagsize",]
        MinSize[nrow(MinSize)-1,] <- adjustspecific["minsize",]
        SeasonLength[nrow(SeasonLength)-1,] <- adjustspecific["seasonlength",]
      }
      BagSize[nrow(BagSize),] <- BagSize[nrow(BagSize)-1,]
      MinSize[nrow(MinSize),] <- MinSize[nrow(MinSize)-1,]
      SeasonLength[nrow(SeasonLength),] <- SeasonLength[nrow(SeasonLength)-1,]
    } else if(InputMngmt == "AdjustMixed"){ # Randomly select to adjust between 0 and 3 input controls & then randomly select that number of input controls to implement together, doesn't work with multi-state decision areas
      pickNumFix <- sample(0:3, ncol(BagSize), replace=TRUE) # pick number of settings to fix
      for(istate in 1:ncol(BagSize)){
        pickFix <- sample(1:3, as.numeric(pickNumFix[istate])) # pick the input controls to fix based on pickNumFix
        if(1 %in% pickFix){
          BagSize[nrow(BagSize),] <- BagSize[nrow(BagSize)-1,]
        } else{
          BagSize[nrow(BagSize)-1,] + adjustments*BagSize[nrow(BagSize)-1,]
        }
        if(2 %in% pickFix){
          MinSize[nrow(MinSize),] <- MinSize[nrow(MinSize)-1,]
        } else{
          MinSize[nrow(MinSize),] <- MinSize[nrow(MinSize)-1,] + adjustments*MinSize[nrow(MinSize)-1,]
        }
        if(3 %in% pickFix){
          SeasonLength[nrow(SeasonLength),] <- SeasonLength[nrow(SeasonLength)-1,]
        } else{
          SeasonLength[nrow(SeasonLength),] <- SeasonLength[nrow(SeasonLength)-1,] + adjustments*SeasonLength[nrow(SeasonLength)-1,]
        }
      }
    } # end "AdjustMixed" option
  } # end adjust management section

  return(list(BagSize = BagSize, MinSize = MinSize, SeasonLength=SeasonLength))

}

# # Read in test data
# test_Bag <- read.csv("/Users/ahart2/Research/temp_Summer_Flounder_MSE/Data/RecBagSize.csv", header = TRUE, row.names = 1) # ??? correct path
# test_Min <- read.csv("/Users/ahart2/Research/temp_Summer_Flounder_MSE/Data/RecMinSize.csv", header = TRUE, row.names = 1) # ??? correct path
# test_Season <- read.csv("/Users/ahart2/Research/temp_Summer_Flounder_MSE/Data/RecSeasonLength.csv", header = TRUE, row.names = 1) # ??? correct path
# test_Catch <- read.csv("/Users/ahart2/Research/temp_Summer_Flounder_MSE/Data/RecreationalCatch.csv", header = TRUE, row.names = 1) # ??? correct path
# test_Quota <- 6384158 # this is the 2018 total commercial & recreational quota
# test_Availability <- matrix(c(0.1,0.1,0.1,0.2,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.4,0.5,0.5,0.5,0.6,0.6,0.6,0.7,0.7,0.7,0.8,0.8,0.8,0.9,0.9,0.9), ncol=9, nrow=3, byrow = TRUE)
#
# # ??? test function
# AreaIndex <- decisionarea(DecisionArea = "StatesIndependent", NStates = 9)
#
# # ??? test function
# store <- allocatequota(QuotaMethod = "BioAvailability", Availability = test_Availability, areaindex = AreaIndex, TotalQuota = test_Quota)
#
# # ??? test function
# inputcontrols(areaindex = AreaIndex, CatchObs = test_Catch, areaquotas = store$areaQuotas, iYear=2, BagSize = test_Bag,
#               MinSize = test_Min, SeasonLength = test_Season, InputMngmt = "AdjustSeason")
#
#
