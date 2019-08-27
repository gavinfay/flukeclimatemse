# This script contains the management procedure function: inputcontrols

# To do:
  # search ???
  # build option so management not adjusted proportional to change in fishing, but how should it be adjusted?

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
#' @param magnitudeSetting A string specifying how the magnitude of management adjustments should be calculated
#'      "PropCatch" Calculates magnitude of adjustments proportional to most recent catch
#'      "Nonlinear" Calculates magnitude of adjustments using relationships identified by a GAM fitted to ______ see ______file/github extension... figure out how to add link in documentation ???
#' @param InputMngmt A string specifying what input controls should be adjusted selected from the following options: "AdjustBagSize", "AdjustMinSize", "AdjustSeason", "AdjustAll", "AdjustMixed", and "AdjustSpecific". No default.
#'      "AdjustBagSize" Specifies that only bag size should be adjusted to alter recreational catch, minimum landing size and season length remain unchanged throughout simulated projection.
#'      "AdjustMinSize" Specifies that only minimum landing size should be adjusted to alter recreational catch, bag size and season length remain unchanged throughout simulated projection.
#'      "AdjustSeason" Specifies that only season length should be adjusted to alter recreational catch, bag size and minimum landing size remain unchanged throughout simulated projection.
#'      "AdjustAll" Specifies that bag size, minimum landing size, and season length should all be be adjusted to alter recreational catch.
#'      "AdjustMixed" Randomly select to adjust between 0 and 3 input controls for each state & then randomly select that number of input controls from: bag size, minimum size, and season length to implement together in each state, this setting only functions if DecisionArea == "StatesIndependent".
#'      "AdjustSpecific" Specifies that bag size, minimum landing size, and season length be fixed at specified settings for entire projection to test the settings of interest, requires an additional argument: adjustspecific.
#' @param adjustspecific A matrix containing specific settings for bag size, minimum landing size, and season length (rows labeled: "bagsize" "minsize" "seasonlength") by state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
#'
#' @return A list containing BagSize, and SeasonLength matrices rounded to the nearest integer and MinSize rounded to the nearest half inch with updated management settings appended in the final row.
#'
#' @family management procedure functions
#'
#' @examples
#' # Example data
#' test_Bag <- matrix(c(8,8,8,8,8,8,8,8,8,3,3,3,3,3,3,3,3,3,7,5,6,7,8,4,8,8,8), ncol=9, nrow=3, byrow = TRUE)
#' test_Min <- matrix(c(18.5,19.5,19.5,21.0,18.0,18.5,19.0,18.5,15.0,17.5,18.5,18.5,20.5,18.0,18.0,17.5,17.5,15.0,16.5,18.5,18.0,19.5,17.5,18.0,17.0,16.5,15.0), ncol=9, nrow=3, byrow = TRUE)
#' test_Season <- matrix(c(44,137,50,78,105,306,152,245,306,102,123,103,115,101,166,220,306,245,132,184,109,153,142,237,199,245,306), ncol=9, nrow=3, byrow=TRUE)
#' test_Catch <- matrix(c(50381.54,71738.58,44944.42,298403.2,824887.0,87231.66,64646.92,289075.05,74641.14,45155.74,118455.00,35027.98,334323.7,552400.5,53512.04,25214.74,260050.29,77157.15,58371.74,161124.75,47071.41,376197.7,736848.2,66819.69,15346.65,317674.46,60422.12), ncol=9, nrow=3, byrow = TRUE)
#' test_Quota <- 6384158 # this is the 2018 total commercial & recreational quota
#' test_Availability <- matrix(c(0.1,0.1,0.1,0.2,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.4,0.5,0.5,0.5,0.6,0.6,0.6,0.7,0.7,0.7,0.8,0.8,0.8,0.9,0.9,0.9), ncol=9, nrow=3, byrow = TRUE)
#' test_adjustspecific <- matrix(c(rep(3,9), rep(15,9), rep(100,9)), ncol=9, nrow=3, byrow=TRUE)
#' rownames(test_adjustspecific) <- c("bagsize", "minsize", "seasonlength")
#' # Generate decision area indexing
#' test_AreaIndex <- decisionarea(DecisionArea = "StatesIndependent", NStates = 9)
#' # Generate necessary quota
#' test_quotas <- allocatequota(QuotaMethod = "PropBioAvailability", Availability = test_Availability, areaindex = test_areaindex, TotalQuota = test_Quota)
#'
#' ##### Examples: Adjust season length input controls #####
#' # AdjustBagSize
#' inputcontrols(areaindex = test_AreaIndex, CatchObs = test_Catch, areaquotas = test_quotas$areaQuotas, iYear=2, BagSize = test_Bag,
#'               MinSize = test_Min, SeasonLength = test_Season, InputMngmt = "AdjustBagSize")
#' # AdjustMinSize
#' inputcontrols(areaindex = test_AreaIndex, CatchObs = test_Catch, areaquotas = test_quotas$areaQuotas, iYear=2, BagSize = test_Bag,
#'               MinSize = test_Min, SeasonLength = test_Season, InputMngmt = "AdjustMinSize")
#' # AdjustSeason
#' inputcontrols(areaindex = test_AreaIndex, CatchObs = test_Catch, areaquotas = test_quotas$areaQuotas, iYear=2, BagSize = test_Bag,
#'               MinSize = test_Min, SeasonLength = test_Season, InputMngmt = "AdjustSeason")
#' # AdjustAll
#' inputcontrols(areaindex = test_AreaIndex, CatchObs = test_Catch, areaquotas = test_quotas$areaQuotas, iYear=2, BagSize = test_Bag,
#'               MinSize = test_Min, SeasonLength = test_Season, InputMngmt = "AdjustAll")
#' # AdjustMixed
#' inputcontrols(areaindex = test_AreaIndex, CatchObs = test_Catch, areaquotas = test_quotas$areaQuotas, iYear=2, BagSize = test_Bag,
#'               MinSize = test_Min, SeasonLength = test_Season, InputMngmt = "AdjustMixed")
#' # AdjustSpecific
#' inputcontrols(areaindex = test_AreaIndex, CatchObs = test_Catch, areaquotas = test_quotas$areaQuotas, iYear=2, BagSize = test_Bag,
#'               MinSize = test_Min, SeasonLength = test_Season, InputMngmt = "AdjustSpecific", adjustspecific = test_adjustspecific)



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

  # Calculate magnitude of necessary adjustment by comparing recent catch in each decision area to each quota
  if(magnitudeSetting = "PropCatch"){ # Magnitude of adjustments are proportional to catch
    for(iarea in 1:length(areaindex)){
      recentAreaCatch <- CatchObs[nrow(CatchObs), areaindex[[iarea]]]
      adjustmentRatio <- recentAreaCatch/areaquotas[iarea] # Compare recent catch to area quota ??? is this the comparison I want to make? currently can lead to unrealistic settings for management
      # ??? this is where I would need to change from proportional to not, currently proportional, this adjustment could be be calculated in other ways

      # All available input controls are adjusted by the same proportion
      if(adjustmentRatio =< 1){ # catch < quota allow increasing adjustment OR catch = quota allow no adjustment
        adjustments[areaindex[[iarea]]] <- adjustmentRatio
      } else { # catch > quota require decreasing adjustment based on how far over quota, if catch greater than 2X quota this requires adjustments to no fishing ??? run this past Gavin & see last few lines before return
        adjustments[areaindex[[iarea]]] <- -1*(recentAreaCatch-areaquotas[iarea])/areaquotas[iarea]
      }
    }
  } else if(magnitudeSetting = "Nonlinear"){ # Based on GAM fitted to ______# ??? need to fill this section in & provide link to code where GAM fitted, also document

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
      BagSize[nrow(BagSize),] <- BagSize[nrow(BagSize)-1,] + adjustments*BagSize[nrow(BagSize)-1,]
      MinSize[nrow(MinSize),] <- MinSize[nrow(MinSize)-1,] + adjustments*MinSize[nrow(MinSize)-1,]
      SeasonLength[nrow(SeasonLength),] <- SeasonLength[nrow(SeasonLength)-1,] + adjustments*SeasonLength[nrow(SeasonLength)-1,]
    } else if(InputMngmt == "AdjustSpecific"){ # require optional argument adjustspecific which is a matrix with the columns by state/rows by bag, min size, season (if similar mngmt tested across multi-state decision area then these states should have the same) rows must be labeled "bagsize" "minsize" "seasonlength" fixes mngmt settings at specified settings for entire projection to test specifics of interest
      # Replace randomly selected starting management settings with the specified settings in iYear == 1
      BagSize[nrow(BagSize)-1,] <- adjustspecific["bagsize",]
      MinSize[nrow(MinSize)-1,] <- adjustspecific["minsize",]
      SeasonLength[nrow(SeasonLength)-1,] <- adjustspecific["seasonlength",]

      BagSize[nrow(BagSize),] <- BagSize[nrow(BagSize)-1,]
      MinSize[nrow(MinSize),] <- MinSize[nrow(MinSize)-1,]
      SeasonLength[nrow(SeasonLength),] <- SeasonLength[nrow(SeasonLength)-1,]
    } else if(InputMngmt == "AdjustMixed"){ # Randomly select to adjust between 0 and 3 input controls & then randomly select that number of input controls to implement together, doesn't work with multi-state decision areas
      pickNumFix <- sample(0:3, ncol(BagSize), replace=TRUE) # pick number of settings to fix
      for(istate in 1:ncol(BagSize)){
        pickFix <- sample(1:3, as.numeric(pickNumFix[istate])) # pick the input controls to fix based on pickNumFix
        if(1 %in% pickFix){
          BagSize[nrow(BagSize),istate] <- BagSize[nrow(BagSize)-1,istate]
        } else{
          BagSize[nrow(BagSize),istate] <- BagSize[nrow(BagSize)-1,istate] + adjustments[istate]*BagSize[nrow(BagSize)-1,istate]
        }
        if(2 %in% pickFix){
          MinSize[nrow(MinSize),istate] <- MinSize[nrow(MinSize)-1,istate]
        } else{
          MinSize[nrow(MinSize),istate] <- MinSize[nrow(MinSize)-1,istate] + adjustments[istate]*MinSize[nrow(MinSize)-1,istate]
        }
        if(3 %in% pickFix){
          SeasonLength[nrow(SeasonLength),istate] <- SeasonLength[nrow(SeasonLength)-1,istate]
        } else{
          SeasonLength[nrow(SeasonLength),istate] <- SeasonLength[nrow(SeasonLength)-1,istate] + adjustments[istate]*SeasonLength[nrow(SeasonLength)-1,istate]
        }
      }
    }  # end "AdjustMixed" option
  } # end adjust management section

  # Check that there are no negative settings which could occur if realized catch >= 2Xquota, if these conditions met essentially shut down fishing
  BagSize[which(BagSize < 0)] <- 0 # Keep no fish
  MinSize[which(MinSize < 0)] <- 30 # Only large fish caught
  SeasonLength[which(SeasonLength < 0)] <- 0 # Closed season

  return(list(BagSize = round(BagSize), MinSize = (round(MinSize/0.5)/0.5), SeasonLength = round(SeasonLength))) # Round BagSize and SeasonLength to nearest integer, round minimum size to neares 0.5 inch
}
