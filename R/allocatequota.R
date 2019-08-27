# This script contains the management procedure function: allocatequota

# To Do:
  # search ???
  # update examples to include historic



#' @title Specify the amount of quota allocated to different decision areas
#' @description This function determines how much quota is allocated to the recreational fishery (40% of the total quota) and breaks down this recreational quota by state or multi-state decision areas using the specified method.
#'
#' @param QuotaMethod A string specifying which method should be used to allocate quota to decision areas: "Historic" or "BioAvailability", no default.
#'      "Historic" allocates quota based on historic participation in the recreational Summer flounder fishery from 1980-1989 as is done in the current (2018) stock assessment.
#'      "PropBioAvailability" allocates quota based on biomass availability by state as determined ??? update once availability determined correctly (not based on OM, use OM+error "survey")
#' @param Availability A matrix of proportional availability by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default. ??? (divide biomass by state in proportion with habitat availability = proportion of total habitat which is in that state's fishing waters) from OM, probably need to make a "SurveyAvailability" = OM availability + error for this piece
#' @param areaindex A list which specifies column indexing for different management decision areas, no default. Produced by the decisionarea()
#' @param TotalBiomass A number specifying the total biomass based on the most recent stock assessment, no default.
#'
#' @return A list containing a number for the commercial quota and a vector of quota allocations by decision area.
#'
#' @family management procedure functions
#'
#' @examples
#' # Example data
#' test_Availability <- matrix(c(0.1,0.1,0.1,0.2,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.4,0.5,0.5,0.5,0.6,0.6,0.6,0.7,0.7,0.7,0.8,0.8,0.8,0.9,0.9,0.9), ncol=9, nrow=3, byrow = TRUE)
#' test_areaindex <- list(1,2,3,4,5,6,7,8,9)
#' test_Quota <- 6384158 # This is the 2018 total commercial & recreational quota
#' # Allocate quota based on historic reference period from 1980 - 1989
#' allocatequota() #??? finish example
#' # Allocate quota proportional to biomass available
#' allocatequota(QuotaMethod = "PropBioAvailability", Availability = test_Availability, areaindex = test_areaindex, TotalQuota = test_Quota)


allocatequota <- function(QuotaMethod = NULL,
                          Availability = NULL,
                          areaindex = NULL,
                          BmsyProxy = NULL){

  # Fixed inputs
  UnfishedBiomass = # ??? This is the long-term average biomass when fishing is turned off, given mean recruitment, OM utilized to calculate in Data_formatting.R


  # Calculate fish biomass availability by state based on habitat Availability ??? update Availability calculation in thermalhabitat
  FishAvailable <- Availability[nrow(Availability),]*TotalBiomass

  # Set ABC at Bmsy proxy, from stock assessment ?????


  # Divide TAC between recreational & commercial fisheries
  RecAllocation <- TAC*0.4 # 40% total allocation to recreational fishery
  CommercialAllocation <- TAC - RecAllocation # 60% total allocation to commercial fishery

  if(QuotaMethod == "Historic"){ # based on reference period from 1980-1989
    # Historic allocation
    RefProps <- c(0.0549, 0.0566, 0.0375, 0.1763, 0.3909, 0.0314, 0.0295, 0.1669, 0.056) # See Data_formatting.R for source and calculation
    names(RefProps) <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")

    areaquotas <- rep(NA, length(areaindex))
    for(iarea in 1:length(areaindex)){
      AvailableRatio <- sum(RefProps[areaindex[[iarea]]])
      areaquotas[iarea] <- RecAllocation*AvailableRatio
    }

    # areaquotas <- ??? # see Data_formatting.R
  } else if (QuotaMethod == "PropBioAvailability"){ # quota proportional to current biomass availability
    areaquotas <- rep(NA, length(areaindex))
    for(iarea in 1:length(areaindex)){
      # Calculate biomass availability in decision area in proportion to coast-wide availability
      BAvailable <- sum(FishAvailable[areaindex[[iarea]]])
      AvailableRatio <- BAvailable/sum(FishAvailable)
      # Calculate quota by decision area
      areaquotas[iarea] <- RecAllocation*AvailableRatio
    }
  }

  return(list(commercialQuota = CommercialAllocation, areaQuotas = areaquotas))
}


