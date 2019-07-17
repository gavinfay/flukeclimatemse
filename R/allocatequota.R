# This script contains the management procedure function: allocatequota

# To Do:
  # search ???
  # fix historic quota allocation option



#' @title Specify the amount of quota allocated to different decision areas
#' @description This function determines how much quota is allocated to the recreational fishery (40% of the total quota) and breaks down this recreational quota by state or multi-state decision areas using the specified method.
#'
#' @param QuotaMethod A string specifying which method should be used to allocate quota to decision areas: "Historic" or "BioAvailability", no default.
#'      "Historic" allocates quota based on historic participation in the recreational summer flounder fishery from 1980-1988 as is done in the current (2018) stock assessment. ??? double check dates
#'      "BioAvailability" allocates quota based on biomass availability by state as determined ??? update once availability determined correctly (not based on OM, use OM+error "survey")
#' @param Availability A matrix of fish biomass availability by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default. ??? (divide biomass by state in proportion with habitat availability = proportion of total habitat which is in that state's fishing waters) from OM, probably need to make a "SurveyAvailability" = OM availability + error for this piece
#' @param areaindex A list which specifies column indexing for different management decision areas, no default. Produced by the decisionarea()
#' @param TotalQuota A number specifying the total allowable quota based on the most recent stock assessment predicted stock size, no default.
#'
#' @return A list containing a number for the commercial quota and a vector of quota allocations by decision area.
#'
#' @family management procedure functions
#'
#' @examples
#'

allocatequota <- function(QuotaMethod = NULL,
                          Availability = NULL,
                          areaindex = NULL,
                          TotalQuota = NULL){
  # Divide total quota between recreational & commercial fisheries
  RecQuota <- TotalQuota*0.4 # 40% total allocation to recreational fishery
  CommercialQuota <- TotalQuota - RecQuota # 60% total allocation to commercial fishery

  if(QuotaMethod == "Historic"){ # base on reference period
    # ??? calculate quota by decision area
    # areaquotas <- ???
  } else if (QuotaMethod == "BioAvailability"){ # base on current biomass availability
    areaquotas <- rep(NA, length(areaindex))
    for(iarea in 1:length(areaindex)){
      # Calculate biomass availability in decision area in proportion to coast-wide availability
      BAvailable <- sum(Availability[nrow(Availability), areaindex[[iarea]]])
      AvailableRatio <- BAvailable/sum(Availability)
      # Calculate quota by decision area
      areaquotas[iarea] <- RecQuota*AvailableRatio
    }
  }

  return(list(commercialQuota = CommercialQuota, areaQuotas = areaquotas))
}


# # ??? test function
# test_Availability <- matrix(c(0.1,0.1,0.1,0.2,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.4,0.5,0.5,0.5,0.6,0.6,0.6,0.7,0.7,0.7,0.8,0.8,0.8,0.9,0.9,0.9), ncol=9, nrow=3, byrow = TRUE)
# AreaIndex <- list(1,2,3,4,5,6,7,8,9)
# test_Quota <- 6384158 # this is the 2018 total commercial & recreational quota
# allocatequota(QuotaMethod = "BioAvailability", Availability = test_Availability, areaindex = AreaIndex, TotalQuota = test_Quota)

