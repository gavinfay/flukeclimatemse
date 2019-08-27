# This script contains the operating model function: flukeOM

# To Do:
# Need to assess what portion of annual catch is from federal waters on average to see if it is reasonable to use habitat available in state waters as proxy for all rec fishing habitat
# state raster
# habitat availability
# parameter estimates from climate GAM


#' @title Generate thermal habitat availability by management decision area
#'
#' @description
#'
#'
#'
#' @param iYear A number representing the simulation year
#' @param
#'
#' @return
#'
#' @family operating model functions
#'
#' @examples
#'

thermalhabitat <- function(){
  # Calculate total suitable habitat in state waters coast wide
  # Calculate suitable habitat available in each state using GAM
  # Calculate proportion of suitable habitat in each state
    # return this value as a mutliplier to calculate biomass available by state
  # Eventually will have inputs to this function related to temperature projection


  # Temporary placeholder for the above distributes habitat proportional to state coastal area
  stateWaterProp <- c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.2) # proportion of coastal waters (within 3mi) from MA to NC belonging to each state # ??? currently a placeholder

  # Mvmt rate north by state from paper ??? currently temporary placeholder
                  # MA,     RI,   CT,   NY,   NJ,   DE, MD,   VA,    NC
  # rateofChange <- c(-1.76, -0.22, 0.29, 1.25, 2.58, NA, 4.82, 13.67, 23.96) # From Dubik et al. 2019 commercial fishery mvmt rate north in km/yr
  rateofChange <- c(rep(0.9,9)) # From Perretti and Thorson 2019 center of gravity for biomass in km/yr

  # Currently assume habitat equally distributed across states, area of state waters correspond to amount of habitat
  thermalhabitat <- stateWaterProp*(1-rateofChange)

  return(list(thermalhabitat = thermalhabitat)) # Thermal habitat availability by state in the order: MA, RI, CT, NY, NJ, DE, MD, VA, NC
}
