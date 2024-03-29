# This script contains the management procedure function: decisionarea

#' @title Specify how quota is allocated to different area(s) by state or groups of states
#'
#' @description
#' This function takes the specified setting for management decision areas and returns the matrix indexing required to perform subsequent calculations for these decision areas.
#'
#' @param DecisionArea A string to specify the management decision areas for the simulation: specify "CoastWide", "StatesIndependent", "CTandNYGroup", "CTandNYandNJGroup", no default.
#'     "CoastWide" specifies one set of management regulations for all states in which summer flounder are caught.
#'     "StatesIndependent" specifies separate management regulations for each state in which summer flounder are caught.
#'     "CTandNYGroup" specifies separate management regulations for Massachusetts/Rhode Island/combined Connecticut and New York/New Jersey/combined Delaware, Maryland, and Virginia/North Carolina
#'     "CTandNYandNJGroup" specifies separate management regulations for Massachusetts/Rhode Island/combined Connecticut, New York, and New Jersey/combined Delaware, Maryland, and Virginia/North Carolina
#' @param NStates A number indicating the number of states, default = 9.
#'
#' @return An areaindex which specifies column indexing for different management decision areas. This indexing assumes that columns are in the following order by state: MA, RI, CT, NY, NJ, DE, MD, VA, NC
#'
#' @family Management procedure functions
#'
#' @examples #??? finish examples for all possible decision areas
#' # Specify independent states as decision areas
#' decisionarea(DecisionArea = "StatesIndependent", NStates = 9)
#' # Specify CT/NY and DE/MD/VA as combined decision areas with remaining states as indpendent decision areas
#' decisionarea(DecisionArea = "CTandNYGroup", NStates = 9)

decisionarea <- function(DecisionArea = NULL,
                         NStates = 9){
 if(DecisionArea == "CoastWide"){
    areaindex <- list(c(seq(1:NStates)))
  } else if(DecisionArea == "StatesIndependent"){
    areaindex <- as.list(seq(1:NStates))
  } else if(DecisionArea == "CTandNYGroup"){
    areaindex <- list(1, 2, c(3,4), 5, c(6,7,8), 9) # MA/RI/CT&NY/NJ/DE&MD&VA/NC
  } else if(DecisionArea == "CTandNYandNJGroup"){
    areaindex <- list(1, 2, c(3,4,5), c(6,7,8), 9) # MA/RI/CT&NY&NJ/DE&MD&VA/NC
  }

  return(areaindex)
}
