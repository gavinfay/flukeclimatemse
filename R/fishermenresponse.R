# This script contains the fishermen response model function: fishermenresponse


#' @title Generate realized catch based on fishermen response
#'
#' @description
#'
#'
#'
#' @param
#' @param PopLat Temp population latitude from thermalhabitat, placeholder forces shift north until after ICES
# #' @param Availability A matrix of proportional availability by year (rows) and state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default. ??? (divide biomass by state in proportion with habitat availability = proportion of total habitat which is in that state's fishing waters) from OM, probably need to make a "SurveyAvailability" = OM availability + error for this piece

#'
#' @return
#'
#' @family fishermen response model functions
#'
#' @examples
#'



fishermenresponse <- function(GAM = FishermanGAM,
                              Year = NULL,
                              State = NULL,
                              MinSize = NULL,
                              BagSize = NULL,
                              SeasonLength = NULL,
                              Effort = NULL,
                              SSB = NULL,
                              PopLat = NULL){ # need Availability = after ICES

  # probably need to add something here to constrain catch so that rec + comemrcial catch can't exceed OM stock size, if it is exceeded how should I make adjustments ???

  # ??? the GAM used here needs to be updated
    # see Data_fomattin.R for model validation questions/concerns
    # GAM should be fitted in runFlukeMSE() before loops (1 time per simulation so as not to slow down with refitting same GAM)

  # Storage
  RealizedCatch <- rep(NA, 9)
  names(RealizedCatch) <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")


  # Massachusetts
  updatedataMA <- data.frame(State = as.character("MA"), MinSize = MinSize, BagSize = BagSize, SeasonLength = SeasonLength, Effort = Effort, SSB = SSB, PopLat = Availability) # ??? need to change PopLat to Availability when updating model after ICES
  RealizedCatch["MA"] <- predict.gam(FishermanGAM, newdata = updatedataMA)

  # Rhode Island
  updatedataRI <- data.frame(State = as.character("RI"), MinSize = MinSize, BagSize = BagSize, SeasonLength = SeasonLength, Effort = Effort, SSB = SSB, PopLat = Availability)
  RealizedCatch["RI"] <- predict.gam(FishermanGAM, newdata = updatedataRI)

  # Connecticut
  updatedataCT <- data.frame(State = as.character("CT"), MinSize = MinSize, BagSize = BagSize, SeasonLength = SeasonLength, Effort = Effort, SSB = SSB, PopLat = Availability)
  RealizedCatch["CT"] <- predict.gam(FishermanGAM, newdata = updatedataCT)

  # New York
  updatedataNY <- data.frame(State = as.character("NY"), MinSize = MinSize, BagSize = BagSize, SeasonLength = SeasonLength, Effort = Effort, SSB = SSB, PopLat = Availability)
  RealizedCatch["NY"] <- predict.gam(FishermanGAM, newdata = updatedataNY)

  # New Jersey
  updatedataNJ <- data.frame(State = as.character("NJ"), MinSize = MinSize, BagSize = BagSize, SeasonLength = SeasonLength, Effort = Effort, SSB = SSB, PopLat = Availability)
  RealizedCatch["NJ"] <- predict.gam(FishermanGAM, newdata = updatedataNJ)

  # Delaware
  updatedataDE <- data.frame(State = as.character("DE"), MinSize = MinSize, BagSize = BagSize, SeasonLength = SeasonLength, Effort = Effort, SSB = SSB, PopLat = Availability)
  RealizedCatch["DE"] <- predict.gam(FishermanGAM, newdata = updatedataDE)

  # Maryland
  updatedataMD <- data.frame(State = as.character("MD"), MinSize = MinSize, BagSize = BagSize, SeasonLength = SeasonLength, Effort = Effort, SSB = SSB, PopLat = Availability)
  RealizedCatch["MD"] <- predict.gam(FishermanGAM, newdata = updatedataMD)

  # Virginia
  updatedataVA <- data.frame(State = as.character("VA"), MinSize = MinSize, BagSize = BagSize, SeasonLength = SeasonLength, Effort = Effort, SSB = SSB, PopLat = Availability)
  RealizedCatch["VA"] <- predict.gam(FishermanGAM, newdata = updatedataVA)

  # North Carolina
  updatedataNC <- data.frame(State = as.character("NC"), MinSize = MinSize, BagSize = BagSize, SeasonLength = SeasonLength, Effort = Effort, SSB = SSB, PopLat = Availability)
  RealizedCatch["NC"] <- predict.gam(FishermanGAM, newdata = updatedataNC)


  return(RealizedCatch)
}



# Script to fit GAM that relates input control settings to resulting catch, similar to preliminary work but use GAM instead
  # also use this to describe relationships that adjust management settings

# I want a matrix with row = year, columns = "Year", "RecCatch", "MinSize", "BagSize", "SeasonLength", "Effort", "SSB" ??? summed over waves



StateList <- c("VA", "CT", "DE", "MD", "MA", "NJ", "NY", "NC", "RI")
StateNames <- c("Virgina", "Connecticut", "Delaware", "Maryland", "Massachusetts", "New Jersey", "New York", "North Carolina", "Rhode Island")




# Recruitment timeseries (000s) from 66th Northeast Regional Stock Assessment Workshop Assessment Report (NEFSC 2019) Table A87, page 238, years 1982-2017.
RData <- c(81955,	102427,	46954,	78263,	81397,	53988,	12474,	36963,	44019,	47704,	47264,	43928,	58403,	78348,	59520,	52374,	54518,	44100,	60551,	64979,	67860,	50131,	71270,	40634,	48153,	52646,	62460,	73747,	51331,	31296,	35187,	36719,	42271,	29833,	35853,	42415)



# 66th Northeast Regional Stock Assessment Workshop Assessment Report (NEFSC 2019)
# Table A1, pg 121 commercial catch by state
# Table A14, pg 143 recreational landings at age old MRIP, A23 pg 156 for new MRIP, A28 pg 163 for dead discards






