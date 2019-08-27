# This script contains the run function which loops over all sub-model components to run the fluke MSE
# ToDo
  # search ??? for unresolved questions/issues
  # search Placeholder to connect data between different model components

#??? this is the input I need to run this model
# Recruitment timeseries (000s) from 66th Northeast Regional Stock Assessment Workshop Assessment Report (NEFSC 2019) Table A87, page 238, years 1982-2017.
RData <- c(81955,	102427,	46954,	78263,	81397,	53988,	12474,	36963,	44019,	47704,	47264,	43928,	58403,	78348,	59520,	52374,	54518,	44100,	60551,	64979,	67860,	50131,	71270,	40634,	48153,	52646,	62460,	73747,	51331,	31296,	35187,	36719,	42271,	29833,	35853,	42415)



#' @title Run through entire MSE loop for fluke
#'
#' @description
#'
#' @param Nproj An integer specifying the length of the projection forward in time.
#' @param SimulationOutput A string containing the file path for the output file
####### From operating model
#' @param Abundance A vector of length two containing historic abundance (number of fish) in the last two years of the provided timeseries below (serves as starting point for abundance projections in Operating Model)
#' @param Recruitment A vector of historic recruitments which are updated by the operating model throughout each simulation
#' @param R_devs A vector of historic recruitment deviations which are updated by the operating model throughout each simulation from the operating model
####### From stock assessment model
#' @param RecreationCatch A matrix containing recreational catch observations by year (rows) and state (columns), no default.
#' @param RecreationDiscard A matrix containing recreational discards observations by year (rows) and state (columns), no default.
#' @param CommercialFishing A matrix of combined commercial catch and discards observations by year (rows) and state (columns), no default.
#' @param SurveyObservation A vector of survey abundance combined across states, no default.
#' @param Survey_SD A vector of survey standard deviations combined across states, no default.
####### From management model
#' @param DecisionArea A string to specify the management decision areas for the simulation: specify "CoastWide", "StatesIndependent", "CTandNYGroup", "CTandNYandNJGroup", no default.
#'     "CoastWide" specifies one set of management regulations for all states in which summer flounder are caught.
#'     "StatesIndependent" specifies separate management regulations for each state in which summer flounder are caught.
#'     "CTandNYGroup" specifies separate management regulations for Massachusetts/Rhode Island/combined Connecticut and New York/New Jersey/combined Delaware, Maryland, and Virginia/North Carolina
#'     "CTandNYandNJGroup" specifies separate management regulations for Massachusetts/Rhode Island/combined Connecticut, New York, and New Jersey/combined Delaware, Maryland, and Virginia/North Carolina
#' @param QuotaMethod A string specifying which method should be used to allocate quota to decision areas: "Historic" or "BioAvailability", no default.
#'      "Historic" allocates quota based on historic participation in the recreational summer flounder fishery from 1980-1988 as is done in the current (2018) stock assessment. ??? double check dates
#'      "BioAvailability" allocates quota based on biomass availability by state as determined ??? update once availability determined correctly (not based on OM, use OM+error "survey")
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
#' @param adjustspecific Optional matrix required by InputMngmt = "AdjustSpecific" setting, contains specific settings for bag size, minimum landing size, and season length (rows labeled: "bagsize" "minsize" "seasonlength") by state (columns in the following order: MA, RI, CT, NY, NJ, DE, MD, VA, NC), no default.
###### From fishermen response model
#' @param FishermanGAMData Matrix containing the following columns "Year","State","RecCatch","MinSize","BagSize","SeasonLength","Effort","SSB","BioAvailable","PopLat" #??? needs updating after ICES with availability instead of latitude proxy
#'
#' @return
#'
#'
#' @examples
#'

runFlukeMSE <- function(Nproj = 1,
                        ){
  ##### Set up storage
  # Operating model output that does not update existing input data set
  StoreOMBiomass <- NULL # OM biomass timeseries
  StoreOMRdevs <- NULL
  StoreHabitatAvailability <- NULL

  # Stock assessment output at end of Nproj length projection
  StoreTotalBiomass <- NULL # Timeseries of total biomass summed across states
  StoreRecruitment <- NULL # Calculated recruitment timeseries
  StoreMeanRecruitment <- NULL # Mean recruitment
  StoreRecDevs <- NULL # Recruitment deviation timeseries
  StoreCatchPred <- NULL # Predicted catch timeseries
  StoreSurvival <- NULL # Survival timeseries
  StoreF_vals <- NULL # Catch F timeseries estimates at end of Nproj projection



  # Store management output
  StoreCommercialQuota <- rep(NA, Nproj) # Vector containing commercial quota from projection
  StoreAreaQuotas # ??? figure out how to specify size to match mngmt # Matrix containing area quota from projection

  # Store fishermen response output

  ##### Initial calculations, constants across all simulations
  # Fit GAM to describe Fishermen Response, output is passed to fishermenresponse()
    # ??? the GAM used here needs to be updated after ICES with habitat availability rather than proxy
  FishermanGAM <- mgcv::gam(RecCatch ~ State*PopLat + s(MinSize) + s(BagSize,k=8) + s(SeasonLength), data = FishermanGAMData)
  # Calculate mean Recruitment from historic dataset
  meanR <- mean(Recruitment)

  for(iyear in 1:Nproj){
    ##### Operating Model ##########################################################################################
    OMcatchinput <- c(sum(RecreationCatch[nrow(RecreationCatch),]), sum(RecreationDiscard[nrow(RecreationDiscard),]), sum(CommercialFishing[nrow(CommercialFishing),]))
    OMcatchinput_tminusone <- c(sum(RecreationCatch[(nrow(RecreationCatch)-1),]), sum(RecreationDiscard[(nrow(RecreationDiscard)-1),]), sum(CommercialFishing[(nrow(CommercialFishing)-1),]))

    OM_Output <- flukeOM(Catch = OMcatchinput,
                         Catch_tminusone = OMcatchinput_tminusone,
                         N_abund = Abundance[length(Abundance)],
                         N_tilda_tminusone = Abundance[(length(Abundance)-1)],
                         Recruitment = meanRecruitment,
                         R_devs = R_devs[length(R_devs)])
    # Update OM biomass, survey, and availability from OM output
    StoreOMBiomass <- c(StoreOMBiomass, OM_Output$OM_Biomass)
    Abundance <- c(Abundance, OM_Output$OM_Abundance)
    Recruitment <- OM_Output$OM_R
    StoreOMRdevs <- OM_Output$OM_Rdevs
    SurveyObservation <- c(SurveyObservation, OM_Output$OM_Survey)
    SurveySD <- c(SurveySD, OM_Output$OM_surveyStdDev)
    StoreHabitatAvailability <- rbind(StoreHabitatAvailability, OM_Output$HabitatAvailable)


    ##### Stock assessment model ##########################################################################################
    assessOutput <- stockassess(Rec_catch = sum(RecreationCatch[nrow(RecreationCatch),]),
                                Rec_disc = sum(RecreationDiscard[nrow(RecreationDiscard),]),
                                Com_catdisc = sum(CommercialFishing[nrow(CommercialFishing),]),
                                Survey_obs = SurveyObservation, # Historic survey series updated by OM Don't know how/what to combine, append OM production here
                                Survey_SD = SurveySD, # Historic survey series updated by OM
                                Recruits = Recruitment) # Historic recruitment updated by OM

    # Update biomass, catch, and survey settings from stock assessment model output, outputs from the end of the projection will be stored in simulation output
    StoreTotalBiomass <- assessOutput$biomass # Total biomass summed across states
    StoreRecruitmentPred <- assessOutput$recruitment # Predicted recruitment timeseries
    StoreMeanRecruitment <- assessOutput$R_mean # Mean recruitment
    StoreRecDevs <- assessOutput$R_devs # Recruitment deviation timeseries
    StoreCatchPred <- assessOutput$catch_pred # Predicted catch timeseries
    StoreSurvival <- assessOutput$survival # Timeseries of survival
    StoreF_vals <- assessOutput$F_vals # Catch F estimate timeseries


    ##### Management procedure model ##########################################################################################
    MngmtOutput <- mngmtprocedure(DecisionArea = DecisionArea,
                                 QuotaMethod = QuotaMethod,
                                 Availability = OM_Output$HabitatAvailable, # Habitat availability by state, stored in StoreHabitatAvailibility
                                 TotalBiomass = assessOutput$biomass, # ??? need to calculate total quota as part of the management procedure model
                                 iYear = iyear,
                                 CatchObs = RecreationCatch,
                                 BagSize = BagSize,
                                 MinSize = MinSize,
                                 SeasonLength = SeasonLength,
                                 InputMngmtMethod = InputMngmtMethod)

    # Update management settings from management procedure model output
    BagSize <- MngmtOutput$BagSize
    MinSize <- MngmtOutput$MinSize
    SeasonLength <- MngmtOutput$SeasonLength
    StoreAreaQuotas[iyear,] <- MngmtOutput$AreaQuota
    StoreCommercialQuota[iyear] <- MngmtOutput$CommercialQuota

    # Update vector of combined commercial discards and landings, assume that entire quota is landed
    CommercialFishing <- c(CommercialFishing, MngmtOutput$CommercialQuota) # do commercial discards need to be calculated & added separately or are discards included in quota ???


    ##### Fishermen response model ##########################################################################################
    responseOutput <- fishermenresponse(Availability = OM_Output$HabitatAvailable) # ??? this needs updating

    # Update catch & discards
    RecreationCatch <- rbind(RecreationCatch, responseOutput$reccatPlaceholder)
    RecreationDiscard <- rbind(RecreationDiscard, responseOutput$recdiscPlaceholder)











  } # End of loop over projection years















  ##### Save simulation settings and outputs ##########################################################################################
  OutputList <- list()
  OutputJSON <- toJSON(OutputList)
  # write(prettify(SimulationOutput), file = SimulationOutput) # ??? do I need prettify?
  write(SimulationOutput, file = Simulationoutput)

} # End of function
