# This script contains the operating model function: truestocksize

# To do:
  # search ??? for unresolved issues
  # test to make sure projections run to unfished when recruitment turned off
  # make sure R distribution matches input when R turned on

#' @title Generate "true" age-structured stock size
#'
#' @description This function projects the operating model recruitment and abundance forward in time and uses these values to calculate stock biomass.
#'
#' @param Catch A vector of catches combined over states (in weight) for each of 3 fleets in the order: recreational catch, recreational discards, combined commercial catch & discards, no default.
#' @param Catch_tminusone A vector of catches combined over states (in weight) for each of 3 fleets in the order: recreational catch, recreational discards, combined commercial catch & discards, in the year prior to Catch (i.e. year t-1), no default.
#' @param N_abund A vector of abundance at age (numbers) at the start of the year (t), no default.
#' @param N_tminusone A vector of abundance at age (numbers) at the start of year (t-1), the year prior to N_abund values, no default.
#' @param OM_R Mean of historical recruitments
#' @param OMR_dev Most recent year's recruitment deviation
#' @param deterministic A string containing "OFF" to allow dynamic equations and "ON" to force deterministic projection without fishing
#'
#' @return A list containing total biomass in year t+1, recruitment in year t+1, survey biomass in year t, abundance in year t+1, recruitment deviation in year t+1
#'
#' @family operating model functions
#'
#' @examples
#' # Example data
#' # Fleet catch & discards
#' fluke_rec_catch_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Recreational catch
#' fluke_rec_disc_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Recreational discards
#' fluke_com_catdisc_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Combined commercial catch and discards
#' # Survey abundance observations and standard deviations
#' fluke_survey_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv",skip=32, nrow=6, head=F)[,2]
#' fluke_survey_SD <- read.csv("/Users/ahart2/Downloads/HWK4.csv",skip=32, nrow=6, head=F)[,3]
#' fluke_Robs <- c(81955,	102427,	46954,	78263,	81397,	53988,	12474,	36963,	44019,	47704,	47264,	43928,	58403,	78348,	59520,	52374,	54518,	44100,	60551,	64979,	67860,	50131,	71270,	40634,	48153,	52646,	62460,	73747,	51331,	31296,	35187,	36719,	42271,	29833,	35853,	42415)# Recruitment timeseries (000s) from 66th Northeast Regional Stock Assessment Workshop Assessment Report (NEFSC 2019) Table A87, page 238, years 1982-2017.
#' # Example
#' truestocksize(Catch = c(fluke_rec_catch_obs[length(fluke_rec_catch_obs)], fluke_rec_disc_obs[length(fluke_rec_disc_obs)], fluke_com_catdisc_obs[length(fluke_com_catdisc_obs)]),
#'               Catch_tminusone = c(fluke_rec_catch_obs[length(fluke_rec_catch_obs)-1], fluke_rec_disc_obs[length(fluke_rec_disc_obs)-1], fluke_com_catdisc_obs[length(fluke_com_catdisc_obs)-1]),
#'               N_abund = c(12000, 13000, 12900, 12800, 12500, 12000, 11000, 10000), # ??? placeholder example values
#'               N_tminusone = c(13000, 14000, 13900, 13800, 13500, 13000, 12000, 11000),
#'               OM_R = c(rep(11000,30)),
#'               OMR_dev = c(rep(0.1,30)))

truestocksize <- function(Catch = NULL,
                          Catch_tminusone = NULL,
                          N_abund = NULL,
                          N_tminusone = NULL,
                          meanR = NULL,
                          OMR_dev = NULL,
                          deterministic = "OFF"){
  # Fixed/locally calculated inputs
    # Weight at age starting with age 0
    w_age <- c(0.001075, 0.022275, 0.110525, 0.282350, 0.527550, 0.824425, 1.149625, 1.483100) # See Summer_Flounder_MSE/Data/Data_formatting.R for source
    # M at age
    M_age <- c(0.26, 0.26, 0.26, 0.25, 0.25, 0.25, 0.25, 0.24) # ages 0-7+ from 66th Northeast Regional Stock Assessment Workshop Assessment Report (NEFSC 2019) Table A90, page 241.
    # Recreational catch selectivity at age
    rec_cat_s <- c(0.02333333, 0.21666667, 0.56666667, 0.73333333, 0.87666667, 0.78333333, 0.80000000, 0.75000000) # ??? may want to update with more accurate numbers in future see Data_formatting.R
    # Recreational discards selectivity at age
    rec_disc_s <- c(0.1000000, 0.7333333, 0.7000000, 0.5600000, 0.4266667, 0.2833333, 0.2666667, 0.2666667) # ??? may want to update with more accurate numbers in future see Data_formatting.R
    # Commercial combined catch and discards selectivity at age
    com_s <- c(0.6483333, 0.5516667, 0.5850000, 0.7666667, 0.4483333, 0.4083333, 0.5550000, 0.8083333) # ??? may want to update with more accurate numbers in future see Data_formatting.R

  # Exploitation rate due to single fleets
    # Recreational catch
    u_rec_cat <- Catch[1]/sum(w_age*rec_cat_s*N_abund*exp(-0.5*M_age))
    # Recreational discards
    u_rec_disc <- Catch[2]/sum(w_age*rec_disc_s*N_abund*exp(-0.5*M_age))
    # Commercial catch & discards
    u_com <- Catch[3]/sum(w_age*com_s*N_abund*exp(-0.5*M_age))

  # Exploitation rate due to all fleets (u_exploit)
  if(deterministic == "OFF"){
    u_exploit <- u_rec_cat*rec_cat_s + u_rec_disc*rec_disc_s + u_com*com_s # dynamic projection
  } else{
    u_exploit <- 0 # deterministic projection
  }

  # Vector of numbers at age after natural and all fishing mortality (N_tilda)
  N_tilda <- N_abund*exp(-M_age)*(1-u_exploit) # ??? check that this vector calculation work

  # Plus group requires addition of growth & survival of age 7+ fish from previous year (t-1)
  u_rec_cat_tminusone <- Catch_tminusone[1]/sum(w_age*rec_cat_s*N_tminusone*exp(-0.5*M_age)) # Recreational catch
  u_rec_disc_tminusone <- Catch_tminusone[2]/sum(w_age*rec_disc_s*N_tminusone*exp(-0.5*M_age)) # Recreational discards
  u_com_tminusone <- Catch_tminusone[3]/sum(w_age*com_s*N_tminusone*exp(-0.5*M_age)) # Commercial catch & discards

  if(deterministic == "OFF"){
    u_exploit_tminusone <- u_rec_cat_tminusone*rec_cat_s + u_rec_disc_tminusone*rec_disc_s + u_com_tminusone*com_s # Exploitation rate due to all fleets (u_exploit) # dynamic projection
  } else{
    u_exploit_tminusone <- 0  # deterministic projection
  }
  N_tilda_tminusone <- N_tminusone*exp(-M_age)*(1-u_exploit_tminusone) # Vector of numbers at age after natural and all fishing mortality (N_tilda) # ??? check that this vector calculation work
    # Add N_tilda_tminusone to N_tilda for plus group to represent survival and growth of surviving individuals
  N_tilda[length(N_tilda)] <- N_tilda[length(N_tilda)] + N_tilda_tminusone[length(N_tilda_tminusone)]
  # Recruitment in year t+1
    # Fixed/locally calculated inputs
  R_sigma <- 0.2 # Fixed input for marginal variance of recruitment deviations ??? update with correct number
  R_rho <- 0.414 # Fixed input for magnitude of recruitment autocorrelation, see Data_formatting.R
    # Calculations
  delta_tplusone <- rnorm(1, 0, (R_sigma*R_sigma)) # 1 obs, mean=0, variance, change in recruitment
  Rdev_t <- OMR_dev # Recruitment deviation from most recent year (ε_t)
  Rdev_tplusone <- R_rho*Rdev_t + delta_tplusone*((1-R_rho^2)^0.5) # (ε_t+1)
  if(deterministic == "OFF"){
    R_tplusone <- meanR*exp(Rdev_tplusone - 0.5*(R_sigma^2)) # dynamic projection
  } else{
    R_tplusone = meanR  # deterministic projection
  }

  # Calculate OM Biomass in year t+1
  OM_Bio <- sum(w_age[1]*R_tplusone + N_tilda[-1]*w_age[-1]) # Age zero fish abundance come only from Recruitment, not abundance*weight calculations ??? # test that vector calculations work ???

  return(list(OM_Bio = OM_Bio, # Number for stock biomass in year t+1
              new_Nabund = N_tilda, # Updated abundance in year t
              OM_Rdevproj = Rdev_tplusone, # Newly calculated recruitment deviation
              OM_Rproj = R_tplusone)) # Newly calculated recruitment
}

 # Example data
# Fleet catch & discards
 fluke_rec_catch_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Recreational catch
 fluke_rec_disc_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Recreational discards
 fluke_com_catdisc_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Combined commercial catch and discards
 # Survey abundance observations and standard deviations
 fluke_survey_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv",skip=32, nrow=6, head=F)[,2]
 fluke_survey_SD <- read.csv("/Users/ahart2/Downloads/HWK4.csv",skip=32, nrow=6, head=F)[,3]
 fluke_Robs <- c(81955,	102427,	46954,	78263,	81397,	53988,	12474,	36963,	44019,	47704,	47264,	43928,	58403,	78348,	59520,	52374,	54518,	44100,	60551,	64979,	67860,	50131,	71270,	40634,	48153,	52646,	62460,	73747,	51331,	31296,	35187,	36719,	42271,	29833,	35853,	42415)# Recruitment timeseries (000s) from 66th Northeast Regional Stock Assessment Workshop Assessment Report (NEFSC 2019) Table A87, page 238, years 1982-2017.
 # Example
 truestocksize(Catch = c(fluke_rec_catch_obs[length(fluke_rec_catch_obs)], fluke_rec_disc_obs[length(fluke_rec_disc_obs)], fluke_com_catdisc_obs[length(fluke_com_catdisc_obs)]),
               Catch_tminusone = c(fluke_rec_catch_obs[length(fluke_rec_catch_obs)-1], fluke_rec_disc_obs[length(fluke_rec_disc_obs)-1], fluke_com_catdisc_obs[length(fluke_com_catdisc_obs)-1]),
               N_abund = c(12000, 13000, 12900, 12800, 12500, 12000, 11000, 10000), # ??? placeholder example values
               N_tminusone = c(13000, 14000, 13900, 13800, 13500, 13000, 12000, 11000),
               meanR = c(rep(11000,30)),
               OMR_dev = c(rep(0.1,30)))


###### Real data example
# Recreational Catch
TotRecCatDat <- as.matrix(read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Data/RecLandings.csv"))
# Recreational Discards
TotRecDiscDat <- as.matrix(read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Data/RecDiscards.csv"))
# Combined commercial catch and discards
ComCatDiscDat <- as.matrix(read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Data/CommercialCatDisc.csv"))

# Recruitment timeseries (000s) from 66th Northeast Regional Stock Assessment Workshop Assessment Report (NEFSC 2019) Table A87, page 238, years 1982-2017.
RData <- c(81955,	102427,	46954,	78263,	81397,	53988,	12474,	36963,	44019,	47704,	47264,	43928,	58403,	78348,	59520,	52374,	54518,	44100,	60551,	64979,	67860,	50131,	71270,	40634,	48153,	52646,	62460,	73747,	51331,	31296,	35187,	36719,	42271,	29833,	35853,	42415)
RMean <- mean(RData)
Rdevs <- c(exp(4.89E-01),	exp(7.12E-01), exp(-6.81E-02),	exp(4.43E-01),	exp(4.82E-01),	exp(7.17E-02),	exp(-1.39E+00),	exp(-3.07E-01),	exp(-1.32E-01),	exp(-5.21E-02),	exp(-6.13E-02),	exp(-1.34E-01),	exp(1.51E-01),	exp(4.45E-01),	exp(1.71E-01),	exp(4.30E-02),	exp(8.36E-02),	exp(-1.28E-01),	exp(1.90E-01),	exp(2.62E-01),	exp(3.06E-01),	exp(4.73E-03),	exp(3.59E-01),	exp(-2.01E-01),	exp(-2.63E-02),	exp(5.39E-02),	exp(2.23E-01),	exp(3.88E-01),	exp(2.63E-02),	exp(-4.70E-01),	exp(-3.55E-01),	exp(-3.12E-01),	exp(-1.79E-01),	exp(-5.34E-01),	exp(-3.68E-01),	exp(-1.83E-01)) # log values of recruitment deviations from assess_cor_file.csv
# 2018 assessment abundance estimates at age from 66th Northeast Regional Stock Assessment Workshop Assessment Report (NEFSC 2019) Table A89, pg240 2018, years 1982-2017
AbundData <- read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Data/SourceFiles/Abund_at_age_A89_NEFSC66AssessmentReport.csv", header=TRUE)
AbundData <- AbundData[,2:9]
for(icol in 1:ncol(AbundData)){
  AbundData[,icol] <- as.numeric(gsub(",","",AbundData[,icol]))
}
colnames(AbundData) <- c("Age0", "Age1", "Age2", "Age3", "Age4", "Age5", "Age6", "Age7plus")

tempRdevs <- c(rep(0.2,length(RData))) # placeholder, assessment estimated as normally distributed and summed to 1, maybe need proxy here???

# 1 year projection
truestocksize(Catch = c(TotRecCatDat[length(TotRecCatDat)], TotRecDiscDat[length(TotRecDiscDat)], ComCatDiscDat[length(ComCatDiscDat)]), # updated
              Catch_tminusone = c(TotRecCatDat[length(TotRecCatDat)-1], TotRecDiscDat[length(TotRecDiscDat)-1], ComCatDiscDat[length(ComCatDiscDat)-1]), # updated
              N_abund = AbundData[nrow(AbundData),], # updated
              N_tminusone = AbundData[nrow(AbundData)-1,], # updated
              meanR = RMean, # updated
              OMR_dev = Rdevs[length(Rdevs)]) # log values of recruitment deviations from assess_cor_file.csv
### Run projection to unfished biomass
Biomass <- NULL
for(iyear in 1:50){
  projResult <- truestocksize(Catch = c(0, 0, 0), # updated
                              Catch_tminusone = c(0, 0, 0), # updated
                              N_abund = AbundData[nrow(AbundData),], # updated
                              N_tminusone = AbundData[nrow(AbundData)-1,], # updated
                              meanR = RMean, # updated
                              OMR_dev = Rdevs[length(Rdevs)],# updated
                              deterministic = "ON")
  AbundData <- rbind(AbundData, projResult$new_Nabund)
  RData <- projResult$OM_Rproj
  tempRdevs <- projResult$OM_Rdevproj
  Biomass <- rbind(Biomass, projResult$OM_Bio)
}
rownames(AbundData) <- c()
Years <- seq(1982, 2017+50)
plot(y = rowSums(AbundData)[1:47], x = Years[1:47])
plot(Biomass)
# Stock crashes after fishing turned off then grows basically exponentially, why ???
