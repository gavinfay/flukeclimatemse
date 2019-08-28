# This script contains the stock assessment model function: stockassess

# TO DO:
  # search ??? for updates with data, currently use HW4 examples for placeholders
  # Update parameters & bounds

#' @title Perform stock assessment
#'
#' @description This function fits a delay-difference model to summer flounder catch, discards, and survey data to predict stock size.
#'
#' @param Rec_catch A vector containing recreational catch observations summed over states, no default.
#' @param Rec_disc A vector containing recreational discards observations summed over states, no default.
#' @param Com_catdisc A vector of combined commercial catch and discards observations summed over states, no default.
#' @param Survey_obs A vector of survey abundance combined across states, no default. observations & std dev
#' @param Survey_SD A vector of survey standard deviations combined across states, no default.
#' @param Recruits A vector of observed recruitment, no default.
#'
#' @useDynLib Fluke_Stock_Assessment # DO NOT MESS WITH THIS, it adds TMB template to the namespace file so it is automatically loaded as a DLL, see also: http://www.hep.by/gnu/r-patched/r-exts/R-exts_94.html
#'
#' @return A list of vectors containing biomass, recruitment, predicted recreational catch, predicted recreational discards, predicted combined commercial catch and discards, and survival.
#'
#' @family stock assessment model functions
#'
#' @examples
#' # Example data
#' Nyear <- 30 # UPDATE ??? # used here to specify changing length of catch/biomass timeseries, calculated in .cpp file
#' # Fleet catch & discards
#' fluke_rec_catch_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Recreational catch
#' fluke_rec_disc_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Recreational discards
#' fluke_com_catdisc_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Combined commercial catch and discards
#' # Survey abundance observations and standard deviations
#' fluke_survey_obs <- read.csv("/Users/ahart2/Downloads/HWK4.csv",skip=32, nrow=6, head=F)[,2]
#' fluke_survey_SD <- read.csv("/Users/ahart2/Downloads/HWK4.csv",skip=32, nrow=6, head=F)[,3]
#' fluke_Robs <- c(81955,	102427,	46954,	78263,	81397,	53988,	12474,	36963,	44019,	47704,	47264,	43928,	58403,	78348,	59520,	52374,	54518,	44100,	60551,	64979,	67860,	50131,	71270,	40634,	48153,	52646,	62460,	73747,	51331,	31296,	35187,	36719,	42271,	29833,	35853,	42415)# Recruitment timeseries (000s) from 66th Northeast Regional Stock Assessment Workshop Assessment Report (NEFSC 2019) Table A87, page 238, years 1982-2017.
#'
#' # Example
#' stockassess(Rec_catch = fluke_rec_catch_obs, Rec_disc = fluke_rec_disc_obs, Com_catdisc = fluke_com_catdisc_obs, Survey_obs = fluke_survey_obs, Survey_SD = fluke_survey_SD, Recruits = fluke_Robs)


stockassess <- function(Rec_catch = NULL,
                        Rec_disc = NULL,
                        Com_catdisc = NULL,
                        Survey_obs = NULL,
                        Survey_SD = NULL,
                        Recruits = NULL){

  # require(TMB) # Added to DESCRIPTION file dependencies

  # Specify number of years of data
  Nyear <- length(Rec_catch)

  # Model data (non-estimated parameter values and survey fitting info) # ??? update bounds to reflect summer flounder
  Survey_yr <- seq(1:length(Survey_obs)) # Could make this a parameter if survey has gaps (shorter than the catch timeseries)
  Survey_length <- length(Survey_obs)
  W_dat <- 0.5 # ??? update to reflect summer flounder weight gain parameter
  M_dat <- 0.25 # Reflect 2018 Assessment
  Rho <- 0.14 # 0.995 # update to reflect summer flounder von Bertalanffy k = growth rate coefficient = 0.14 combined across sexes from the 66th Northeast Regional Stock Assessment Workshop Assessment Report (NEFSC 2019) page 58
  Nproj <- 0 # project forward 1 year, not currently used (needs further development if projection desired in future)
  Ctarget <- 0 # no fishing in projection, not currently used

  # Create list of data objects, names of list items must match DATA objects in Cpp code
  ModelData <- list(rec_catch_obs = Rec_catch,
                    rec_disc_obs = Rec_disc,
                    com_catdisc_obs = Com_catdisc,
                    survey_obs = Survey_obs,
                    survey_SD = Survey_SD,
                    survey_yr = Survey_yr,
                    survey_length = Survey_length,
                    R_obs = Recruits,
                    w_dat = W_dat,
                    M_dat = M_dat,
                    rho = Rho,
                    Nproj = Nproj,
                    Ctarget = Ctarget)

  # Create list of parameters and provide initial values (may include parameter vector, e.g. param_vec = rep(0,5))
  ModelParameters <- list(dummy=0,
                          Bstartval = c(1400, 1400), # Vector of biomass estimated in the first 2 years rather than assuming population is unfished at start of timeseries
                          Fstartval = 0.2, # Vector length 3 containing: Fstartval is estimated as the unobserved fishing rate for recreational catch, discard and commercial fishing in the year before observed timeseries begins
                          Fvals = rep(-2.0, Nyear-1), # Vector of fishing mortalities for years 1 to Nyear -1 accounting for total recreational catch & discards and commercial catch & discards
                          R_mean = 10000, # Mean recruitment
                          sigmaR = 0.2, # Recruitment deviation
                          R_randEffect = rep(0.001, Nyear)) # Recruitment random effect, normally distributed expect bounds between -2 and 2 so bounding at extremes of -5 ot 5 appropriate

  # Compile Cpp code, automatically done when package is loaded
  # compile("/Users/ahart2/Research/flukeclimatemse/src/Fluke_Stock_Assessment.cpp") # file must be in working directory or provide full file path name
  # # compile("Fluke_Stock_Assessment.cpp")
  # # dyn.load(dynlib("/Users/ahart2/Research/flukeclimatemse/src/Fluke_Stock_Assessment")) # automatically loaded due to @useDynLib comment when package is built

  # Test code to check for minimization
  # testmap<-list(Logith_steep=factor(NA),Bzero=factor(NA),Fval=rep(factor(NA),Nyear-1)) # only estimate dummy parameter, other parameters fixed at starting commercials
  # testmodel <- MakeADFun(data=ModelData, parameters=ModelParameters, DLL="Fluke_Stock_Assessment",silent=T,map=testmap)
  # xx <- testmodel$fn(testmodel$env$last.par)


  ##### Fit model to fluke #####
  # Use map function to specify which parameters to estimate, those that are not estimated are fixed at initial values and must have a factor(NA) in map list
  ModelMap <- list(dummy = factor(NA)) # rep(factor(NA),5) for a parameter vector of length 5

  # Construct objective function to optimize based on data, parameters, and Cpp code
  Model <- MakeADFun(data = ModelData, parameters = ModelParameters, DLL="Fluke_Stock_Assessment",silent=T,map = ModelMap) # silent=T silences a bunch of extra print statements

  # Set bounds on different parameters, length of this vector must equal number of estimate parameters # ??? update bounds to reflect summer flounder
    # Bstartval, Fstartval, Fvals, R_mean, sigmaR, R_randEffect
  lowbnd <- c(c(500, 500), c(0,0,0), rep(-20,Nyear-1), 1000, 0.001, rep(-5, Nyear)) # rep( 0.1, 5) for a parameter vector of length 5 with lower bound 0.1, syntax for upper bound is the same
  uppbnd <- c(c(1500, 1500), c(0.999,0.999,0.999), rep(-0.01,Nyear-1), 15000, 1.0, rep(5, Nyear)) # no bounds for mapped params!!!!!!!!!!

  # Fit model to data using structure provided by MakeADFun() function call
    # eval.max = max number evaluations of objective function
    # iter.max = max number of iterations allowed
    # rel.tol = relative tolerance
  fit <- nlminb(Model$par, Model$fn, Model$gr, control=list(rel.tol=1e-12,eval.max=100000,iter.max=1000), lower=lowbnd,upper=uppbnd) # notice the inclusion of the lower and upper bound vectors

  ##### Fitted model results #####
  # Best parameter estimates
  best <- Model$env$last.par.best
  # print(best)

  # Report parameter estimates & std error
  rep <- sdreport(Model)
  # print(summary(rep))

  # Print objective function
  # print(Model$report()$obj_fun)

  # Print objective (likelihood)
  # fit$objective

  # Check for Hessian ??? not positive definite probably a problem with Fstartval
  # VarCo <- solve(Model$he())
  # print(sqrt(diag(VarCo)))

  # Get reported info & predicted data
  biomass <- Model$report()$biomass # first two years set to the Bstartval estimated parameters
  recruitment <- Model$report()$recruitment
  catch_pred <- Model$report()$catch_pred
  survival <- Model$report()$survival
  # Get parameter estimates ??? check indexing
  R_mean <- rep$value[4]
  R_devs <- rep$value[7:(6+Nyear)]
  Fvals <- cbind(rep$value[(7+Nyear):length(rep$value)], rep$sd[(7+Nyear):length(rep$value)])
  colnames(Fvals) <- c("Value", "SD")
  # Other outputs to check
  Bstartval <- cbind(rep$value[1:2], rep$sd[1:2])
  Fstartval <- c(rep$value[3], rep$sd[3])

  return(list(biomass = biomass,
              recruitment = recruitment,
              catch_pred = catch_pred,
              survival = survival,
              R_mean = R_mean,
              R_devs = R_devs,
              Fvals = Fvals))
}


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

stockassess(Rec_catch = TotRecCatDat,
            Rec_disc = TotRecDiscDat,
            Com_catdisc = ComCatDiscDat,
            Survey_obs = rowSums(AbundData),
            Survey_SD = rep(0.2,36),
            Recruits = RData)

