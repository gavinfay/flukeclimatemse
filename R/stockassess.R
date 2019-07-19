# This script contains the stock assessment model function: stockassess

# TO DO:
  # search ??? for updates with data, currently use HW4 examples for placeholders

#' @title Generate operating model data
#'
#' @description This function fits a delay-difference model to summer flounder catch, discards, and survey data to predict stock size.
#'
#' @param Rec_catch A vector containing recreational catch observations by state, no default.
#' @param Rec_disc A vector containing recreational discards observations by state, no default.
#' @param Com_catdisc A vector of combined commercial combined and discards observations by state, no default.
#' @param Survey_obs A vector of survey abundance by state, no default. observations & std dev
#' @param Survey_SD A vector of survey standard deviations by state, no default.
#'
#' @return A list of vectors containing biomass, recruitment, predicted recreational catch, predicted recreational discards, predicted combined commercial catch and discards, and survival.
#'
#' @family stock assessment model functions
#'
#' @examples
#'

stockassess <- function(){
  # setwd("/Users/arhart/Downloads")
  setwd("/Users/arhart/Downloads") # ??? this won't work in an R package, I need a way of referencing a cpp script or dll but I haven't figured out how to do this yet

  require(TMB) # ??? this needs to be added to the list of dependent packages

  # Model data (non-estimated parameter values and survey fitting info)
  Survey_yr <- seq(1:length(Survey_obs)) # Could make this a parameter if survey has gaps (shorter than the catch timeseries)
  Survey_length <- length(Survey_obs)
  W_dat <- 0.5
  M_dat <- 0.25
  Rho <- 0.995
  Nproj <- 0 # project forward 1 year, not currently used
  Ctarget <- 0 # no fishing in projection, not currently used

  # Create list of data objects, names of list items must match DATA objects in Cpp code
  ModelData <- list(rec_catch_obs = Rec_catch,
                    rec_disc_obs = Rec_disc,
                    com_catdisc_obs = Com_catdisc,
                    survey_obs = Survey_obs,
                    survey_SD = Survey_SD,
                    survey_yr = Survey_yr,
                    survey_length = Survey_length,
                    w_dat = W_dat,
                    M_dat = M_dat,
                    rho = Rho,
                    Nproj = Nproj,
                    Ctarget = Ctarget)

  # Create list of parameters and provide initial values (may include parameter vector, e.g. param_vec = rep(0,5))
  ModelParameters <- list(dummy=0, Logith_steep = 0.25, Bzero = 1400, Fval = rep(-2.0, Nyear-1))

  # Compile Cpp code
  # compile("/Users/ahart2/Research/flukeclimatemse/Fluke_StockAssessment.cpp") # file must be in working directory or provide full file path name
  compile("Fluke_Stock_Assessment.cpp")
  dyn.load(dynlib("Fluke_Stock_Assessment")) # ??? these two lines probably need to be adjusted so the cpp code can be referenced from this function properly, I just don't know how to do this yet

  # # Test code to check for minimization
  # testmap<-list(Logith_steep=factor(NA),Bzero=factor(NA),Fval=rep(factor(NA),Nyear-1)) # only estimate dummy parameter, other parameters fixed at starting commercials
  # testmodel <- MakeADFun(data=ModelData, parameters=ModelParameters, DLL="Fluke_Stock_Assessment",silent=T,map=testmap)
  # xx <- testmodel$fn(testmodel$env$last.par)
  #


  ##### Fit model to fluke #####
  # Use map function to specify which parameters to estimate, those that are not estimated are fixed at initial values and must have a factor(NA) in map list
  ModelMap <- list(dummy = factor(NA)) # rep(factor(NA),5) for a parameter vector of length 5

  # Construct objective function to optimize based on data, parameters, and Cpp code
  Model <- MakeADFun(data = ModelData, parameters = ModelParameters, DLL="Fluke_Stock_Assessment",silent=T,map = ModelMap) # silent=T silences a bunch of extra print statements

  # Set bounds on different parameters, length of this vector must equal number of estimate parameters
  lowbnd <- c(-100, 500,rep(-20,Nyear-1)) # rep( 0.1, 5) for a parameter vector of length 5 with lower bound 0.1, syntax for upper bound is the same
  uppbnd <- c(100,1500,rep(-0.01,Nyear-1)) # no bounds for mapped params!!!!!!!!!!

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

  # Check for Hessian
  # VarCo <- solve(Model$he())
  # print(sqrt(diag(VarCo)))

  # Get reported info & predicted data
  biomass <- Model$report()$biomass
  recruitment <- Model$report()$recruitment
  rec_catch_pred <- Model$report()$rec_catch_pred
  rec_disc_pred <- Model$report()$rec_disc_pred
  com_catdisc_pred <- Model$report()$com_catdisc_pred
  survival <- Model$report()$survival

  return(list(biomass = biomass,
              recruitment = recruitment,
              rec_catch_pred = rec_catch_pred,
              rec_disc_pred = rec_disc_pred,
              com_catdisc_pred = com_catdisc_pred,
              survival = survival))
}



##### Example #####
# Example data
Nyear <- 30 # UPDATE ??? # used here to specify changing length of catch/biomass timeseries, calculated in .cpp file
# Fleet catch & discards
fluke_rec_catch_obs <- read.csv("HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Recreational catch
fluke_rec_disc_obs <- read.csv("HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Recreational discards
fluke_com_catdisc_obs <- read.csv("HWK4.csv", skip=1, nrow=Nyear, head=F)[,2] # Combined commercial catch and discards
# Survey abundance observations and standard deviations
fluke_survey_obs <- read.csv("HWK4.csv",skip=32, nrow=6, head=F)[,2]
fluke_survey_SD <- read.csv("HWK4.csv",skip=32, nrow=6, head=F)[,3]




