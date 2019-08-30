library(TMB)


# Test compilation of stock assessment
compile("src/fluke_dd.cpp") # file must be in working directory or provide full file path name
dyn.load(dynlib("src/fluke_dd")) # automatically loaded due to @useDynLib comment when package is built



#######################
catch <- c(0.86,0.86,0.84,0.81,0.78,0.75,0.73,0.71,0.69,0.68,0.67,0.66,0.65,0.65,0.65,0.64,0.64,0.64,0.64,0.64,0.64,0.64,0.64,0.64,0.64,0.64,0.64,0.64,0.64,0.64)
bio <- c(10.00,10.00,9.76,9.42,9.06,8.74,8.45,8.22,8.02,7.87,7.75,7.65,7.58,7.52,7.48,7.45,7.42,7.40,7.39,7.38,7.37,7.37,7.36,7.36,7.36,7.35,7.35,7.35,7.35,7.35)

# Specify number of years of data
Nyear <- length(catch)

# Model data (non-estimated parameter values and survey fitting info) # ??? update bounds to reflect summer flounder
Survey_yr <- 1:length(bio) # Could make this a parameter if survey has gaps (shorter than the catch timeseries)
Survey_length <- length(bio)
Survey_SD <- rep(0.2,length(bio))
W_dat <- 0.5 # ??? update to reflect summer flounder weight gain parameter
M_dat <- 0.25 # Reflect 2018 Assessment
Rho <- exp(-0.14) # 0.995 # update to reflect summer flounder von Bertalanffy k = growth rate coefficient = 0.14 combined across sexes from the 66th Northeast Regional Stock Assessment Workshop Assessment Report (NEFSC 2019) page 58
Nproj <- 0 # project forward 1 year, not currently used (needs further development if projection desired in future)
Ctarget <- 0 # no fishing in projection, not currently used

# Create list of data objects, names of list items must match DATA objects in Cpp code
ModelData <- list(catch_obs = catch,
                  survey_obs = bio,
                  survey_SD = Survey_SD,
                  survey_yr = Survey_yr,
                  survey_length = Survey_length,
                  # R_obs = Recruits,
                  w_dat = W_dat,
                  M_dat = M_dat,
                  rho = Rho) #,
                  # Nproj = Nproj,
                  # Ctarget = Ctarget)

# Create list of parameters and provide initial values (may include parameter vector, e.g. param_vec = rep(0,5))
ModelParameters <- list(dummy=0,
                        logBstartval = rep(log(10),2), #c(1400, 1400), # Vector of biomass estimated in the first 2 years rather than assuming population is unfished at start of timeseries
                        #Fstartval = 0.2, # Vector length 3 containing: Fstartval is estimated as the unobserved fishing rate for recreational catch, discard and commercial fishing in the year before observed timeseries begins
                        logFvals = rep(log(0.1), Nyear-1), # Vector of fishing mortalities for years 1 to Nyear -1 accounting for total recreational catch & discards and commercial catch & discards
                        logq = log(1),
                        logR_mean = log(1), # Mean recruitment
                        logsigmaR = log(0.1),
                        R_randEffect = rep(0.0, Nyear-1)) # Recruitment random effect, normally distributed expect bounds between -2 and 2 so bounding at extremes of -5 ot 5 appropriate

# Compile Cpp code, automatically done when package is loaded
# compile("/Users/ahart2/Research/flukeclimatemse/src/Fluke_Stock_Assessment.cpp") # file must be in working directory or provide full file path name
# dyn.load(dynlib("/Users/ahart2/Research/flukeclimatemse/src/Fluke_Stock_Assessment")) # automatically loaded due to @useDynLib comment when package is built

# Test code to check for minimization
# testmap<-list(Logith_steep=factor(NA),Bzero=factor(NA),Fval=rep(factor(NA),Nyear-1)) # only estimate dummy parameter, other parameters fixed at starting commercials
# testmodel <- MakeADFun(data=ModelData, parameters=ModelParameters, DLL="Fluke_Stock_Assessment",silent=T,map=testmap)
# xx <- testmodel$fn(testmodel$env$last.par)


##### Fit model to fluke #####
# Use map function to specify which parameters to estimate, those that are not estimated are fixed at initial values and must have a factor(NA) in map list
ModelMap <- list(dummy = factor(NA)) # rep(factor(NA),5) for a parameter vector of length 5
ModelMap <- list(logBstartval = rep(factor(NA),2),
                 logFvals = rep(factor(NA),Nyear),
                 logR_mean = factor(NA),
                 logsigmaR = factor(NA),
                 logq = factor(NA),
                 R_randEffect = rep(factor(NA),Nyear-1)) # rep(factor(NA),5) for a parameter vector of length 5

# Construct objective function to optimize based on data, parameters, and Cpp code
Model <- MakeADFun(data = ModelData, parameters = ModelParameters, DLL="fluke_dd",silent=F,map = ModelMap) # silent=T silences a bunch of extra print statements
Model <- MakeADFun(data = ModelData, parameters = ModelParameters, DLL="fluke_dd") # silent=T silences a bunch of extra print statements
# Set bounds on different parameters, length of this vector must equal number of estimate parameters # ??? update bounds to reflect summer flounder
# Bstartval, Fstartval, Fvals, R_mean, sigmaR, R_randEffect
#lowbnd <- c(c(500, 500), c(0,0,0), rep(-20,Nyear-1), 1000, 0.001, rep(-5, Nyear)) # rep( 0.1, 5) for a parameter vector of length 5 with lower bound 0.1, syntax for upper bound is the same
#uppbnd <- c(c(1500, 1500), c(0.999,0.999,0.999), rep(-0.01,Nyear-1), 15000, 1.0, rep(5, Nyear)) # no bounds for mapped params!!!!!!!!!!

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

