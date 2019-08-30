// Stock Assessment Model: Deriso delay-difference model
// last modified by G.Fay 08/30/19
// test with fluke data

#include <TMB.hpp> // Maybe this needs .h extension for Rcpp ???

// Optional function which square the value provided
template <class Type> Type square(Type x){return x*x;}

// Objective function which returns the value to minimize (often the negative log likelihood)
template <class Type>
Type objective_function<Type>::operator() ()
{
  ///// Data section /////
  // Catch observations, vectors must be of equal lengths
  DATA_VECTOR(catch_obs); // catch over time
  // DATA_VECTOR(rec_disc_obs); // Recreational discards observed numbers over time (summed over states)
  // DATA_VECTOR(com_catdisc_obs); // Commercial combined catch & discards observed numbers over time (summed over states)

  // // Survey abundance observations & std dev
  DATA_VECTOR(survey_obs);
  DATA_VECTOR(survey_SD);
  DATA_INTEGER(survey_length); // Length of survey (# years survey conducted) ??? may need to be altered for individual surveys
  DATA_IVECTOR(survey_yr); // Survey years ??? see above

  // // Recruitment observations
  // DATA_VECTOR(R_obs);
  //

  // Other data
  DATA_SCALAR(w_dat); // weight gain parameter value (not estimated)
  DATA_SCALAR(M_dat); // Natural mortality rate
  DATA_SCALAR(rho); //Prow = rho

  // // For projections
  // DATA_INTEGER(Nproj); // Number of projection years
  // DATA_SCALAR(Ctarget); // Catch target
  //
  //

  ///// Parameter section /////
  PARAMETER(dummy); // Include dummy variable to debug code without full estimation of likelihood
  // Estimated starting conditions
  PARAMETER_VECTOR(logBstartval); // Vector of biomass estimated in the first 2 years rather than assuming population is unfished at start of timeseries
  //PARAMETER(Fstartval); // Fstartval is estimated as the unobserved fishing rate in the year before observed timeseries begins
  // Fishing mortality
  PARAMETER_VECTOR(logFvals); // Vector of fishing mortalities for years 1 to Nyear -1 accounting for total recreational catch & discards and commercial catch & discards
  // survey
  PARAMETER(logq);
  // Recruitment
  PARAMETER(logR_mean); // Mean recruitment
  PARAMETER(logsigmaR); // Annual recruitment deviations
  PARAMETER_VECTOR(R_randEffect); // Recruitment random effect


  // Local variables
  int Nyear; // Number of data years
  //Nyear = rec_catch_obs.size();
  Nyear = catch_obs.size();

  // int TotalYears; // Determine total number of years = number of data years + number of projection years
  int TotalYears = Nyear; // + Nproj;

  vector<Type> biomass(TotalYears + 1); // Biomass prediction storage vector + 1 since projected 1 year into the future
  vector<Type> recruitment(TotalYears); // Recruitment storage vector
  vector<Type> survival(TotalYears); // Survival prediction storage vector
  vector<Type> catch_pred(TotalYears); // Total catch prediction (combined recretional catch & discards and commercial catch & discards) storage vector = CatHat in Andre's code

  //vector<Type> catch_obs(TotalYears); // Total observed catch summed across (recreational catch & discards and commercial catch & discards from data input)
  //catch_obs = rec_catch_obs + rec_disc_obs + com_catdisc_obs; // element wise sum of catch observations

  Type Term1, Term2, Term3; // temporary variables for biomass prediction equation terms
  int Lag_tplusone,Lag_t; // Indexing integers for recruitment lag in timestep t and t+1 in

  // Objective function
  Type obj_fun;
  obj_fun = 0; // NegativeLogLikelihood initialized at zero


  /// model predictions
  survival = exp(-1.0*(M_dat+exp(logFvals)));   // this is continuous mortality, but is not the equation you have in your document
  ///// Years 1 and 2 are special because of delay /////
  // Year 1
  // Biomass
  biomass(0) = exp(logBstartval(0)); // Estimate first year biomass
  // Survival
  //survival(0) = exp(-1.0*(M_dat+exp(logFvals(0)))); // survival rate after M and F, where F = Fstartval is estimated as the unobserved fishing rate in the year before observed timeseries begins
  // Year 2
  biomass(1) = exp(logBstartval(1));
  //survival(1) = exp(-1.0*(M_dat+exp(logFvals(1))));
  for(int iyear=2; iyear<Nyear; iyear++){
   recruitment(iyear-1) = exp(logR_mean)*exp(exp(logsigmaR)*R_randEffect(iyear-1) - 0.5*square(exp(logsigmaR)));  //recruitment index is lagged 1 because we don't need R in year 1
   biomass(iyear) = (1 + rho)*survival(iyear-1)*biomass(iyear-1) -
                    rho*survival(iyear-1)*survival(iyear)*biomass(iyear-2) -
                    rho*w_dat*survival(iyear-1)*recruitment(iyear-2) +
                    recruitment(iyear-1);

  }

  //catch predictions  (GF assuming instantaneous mortality rates)
  catch_pred = biomass*(1.0-survival)*exp(logFvals)/(M_dat+exp(logFvals));


  // objective function
  //recruitment
  obj_fun -= sum(dnorm(R_randEffect,0.0,1.0,1));

  vector<Type> catch_sd(Nyear);
  catch_sd = 0.2; // assuming 0.2 CV on catches?
  //catch
  obj_fun -= sum(dnorm(log(catch_pred),log(catch_obs),catch_sd,1));

  //survey index
  //int temp_survey_yr; // temporary placeholder for current survey year
  vector<Type> bio_pred(survey_length);
  for(int iyear=0; iyear<survey_length; iyear++)
    bio_pred(iyear) = logq + log(biomass(survey_yr(iyear)));
    //temp_survey_yr = survey_yr(iyear);
    // Biomass log likelihood component
  obj_fun -= sum(dnorm(bio_pred,log(survey_obs),survey_SD,1));
      //0.5*square(log(biomass(temp_survey_yr)) - log(survey_obs(iyear)))/square(survey_SD(iyear));

//
//
//   // Recruitment prediction & log likelihood
//   recruitment(0) = R_mean*exp(R_randEffect(0) - sigmaR*sigmaR/2); // Recruitment prediction
//   vector<Type> tempRandEffect(1);
//   tempRandEffect = R_randEffect(0);
//   vector<Type> tempsigmaR(1);
//   tempsigmaR = sigmaR; // Could maybe get away with single values for temp sigmaR and tempMean
//   vector<Type> tempMean(1);
//   tempMean  = 0.0;
//   obj_fun += dnorm(tempRandEffect,tempMean,tempsigmaR,1)[0]; // Recruitment log likelihood component, give_log = 1 returns log probability
//   // Gave error that couldn't find function definition that matched arguments when single numbers provided instead of vectors, check "type" of argument (e.g. Type, double...) & format (e.g. vector...)
//
//   // Total catch prediction & likelihood
//   catch_pred(0) = exp(Fstartval)*biomass(0);
//   obj_fun += 0.5*square(log(catch_pred(0)) - log(catch_obs(0)))/square(0.2);
//
//   // Year 2 biomass
//   biomass(1) = Bstartval(1); // Estimate second year biomass
//
//   ///// Year 2+ rec_catch_pred, com_catdisc_pred, recruitment & Year 3+ biomass /////
//   for(int iyear=1; iyear<Nyear; iyear++){
//     // Total catch prediction & log likelihood
//     catch_pred(iyear) = exp(Fvals(iyear-1))*biomass(iyear); // ??? Check this, how break into rec/commercial & catch/discards
//     obj_fun += 0.5*square(log(catch_pred(iyear)) - log(catch_obs(iyear)))/square(0.2); // Catch SD = 0.2 ???
//     // Survival
//     survival(iyear) = exp(-1.0*M_dat)*(1-exp(Fvals(iyear-1))); // survival rate after M and F
//     // Recruitment prediction & log likelihood
//     recruitment(0) = R_mean*exp(R_randEffect(iyear) - sigmaR*sigmaR/2); // Recruitment prediction
//     vector<Type> temp2RandEffect(1);
//     temp2RandEffect = R_randEffect(iyear);
//     vector<Type> temp2sigmaR(1);
//     temp2sigmaR = sigmaR; // Could maybe get away with single values for temp sigmaR and tempMean
//     vector<Type> temp2Mean(1);
//     temp2Mean  = 0.0;
//     obj_fun += dnorm(temp2RandEffect,temp2Mean,temp2sigmaR,1)[0]; // Recruitment log likelihood component, give_log = 1 returns log probability
//
//     // Lag indexing to recruitment
//     // index for year t+1
//     if(iyear+1 - 5 < 0){ // Year 1-3
//       Lag_tplusone = 0; // previously called Lag
//     } else { // Year 4+
//       Lag_tplusone = iyear+1 - 5;
//     }
//     // index for year t
//     if(iyear-5 < 0){
//       Lag_t = 0; // previously called Lag1
//     } else {
//       Lag_t = iyear - 5;
//     }
//
//     // Update dynamics to predict biomass in next year (based on Bt+1 = (1+rho)*exp(-M)(Bt-Ct) - rho*exp(-2M)*(1-Ct/Bt)*((Bt-1)-(Ct-1))-rho*weight*exp(-M)*(1-Ct/Bt)*Rt + Rt+1
//     Term1 = (1+rho)*survival(iyear)*biomass(iyear);
//     Term2 = rho*survival(iyear)*survival(iyear-1)*biomass(iyear-1);
//     Term3 = rho*w_dat*survival(iyear)*recruitment(Lag_t);
//     biomass(iyear+1) = Term1 - Term2 - Term3 + recruitment(Lag_tplusone);
//   }
//
//
//   ///// Survey data /////
//   int temp_survey_yr; // temporary placeholder for current survey year
//   for(int iyear=0; iyear<survey_length; iyear++){
//     temp_survey_yr = survey_yr(iyear);
//     // Biomass log likelihood component
//     obj_fun += 0.5*square(log(biomass(temp_survey_yr)) - log(survey_obs(iyear)))/square(survey_SD(iyear));
//   }
//

  // ///// Projections ///// Needs further development if used (currently only has 1 fleet and fixed target catch)
  // for(int iyear = Nyear; iyear<TotalYears-1; iyear++){
  //   // Catch under constant F (Ctarget)
  //   // Survival from last year of data onwards
  //   survival(iyear) = survival(0)*(1-Ctarget/biomass(iyear)); // (survival before fishing(=M)) * (Fishing mortality(=1-Ctarget/Bobs)), currently combined survival across fleets
  //   // Recruitment projections fixed at same recruitment deviation as in last year
  //   // recruitment(iyear) = rnorm(1, R_mean, R_devs(Nyear-1)); // Recruitment prediction rnorm(number pred desired, mean, std dev)
  //   // ??? the temporary recruitment below used because the line above returns error I don't know how to fix
  //   recruitment(0) = 1000;
  //
  //   // Lag indexing to recruitment
  //   // index for year t+1
  //   if(iyear+1 - 5 < 0){ // Projection year 1-3
  //     Lag_tplusone = 0; // previously Lag
  //   } else { // Projection year 4+
  //     Lag_tplusone = iyear+1 - 5;
  //   }
  //   // index for year t
  //   if(iyear-5 < 0){
  //     Lag_t = 0; // previously Lag1
  //   } else {
  //     Lag_t = iyear - 5;
  //   }
  //
  //   // Update dynamics to predict biomass in next year (based on Bt+1 = (1+rho)*exp(-M)(Bt-Ct) - rho*exp(-2M)*(1-Ct/Bt)*((Bt-1)-(Ct-1))-rho*weight*exp(-M)*(1-Ct/Bt)*Rt + Rt+1
  //   Term1 = (1+rho)*survival(iyear)*biomass(iyear);
  //   Term2 = rho*survival(iyear)*survival(iyear-1)*biomass(iyear-1);
  //   Term3 = rho*w_dat*survival(iyear)*recruitment(Lag_t);
  //   biomass(iyear+1) = Term1 - Term2 - Term3 + recruitment(Lag_tplusone);
  // }
  //
  obj_fun += square(dummy);

  ///// Report /////
  // objective function
  REPORT(obj_fun);
  // predicted time-series
  REPORT(biomass);
  REPORT(recruitment);
  REPORT(survival);
  REPORT(catch_pred);
  REPORT(bio_pred)
  // estimated parameters
  ADREPORT(logBstartval); // Vector of biomass estimated in the first 2 years rather than assuming population is unfished at start of timeseries
  //ADREPORT(Fstartval); // Fstartval is estimated as the unobserved fishing rate in the year before observed timeseries begins
  ADREPORT(logR_mean); // Mean recruitment
  ADREPORT(logsigmaR); // Recruitment deviation
  ADREPORT(logFvals); // Vector of fishing mortalities for years 1 to Nyear -1, account for fishing from recreational catch & discards and commercial catch & discards
  ADREPORT(logq);

  return(obj_fun);
}
