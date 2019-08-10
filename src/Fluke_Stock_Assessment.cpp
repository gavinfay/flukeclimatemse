// Stock Assessment Model: Deriso delay-difference model 
// To do:
   // search ??? for questions/concerns
   // test with fluke data

#include <TMB.hpp>

// Optional function which square the value provided
template <class Type> Type square(Type x){return x*x;}

// Objective function which returns the value to minimize (often the negative log likelihood)
template <class Type>
Type objective_function<Type>::operator() ()
{

  ///// Data section /////
  DATA_VECTOR(rec_catch_obs); // Recreational catch observed numbers
  DATA_VECTOR(rec_disc_obs); // Recreational discards observed numbers
  DATA_VECTOR(com_catdisc_obs); // Commercial combined catch & discards observed numbers
    // Survey abundance observations & std dev
  DATA_VECTOR(survey_obs); 
  DATA_VECTOR(survey_SD);  
  DATA_INTEGER(survey_length); // Length of survey (# years survey conducted) ??? may need to be altered for individual surveys
  DATA_IVECTOR(survey_yr); // Survey years ??? see above
    // Other data
  DATA_SCALAR(w_dat); // weight gain parameter value (not estimated)
  DATA_SCALAR(M_dat); // Natural mortality rate
  DATA_SCALAR(rho); //Prow = rho
    // For projections
  DATA_INTEGER(Nproj); // Number of projection years
  DATA_SCALAR(Ctarget); // Catch target
  

  ///// Parameter section /////
  PARAMETER(dummy); // Include dummy variable to debug code without full estimation of likelihood
  PARAMETER(Logith_steep); // Logit transformed steepness parameter (h)
  PARAMETER(Bzero); // Bzero = K = biomass at equilibrium
  PARAMETER_VECTOR(Fval); // Vector of fishing mortalities for projections 
    

  // Retransform variables
  Type h_steep; 
  h_steep = 0.2+0.8*exp(Logith_steep)/(1+exp(Logith_steep));

  // Local variables
  int Nyear; // Number of data years
  Nyear = rec_catch_obs.size(); 

  int TotalYears; // Determine total number of years = number of data years + number of projection years
  TotalYears = Nyear + Nproj;

  vector<Type> biomass(TotalYears + 1); // Biomass prediction storage vector + 1 since projected 1 year into the future
  vector<Type> recruitment(TotalYears); // Recruitment storage vector
  vector<Type> survival(TotalYears); // Survival prediction storage vector
  vector<Type> rec_catch_pred(TotalYears); // Recreational catch prediction storage vector = CatHat in Andre's code
  vector<Type> rec_disc_pred(TotalYears); // Recreational discards prediction storage vector
  vector<Type> com_catdisc_pred(TotalYears); // Commercial combined catch and discard prediction storage vector
  
  Type Rzero = 0; // Initial value for Rzero
  Type Term1, Term2, Term3; // temporary variables for biomass prediction equation terms
  int Lag_tplusone,Lag_t; // Indexing integers for recruitment lag in timestep t and t+1 in 
  Type Prior; 
  Prior = 0.5*square(Logith_steep-0.5108256)/square(2); // Prior for steepness: logit((h-0.2)/0.8) ~ N(0.51,2^2) 


  // Objective function
  Type obj_fun;
  obj_fun = 0; // NegativeLogLikelihood initialized at zero
  

  // Equilibrium starting conditions   
  Rzero = (Bzero - (1.0 + rho)*exp(-1.0*M_dat)*Bzero + rho*exp(-2.0*M_dat)*Bzero)/(1.0 - rho*w_dat*exp(-1.0*M_dat)); 




 
  /////////////// Fit to data ////////////////////////////////////////////////////////////////////////////////////////////
  ///// Years 1 and 2 are special because of delay /////
  // Year 1
  biomass(0) = Bzero;
  survival(0) = exp(-1.0*M_dat); // survival rate in first year = exp(-M), only M since no catch in this year
  recruitment(0) = (4*h_steep*Rzero*biomass(0)/Bzero)/((1-h_steep) + (5*h_steep-1)*biomass(0)/Bzero); 
    // Recreational catch prediction
    rec_catch_pred(0) = 0; // No recreational catch in year 1
    Prior += square((rec_catch_pred(0) - rec_catch_obs(0))/(2.0*0.2*0.2)); // Recreational catch likelihood component, ??? is 0.2 correct std dev
    // Recreational discards prediction
    rec_disc_pred(0) = 0; // No recreational discards in year 1
    Prior += square((rec_disc_pred(0) - rec_disc_obs(0))/(2.0*0.2*0.2)); // Recreational discards likelihood component, ??? is 0.2 correct
    // Combined commercial catch and discards prediction
    com_catdisc_pred(0) = 0; // No commercial catch or discards in year 1
    Prior += square((com_catdisc_pred(0) - com_catdisc_obs(0))/(2.0*0.2*0.2)); // Commercial catch and discards likelihood component, ??? is 0.2 correct std dev

  // Year 2 biomass
  biomass(1) = (1 + rho)*exp(-1.0*M_dat)*(biomass(0) - rec_catch_pred(0)) - rho*exp(-2.0*M_dat)*(1.0 - (rec_catch_pred(0)/biomass(0)))*Bzero - rho*w_dat*exp(-1.0*M_dat)*(1.0 - (rec_catch_pred(0)/biomass(0)))*Rzero + Rzero; 

  ///// Year 2+ rec_catch_pred, com_catdisc_pred, recruitment & Year 3+ biomass /////
  for(int iyear=1; iyear<Nyear; iyear++){
    // Recreational catch prediction & likelihood
    rec_catch_pred(iyear) = exp(Fval(iyear-1))*biomass(iyear); // ??? Check this, how break into rec/commercial & catch/discards
    Prior += square(rec_catch_pred(iyear) - rec_catch_obs(iyear))/(2.0*0.2*0.2); // Catch SD = 0.2 ???
    // Recreational catch prediction & likelihood
    rec_disc_pred(iyear) = exp(Fval(iyear-1))*biomass(iyear); // ??? Check this, how break into rec/commercial & catch/discards
    Prior += square(rec_disc_pred(iyear) - rec_disc_obs(iyear))/(2.0*0.2*0.2); // Catch SD = 0.2 ???
    // Combined commercial catch and discards prediction & likelihood
    com_catdisc_pred(iyear) = exp(Fval(iyear-1))*biomass(iyear); // ??? Check this, how break into rec/commercial & catch/discards
    Prior += square(com_catdisc_pred(iyear) - com_catdisc_obs(iyear))/(2.0*0.2*0.2); // Catch SD = 0.2 ???
    // Survival 
    survival(iyear) = exp(-1.0*M_dat)*(1-exp(Fval(iyear-1))); // survival rate after M and F
    // Recruitment
    recruitment(iyear) = (4*h_steep*Rzero*biomass(iyear)/Bzero)/((1-h_steep) + (5*h_steep-1)*biomass(iyear)/Bzero); 

    // Lag indexing to recruitment
      // index for year t+1
      if(iyear+1 - 5 < 0){ // Year 1-3
      Lag_tplusone = 0; // previously called Lag
      } else { // Year 4+
      Lag_tplusone = iyear+1 - 5;
      }
      // index for year t
      if(iyear-5 < 0){
        Lag_t = 0; // previously called Lag1
      } else {
        Lag_t = iyear - 5;
      }

    // Update dynamics to predict biomass in next year (based on Bt+1 = (1+rho)*exp(-M)(Bt-Ct) - rho*exp(-2M)*(1-Ct/Bt)*((Bt-1)-(Ct-1))-rho*weight*exp(-M)*(1-Ct/Bt)*Rt + Rt+1
    Term1 = (1+rho)*survival(iyear)*biomass(iyear);
    Term2 = rho*survival(iyear)*survival(iyear-1)*biomass(iyear-1);
    Term3 = rho*w_dat*survival(iyear)*recruitment(Lag_t);
    biomass(iyear+1) = Term1 - Term2 - Term3 + recruitment(Lag_tplusone);
  }  

  obj_fun += Prior; // Add priors to objective function value


  ///// Survey data /////
  int temp_survey_yr; // temporary placeholder for current survey year
  for(int iyear=0; iyear<survey_length; iyear++){
    temp_survey_yr = survey_yr(iyear);
    // Biomass likelihood component
    obj_fun += 0.5*square(biomass(temp_survey_yr) - survey_obs(iyear))/square(survey_SD(iyear)); // (est-obs)^2/SD^2 ??? is this lognormal?
  }
  // ??? need to add surveys here ????????????????????????????????????

 // ??? I don't think I need this section since the biomass is projected to year t+1 and that is all I need to set management regulations but I am missing a piece in my model about F-based regulations/picking fishing targets
  // ??? see if this runs with Nproj = 0
  ///// Projections /////
  for(int iyear = Nyear; iyear<TotalYears-1; iyear++){
    // Catch under constant F (Ctarget) which is fine since 
    // Survival from last year of data onwards
    survival(iyear) = survival(0)*(1-Ctarget/biomass(iyear)); // (survival before fishing(=M)) * (Fishing mortality(=1-Ctarget/Bobs)) ??? Ctarget currently fixed in projection & passed to function, should this target be calculated in a different manner?
    // Recruitment
    recruitment(iyear) = (4*h_steep*Rzero*biomass(iyear)/Bzero)/((1-h_steep) + (5*h_steep-1)*biomass(iyear)/Bzero);

    // Lag indexing to recruitment
      // index for year t+1
      if(iyear+1 - 5 < 0){ // Projection year 1-3
      Lag_tplusone = 0; // previously Lag
      } else { // Projection year 4+
      Lag_tplusone = iyear+1 - 5;
      }
      // index for year t
      if(iyear-5 < 0){
        Lag_t = 0; // previously Lag1
      } else {
        Lag_t = iyear - 5;
      }

    // Update dynamics to predict biomass in next year (based on Bt+1 = (1+rho)*exp(-M)(Bt-Ct) - rho*exp(-2M)*(1-Ct/Bt)*((Bt-1)-(Ct-1))-rho*weight*exp(-M)*(1-Ct/Bt)*Rt + Rt+1
    Term1 = (1+rho)*survival(iyear)*biomass(iyear);
    Term2 = rho*survival(iyear)*survival(iyear-1)*biomass(iyear-1);
    Term3 = rho*w_dat*survival(iyear)*recruitment(Lag_t);
    biomass(iyear+1) = Term1 - Term2 - Term3 + recruitment(Lag_tplusone);
  }


  obj_fun += dummy*dummy;


  ///// Report /////
  // objective function
  REPORT(obj_fun);
  // predicted time-series
  REPORT(biomass);
  REPORT(recruitment);
  REPORT(survival);
  REPORT(rec_catch_pred);
  REPORT(rec_disc_pred);
  REPORT(com_catdisc_pred);
  // estimated parameters
  ADREPORT(h_steep); // steepness, note: this has been retransformed from Logith_steep
  ADREPORT(Bzero); // Bzero = K = biomass at equilibrium
  ADREPORT(Fval); // Vector of fishing mortalities for projections 

  return(obj_fun);
}
