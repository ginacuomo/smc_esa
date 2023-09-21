## example from Ellie Sherard-Smith

library(malariasimulation)

##########################################
##
## malsim test to simulate trial arms (n = 4)
## later we will simulate every village (n = 53)

year <- 365
month <- 30
sim_length <- 11 * year ## Jan 2014 onward
## This is spanning Jan 2014 - Dec 2025
## Nov 2014 Community engagement begins with workshops every 2 weeks
## Simulate this as increasing access to care
## Everyone with ACT (LA) throughout
## Starting in 2014 at 58.8% treatment of people MIS data in Malenga et al 2017
## 2016 at 67% DHIS data in Malenga et al 2017
## baseline net use known 
## mass campaign 1st May 2016
## initiate LSM from Jan 2016 onward, as habitat modification (permanent change in larval breeding K)
## baseline prevalence known for 6-59 months an 15-49 years (women)

run_mod_f = function(eir,cov_hi,net_params,lsm_coverage){
  ## outputs prevalence every two months throughout 
  human_population <- 10000
  starting_EIR <- eir ## to be estimated later
  simparams <- get_parameters(
    list(
      human_population = human_population,
      # irs_correlation = 
      
      prevalence_rendering_min_ages = c(0.5,15) * 365, ## Prev in 6 months to 5 years measured
      prevalence_rendering_max_ages = c(5,49) * 365, 
      
      ## Prev in 15 years to 49 years(females)
      
      clinical_incidence_rendering_min_ages = c(0.5,15) * 365, ## All age clin_inc
      clinical_incidence_rendering_max_ages = c(5,49) * 365,
      
      ## Malawi, Chikhwawa District 102014
      model_seasonality = TRUE, ## Seasonality need to check this with Anja/Rob
      ## These are from rainfall
      g0 = 2.298703,
      g = c(3.094974, 1.926434, 0.6723709),
      h = c(1.342555, 1.020905, 0.309696),
      Q0 = 0.71, ## To be updated
      individual_mosquitoes = FALSE, ## Update next
      carrying_capacity = TRUE
    )
  )
  
  simparams <- set_equilibrium(simparams, starting_EIR)
  
  # set species
  fun_params['Q0'] <- 0.974 # human blood index: update from PMI report 2020
  
  simparams <- set_species(simparams,
                           species=list(gamb_params, fun_params, arab_params),
                           proportions=c(1 - 0.87 - 0.06, ## crude from PMI funestus dominant
                                         0.87,
                                         0.06))
  # set treatment
  simparams <- set_drugs(simparams, list(AL_params))
  simparams <- set_clinical_treatment(simparams, 
                                      drug=1,
                                      time=    c(1,  456,  851), ## make this 1 jan 2014 (MIS survey), mid-2015 (DHIS survey), 1 may 2016 (trial starts, but engagement started in nov 2015)
                                      coverage=c(0.588,0.677,0.677))
  
  ## Set up carrying capacity for LSM
  
  # Specify the LSM coverage
  cc <- get_init_carrying_capacity(simparams)
  cc
  
  ## Need to check if habitat modification was done everywhere?
  ## Then larviciding was included 'on top' for the two LSM arms?
  lsm_coverage <- lsm_coverage ## c(0,0,0,0,0)  ## work out what this will be for each arm / cluster
  # lsm_coverage <- c(0,0.6,0.7,0.8,0.8)  ## work out what this will be for each arm / cluster
  
  # Set LSM by reducing the carrying capacity by (1 - coverage)
  
  simparams <- simparams |>
    set_carrying_capacity(
      carrying_capacity = t(matrix(c(cc * rep(1 - lsm_coverage[1],3),
                                     cc * rep(1 - lsm_coverage[2],3),
                                     cc * rep(1 - lsm_coverage[3],3)),nrow = 3)),
      timesteps = c(1,731,851)
    )
  
  ## Set up bed nets
  
  bednetparams <- simparams
  
  ## as done
  bednet_events = data.frame(
    timestep = c(0, 851,1946), ## first jan 2014 we will make net efficacy for year-old nets
    ## then 1 may 2016 the mass campaign happens with net use parameters from publication ()
    name=c("background", 
           "trial_2016_nets",
           "next_campaign_2019_nets")
  )
  
  
  # Will run over a loop so this is for either net
  # this is using what was done i.e. actually used
  simparams <- set_bednets(
    bednetparams,
    timesteps = bednet_events$timestep,
    coverages = c(0.7, ## historic prior to RCT this is from MIS 2014 (Malenga et al 2017)
                  cov_hi, ## trial during RCT (on deployment) net use maintained at high levels throughout
                  cov_hi),   ## planned for 2020 - ** Assuming the distribution coverage matched the RCT estimate
    retention = 15 * year, ## observed during RCT at high retention levels thanks to workshops
    
    ## each row needs to show the efficacy parameter across years (and cols are diff mosquito)
    ## resistance 16%
    dn0 = matrix(c(0.261354002, ## this is 65% survival
                   net_params[1],    ## up to 90% 
                   net_params[1],
                   0.261354002,net_params[1],net_params[1],
                   0.261354002,net_params[1],net_params[1]), nrow=3, ncol=3),
    rn = matrix(c(0.69613767,net_params[2],net_params[2],
                  0.69613767,net_params[2],net_params[2],
                  0.69613767,net_params[2],net_params[2]), nrow=3, ncol=3),
    rnm = matrix(c(.24, .24, .24), nrow=3, ncol=3),
    gamman = as.numeric(c(2.195136394,net_params[3],net_params[3]) * 365)
  )
  
  ## assume the same people are getting nets each round
  correlations <- get_correlation_parameters(simparams)
  correlations$inter_round_rho('bednets', 1)
  
  ## Run the simulations
  output <- run_simulation(sim_length, simparams,correlations)
  output$prev_182.5_1825 = output$p_detect_182.5_1825/output$n_182.5_1825
  
  return(output)
}



net_pars = read.csv("data/pyrethroid_only_nets_adjusted_for_housing.csv",header = T)



##control
output0 = run_mod_f(eir = 6,
                    cov_hi = 0.8,
                    net_params = c(net_pars$dn0_med[91],
                                   net_pars$rn0_med[91],
                                   net_pars$gamman_med[91]), ## 85% resistance
                    lsm_coverage = c(0,0,0))
