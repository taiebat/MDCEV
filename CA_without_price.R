# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="MDCEV_CAHH_NoPrice",
  modelDescr ="MDCEV model on mode and milage data, alpha-gamma profile with socio-demographics",
  indivID    ="HOUSEID"
)

# ################################################################# #
#### LOAD DATA                      ####
# ################################################################# #


database <- read.csv("/Users/taiebat/Box/Apollo Package/NHTS/CAHHData.csv")

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(alpha_base        = 0,
                gamma_avtive      = 1,
                gamma_car1        = 1,
                gamma_car2        = 1,
                gamma_car3plus    = 1,
                gamma_pubtransp   = 1,
                gamma_ridehail    = 1,
                delta_active      = 0,
                delta_car1        = 0,
                delta_car2        = 0,
                delta_car3plus    = 0,
                delta_pubtransp   = 0,
                delta_ridehail    = 0,
                #delta_work_wknd    = 0,
                #delta_school_young = 0,
                #delta_leisure_wknd = 0,
                sigma              = 1)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("sigma")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()
  
  ### Define individual alternatives
  alternatives  = c("outside",
                    "work",
                    "school",
                    "shopping",
                    "private", 
                    "leisure")
  
  ### Define availabilities
  avail = list(outside  = 1, 
               work     = 1, 
               school   = 1, 
               shopping = 1, 
               private  = 1,
               leisure  = 1)
  
  ### Define continuous consumption for individual alternatives
  continuousChoice = list(outside  = t_outside/60,
                          work     = t_a02/60,
                          school   = t_a03/60,
                          shopping = t_a04/60,
                          private  = t_a05/60,
                          leisure  = t_leisure/60)

  ### Define utilities for individual alternatives
  V = list()
  V[["outside"]]  = 0
  V[["work"]]     = delta_work     + delta_work_FT * occ_full_time + delta_work_wknd * weekend
  V[["school"]]   = delta_school   + delta_school_young * (age<=30)
  V[["shopping"]] = delta_shopping
  V[["private"]]  = delta_private
  V[["leisure"]]  = delta_leisure  + delta_leisure_wknd*weekend
  
  ### Define alpha parameters
  alpha = list(outside  = 1 /(1 + exp(-alpha_base)), 
               work     = 1 /(1 + exp(-alpha_base)), 
               school   = 1 /(1 + exp(-alpha_base)), 
               shopping = 1 /(1 + exp(-alpha_base)), 
               private  = 1 /(1 + exp(-alpha_base)),
               leisure  = 1 /(1 + exp(-alpha_base)))
  
  ### Define gamma parameters
  gamma = list(outside  = 1,
               work     = gamma_work,    
               school   = gamma_school,
               shopping = gamma_shopping,
               private  = gamma_private,
               leisure  = gamma_leisure)

  ### Define costs for individual alternatives
  cost = list(outside  = 1, 
              work     = 1, 
              school   = 1, 
              shopping = 1, 
              private  = 1,
              leisure  = 1)
  
  ### Define budget
  budget = budget/60
  
  ### Define settings for MDCEV model
  mdcev_settings <- list(alternatives      = alternatives,
                         avail             = avail,
                         continuousChoice = continuousChoice,
                         V                 = V,
                         alpha             = alpha,
                         gamma             = gamma, 
                         sigma             = sigma, 
                         cost              = cost,
                         budget            = budget)
  
  ### Compute probabilities using MDCEV model
  P[["model"]] = apollo_mdcev(mdcev_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

















# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS                                          ----
# ----------------------------------------------------------------- #

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model, apollo_probabilities, apollo_inputs)

colMeans(predictions_base)

