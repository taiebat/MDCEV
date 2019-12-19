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


database <- read.csv("/Users/taiebat/Box/Apollo Package/MDCEV/CAHHData.csv")
attach(database)
database$mileBudget <- car1+car2+car3plus+active+pubtransp+ridehail
detach(database)

sum(database$mileBudget <= 0)
database <- database %>% 
  filter(!(database$mileBudget <= 0))
sum(database$mileBudget <= 0)

database <- rbind(database,database)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(alpha_base        = 0,
                gamma_active      = 1,
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
  
  ### Define individual alternatives modes
  alternatives  = c("car1",
                    "car2",
                    "car3plus",
                    "active",
                    "pubtransp", 
                    "ridehail")
  
  ### Define availabilities
  avail = list(car1       = 1, 
               car2       = 1, 
               car3plus   = 1, 
               active     = 1, 
               pubtransp  = 1,
               ridehail   = 1)
  
  ### Define continuous consumption for alternative modes
  continuousChoice = list(car1       = car1,
                          car2       = car2,
                          car3plus   = car3plus,
                          active     = active,
                          pubtransp  = pubtransp,
                          ridehail   = ridehail)

  ### Define utilities for individual alternative modes
  V = list()
  V[["car1"]]  = delta_car1
  V[["car2"]]     = delta_car2
  V[["car3plus"]]   = delta_car3plus  
  V[["active"]] = delta_active
  V[["pubtransp"]]  = delta_pubtransp
  V[["ridehail"]]  = delta_ridehail 
  
  ### Define alpha parameters
  alpha = list(car1  = 1 /(1 + exp(-alpha_base)), 
               car2     = 1 /(1 + exp(-alpha_base)), 
               car3plus   = 1 /(1 + exp(-alpha_base)), 
               active = 1 /(1 + exp(-alpha_base)), 
               pubtransp  = 1 /(1 + exp(-alpha_base)),
               ridehail  = 1 /(1 + exp(-alpha_base)))
  
  ### Define gamma parameters
  gamma = list(car1  = gamma_car1,
               car2     = gamma_car2,    
               car3plus   = gamma_car3plus,
               active = gamma_active,
               pubtransp  = gamma_pubtransp,
               ridehail  = gamma_ridehail)

  ### Define costs for individual alternatives
  cost = list(car1      = 1, 
              car2      = 1, 
              car3plus  = 1, 
              active    = 1, 
              pubtransp = 1,
              ridehail  = 1)
  
  ### Define budget
  budget = mileBudget
  
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

