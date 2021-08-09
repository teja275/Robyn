# Copyright (c) Facebook, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#############################################################################################
####################         Facebook MMM Open Source - Robyn 3.0.0    ######################
####################                    Quick guide                   #######################
#############################################################################################

################################################################
#### Step 0: setup environement

## Install and load libraries
library(Robyn) # devtools::install_github("facebookexperimental/Robyn@package_test")

## Must install the python library Nevergrad once
## please see here for more info about installing Python packages via reticulate
## https://rstudio.github.io/reticulate/articles/python_packages.html

## Option 1: nevergrad installation via conda
# conda_create("r-reticulate") # must run this line once
# conda_install("r-reticulate", "nevergrad", pip=TRUE)
# use_condaenv("r-reticulate")

## Option 2: nevergrad installation via PIP
# virtualenv_create("r-reticulate")
# py_install("nevergrad", pip = TRUE)
# use_virtualenv("r-reticulate", required = TRUE)

## In case nevergrad still can't be imported after installation,
## please locate your python file and run this line with your path:
# use_python("~/Library/r-miniconda/envs/r-reticulate/bin/python3.9")


################################################################
#### Step 1: load data

## Check simulated dataset or load your own dataset
data("dt_simulated_weekly")
head(dt_simulated_weekly)

## Check holidays from Prophet
# 59 countries included. If your country is not included, please manually add it.
# Tipp: any events can be added into this table, school break, events etc.
data("dt_prophet_holidays")
head(dt_prophet_holidays)

## Set robyn_object. It must have extension .RData. The object name can be different than Robyn:
robyn_object <- "~/Desktop/Robyn.RData"

################################################################
#### Step 2: define model variables & parameters

InputCollect <- robyn_inputs(
  dt_input = dt_simulated_weekly
  ,dt_holidays = dt_prophet_holidays

  #######################
  #### set variables ####

  ,date_var = "DATE" # date format must be "2020-01-01"
  ,dep_var = "revenue" # there should be only one dependent variable
  ,dep_var_type = "revenue" # "revenue" or "conversion"

  # "trend","season", "weekday", "holiday" are provided and case-sensitive.
  # Recommended to at least keep Trend & Holidays
  ,prophet_vars = c("trend", "season", "holiday")
  # c("default", "positive", and "negative"). Recommend as default.
  # Must be same length as prophet_vars
  ,prophet_signs = c("default","default", "default")
  # only one country allowed once. Including national holidays for 59 countries,
  # whose list can be found on our githut guide
  ,prophet_country = "DE"

  # typically competitors, price & promotion, temperature,  unemployment rate etc
  ,context_vars = c("competitor_sales_B", "events")
  # c("default", "positive", and "negative"), control the signs of coefficients for baseline variables
  ,context_signs = c("default", "default")

  # c("tv_S"	,"ooh_S",	"print_S"	,"facebook_I", "facebook_S"	,"search_clicks_P"	,"search_S").
  # We recommend to use media exposure metrics like impressions, GRP etc for the model.
  # If not applicable, use spend instead
  ,paid_media_vars = c("tv_S","ooh_S"	,	"print_S"	,"facebook_I"	,"search_clicks_P")
  # c("default", "positive", and "negative"). must have same length as paid_media_vars.
  # control the signs of coefficients for media variables
  ,paid_media_signs = c("positive", "positive","positive", "positive", "positive")
  # spends must have same order and same length as paid_media_vars
  ,paid_media_spends = c("tv_S","ooh_S",	"print_S"	,"facebook_S"	,"search_S")

  ,organic_vars = c("newsletter")
  ,organic_signs = c("positive") # must have same length as organic_vars

  ,factor_vars = c("events") # specify which variables in context_vars and organic_vars are factorial

  ##############################
  #### set model parameters ####

  ## set cores for parallel computing
  ,cores = 6 # I am using 6 cores from 8 on my local machine. Use detectCores() to find out cores

  ## set rolling window start
  ,window_start = "2016-11-23"
  ,window_end = "2018-08-22"

  ## set model core features
  # Weibull is more flexible, yet has one more parameter and thus takes longer
  ,adstock = "geometric" # geometric or weibull
  ,iterations = 100  # number of allowed iterations per trial. 500 is recommended

  # selected algorithm for Nevergrad, the gradient-free optimization library
  # https://facebookresearch.github.io/nevergrad/index.html
  ,nevergrad_algo = "TwoPointsDE"
  # 40 is recommended without calibration, 100 with calibration.
  ,trials = 2 # number of allowed iterations per trial

  ## Time estimation: with geometric adstock, 2000 iterations * 5 trials and 6 cores,
  # it takes about 30 minutes. Weibull takes at least twice as much time.

  #,hyperparameters = hyperparameters

  # ,calibration_input = data.table(channel = c("facebook_I",  "tv_S", "facebook_I"),
  #                        liftStartDate = as.Date(c("2018-05-01", "2017-11-27", "2018-07-01")),
  #                        liftEndDate = as.Date(c("2018-06-10", "2017-12-03", "2018-07-20")),
  #                        liftAbs = c(400000, 300000, 200000))
)


################################################################
#### Step 3: define and add hyperparameters

## Guide to setup hyperparameters

## 1. get correct hyperparameter names:
# all variables in paid_media_vars or organic_vars require hyperpameter and will be
# transformed by adstock & saturation difference between paid_media_vars and organic_vars
# is that paid_media_vars has spend that needs to be specified in paid_media_spends specifically
# run hyper_names() to get correct hyperparameter names. all names in hyperparameters must
# equal names from hyper_names(), case sensitive

## 2. get guidance for setting hyperparameter bounds:
# For geometric adstock, use theta, alpha & gamma. For weibull adstock, use shape, scale, alpha, gamma
# theta: In geometric adstock, theta is decay rate. guideline for usual media genre:
# TV c(0.3, 0.8), OOH/Print/Radio c(0.1, 0.4), digital c(0, 0.3)
# shape: In weibull adstock, shape controls the decay shape. Recommended c(0.0001, 2).
# The larger, the more S-shape. The smaller, the more L-shape
# scale: In weibull adstock, scale controls the decay inflexion point.
# Very conservative recommended bounce c(0, 0.1), because scale can increase adstocking half-life greaetly
# alpha: In s-curve transformation with hill function, alpha controls the shape between exponential and s-shape.
# Recommended c(0.5, 3). The larger the alpha, the more S-shape. The smaller, the more C-shape
# gamma: In s-curve transformation with hill function, gamma controls the inflexion point.
# Recommended bounce c(0.3, 1). The larger the gamma, the later the inflection point in the response curve

# helper plots: set plot to TRUE for transformation examples:
# adstock transformation example plot, helping you understand geometric/theta and weibull/shape/scale transformation
plot_adstock(FALSE)
# s-curve transformation example plot, helping you understand hill/alpha/gamma transformatio
plot_saturation(FALSE)

## 3. set each hyperparameter bounds. They either contains two values e.g. c(0, 0.5),
# or only one value (in which case you've "fixed" that hyperparameter)

hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

hyperparameters <- list(
  facebook_I_alphas = c(0.5, 3) # example bounds for alpha
  ,facebook_I_gammas = c(0.3, 1) # example bounds for gamma
  ,facebook_I_thetas = c(0, 0.3) # example bounds for theta
  #,facebook_I_shapes = c(0.0001, 2) # example bounds for shape
  #,facebook_I_scales = c(0, 0.1) # example bounds for scale

  ,print_S_alphas = c(0.5, 3)
  ,print_S_gammas = c(0.3, 1)
  ,print_S_thetas = c(0.1, 0.4)
  #,print_S_shapes = c(0.0001, 2)
  #,print_S_scales = c(0, 0.1)

  ,tv_S_alphas = c(0.5, 3)
  ,tv_S_gammas = c(0.3, 1)
  ,tv_S_thetas = c(0.3, 0.8)
  #,tv_S_shapes = c(0.0001, 2)
  #,tv_S_scales= c(0, 0.1)

  ,search_clicks_P_alphas = c(0.5, 3)
  ,search_clicks_P_gammas = c(0.3, 1)
  ,search_clicks_P_thetas = c(0, 0.3)
  #,search_clicks_P_shapes = c(0.0001, 2)
  #,search_clicks_P_scales = c(0, 0.1)

  ,ooh_S_alphas = c(0.5, 3)
  ,ooh_S_gammas = c(0.3, 1)
  ,ooh_S_thetas = c(0.1, 0.4)
  #,ooh_S_shapes = c(0.0001, 2)
  #,ooh_S_scales = c(0, 0.1)

  ,newsletter_alphas = c(0.5, 3)
  ,newsletter_gammas = c(0.3, 1)
  ,newsletter_thetas = c(0.1, 0.4)
  #,newsletter_shapes = c(0.0001, 2)
  #,newsletter_scales = c(0, 0.1)
)

## Add hyperparameters intu robyn_inputs()

InputCollect <- robyn_inputs(InputCollect = InputCollect
                             , hyperparameters = hyperparameters)

## NOTE: The example above is to get InputCollect for the first time and done in 2 steps
## NOTE: If hyperparameters are set before, robyn_inputs() can be also done
# in 1 step by providing the hyperparameters parameter immediately

################################################################
#### Step 4: Build initial model


OutputCollect <- robyn_run(InputCollect = InputCollect
                           , plot_folder = robyn_object
                           , pareto_fronts = 1
                           , plot_pareto = TRUE)


################################################################
#### Step 5: Select and save the initial model

## Compare all model onepagers in the plot folder and select one
## that mostly represents your business reality

OutputCollect$allSolutions
select_model <- "2_13_3"
robyn_save(robyn_object = robyn_object
           , select_model = select_model
           , InputCollect = InputCollect
           , OutputCollect = OutputCollect)


################################################################
#### Step 6: Get budget allocation based on the selected model above

## Budget allocator result requires further validation. Please use this result with caution.
## Don't interpret budget allocation result if selected model result doesn't meet business expectation

# Check media summary for selected model
OutputCollect$xDecompAgg[solID == select_model & !is.na(mean_spend),
                         .(rn, coef,mean_spend, mean_response, roi_mean, total_spend,
                           total_response = xDecompAgg, roi_total, solID)]

AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect
  ,OutputCollect = OutputCollect
  # input one of the model IDs in model_output_collect$allSolutions to get optimization result
  ,select_model = select_model
  ,optim_algo = "SLSQP_AUGLAG" # "MMA_AUGLAG", "SLSQP_AUGLAG"
  ,scenario = "max_historical_response" # c(max_historical_response, max_response_expected_spend)
  # specify future spend volume. only applies when scenario = "max_response_expected_spend"
  #,expected_spend = 100000
  # specify period for the future spend volume in days. Only applies when scenario = "max_response_expected_spend"
  #,expected_spend_days = 90
  # must be between 0.01-1 and has same length and order as paid_media_vars
  ,channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7)
  # not recommended to 'exaggerate' upper bounds.
  # 1.5 means channel budget can increase to 150% of current level
  ,channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5)
)

## QA optimal response
# select_media <- "facebook_I"
# optimal_spend <- AllocatorCollect$dt_optimOut[channels== select_media, optmSpendUnit]
# optimal_response_allocator <- AllocatorCollect$dt_optimOut[channels== select_media, optmResponseUnit]
# optimal_response <- robyn_response(robyn_object = robyn_object
#                                    , select_run = 0
#                                    , paid_media_var = select_media
#                                    , Spend = optimal_spend)
# round(optimal_response_allocator) == round(optimal_response); optimal_response_allocator; optimal_response


################################################################
#### Step 7: Model refresh based on selected model and saved Robyn.RData object - Alpha

## NOTE: must run robyn_save to select and save an initial model first, before refreshing below

Robyn <- robyn_refresh(robyn_object = robyn_object # the location of your Robyn.RData object
                       , dt_input = dt_simulated_weekly
                       , dt_holidays = dt_prophet_holidays
                       # refresh_steps = 4 means refresh model's rolling window will move forward 4 weeks
                       , refresh_steps = 13
                       # "auto" means the refresh function will move forward until no more data available
                       , refresh_mode = "manual"
                       , refresh_iters = 150 # iteration for refresh
                       , refresh_trials = 1 # trial for refresh
)

######## Please check plot output folders. The following 4 reporting CSVs are new and
######## accumulated result with all previous refreshes that will be the data for reporting plots
######## report_aggregated.csv, report_alldecomp_matrix.csv, report_hyperparameters.csv, report_media_transform_matrix.csv

# QA
# load(robyn_object)
# length(Robyn);names(Robyn)
# Robyn$listRefresh1 <- NULL
# save(Robyn, file = robyn_object)

################################################################
#### Step 8: Get budget allocation recommendation based on selected refresh runs

AllocatorCollect <- robyn_allocator(
  robyn_object = robyn_object
  # , select_run
  , optim_algo = "SLSQP_AUGLAG" # "MMA_AUGLAG", "SLSQP_AUGLAG"
  , scenario = "max_historical_response" # c(max_historical_response, max_response_expected_spend)
  # specify future spend volume. only applies when scenario = "max_response_expected_spend"
  #, expected_spend = 100000
  # specify period for the future spend volumne in days. only applies when scenario = "max_response_expected_spend"
  #, expected_spend_days = 90
  # must be between 0.01-1 and has same length and order as paid_media_vars
  , channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7)
  # not recommended to 'exaggerate' upper bounds. 1.5 means channel budget can increase to 150% of current level
  , channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5)
)


################################################################
#### Step 9: get marginal returns

## Example of how to get marginal ROI of next 1000$ from the 80k spend level for search channel

# get response for 80k
Spend1 <- 80000
Response1 <- robyn_response(
  robyn_object = robyn_object
  #, select_run = 1 # 2 means the second refresh model. 0 means the initial model
  , paid_media_var = "search_clicks_P"
  , Spend = Spend1)
Response1/Spend1 # ROI for search 80k

# get response for 81k
Spend2 <- Spend1+1000
Response2 <- robyn_response(
  robyn_object = robyn_object
  #, select_run = 1
  , paid_media_var = "search_clicks_P"
  , Spend = Spend2)
Response2/Spend2 # ROI for search 81k

# marginal ROI of next 1000$ from 80k spend level for search
(Response2-Response1)/(Spend2-Spend1)


################################################################
#### Optional: get old model results

# Get old hyperparameters and select model
dt_hyper_fixed <- fread("~/Desktop/2021-07-29 00.56 init/pareto_hyperparameters.csv")
select_model <- "1_24_5"
dt_hyper_fixed <- dt_hyper_fixed[solID == select_model]

OutputCollectFixed <- robyn_run(
  # InputCollect must be provided by robyn_inputs with same dataset and parameters as before
  InputCollect = InputCollect
  , plot_folder = robyn_object
  , dt_hyper_fixed = dt_hyper_fixed)

# Save Robyn object for further refresh
robyn_save(robyn_object = robyn_object
           , select_model = select_model
           , InputCollect = InputCollect
           , OutputCollect = OutputCollectFixed)
