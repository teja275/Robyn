# DONE: functionalise input/ remove global param to enable packaging: f.inputDT, f.inputParam, f.featureEngineering 
# DONE: rolling window : # a. replace window , b. replace hyppar bounces, c. replace decomp.rssd
# DONE: get mROI function?
# DONE: decomp fix for categorical vars?
# DONE: nls fit: try Hill? rewrite trycatch? 
# DONE: close all connection bug=? https://github.com/facebookexperimental/Robyn/issues/108; https://github.com/facebookexperimental/Robyn/pull/106 
# DONE: adapt allocator and fix plot
# DONE: fixed week count wday
# DONE: adapt all paths
# DONE: adapt robyn_response
# DONE: adapt prophet plot to be replaced & combine adstock plots
# DONE: add bar plot for reporting
# DONE: organic channels
# DONE: adapt param names, function names 
# DONE: adapt getting fixed model result
# DONE: new dummy vars newsletter and events
# DONE: put max run as default for response function
# DONE: adapt dep type, CPA or add avg conv value for ROI
# DONE: add observation warning
# DONE: adjusted rsq
# add ROI report plot
# adapt allocator for robyn_object
# add survey
# clean up comments & prints

# correlation matrix plot,  channel detail plot for selecte model (mROI)
# add pareto clustering
# returning pParFront, change cat() to message() for shiny


# Copyright (c) Facebook, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#############################################################################################
####################    Facebook MMM Open Source 'Robyn' Beta - V21.0  ######################
####################                    2021-03-03                     ######################
#############################################################################################

################################################################
#### set locale for non English R
# Sys.setlocale("LC_TIME", "English") # Sys.setlocale("LC_ALL", 'en_US.UTF-8')

################################################################
#### load libraries
## R version 4.0.3 (2020-10-10) ## Update to R version 4.0.3 to avoid potential errors
## RStudio version 1.2.1335
rm(list=ls()); gc()

################################################################
#### load data & scripts
script_path = substr(rstudioapi::getActiveDocumentContext()$path, start = 1, stop = max(gregexpr("/", rstudioapi::getActiveDocumentContext()$path)[[1]]))
source(paste(script_path, "fb_robyn.func.R", sep=""))
source(paste(script_path, "fb_robyn.optm.R", sep=""))

## Please make sure to install all libraries before rurnning the scripts
load_libs()
# virtualenv_create("r-reticulate")
# use_virtualenv("r-reticulate", required = TRUE)
# py_install("nevergrad", pip = TRUE)

## please see https://rstudio.github.io/reticulate/index.html for info on installing reticulate
# conda_create("r-reticulate") # must run this line once
# conda_install("r-reticulate", "nevergrad", pip=TRUE)  #  must install nevergrad in conda before running Robyn
# use_python("/Users/gufengzhou/Library/r-miniconda/envs/r-reticulate/bin/python3.9") # in case nevergrad still can't be imported after installation, please locate your python file and run this line
use_condaenv("r-reticulate") 
#import("nevergrad")

dt_input <- fread(paste0(script_path, "de_simulated_data.csv"))
dt_holidays <- fread(paste0(script_path, "holidays.csv"))
robyn_object <- "/Users/gufengzhou/Documents/robyn_dev_output/Robyn.RData"
registerDoSEQ(); detectCores()


################################################################
#### set model input & feature engineering

#### Guidance to set hypereparameter bounds ####

## 1. get correct hyperparameter names:
# run hyper_names() to get correct hyperparameter names. all names in hyperparameters must equal names in local_name, case sensitive

## 2. get guidance for setting hyperparameter bounds:
# For geometric adstock, use theta, alpha & gamma. For weibull adstock, use shape, scale, alpha, gamma
# theta: In geometric adstock, theta is decay rate. guideline for usual media genre: TV c(0.3, 0.8), OOH/Print/Radio c(0.1, 0.4), digital c(0, 0.3)
# shape: In weibull adstock, shape controls the decay shape. Recommended c(0.0001, 2). The larger, the more S-shape. The smaller, the more L-shape
# scale: In weibull adstock, scale controls the decay inflexion point. Very conservative recommended bounce c(0, 0.1), becausee scale can increase adstocking half-life greaetly
# alpha: In s-curve transformation with hill function, alpha controls the shape between exponential and s-shape. Recommended c(0.5, 3). The larger the alpha, the more S-shape. The smaller, the more C-shape
# gamma: In s-curve transformation with hill function, gamma controls the inflexion point. Recommended bounce c(0.3, 1). The larger the gamma, the later the inflection point in the response curve

## 3. set each hyperparameter bounds. They either contains two values e.g. c(0, 0.5), or only one value (in which case you've "fixed" that hyperparameter)
set_adstock <- "geometric"
paid_media_vars <- c("tv_S","ooh_S"	,	"print_S"	,"facebook_I"	,"search_clicks_P") 
organic_vars <- c("newsletter")
hyper_names(adstock = set_adstock, all_media = c(paid_media_vars, organic_vars) )

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


InputCollect <- robyn_inputs(dt_input = dt_input
                             ,dt_holidays = dt_holidays
                             ,date_var = "DATE" # date format must be "2020-01-01"
                             ,dep_var = "revenue" # there should be only one dependent variable
                             ,dep_var_type = "revenue" # "revenue" or "conversion"
                             
                             ,prophet_vars = c("trend", "season", "holiday") # "trend","season", "weekday", "holiday" are provided and case-sensitive. Recommended to at least keep Trend & Holidays
                             ,prophet_signs = c("default","default", "default") # c("default", "positive", and "negative"). Recommend as default. Must be same length as prophet_vars
                             ,prophet_country = "DE" # only one country allowed once. Including national holidays for 59 countries, whose list can be found on our githut guide
                             
                             ,context_vars = c("competitor_sales_B", "events") # typically competitors, price & promotion, temperature,  unemployment rate etc
                             ,context_signs = c("default", "default") # c("default", "positive", and "negative"), control the signs of coefficients for baseline variables
                             
                             ,paid_media_vars = paid_media_vars# c("tv_S"	,"ooh_S",	"print_S"	,"facebook_I", "facebook_S"	,"search_clicks_P"	,"search_S") we recommend to use media exposure metrics like impressions, GRP etc for the model. If not applicable, use spend instead
                             ,paid_media_signs = c("positive", "positive","positive", "positive", "positive") # c("default", "positive", and "negative"), control the signs of coefficients for media variables
                             ,paid_media_spends = c("tv_S","ooh_S",	"print_S"	,"facebook_S"	,"search_S") # spends must have same order and same length as paid_media_vars
                             
                             ,organic_vars = organic_vars
                             ,organic_signs = c("positive")
                             
                             ,factor_vars = c("events") # please specify which variable above should be factor
                             
                             ################################################################
                             #### set global model parameters
                             
                             ## set cores for parallel computing
                             
                             ,cores = 6 # I am using 6 cores from 8 on my local machine. Use detectCores() to find out cores
                             
                             ## set rolling window start (only works for whole dataset for now)
                             ,window_start = "2016-11-23"
                             ,window_end = "2018-08-22"
                             
                             ## set model core features
                             ,adstock = "geometric" # geometric or weibull. weibull is more flexible, yet has one more parameter and thus takes longer
                             ,iterations = 150  # number of allowed iterations per trial. 500 is recommended
                             
                             ,nevergrad_algo = "DiscreteOnePlusOne" # selected algorithm for Nevergrad, the gradient-free optimisation library https://facebookresearch.github.io/nevergrad/index.html
                             ,trials = 1 # number of allowed iterations per trial. 40 is recommended without calibration, 100 with calibration.
                             ## Time estimation: with geometric adstock, 500 iterations * 40 trials and 6 cores, it takes less than 1 hour. Weibull takes at least twice as much time.
                             
                             ,hyperparameters = hyperparameters
                             
                             # ,calibration_input = data.table(channel = c("facebook_I",  "tv_S", "facebook_I"),
                             #                        liftStartDate = as.Date(c("2018-05-01", "2017-11-27", "2018-07-01")),
                             #                        liftEndDate = as.Date(c("2018-06-10", "2017-12-03", "2018-07-20")),
                             #                        liftAbs = c(400000, 300000, 200000))
                             
)


## helper plots: set plot to TRUE for transformation examples
plot_adstock(F) # adstock transformation example plot, helping you understand geometric/theta and weibull/shape/scale transformation
plot_saturation(F) # s-curve transformation example plot, helping you understand hill/alpha/gamma transformatio

################################################################
#### Run models

OutputCollect <- robyn_run(InputCollect = InputCollect
                           , plot_folder = robyn_object
                           , pareto_fronts = 1
                           , plot_pareto = T)


######################### NOTE: must run robyn_save to select and save ONE model first, before refreshing below
#### save selected model

OutputCollect$allSolutions
select_model <- "1_25_5"
robyn_save(robyn_object = robyn_object, select_model = select_model, InputCollect = InputCollect, OutputCollect = OutputCollect)


################################################################
#### Budget Allocator - Beta

## Budget allocator result requires further validation. Please use this result with caution.
## Please don't interpret budget allocation result if there's no satisfying MMM result

OutputCollect$xDecompAgg[solID == select_model & !is.na(mean_spend), .(rn, coef,mean_spend, mean_response, roi_mean, total_spend, total_response=xDecompAgg, roi_total,  solID)] #check media summary for selected model
AllocatorCollect <- robyn_allocator(InputCollect
                                    ,OutputCollect
                                    ,select_model = select_model # input one of the model IDs in model_output_collect$allSolutions to get optimisation result
                                    ,optim_algo = "SLSQP_AUGLAG" # "MMA_AUGLAG", "SLSQP_AUGLAG"
                                    ,scenario = "max_historical_response" # c(max_historical_response, max_response_expected_spend)
                                    #,expected_spend = 100000 # specify future spend volume. only applies when scenario = "max_response_expected_spend"
                                    #,expected_spend_days = 90 # specify period for the future spend volumne in days. only applies when scenario = "max_response_expected_spend"
                                    ,channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7) # must be between 0.01-1 and has same length and order as paid_media_vars
                                    ,channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5) # not recommended to 'exaggerate' upper bounds. 1.5 means channel budget can increase to 150% of current level
)

## QA optimal response
select_media <- "facebook_I"
optimal_spend <- AllocatorCollect$dt_optimOut[channels== select_media, optmSpendUnit]
optimal_response_allocator <- AllocatorCollect$dt_optimOut[channels== select_media, optmResponseUnit]
optimal_response <- robyn_response(robyn_object = robyn_object
                                   , select_run = 0
                                   , paid_media_var = select_media
                                   , Spend = optimal_spend)
round(optimal_response_allocator) == round(optimal_response); optimal_response_allocator; optimal_response


################################################################
#### Model refresh - Alpha

# source(paste(script_path, "fb_robyn.func.R", sep=""))

Robyn <- robyn_refresh(robyn_object = robyn_object # the location of your Robyn.RData object
                       , dt_input = dt_input
                       , dt_holidays = dt_holidays
                       , refresh_steps = 13 # refresh_steps = 4 means refresh model's rolling window will move forward 4 weeks 
                       , refresh_mode = "manual" # "auto" means the refresh function will move forward until no more data available
                       , refresh_iters = 150 # iteration for refresh
                       , refresh_trials = 1 # trial for refresh
)

######## Please check plot output folders. The following 4 reporting CSVs are new and accummulated result with all previous refreshes ...
######## ... that will be the data for reporting plots 
######## report_aggregated.csv, report_alldecomp_matrix.csv, report_hyperparameters.csv, report_media_transform_matrix.csv


# QA
# load(robyn_object)
# length(Robyn);names(Robyn)
# Robyn$listRefresh1 <- NULL
# save(Robyn, file = robyn_object)

################################################################
#### get marginal returns

## example of how to get marginal ROI for 80k spend for search channel from the second refresh model
Robyn$listRefresh1$ReportCollect$xDecompAggReport[, .(refreshStatus, rn, mean_spend, mean_response, roi_mean, roi_total)]

# get response for 80k
Spend <- 50000
Response <- robyn_response(robyn_object = robyn_object
                           #, select_run = 1 # 2 means the sedond refresh model. 0 means the initial model
                           , paid_media_var = "search_clicks_P"
                           , Spend = Spend)
Response/Spend # ROI for search 80k

# get response for 80k+1
Spend1 <- Spend+1000
Response1 <- robyn_response(robyn_object = robyn_object
                            #, select_run = 1
                            , paid_media_var = "search_clicks_P"
                            , Spend = Spend1)
Response1/Spend1 # ROI for search 80k+1

# marginal ROI of 80k search
(Response1-Response)/(Spend1-Spend)


################################################################
#### get old model results

# get old hyperparameters and select model
dt_hyper_fixed <- fread("/Users/gufengzhou/Documents/robyn_dev_output/2021-07-29 00.56 init/pareto_hyperparameters.csv")
select_model <- "1_24_5" 
dt_hyper_fixed <- dt_hyper_fixed[solID == select_model]

OutputCollectFixed <- robyn_run(InputCollect = InputCollect # InputCollect must be provided by robyn_inputs with same dataset and parameters as before
                                , plot_folder = robyn_object 
                                , dt_hyper_fixed = dt_hyper_fixed) 

# save Robyn object for further refresh
robyn_save(robyn_object = robyn_object, select_model = select_model, InputCollect = InputCollect, OutputCollect = OutputCollectFixed)


# remotes::install_github("laresbernardo/Robyn")