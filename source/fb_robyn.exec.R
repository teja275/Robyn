# DONE: 1.functionalise input/ remove global param to enable packaging: f.inputDT, f.inputParam, f.featureEngineering 
# DONE: 2.rolling window : # a. replace window , b. replace hyppar bounces, c. replace decomp.rssd
# 3.plot enhancement: prophet plot to be replaced, channel detail plot for selecte model f.plotModel (saturation + mROI), correlation matrix for f.inputDT, "reporting" of rolling results
# 4.add pareto clustering
# DONE: 5.get mROI function?
# 6.organic channels
# 7.decomp fix for categorical vars? FIX THE BUG in line  xDecompOutAgg <- sapply(xDecompOut[, c("intercept", indepVarName), with =F], function(x) sum(x))
# DONE: 8.nls fit: try Hill? rewrite trycatch? 
# close all connection bug=? https://github.com/facebookexperimental/Robyn/issues/108; https://github.com/facebookexperimental/Robyn/pull/106 
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
f.loadLibrary()

## please see https://rstudio.github.io/reticulate/index.html for info on installing reticulate
# conda_create("r-reticulate") # must run this line once
# conda_install("r-reticulate", "nevergrad", pip=TRUE)  #  must install nevergrad in conda before running Robyn
# use_python("/Users/gufengzhou/Library/r-miniconda/envs/r-reticulate/bin/python3.6") # in case nevergrad still can't be imported after installation, please locate your python file and run this line
use_condaenv("r-reticulate") 

f.inputDT(data_csv_name = "de_simulated_data.csv"  # input time series should be daily, weekly or monthly
, holiday_csv_name = "holidays.csv" # when using own holidays, please keep the header c("ds", "holiday", "country", "year")
, csv_path = script_path)
# f.inputDT(data_csv_name = "cds_web_simulated_data.csv")
registerDoSEQ(); detectCores()

################################################################
#### set model input variables

f.inputParam(set_dateVarName = "DATE" # date format must be "2020-01-01"
             ,set_depVarName = "revenue" # there should be only one dependent variable

             ,set_prophet = c("trend", "season", "holiday") # "trend","season", "weekday", "holiday" are provided and case-sensitive. Recommended to at least keep Trend & Holidays
             ,set_prophetVarSign = c("default","default", "default") # c("default", "positive", and "negative"). Recommend as default. Must be same length as set_prophet
             ,set_prophetCountry = "DE" # only one country allowed once. Including national holidays for 59 countries, whose list can be found on our githut guide

             # ,set_baseVarName = c("sem_brand_S") # typically competitors, price & promotion, temperature,  unemployment rate etc
             # ,set_baseVarSign = c("positive") # c("default", "positive", and "negative"), control the signs of coefficients for baseline variables

             ,set_baseVarName = c("competitor_sales_B") # typically competitors, price & promotion, temperature,  unemployment rate etc
             ,set_baseVarSign = c("default") # c("default", "positive", and "negative"), control the signs of coefficients for baseline variables

             #,set_mediaVarName = c("affiliates_S", "sem_non_brand_I"	,"rmk_I",	"sho_I"	,"fb_I") # c("tv_S"	,"ooh_S",	"print_S"	,"facebook_I", "facebook_S"	,"search_clicks_P"	,"search_S") we recommend to use media exposure metrics like impressions, GRP etc for the model. If not applicable, use spend instead
             #,set_mediaVarSign = c("positive", "positive", "positive", "positive", "positive") # c("default", "positive", and "negative"), control the signs of coefficients for media variables
             #,set_mediaSpendName = c("affiliates_S", "sem_non_brand_S"	,"rmk_S",	"sho_S"	,"fb_S") # spends must have same order and same length as set_mediaVarName

             ,set_mediaVarName = c("tv_S"	,"ooh_S",	"print_S"	,"facebook_I"	,"search_clicks_P") # c("tv_S"	,"ooh_S",	"print_S"	,"facebook_I", "facebook_S"	,"search_clicks_P"	,"search_S") we recommend to use media exposure metrics like impressions, GRP etc for the model. If not applicable, use spend instead
             ,set_mediaVarSign = c("positive", "positive", "positive", "positive", "positive") # c("default", "positive", and "negative"), control the signs of coefficients for media variables
             ,set_mediaSpendName = c("tv_S"	,"ooh_S",	"print_S"	,"facebook_S"	,"search_S") # spends must have same order and same length as set_mediaVarName

             #,set_organicMedia

             #,set_factorVarName = NULL # please specify which variable above should be factor

             ################################################################
             #### set global model parameters

             ## set cores for parallel computing

             ,set_cores = 6 # I am using 6 cores from 8 on my local machine. Use detectCores() to find out cores

             ## set rolling window start (only works for whole dataset for now)
             #,set_rollingWindowStartDate = "2019-04-29"
             ,set_rollingWindowStartDate = "2016-11-23"
             ,set_rollingWindowEndDate = "2018-08-22"

             ## set model core features
             ,adstock = "geometric" # geometric or weibull. weibull is more flexible, yet has one more parameter and thus takes longer
             ,set_iter = 100  # number of allowed iterations per trial. 500 is recommended

             ,set_hyperOptimAlgo = "DiscreteOnePlusOne" # selected algorithm for Nevergrad, the gradient-free optimisation library https://facebookresearch.github.io/nevergrad/index.html
             ,set_trial = 5 # number of allowed iterations per trial. 40 is recommended without calibration, 100 with calibration.
             ## Time estimation: with geometric adstock, 500 iterations * 40 trials and 6 cores, it takes less than 1 hour. Weibull takes at least twice as much time.

             ################################################################
             #### tune channel hyperparameters bounds

             #### Guidance to set hypereparameter bounds ####

             ## 1. get correct hyperparameter names:
             #local_name <- f.getHyperNames(); local_name # names in set_hyperBoundLocal must equal names in local_name, case sensitive

             ## 2. get guidance for setting hyperparameter bounds:
             # For geometric adstock, use theta, alpha & gamma. For weibull adstock, use shape, scale, alpha, gamma
             # theta: In geometric adstock, theta is decay rate. guideline for usual media genre: TV c(0.3, 0.8), OOH/Print/Radio c(0.1, 0.4), digital c(0, 0.3)
             # shape: In weibull adstock, shape controls the decay shape. Recommended c(0.0001, 2). The larger, the more S-shape. The smaller, the more L-shape
             # scale: In weibull adstock, scale controls the decay inflexion point. Very conservative recommended bounce c(0, 0.1), becausee scale can increase adstocking half-life greaetly
             # alpha: In s-curve transformation with hill function, alpha controls the shape between exponential and s-shape. Recommended c(0.5, 3). The larger the alpha, the more S-shape. The smaller, the more C-shape
             # gamma: In s-curve transformation with hill function, gamma controls the inflexion point. Recommended bounce c(0.3, 1). The larger the gamma, the later the inflection point in the response curve

             ## 3. set each hyperparameter bounds. They either contains two values e.g. c(0, 0.5), or only one value (in which case you've "fixed" that hyperparameter)

             ,set_hyperBoundLocal = list(
               facebook_I_alphas = c(0.5, 3) # example bounds for alpha
               ,facebook_I_gammas = c(0.3, 1) # example bounds for gamma
               ,facebook_I_thetas = c(0, 0.3) # example bounds for theta
               #,facebook_I_shapes = c(0.0001, 2) # example bounds for shape
               #,facebook_I_scales = c(0, 0.1) # example bounds for scale

               ,ooh_S_alphas = c(0.5, 3)
               ,ooh_S_gammas = c(0.3, 1)
               ,ooh_S_thetas = c(0.1, 0.4)
               #,ooh_S_shapes = c(0.0001, 2)
               #,ooh_S_scales = c(0, 0.1)

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
             )

             # ,set_lift = data.table(channel = c("facebook_I",  "tv_S", "facebook_I"),
             #                        liftStartDate = as.Date(c("2018-05-01", "2017-11-27", "2018-07-01")),
             #                        liftEndDate = as.Date(c("2018-06-10", "2017-12-03", "2018-07-20")),
             #                        liftAbs = c(400000, 300000, 200000))

)


## helper plots: set plot to TRUE for transformation examples
f.plotAdstockCurves(F) # adstock transformation example plot, helping you understand geometric/theta and weibull/shape/scale transformation
f.plotResponseCurves(F) # s-curve transformation example plot, helping you understand hill/alpha/gamma transformation

################################################################
#### Prepare input data

f.featureEngineering()

################################################################
#### Run models

f.robyn(plot_folder = "~/Documents/GitHub/plots", pareto_fronts =1) # please set your folder path to save plots. It ends without "/".


######################### NOTE: must run f.saveInitMod to select and save ONE model first, before refreshing below
## save selected model
listOutput$allSolutions
f.saveInitMod(initModPath = "/Users/gufengzhou/Documents/GitHub/plots/Robyn.RData", initModID = "4_6_6")
# load("/Users/gufengzhou/Documents/GitHub/plots/listInit.RData")

## reload old models from csv
# dt_oldModels <- fread("/Users/gufengzhou/Documents/GitHub/plots/2021-06-09 12.54/pareto_hyperparameters.csv") # load hyperparameter csv. Provide your own path.
# f.robyn.fixed(plot_folder = "~/Documents/GitHub/plots", dt_hyppar_fixed = dt_oldModels, modID = "3_4_6") # solID must be included in the csv


################################################################
#### Budget Allocator - Beta

## Budget allocator result requires further validation. Please use this result with caution.
## Please don't interpret budget allocation result if there's no satisfying MMM result

listOutput$allSolutions
f.budgetAllocator(modID = "1_14_6" # input one of the model IDs in model_output_collect$allSolutions to get optimisation result
                  ,optim_algo = "SLSQP_AUGLAG" # "MMA_AUGLAG", "SLSQP_AUGLAG"
                  ,scenario = "max_historical_response" # c(max_historical_response, max_response_expected_spend)
                  #,expected_spend = 100000 # specify future spend volume. only applies when scenario = "max_response_expected_spend"
                  #,expected_spend_days = 90 # specify period for the future spend volumne in days. only applies when scenario = "max_response_expected_spend"
                  ,channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7) # must be between 0.01-1 and has same length and order as set_mediaVarName
                  ,channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5) # not recommended to 'exaggerate' upper bounds. 1.5 means channel budget can increase to 150% of current level
)


################################################################
#### Model refresh - Alpha

# rm(list=ls())
# script_path = substr(rstudioapi::getActiveDocumentContext()$path, start = 1, stop = max(gregexpr("/", rstudioapi::getActiveDocumentContext()$path)[[1]]))
source(paste(script_path, "fb_robyn.func.R", sep=""))
# source(paste(script_path, "fb_robyn.optm.R", sep=""))

# initModPath = "/Users/gufengzhou/Documents/GitHub/plots/Robyn.RData";dataPath = script_path;data_csv_name = "de_simulated_data.csv"; holiday_csv_name = "holidays.csv";stepForward = 3; refreshMode = "manual"; refreshIter = 50 ; refreshTrial = 3; plotPath = "~/Documents/GitHub/plots"

f.robyn.refresh(initModPath = "/Users/gufengzhou/Documents/GitHub/plots/Robyn.RData" # the location of your Robyn.RData object
                , dataPath = script_path
                , data_csv_name = "de_simulated_data.csv" 
                , holiday_csv_name = "holidays.csv" 
                , stepForward = 13 # stepForward = 4 means refresh model's rolling window will move forward 4 weeks 
                , refreshMode = "auto" # "auto" means the refresh function will move forward until no more data available
                , refreshIter = 50 # iteration for refresh
                , refreshTrial = 2 # trial for refresh
                )

######## Please check plot output folders. The following 4 reporting CSVs are new and accummulated result with all previous refreshes ...
######## ... that will be the data for reporting plots 
######## report_aggregated.csv, report_alldecomp_matrix.csv, report_hyperparameters.csv, report_media_transform_matrix.csv


# QA
load("/Users/gufengzhou/Documents/GitHub/plots/Robyn.RData")
length(Robyn);names(Robyn)
Robyn$listRefresh1 <- NULL
save(Robyn, file = "/Users/gufengzhou/Documents/GitHub/plots/Robyn.RData")


################################################################
#### get marginal returns
# Spend <- 1000
# Response <- f.response(mediaVarName = "facebook_I", modID = "1_3_6", Spend = Spend) 
# Response/Spend
# 
# Spend1 <- 1001
# Response1 <- f.response(mediaVarName = "facebook_I", modID = "1_3_6", Spend = Spend1) 
# Response1/Spend1
# 
# Response1-Response


remotes::install_github("laresbernardo/Robyn")




