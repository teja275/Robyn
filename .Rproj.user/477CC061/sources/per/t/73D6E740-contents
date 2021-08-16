### load or reinstall pkg
if (F) {
  rm(list=ls())
  library(devtools)
  library(roxygen2)
  setwd("/Users/gufengzhou/Documents/GitHub/Robyn")
  document()
  setwd("/Users/gufengzhou/Documents/GitHub")
  install("Robyn")
  .rs.restartR()
}



### line by line debug

## clear env
# .rs.restartR()
rm(list=ls())

## load libs
load_libs <- function() {

  libsNeeded <- c("devtools", "roxygen2","data.table","stringr","lubridate","foreach","glmnet","car","prophet","ggplot2","nloptr","minpack.lm","rPref","reticulate","rstudioapi","doFuture", "doRNG", "patchwork"
  )
  cat(paste0("Loading required libraries: ", paste(libsNeeded, collapse = ", "), "\n"))
  libsInstalled <- rownames(installed.packages())
  for (l in libsNeeded) {
    if (require(l, character.only = TRUE)) {} else {
      install.packages(l)
      require(l, character.only = TRUE)
    }
  }
}
load_libs()

## load scripts
script_path = substr(rstudioapi::getActiveDocumentContext()$path, start = 1, stop = max(gregexpr("/", rstudioapi::getActiveDocumentContext()$path)[[1]]))
script_path <- sub("/inst/", "/R/", script_path)
source(paste(script_path, "allocator.R", sep=""))
source(paste(script_path, "auxiliary.R", sep=""))
source(paste(script_path, "checks.R", sep=""))
source(paste(script_path, "inputs.R", sep=""))
source(paste(script_path, "model.R", sep=""))
source(paste(script_path, "refresh.R", sep=""))
source(paste(script_path, "transformation.R", sep=""))
data_path <- sub("/R/", "/data/", script_path)
load(paste0(data_path, "dt_prophet_holidays.RData"))
load(paste0(data_path, "dt_simulated_weekly.RData"))


## get input for robyn_run() debugging
if (T) {
  dt_input = dt_simulated_weekly
  dt_holidays = dt_prophet_holidays
  date_var = "DATE" # date format must be "2020-01-01"
  dep_var = "revenue" # there should be only one dependent variable
  dep_var_type = "revenue" # "revenue" or "conversion"
  prophet_vars = c("trend", "season", "holiday") # "trend","season", "weekday", "holiday"
  prophet_signs = c("default","default", "default") # c("default", "positive", and "negative").
  prophet_country = "DE"# only one country allowed once. Including national holidays
  context_vars = c("competitor_sales_B", "events") # typically competitors, price &
  context_signs = c("default", "default") # c("default", " positive", and "negative"),
  paid_media_vars = c("tv_S", "ooh_S"	,	"print_S"	,"facebook_I" ,"search_clicks_P")
  paid_media_signs = c("positive", "positive","positive", "positive", "positive")
  paid_media_spends = c("tv_S","ooh_S",	"print_S"	,"facebook_S", "search_S")
  organic_vars = c("newsletter")
  organic_signs = c("positive") # must have same length as organic_vars
  factor_vars = c("events") # specify which variables in context_vars and
  cores = 6 # I am using 6 cores from 8 on my local machine. Use availableCores() to find out cores
  window_start = "2016-11-23"
  window_end = "2018-08-22"
  adstock = "geometric" # geometric or weibull. weibull is more flexible, yet has one more
  iterations = 100  # number of allowed iterations per trial. 2000 is recommended
  nevergrad_algo = "TwoPointsDE" # recommended algorithm for Nevergrad, the gradient-free
  trials = 2 # number of allowed iterations per trial. 5 is recommended without calibration,
  calibration_input = NULL
  dt_calibration <- data.frame(
    channel = c("facebook_I",  "tv_S", "facebook_I")
    # channel name must in paid_media_vars
    , liftStartDate = as.Date(c("2018-05-01", "2017-11-27", "2018-07-01"))
    # liftStartDate must be within input data range
    , liftEndDate = as.Date(c("2018-06-10", "2017-12-03", "2018-07-20"))
    # liftEndDate must be within input data range
    , liftAbs = c(400000, 300000, 200000) # Provided value must be
    # tested on same campaign level in model and same metric as dep_var_type
  )
}





