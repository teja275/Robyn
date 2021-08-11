
# Copyright (c) Facebook, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

########################################################################
###### Input and setup functions
########################################################################

load_libs <- function() {
  
  libsNeeded <- c("data.table","stringr","lubridate","foreach","glmnet","car","StanHeaders","prophet","rstan","ggplot2","gridExtra","grid","ggpubr","see","PerformanceAnalytics","nloptr","minpack.lm","rPref","reticulate","rstudioapi","corrplot","doFuture", "doRNG"
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

robyn_inputs <- function(dt_input
                         ,dt_holidays
                         
                         ,date_var = NULL # date format must be "2020-01-01"
                         ,dep_var = NULL # there should be only one dependent variable
                         ,dep_var_type = NULL # "revenue" or "conversion" are allowed
                         
                         ,prophet_vars = NULL # "trend","season", "weekday", "holiday" are provided and case-sensitive. Recommended to at least keep Trend & Holidays
                         ,prophet_signs = NULL # c("default", "positive", and "negative"). Recommend as default. Must be same length as prophet_vars
                         ,prophet_country = NULL # only one country allowed once. Including national holidays for 59 countries, whose list can be found on our githut guide 
                         
                         ,context_vars = NULL # typically competitors, price & promotion, temperature,  unemployment rate etc
                         ,context_signs = NULL # c("default", "positive", and "negative"), control the signs of coefficients for baseline variables
                         
                         ,paid_media_vars = NULL # c("tv_S"	,"ooh_S",	"print_S"	,"facebook_I", "facebook_S"	,"search_clicks_P"	,"search_S") we recommend to use media exposure metrics like impressions, GRP etc for the model. If not applicable, use spend instead
                         ,paid_media_signs = NULL # c("default", "positive", and "negative"), control the signs of coefficients for media variables
                         ,paid_media_spends = NULL # spends must have same order and same length as paid_media_vars
                         
                         ,organic_vars = NULL
                         ,organic_signs = NULL
                         
                         ,factor_vars = NULL # please specify which variable above should be factor, otherwise leave empty c()
                         
                         ################################################################
                         #### set global model parameters
                         
                         ## set cores for parallel computing
                         ,cores = 1 # I am using 6 cores from 8 on my local machine. Use availableCores() to find out cores
                         
                         ## set rolling window start (only works for whole dataset for now)
                         ,window_start = NULL 
                         ,window_end = NULL
                         
                         ## set model core features
                         ,adstock = NULL # geometric or weibull. weibull is more flexible, yet has one more parameter and thus takes longer
                         ,iterations = 2000  # number of allowed iterations per trial. 2000 is recommended
                         
                         ,nevergrad_algo = "TwoPointsDE" # selected algorithm for Nevergrad, the gradient-free optimisation library https://facebookresearch.github.io/nevergrad/index.html
                         ,trials = 5 # number of allowed iterations per trial. 5 is recommended without calibration, 10 with calibration.
                         ## Time estimation: with geometric adstock, 2000 iterations * 5 trials and 6 cores, it half hour. Weibull takes at least twice as much time.
                         
                         ,hyperparameters = NULL
                         ,calibration_input = NULL
                         ,InputCollect = NULL
                         
) {
  
  if (is.null(InputCollect)) {
    
    ## check listDT existence
    #if (is.null(listDT)) {stop("Object listDT is missing. Must run f.inputDT first")}
    
    ## check date input
    inputLen <- length(dt_input[, get(date_var)])
    inputLenUnique <- length(unique(dt_input[, get(date_var)]))
    
    if (is.null(date_var) | !(date_var %in% names(dt_input)) | length(date_var)>1) {
      stop("Must provide correct only 1 date variable name for date_var")
    } else if (any(is.na(as.Date(as.character(dt_input[, get(date_var)]), "%Y-%m-%d")))) {
      stop("Date variable in date_var must have format '2020-12-31'")
    } else if (inputLen != inputLenUnique) {
      stop("Date variable has duplicated dates. Please clean data first")
    } else if (any(apply(dt_input, 2, function(x) any(is.na(x) | is.infinite(x))))) {
      stop("dt_input has NA or Inf. Please clean data first")
    }
    
    dt_input <- dt_input[order(as.Date(dt_input[, get(date_var)]))]
    dayInterval <- as.integer(difftime(as.Date(dt_input[, get(date_var)])[2], as.Date(dt_input[, get(date_var)])[1], units = "days"))
    intervalType <- if(dayInterval==1) {"day"} else if (dayInterval==7) {"week"} else if (dayInterval %in% 28:31) {"month"} else {stop("input data has to be daily, weekly or monthly")}
    
    ## check dependent var
    if (is.null(dep_var) | !(dep_var %in% names(dt_input)) | length(dep_var)>1) {
      stop("Must provide only 1 correct dependent variable name for dep_var")
    } else if ( !(is.numeric(dt_input[, get(dep_var)]) | is.integer(dt_input[, get(dep_var)]))) {
      stop("dep_var must be numeric or integer")
    } else if (!(dep_var_type %in% c("conversion", "revenue")) | length(dep_var_type)!=1) {
      stop("dep_var_type must be conversion or revenue")
    }
    
    ## check prophet
    if (is.null(prophet_vars)) {
      prophet_signs <- NULL; prophet_country <- NULL
    } else if (!all(prophet_vars %in% c("trend","season", "weekday", "holiday"))) {
      stop("allowed values for prophet_vars are 'trend', 'season', 'weekday' and 'holiday'")
    } else if (is.null(prophet_country) | length(prophet_country) >1) {
      stop("1 country code must be provided in prophet_country. ",dt_holidays[, uniqueN(country)], " countries are included: ", paste(dt_holidays[, unique(country)], collapse = ", "), ". If your country is not available, please add it to the holidays.csv first")
    } else if (is.null(prophet_signs)) {
      prophet_signs <- rep("default", length(prophet_vars))
      message("prophet_signs is not provided. 'default' is used")
    } else if (length(prophet_signs) != length(prophet_vars) | !all(prophet_signs %in% c("positive", "negative", "default"))) {
      stop("prophet_signs must have same length as prophet_vars. allowed values are 'positive', 'negative', 'default'")
    }
    
    ## check baseline variables
    if (is.null(context_vars)) {
      context_signs <- NULL
    } else if ( !all(context_vars %in% names(dt_input)) ) {
      stop("Provided context_vars is not included in input data")
    } else if (is.null(context_signs)) {
      context_signs <- rep("default", length(context_vars))
      message("context_signs is not provided. 'default' is used")
    } else if (length(context_signs) != length(context_vars) | !all(context_signs %in% c("positive", "negative", "default"))) {
      stop("context_signs must have same length as context_vars. allowed values are 'positive', 'negative', 'default'")
    }
    
    ## check paid media variables
    mediaVarCount <- length(paid_media_vars)
    spendVarCount <- length(paid_media_spends)
    if (is.null(paid_media_vars) | is.null(paid_media_spends)) {
      stop("Must provide paid_media_vars and paid_media_spends")
    } else if ( !all(paid_media_vars %in% names(dt_input)) ) {
      stop("Provided paid_media_vars is not included in input data")
    } else if (is.null(paid_media_signs)) {
      paid_media_signs <- rep("positive", mediaVarCount)
      message("paid_media_signs is not provided. 'positive' is used")
    } else if (length(paid_media_signs) != mediaVarCount | !all(paid_media_signs %in% c("positive", "negative", "default"))) {
      stop("paid_media_signs must have same length as paid_media_vars. allowed values are 'positive', 'negative', 'default'")
    } else if (!all(paid_media_spends %in% names(dt_input))) {
      stop("Provided paid_media_spends is not included in input data")
    } else if (spendVarCount != mediaVarCount) {
      stop("paid_media_spends must have same length as paid_media_vars.")
    } else if (any(dt_input[, unique(c(paid_media_vars, paid_media_spends)), with=FALSE]<0)) {
      check_media_names <- unique(c(paid_media_vars, paid_media_spends))
      check_media_val <- sapply(dt_input[, check_media_names, with=FALSE], function(X) { any(X <0) })
      stop( paste(names(check_media_val)[check_media_val], collapse = ", "), " contains negative values. Media must be >=0")
    }
    
    exposureVarName <- paid_media_vars[!(paid_media_vars==paid_media_spends)]
    
    ## check organic media variables
    if (!all(organic_vars %in% names(dt_input)) ) {
      stop("Provided organic_vars is not included in input data")
    } else if (!is.null(organic_vars) & is.null(organic_signs)) {
      organic_signs <- rep("positive", length(organic_vars))
      message("organic_signs is not provided. 'positive' is used")
    } else if (length(organic_signs) != length(organic_vars) | !all(organic_signs %in% c("positive", "negative", "default"))) {
      stop("organic_signs must have same length as organic_vars. allowed values are 'positive', 'negative', 'default'")
    }
    
    ## check factor_vars
    if (!is.null(factor_vars)) {
      if (!all(factor_vars %in% c(context_vars, organic_vars))) {stop("factor_vars must be from context_vars or organic_vars")}
    }
    
    ## check all vars
    all_media <- c(paid_media_vars, organic_vars)
    all_ind_vars <- unique(c(prophet_vars, context_vars, all_media #, set_keywordsVarName, paid_media_spends
    ))
    all_ind_vars_check <- c(prophet_vars, context_vars, all_media)
    if(!identical(all_ind_vars, all_ind_vars_check)) {stop("Input variables must have unique names")}
    
    ## check data dimension
    num_obs <- nrow(dt_input)
    all_ind_vars
    if (num_obs < length(all_ind_vars)*10 ) {
      message("There are ",length(all_ind_vars), " independent variables & ", num_obs, " data points. We recommend row:column ratio >= 10:1")
    }
    
    
    ## check window_start & window_end
    if (is.null(window_start)) {
      window_start <- min(as.character(dt_input[, get(date_var)]))
    } else if (is.na(as.Date(window_start, "%Y-%m-%d"))) {
      stop("window_start must have format '2020-12-31'")
    } else if (window_start < min(as.character(dt_input[, get(date_var)]))) {
      window_start <- min(as.character(dt_input[, get(date_var)]))
      message("window_start is smaller than the earliest date in input data. It's set to the earliest date")
    } else if (window_start > max(as.character(dt_input[, get(date_var)]))) {
      stop("window_start can't be larger than the the latest date in input data")
    }
    
    rollingWindowStartWhich <- which.min(abs(difftime(as.Date(dt_input[, get(date_var)]), as.Date(window_start), units = "days")))
    if (!(as.Date(window_start) %in% dt_input[, get(date_var)])) {
      window_start <- dt_input[rollingWindowStartWhich, get(date_var)]
      message("window_start is adapted to the closest date contained in input data: ", window_start)
    }
    refreshAddedStart <- window_start
    
    if (is.null(window_end)) {
      window_end <- max(as.character(dt_input[, get(date_var)]))
    } else if (is.na(as.Date(window_end, "%Y-%m-%d"))) {
      stop("window_end must have format '2020-12-31'")
    } else if (window_end > max(as.character(dt_input[, get(date_var)]))) {
      window_end <- max(as.character(dt_input[, get(date_var)]))
      message("window_end is larger than the latest date in input data. It's set to the latest date")
    } else if (window_end < window_start) {
      window_end <- max(as.character(dt_input[, get(date_var)]))
      message("window_end must be >= window_start. It's set to latest date in input data")
    }
    
    rollingWindowEndWhich <- which.min(abs(difftime(as.Date(dt_input[, get(date_var)]), as.Date(window_end), units = "days")))
    if (!(as.Date(window_end) %in% dt_input[, get(date_var)])) {
      window_end <- dt_input[rollingWindowEndWhich, get(date_var)]
      message("window_end is adapted to the closest date contained in input data: ", window_end)
    }
    
    rollingWindowLength <- rollingWindowEndWhich - rollingWindowStartWhich +1
    
    dt_init <- dt_input[rollingWindowStartWhich:rollingWindowEndWhich, all_media, with =FALSE]
    init_all0 <- colSums(dt_init)==0
    if(any(init_all0)) {
      stop("These media channels contains only 0 within training period ",dt_input[rollingWindowStartWhich, get(date_var)], " to ", dt_input[rollingWindowEndWhich, get(date_var)], ": ", paste(names(dt_init)[init_all0], collapse = ", ")
           , " \nRecommendation: adapt InputCollect$window_start, remove or combine these channels")
    }
    
    ## check adstock
    
    if((adstock %in% c("geometric", "weibull")) == FALSE) {stop("adstock must be 'geometric' or 'weibull'")}
    
    ## get all hyper names
    global_name <- c("thetas",  "shapes",  "scales",  "alphas",  "gammas",  "lambdas")
    if (adstock == "geometric") {
      local_name <- sort(apply(expand.grid(all_media, global_name[global_name %like% 'thetas|alphas|gammas']), 1, paste, collapse="_"))
    } else if (adstock == "weibull") {
      local_name <- sort(apply(expand.grid(all_media, global_name[global_name %like% 'shapes|scales|alphas|gammas']), 1, paste, collapse="_"))
    }
    
    ## collect input
    InputCollect <- list(dt_input=dt_input
                         , dt_holidays=dt_holidays
                         , dt_mod=NULL
                         , dt_modRollWind=NULL
                         , xDecompAggPrev = NULL
                         ,date_var=date_var
                         ,dayInterval=dayInterval
                         ,intervalType=intervalType
                         
                         ,dep_var=dep_var
                         ,dep_var_type=dep_var_type
                         
                         #,activate_prophet=activate_prophet
                         ,prophet_vars=prophet_vars
                         ,prophet_signs=prophet_signs 
                         ,prophet_country=prophet_country
                         
                         #,activate_baseline=activate_baseline 
                         ,context_vars=context_vars 
                         ,context_signs=context_signs
                         
                         ,paid_media_vars=paid_media_vars
                         ,paid_media_signs=paid_media_signs
                         ,paid_media_spends=paid_media_spends
                         ,mediaVarCount=mediaVarCount
                         ,exposureVarName=exposureVarName
                         ,organic_vars=organic_vars
                         ,organic_signs=organic_signs
                         ,all_media=all_media
                         ,all_ind_vars=all_ind_vars
                         
                         ,factor_vars=factor_vars
                         
                         ,cores=cores
                         
                         ,window_start=window_start
                         ,rollingWindowStartWhich=rollingWindowStartWhich
                         ,window_end=window_end
                         ,rollingWindowEndWhich=rollingWindowEndWhich
                         ,rollingWindowLength=rollingWindowLength
                         ,refreshAddedStart=refreshAddedStart
                         
                         ,adstock=adstock
                         ,iterations=iterations
                         
                         ,nevergrad_algo=nevergrad_algo 
                         ,trials=trials 
                         
                         ,hyperparameters = hyperparameters 
                         ,local_name=local_name
                         #,activate_calibration=activate_calibration 
                         ,calibration_input=calibration_input
    )
    
    ## check hyperparameter names in hyperparameters
    
    
    ## output condition check
    
    # when hyperparameters is not provided
    if (is.null(hyperparameters)) { 
      
      message("\nhyperparameters is not provided yet. run robyn_inputs(InputCollect = InputCollect, hyperparameter = ...) to add it\n")
      invisible(InputCollect)
      
      # when hyperparameters is provided wrongly
    } else if (!identical(sort(names(hyperparameters)), local_name)) {
      
      stop("\nhyperparameters must be a list and contain vectors or values named as followed: ", paste(local_name, collapse = ", "), "\n")
      
    } else {
      
      ## check calibration
      
      if(!is.null(calibration_input)) {
        if ((min(calibration_input$liftStartDate) < min(dt_input[, get(date_var)])) | (max(calibration_input$liftEndDate) >  (max(dt_input[, get(date_var)]) + dayInterval-1))) {
          stop("we recommend you to only use experimental results conducted within your MMM input data date range")
        } else if (iterations < 2000 | trials < 10) {
          message("you are calibrating MMM. we recommend to run at least 2000 iterations per trial and at least 10 trials at the beginning")
        }
      } else {
        if (iterations < 2000 | trials < 5) {message("we recommend to run at least 2000 iterations per trial and at least 5 trials at the beginning")}
      }
    
      # when all provided once correctly
      message("\nAll input in robyn_inputs() correct. Ready to run robyn_run(...)")
      outFeatEng <- robyn_engineering(InputCollect = InputCollect, refresh = FALSE)
      invisible(outFeatEng)
      
    }
    
    
    
  } else if (!is.null(InputCollect) & is.null(hyperparameters) & is.null(InputCollect$hyperparameters) ) {
    
    # when InputCollect is provided, but hyperparameters not
    stop("\nhyperparameters is not provided yet. run robyn_inputs(InputCollect = InputCollect, hyperparameter = ...) to add it\n")
    
  } else {
    
    if (is.null(InputCollect$hyperparameters)) {InputCollect$hyperparameters <- hyperparameters}
    
    # when added hyperparameters 
    global_name <- c("thetas",  "shapes",  "scales",  "alphas",  "gammas",  "lambdas")
    if (InputCollect$adstock == "geometric") {
      local_name <- sort(apply(expand.grid(InputCollect$all_media, global_name[global_name %like% 'thetas|alphas|gammas']), 1, paste, collapse="_"))
    } else if (InputCollect$adstock == "weibull") {
      local_name <- sort(apply(expand.grid(InputCollect$all_media, global_name[global_name %like% 'shapes|scales|alphas|gammas']), 1, paste, collapse="_"))
    }
    
    if (!identical(sort(names(InputCollect$hyperparameters)), local_name)) {
      
      stop("\nhyperparameters must be a list and contain vectors or values named as followed: ", paste(local_name, collapse = ", "), "\n")
      
    } else {
      
      ## check calibration
      
      if(!is.null(calibration_input)) {
        if ((min(calibration_input$liftStartDate) < min(InputCollect$dt_input[, get(InputCollect$date_var)])) | 
            (max(calibration_input$liftEndDate) >  (max(InputCollect$dt_input[, get(InputCollect$date_var)]) + InputCollect$dayInterval-1))) {
          stop("we recommend you to only use experimental results conducted within your MMM input data date range")
        } else if (InputCollect$iterations < 2000 | InputCollect$trials < 10) {
          message("you are calibrating MMM. we recommend to run at least 2000 iterations per trial and at least 10 trials at the beginning")
        }
      } else {
        if (InputCollect$iterations < 2000 | InputCollect$trials < 5) {message("we recommend to run at least 2000 iterations per trial and at least 5 trials at the beginning")}
      }
      
      message("\nAll input in robyn_inputs() correct. running robyn_engineering()")
      outFeatEng <- robyn_engineering(InputCollect = InputCollect, refresh = FALSE)
      invisible(outFeatEng)
      
    }
  }
}



########################################################################
###### Data transformation and helper functions
########################################################################


plot_adstock <- function(plotAdstockCurves) {
  if (plotAdstockCurves) {
    # plot weibull
    weibullCollect <- list()
    shapeVec <- c(2, 2, 2, 2, 2, 2, 0.01, 0.1, 0.5, 1, 1.5, 2)
    scaleVec <- c(0.01, 0.05, 0.1, 0.15, 0.2, 0.5, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)
    paramRotate <- c(rep("scale",6), rep("shape",6))
    
    for (v in 1:length(shapeVec)) {
      dt_weibull<- data.table(x=1:100,
                              decay_accumulated=adstock_weibull(rep(1, 100), shape=shapeVec[v], scale=scaleVec[v])$thetaVecCum
                              ,shape=shapeVec[v]
                              ,scale=scaleVec[v]
                              ,param=paramRotate[v])
      dt_weibull[, halflife:= which.min(abs(decay_accumulated -0.5))]
      
      weibullCollect[[v]] <- dt_weibull
    }
    
    weibullCollect <- rbindlist(weibullCollect)
    #weibullCollect[, ':='(shape=as.factor(shape), scale=as.factor(scale))]
    weibullCollect[, scale_shape_halflife:=paste(scale, shape,halflife, sep = "_")]
    p1 <- ggplot(weibullCollect[param=="scale"], aes(x=x, y=decay_accumulated)) + 
      geom_line(aes(color=scale_shape_halflife)) +
      geom_hline(yintercept=0.5, linetype="dashed", color = "gray") +
      geom_text(aes(x = max(x), y = 0.5, vjust = -0.5, hjust= 1, label = "Halflife = time until effect reduces to 50%"), colour="gray") +
      labs(title="Weibull adstock transformation - scale changes", 
           subtitle="Halflife = time until effect reduces to 50%",
           x="time unit",
           y="Media decay accumulated") 
    p2 <- ggplot(weibullCollect[param=="shape"], aes(x=x, y=decay_accumulated)) + 
      geom_line(aes(color=scale_shape_halflife)) +
      geom_hline(yintercept=0.5, linetype="dashed", color = "gray") +
      geom_text(aes(x = max(x), y = 0.5, vjust = -0.5, hjust= 1, label = "Halflife = time until effect reduces to 50%"), colour="gray") +
      labs(title="Weibull adstock transformation - shape changes", 
           subtitle="Halflife = time until effect reduces to 50%",
           x="time unit",
           y="Media decay accumulated") 
    
    ## plot geometric
    
    geomCollect <- list()
    thetaVec <- c(0.01, 0.05, 0.1, 0.2, 0.5, 0.6, 0.7, 0.8, 0.9)
    
    for (v in 1:length(thetaVec)) {
      thetaVecCum <- 1
      for (t in 2:100) {thetaVecCum[t] <- thetaVecCum[t-1]*thetaVec[v]}
      dt_geom <- data.table(x=1:100,
                            decay_accumulated = thetaVecCum,
                            theta=thetaVec[v])
      dt_geom[, halflife:= which.min(abs(decay_accumulated -0.5))]
      geomCollect[[v]] <- dt_geom
    }
    geomCollect <- rbindlist(geomCollect)
    geomCollect[, theta_halflife:=paste(theta,halflife, sep = "_")]
    
    p3 <- ggplot(geomCollect, aes(x=x, y=decay_accumulated)) + 
      geom_line(aes(color=theta_halflife)) +
      geom_hline(yintercept=0.5, linetype="dashed", color = "gray") +
      geom_text(aes(x = max(x), y = 0.5, vjust = -0.5, hjust= 1, label = "Halflife = time until effect reduces to 50%"), colour="gray") +
      labs(title="Geometric adstock transformation - theta changes", 
           subtitle="Halflife = time until effect reduces to 50%",
           x="time unit",
           y="Media decay accumulated") 
    #print(p3)
    
    grid.arrange(p1,p2, p3, layout_matrix = rbind(c(3,1),
                                                  c(3,2)))
    
  }
}

plot_saturation <- function(plotResponseCurves) {
  if (plotResponseCurves) {
    xSample <- 1:100
    alphaSamp <- c(0.1, 0.5, 1, 2, 3)
    gammaSamp <- c(0.1, 0.3, 0.5, 0.7, 0.9)
    
    ## plot alphas
    hillAlphaCollect <- list()
    for (i in 1:length(alphaSamp)) {
      hillAlphaCollect[[i]] <- data.table(x=xSample
                                          ,y=xSample**alphaSamp[i] / (xSample**alphaSamp[i] + (0.5*100)**alphaSamp[i])
                                          ,alpha=alphaSamp[i])
    }
    hillAlphaCollect <- rbindlist(hillAlphaCollect)
    hillAlphaCollect[, alpha:=as.factor(alpha)]
    p1 <- ggplot(hillAlphaCollect, aes(x=x, y=y, color=alpha)) + 
      geom_line() +
      labs(title = "Cost response with hill function"
           ,subtitle = "Alpha changes while gamma = 0.5")
    
    ## plot gammas
    hillGammaCollect <- list()
    for (i in 1:length(gammaSamp)) {
      hillGammaCollect[[i]] <- data.table(x=xSample
                                          ,y=xSample**2 / (xSample**2 + (gammaSamp[i]*100)**2)
                                          ,gamma=gammaSamp[i])
    }
    hillGammaCollect <- rbindlist(hillGammaCollect)
    hillGammaCollect[, gamma:=as.factor(gamma)]
    p2 <- ggplot(hillGammaCollect, aes(x=x, y=y, color=gamma)) + 
      geom_line() +
      labs(title = "Cost response with hill function"
           ,subtitle = "Gamma changes while alpha = 2")
    
    grid.arrange(p1,p2, nrow=1)
  }
}

#####################################
#### Define helper unit format function for axis 

format_unit <- function(x_in) {
  x_out <- sapply(x_in, function(x) {
    if (abs(x) >= 1000000000) {
      x_out <- paste0(round(x/1000000000, 1), " bln")
    } else if (abs(x) >= 1000000 & abs(x)<1000000000) {
      x_out <- paste0(round(x/1000000, 1), " mio")
    } else if (abs(x) >= 1000 & abs(x)<1000000) {
      x_out <- paste0(round(x/1000, 1), " tsd")
    } else {
      x_out <- round(x,0)
    }
  }, simplify = TRUE) 
  return(x_out)
}

################################################################
#### Define major input data transformation function


robyn_engineering <- function(InputCollect = InputCollect
                              , refresh = FALSE) {
  
  paid_media_vars <- InputCollect$paid_media_vars
  paid_media_spends <- InputCollect$paid_media_spends
  context_vars <- InputCollect$context_vars
  organic_vars <- InputCollect$organic_vars
  all_media <- InputCollect$all_media
  all_ind_vars <- InputCollect$all_ind_vars
  
  dt_input <- copy(InputCollect$dt_input) # dt_input <- copy(InputCollect$dt_input)
  
  dt_inputRollWind <- dt_input[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich] # dt_inputRollWind <- InputCollect$dt_input[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
  
  dt_transform <- copy(InputCollect$dt_input) # dt_transform <- copy(InputCollect$dt_input)
  setnames(dt_transform, InputCollect$date_var, "ds", skip_absent = TRUE)
  dt_transform <- dt_transform[, ':='(ds= as.Date(ds))][order(ds)]
  dt_transformRollWind <- dt_transform[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
  
  setnames(dt_transform, InputCollect$dep_var, "dep_var", skip_absent = TRUE) #; InputCollect$dep_var <- "dep_var"
  
  ################################################################
  #### model exposure metric from spend
  
  mediaCostFactor <- unlist(dt_inputRollWind[, lapply(.SD, sum), .SDcols = paid_media_spends] / dt_inputRollWind[, lapply(.SD, sum), .SDcols = paid_media_vars])
  names(mediaCostFactor) <- paid_media_vars
  costSelector <- !(paid_media_spends == paid_media_vars)
  names(costSelector) <- paid_media_vars
  
  if (any(costSelector)) {
    modNLSCollect <- list()
    yhatCollect <- list()
    plotNLSCollect <- list()
    for (i in 1:InputCollect$mediaVarCount) {
      if (costSelector[i]) {
        dt_spendModInput <- dt_inputRollWind[, c(paid_media_spends[i],paid_media_vars[i]), with =FALSE]
        setnames(dt_spendModInput, names(dt_spendModInput), c("spend", "exposure"))
        #dt_spendModInput <- dt_spendModInput[spend !=0 & exposure != 0]
        
        # scale 0 spend and exposure to a tiny number
        dt_spendModInput[, spend:=as.numeric(spend)][spend==0, spend:=0.01] # remove spend == 0 to avoid DIV/0 error
        dt_spendModInput[, exposure:=as.numeric(exposure)][exposure==0, exposure:=spend / mediaCostFactor[i]] # adapt exposure with avg when spend == 0
        
        # mod_nls <- nls(exposure ~ SSmicmen(spend, Vmax, Km)
        #                ,data = dt_spendModInput
        #                ,control = nls.control(minFactor=1/2048, warnOnly = TRUE))
        
        # estimate starting values for nls
        # modLM <- lm(log(exposure) ~ spend, dt_spendModInput)
        # nlsStartVal <- list(Vmax = exp(coef(modLM)[1]), Km = coef(modLM)[2])
        # nlsStartVal <- list(Vmax = dt_spendModInput[, max(exposure)/2], Km = dt_spendModInput[, max(exposure)])
        # run nls model
        # modNLS <- nlsLM(exposure ~ Vmax * spend/(Km + spend), #Michaelis-Menten model Vmax * spend/(Km + spend)
        #                data = dt_spendModInput,
        #                start = nlsStartVal
        #                ,control = nls.control(warnOnly = TRUE)
        # )
        
        tryCatch(
          {
            #dt_spendModInput[, exposure:= rep(0,(nrow(dt_spendModInput)))]
            nlsStartVal <- list(Vmax = dt_spendModInput[, max(exposure)], Km = dt_spendModInput[, max(exposure)/2])
            suppressWarnings(modNLS <- nlsLM(exposure ~ Vmax * spend/(Km + spend), #Michaelis-Menten model Vmax * spend/(Km + spend)
                                             data = dt_spendModInput,
                                             start = nlsStartVal
                                             ,control = nls.control(warnOnly = TRUE)))
            
            yhatNLS <- predict(modNLS)
            modNLSSum <- summary(modNLS)
            
            # QA nls model prediction
            yhatNLSQA <- modNLSSum$coefficients[1,1] * dt_spendModInput$spend / (modNLSSum$coefficients[2,1] + dt_spendModInput$spend) #exposure = v  * spend / (k + spend)
            identical(yhatNLS, yhatNLSQA)
            
            rsq_nls <- get_rsq(true = dt_spendModInput$exposure, predicted = yhatNLS)
          },
          
          error=function(cond) {
            # nlsStartVal <- list(Vmax=1, Km=1)
            # suppressWarnings(modNLS <- nlsLM(exposure ~ Vmax * spend/(Km + spend), #Michaelis-Menten model Vmax * spend/(Km + spend)
            #                                  data = dt_spendModInput,
            #                                  start = nlsStartVal
            #                                  ,control = nls.control(warnOnly = TRUE)))
            # warning("default start value for nls out of range. using c(1,1) instead")
            # return(modNLS)
            message("michaelis menten fitting for ", paid_media_vars[i]," out of range. using lm instead")
            
          }
        )
        if (!exists("modNLS")) {modNLS <- NULL; yhatNLS <- NULL; modNLSSum <- NULL; rsq_nls <- NULL}
        # build lm comparison model
        modLM <- lm(exposure ~ spend-1, data = dt_spendModInput)
        yhatLM <- predict(modLM)
        modLMSum <- summary(modLM)
        rsq_lm <- get_rsq(true = dt_spendModInput$exposure, predicted = yhatLM) 
        if (is.na(rsq_lm)) {stop("please check if ",paid_media_vars[i]," constains only 0")}
        
        # compare NLS & LM, takes LM if NLS fits worse
        costSelector[i] <- if(is.null(rsq_nls)) {FALSE} else {rsq_nls > rsq_lm}
        
        modNLSCollect[[paid_media_vars[i]]] <- data.table(channel = paid_media_vars[i],
                                                          Vmax = if (!is.null(modNLS)) {modNLSSum$coefficients[1,1]} else {NA},
                                                          Km =  if (!is.null(modNLS)) {modNLSSum$coefficients[2,1]} else {NA},
                                                          aic_nls = if (!is.null(modNLS)) {AIC(modNLS)} else {NA},
                                                          aic_lm = AIC(modLM),
                                                          bic_nls = if (!is.null(modNLS)) {BIC(modNLS)} else {NA},
                                                          bic_lm = BIC(modLM),
                                                          rsq_nls = if (!is.null(modNLS)) {rsq_nls} else {0},
                                                          rsq_lm = rsq_lm,
                                                          coef_lm = coef(modLMSum)[1]
        )
        
        dt_plotNLS <- data.table(channel = paid_media_vars[i],
                                 yhatNLS = if(costSelector[i]) {yhatNLS} else {yhatLM},
                                 yhatLM = yhatLM,
                                 y = dt_spendModInput$exposure,
                                 x = dt_spendModInput$spend)
        dt_plotNLS <- melt.data.table(dt_plotNLS, id.vars = c("channel", "y", "x"), variable.name = "models", value.name = "yhat")
        dt_plotNLS[, models:= str_remove(tolower(models), "yhat")]
        
        yhatCollect[[paid_media_vars[i]]] <- dt_plotNLS
        
        # create plot
        plotNLSCollect[[paid_media_vars[i]]] <- ggplot(dt_plotNLS, aes(x=x, y=y, color = models)) +
          geom_point() +
          geom_line(aes(y=yhat, x=x, color = models)) +
          labs(subtitle = paste0("y=",paid_media_vars[i],", x=", paid_media_spends[i],
                                 "\nnls: aic=", round(AIC(if(costSelector[i]) {modNLS} else {modLM}),0), ", rsq=", round(if(costSelector[i]) {rsq_nls} else {rsq_lm},4),
                                 "\nlm: aic= ", round(AIC(modLM),0), ", rsq=", round(rsq_lm,4)),
               x = "spend",
               y = "exposure"
          ) +
          theme(legend.position = 'bottom')
        
      }
    }
    
    modNLSCollect <- rbindlist(modNLSCollect)
    yhatNLSCollect <- rbindlist(yhatCollect)
    yhatNLSCollect[, ds:= rep(dt_transformRollWind$ds, nrow(yhatNLSCollect)/nrow(dt_transformRollWind))]
    
  } else {
    modNLSCollect <- NULL
    plotNLSCollect <- NULL
    yhatNLSCollect <- NULL
  }
  
  getSpendSum <- dt_input[, lapply(.SD, sum), .SDcols=paid_media_spends]
  names(getSpendSum) <- paid_media_vars
  getSpendSum <- suppressWarnings(melt.data.table(getSpendSum, measure.vars= paid_media_vars, variable.name = "rn", value.name = "spend"))
  
  ################################################################
  #### clean & aggregate data
  
  ## transform all factor variables
  factor_vars <- InputCollect$factor_vars
  if (length(factor_vars)>0) {
    dt_transform[, (factor_vars):= lapply(.SD, as.factor), .SDcols = factor_vars ]
  } 
  
  ################################################################
  #### Obtain prophet trend, seasonality and changepoints
  
  if ( !is.null(InputCollect$prophet_vars) ) {
    
    if(length(InputCollect$prophet_vars) != length(InputCollect$prophet_signs)) {stop("InputCollect$prophet_vars and InputCollect$prophet_signs have to be the same length")}
    if(any(length(InputCollect$prophet_vars)==0, length(InputCollect$prophet_signs)==0)) {stop("InputCollect$prophet_vars and InputCollect$prophet_signs must be both specified")}
    if(!(InputCollect$prophet_country %in% InputCollect$dt_holidays$country)) {stop("InputCollect$prophet_country must be already included in the holidays.csv and as ISO 3166-1 alpha-2 abbreviation")}
    
    recurrance <- dt_transform[, .(ds = ds, y = dep_var)]
    use_trend <- any(str_detect("trend", InputCollect$prophet_vars))
    use_season <- any(str_detect("season", InputCollect$prophet_vars))
    use_weekday <- any(str_detect("weekday", InputCollect$prophet_vars))
    use_holiday <- any(str_detect("holiday", InputCollect$prophet_vars))
    
    if (InputCollect$intervalType == "day") {
      
      holidays <- InputCollect$dt_holidays
      
    } else if (InputCollect$intervalType == "week") {
      
      weekStartInput <- wday(dt_transform[1, ds])
      weekStartMonday <- if(weekStartInput==2) {TRUE} else if (weekStartInput==1) {FALSE} else {stop("week start has to be Monday or Sunday")}
      InputCollect$dt_holidays[, dsWeekStart:= cut(as.Date(ds), breaks = InputCollect$intervalType, start.on.monday = weekStartMonday)]
      holidays <- InputCollect$dt_holidays[, .(ds=dsWeekStart, holiday, country, year)]
      holidays <- holidays[, lapply(.SD, paste0, collapse="#"), by = c("ds", "country", "year"), .SDcols = "holiday"]
      
    } else if (InputCollect$intervalType == "month") {
      
      monthStartInput <- all(day(dt_transform[, ds]) ==1)
      if (monthStartInput==FALSE) {stop("monthly data should have first day of month as datestampe, e.g.'2020-01-01' ")}
      InputCollect$dt_holidays[, dsMonthStart:= cut(as.Date(ds), InputCollect$intervalType)]
      holidays <- InputCollect$dt_holidays[, .(ds=dsMonthStart, holiday, country, year)]
      holidays <- holidays[, lapply(.SD, paste0, collapse="#"), by = c("ds", "country", "year"), .SDcols = "holiday"]
      
    }
    
    
    if (!is.null(factor_vars)) {
      dt_regressors <- cbind(recurrance, dt_transform[, c(context_vars, paid_media_vars), with =FALSE])
      modelRecurrance <- prophet(holidays = if(use_holiday) {holidays[country==InputCollect$prophet_country]} else {NULL}
                                 ,yearly.seasonality = use_season
                                 ,weekly.seasonality = use_weekday
                                 ,daily.seasonality= FALSE)
      # for (addreg in factor_vars) {
      #   modelRecurrance <- add_regressor(modelRecurrance, addreg)
      # }
      
      dt_ohe <- as.data.table(model.matrix(y ~., dt_regressors[, c("y",factor_vars), with =FALSE])[,-1])
      ohe_names <- names(dt_ohe)
      for (addreg in ohe_names) {
        modelRecurrance <- add_regressor(modelRecurrance, addreg)
      }
      dt_ohe <- cbind(dt_regressors[, !factor_vars, with=FALSE], dt_ohe)
      mod_ohe <- fit.prophet(modelRecurrance, dt_ohe)
      # prophet::regressor_coefficients(mxxx)
      dt_forecastRegressor <- predict(mod_ohe, dt_ohe)
      # prophet::prophet_plot_components(mod_ohe, dt_forecastRegressor)
      
      forecastRecurrance <- dt_forecastRegressor[, str_detect(names(dt_forecastRegressor), "_lower$|_upper$", negate = TRUE), with =FALSE]
      for (aggreg in factor_vars) {
        oheRegNames <- na.omit(str_extract(names(forecastRecurrance), paste0("^",aggreg, ".*")))
        forecastRecurrance[, (aggreg):=rowSums(.SD), .SDcols=oheRegNames]
        get_reg <- forecastRecurrance[, get(aggreg)]
        dt_transform[, (aggreg):= scale(get_reg, center = min(get_reg), scale = FALSE)]
        #dt_transform[, (aggreg):= get_reg]
      }
      # modelRecurrance <- fit.prophet(modelRecurrance, dt_regressors)
      # forecastRecurrance <- predict(modelRecurrance, dt_transform[, c("ds",context_vars, paid_media_vars), with =FALSE])
      # prophet_plot_components(modelRecurrance, forecastRecurrance)
      
    } else {
      modelRecurrance<- prophet(recurrance
                                ,holidays = if(use_holiday) {holidays[country==InputCollect$prophet_country]} else {NULL}
                                ,yearly.seasonality = use_season
                                ,weekly.seasonality = use_weekday
                                ,daily.seasonality= FALSE
                                #,changepoint.range = 0.8
                                #,seasonality.mode = 'multiplicative'
                                #,changepoint.prior.scale = 0.1
      )
      
      #futureDS <- make_future_dataframe(modelRecurrance, periods=1, freq = InputCollect$intervalType)
      forecastRecurrance <- predict(modelRecurrance, dt_transform[, "ds", with =FALSE])
      
    }
    
    # plot(modelRecurrance, forecastRecurrance)
    # prophet_plot_components(modelRecurrance, forecastRecurrance, render_plot = TRUE)
    
    if (use_trend) {
      fc_trend <- forecastRecurrance$trend[1:NROW(recurrance)]
      dt_transform[, trend := fc_trend]
      # recurrance[, trend := scale(fc_trend, center = min(fc_trend), scale = FALSE) + 1]
      # dt_transform[, trend := recurrance$trend]
    }
    if (use_season) {
      fc_season <- forecastRecurrance$yearly[1:NROW(recurrance)]
      dt_transform[, season := fc_season]
      # recurrance[, seasonal := scale(fc_season, center = min(fc_season), scale = FALSE) + 1]
      # dt_transform[, season := recurrance$seasonal]
    }
    if (use_weekday) {
      fc_weekday <- forecastRecurrance$weekly[1:NROW(recurrance)]
      dt_transform[, weekday := fc_weekday]
      # recurrance[, weekday := scale(fc_weekday, center = min(fc_weekday), scale = FALSE) + 1]
      # dt_transform[, weekday := recurrance$weekday]
    }
    if (use_holiday) {
      fc_holiday <- forecastRecurrance$holidays[1:NROW(recurrance)]
      dt_transform[, holiday := fc_holiday]
      # recurrance[, holidays := scale(fc_holiday, center = min(fc_holiday), scale = FALSE) + 1]
      # dt_transform[, holiday := recurrance$holidays]
    }
  }
  
  ################################################################
  #### Finalize input
  
  #dt <- dt[, all_name, with = FALSE]
  dt_transform <- dt_transform[, c("ds", "dep_var", all_ind_vars), with = FALSE]
  
  InputCollect$dt_mod <- dt_transform
  InputCollect$dt_modRollWind <- dt_transform[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
  InputCollect$dt_inputRollWind <- dt_inputRollWind
  InputCollect$all_media <- all_media
  
  InputCollect[['modNLSCollect']] <- modNLSCollect
  InputCollect[['plotNLSCollect']] <- plotNLSCollect  
  InputCollect[['yhatNLSCollect']] <- yhatNLSCollect  
  InputCollect[['costSelector']] <- costSelector  
  InputCollect[['mediaCostFactor']] <- mediaCostFactor  
  
  #f.checkConditions(dt_transform = dt_transform, listParam = listParam)
  
  # if (!refresh) {
  #   assign("InputCollect", InputCollect, envir = .GlobalEnv)
  #   assign("listParam", listParam, envir = .GlobalEnv)
  # } else {
  #   assign("listDTRefresh", InputCollect, envir = .GlobalEnv)
  #   assign("listParamRefresh", listParam, envir = .GlobalEnv)
  # }
  
  return(InputCollect)
  #return(dt_transform)
}

################################################################
#### Define hyperparameter names extraction function

hyper_names <- function(adstock = InputCollect$adstock, all_media=InputCollect$all_media) {
  global_name <- c("thetas",  "shapes",  "scales",  "alphas",  "gammas",  "lambdas")
  if (adstock == "geometric") {
    local_name <- sort(apply(expand.grid(all_media, global_name[global_name %like% 'thetas|alphas|gammas']), 1, paste, collapse="_"))
  } else if (adstock == "weibull") {
    local_name <- sort(apply(expand.grid(all_media, global_name[global_name %like% 'shapes|scales|alphas|gammas']), 1, paste, collapse="_"))
  }
  return(local_name)
}

################################################
#### Define Michaelis Menten function

mic_men <- function(x, Vmax, Km, reverse = FALSE) {
  if (!reverse) {
    mm_out <- exposure <- Vmax * x/(Km + x)
  } else {
    mm_out <- spend <- x * Km / (Vmax - x)
  }
  return(mm_out)
}

################################################
#### Define adstock geometric function

adstock_geometric <- function(x, theta) {
  x_decayed <- c(x[1] ,rep(0, length(x)-1))
  for (xi in 2:length(x_decayed)) {
    x_decayed[xi] <- x[xi] + theta * x_decayed[xi-1]
  }
  
  thetaVecCum <- theta
  for (t in 2:length(x)) {thetaVecCum[t] <- thetaVecCum[t-1]*theta} # plot(thetaVecCum)
  
  return(list(x_decayed=x_decayed, thetaVecCum = thetaVecCum))
}

################################################
#### Define adstock weibull function

adstock_weibull <- function(x, shape , scale) {
  x.n <- length(x)
  x_bin <- 1:x.n
  scaleTrans = round(quantile(x_bin, scale),0) #
  thetaVec <- c(1, 1-pweibull(head(x_bin, -1), shape = shape, scale = scaleTrans)) # plot(thetaVec)
  thetaVecCum <- cumprod(thetaVec)  # plot(thetaVecCum)
  
  x_decayed <- mapply(function(x, y) {
    x.vec <- c(rep(0,y-1), rep(x, x.n-y+1))
    thetaVecCumLag <- shift(thetaVecCum, y-1, fill = 0)
    x.matrix <- cbind(x.vec, thetaVecCumLag) #  plot(x.vec)
    x.prod <- apply(x.matrix, 1, prod)
    return(x.prod)
  }, x=x , y=x_bin)
  x_decayed <- rowSums(x_decayed)
  
  return(list(x_decayed=x_decayed, thetaVecCum = thetaVecCum))
}

################################################
#### Define saturation hill function

saturation_hill <- function(x, alpha, gamma, x_marginal = NULL) {
  
  gammaTrans <- round(quantile(seq(range(x)[1], range(x)[2], length.out = 100), gamma),4)
  
  if (is.null(x_marginal)) {
    x_scurve <-  x**alpha / (x**alpha + gammaTrans**alpha) # plot(x_scurve) summary(x_scurve)
  } else {
    x_scurve <-  x_marginal**alpha / (x_marginal**alpha + gammaTrans**alpha)
  }
  return(x_scurve)
}


################################################
#### Define r-squared function

get_rsq <- function(true, predicted, p = NULL, df.int = NULL) {
  
  sse <- sum((predicted - true)^2)
  sst <- sum((true - mean(true))^2)
  rsq <- 1 - sse / sst
  
  # adjusted rsq formula from summary.lm: ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - df.int)/rdf) # n = num_obs, p = num_indepvar, rdf = n-p-1
  if (!is.null(p) & !is.null(df.int)) {
    n <- length(true)
    rdf <- n - p - 1
    rsq <- 1 - (1 - rsq) * ((n - df.int)/rdf)
  }
  
  return(rsq)
}

################################################
#### Define ridge lambda sequence function

ridge_lambda <- function(x, y, seq_len = 100, lambda_min_ratio = 0.0001) {
  mysd <- function(y) sqrt(sum((y-mean(y))^2)/length(y))
  sx <- scale(x,scale=apply(x, 2, mysd))
  sx <- as.matrix(sx, ncol=ncol(x), nrow=nrow(x))
  #sy <- as.vector(scale(y, scale=mysd(y)))
  sy <- y
  lambda_max <- max(abs(colSums(sx*sy))) / (0.001 * nrow(x)) # 0.001 is the default smalles alpha value of glmnet for ridge (alpha = 0)
  
  lambda_max_log <- log(lambda_max)
  log_step <- (log(lambda_max)-log(lambda_max * lambda_min_ratio)) / (seq_len-1)
  log_seq <- seq(log(lambda_max) , log(lambda_max*lambda_min_ratio), length.out = seq_len)
  lambda_seq <- exp(log_seq)
  return(lambda_seq)
}

################################################
#### Define model decomposition function

model_decomp <- function(coefs, dt_modSaturated, x, y_pred, i, dt_modRollWind, refreshAddedStart) {
  
  ## input for decomp
  y <- dt_modSaturated$dep_var
  indepVar <- dt_modSaturated[, (setdiff(names(dt_modSaturated), "dep_var")), with = FALSE]
  x <- as.data.table(x)
  intercept <- coefs[1]
  indepVarName <- names(indepVar)
  indepVarCat <- indepVarName[sapply(indepVar, is.factor)]
  
  ## decomp x
  xDecomp <- data.table(mapply(function(regressor, coeff) {regressor*coeff}, regressor = x, coeff= coefs[-1]))
  xDecomp <- cbind(data.table(intercept = rep(intercept, nrow(xDecomp))), xDecomp)
  #xDecompOut <- data.table(sapply(indepVarName, function(x) xDecomp[, rowSums(.SD,), .SDcols = str_which(names(xDecomp), x)]))
  xDecompOut <- cbind(data.table(ds = dt_modRollWind$ds, y = y, y_pred = y_pred) ,xDecomp)
  
  ## QA decomp
  y_hat <- rowSums(xDecomp)
  errorTerm <- y_hat - y_pred
  if (prod(round(y_pred) == round(y_hat)) == 0) {cat("\n### attention for loop", i,": manual decomp is not matching linear model prediction. Deviation is", mean(errorTerm / y)*100,"% ### \n")}
  
  ## output decomp
  y_hat.scaled <- rowSums(abs(xDecomp))
  xDecompOutPerc.scaled <- abs(xDecomp)/y_hat.scaled
  xDecompOut.scaled <- y_hat*xDecompOutPerc.scaled
  
  xDecompOutAgg <- sapply(xDecompOut[, c("intercept", indepVarName), with =FALSE], function(x) sum(x))
  xDecompOutAggPerc <- xDecompOutAgg / sum(y_hat)
  xDecompOutAggMeanNon0 <- sapply(xDecompOut[, c("intercept", indepVarName), with =FALSE], function(x) ifelse(is.na(mean(x[x>0])),0,mean(x[x!=0])))
  xDecompOutAggMeanNon0[is.nan(xDecompOutAggMeanNon0)] <- 0
  xDecompOutAggMeanNon0Perc <- xDecompOutAggMeanNon0/sum(xDecompOutAggMeanNon0)
  #xDecompOutAggPerc.scaled <- abs(xDecompOutAggPerc)/sum(abs(xDecompOutAggPerc))
  #xDecompOutAgg.scaled <- sum(xDecompOutAgg)*xDecompOutAggPerc.scaled
  
  refreshAddedStartWhich <- which(xDecompOut$ds==refreshAddedStart)
  refreshAddedEnd <- max(xDecompOut$ds)
  refreshAddedEndWhich <- which(xDecompOut$ds==refreshAddedEnd)
  xDecompOutAggRF <- sapply(xDecompOut[refreshAddedStartWhich:refreshAddedEndWhich, c("intercept", indepVarName), with =FALSE], function(x) sum(x))
  y_hatRF <- y_hat[refreshAddedStartWhich:refreshAddedEndWhich]
  xDecompOutAggPercRF <- xDecompOutAggRF / sum(y_hatRF)
  xDecompOutAggMeanNon0RF <- sapply(xDecompOut[refreshAddedStartWhich:refreshAddedEndWhich, c("intercept", indepVarName), with =FALSE], function(x) ifelse(is.na(mean(x[x>0])),0,mean(x[x!=0])))
  xDecompOutAggMeanNon0RF[is.nan(xDecompOutAggMeanNon0RF)] <- 0
  xDecompOutAggMeanNon0PercRF <- xDecompOutAggMeanNon0RF/sum(xDecompOutAggMeanNon0RF)
  
  coefsOut <- data.table(coefs, keep.rownames = TRUE)
  coefsOutCat <- copy(coefsOut)
  coefsOut[, rn := if (length(indepVarCat) == 0) {rn} else {sapply(indepVarCat, function(x) str_replace(coefsOut$rn, paste0(x,".*"), x))}]
  coefsOut <- coefsOut[, .(coef = mean(s0)), by = rn]
  
  decompOutAgg <- cbind(coefsOut, data.table(xDecompAgg = xDecompOutAgg
                                             ,xDecompPerc = xDecompOutAggPerc
                                             ,xDecompMeanNon0 = xDecompOutAggMeanNon0
                                             ,xDecompMeanNon0Perc = xDecompOutAggMeanNon0Perc
                                             ,xDecompAggRF = xDecompOutAggRF
                                             ,xDecompPercRF = xDecompOutAggPercRF
                                             ,xDecompMeanNon0RF = xDecompOutAggMeanNon0RF
                                             ,xDecompMeanNon0PercRF = xDecompOutAggMeanNon0PercRF
                                             #,xDecompAgg.scaled = xDecompOutAgg.scaled
                                             #,xDecompPerc.scaled = xDecompOutAggPerc.scaled
  ))
  decompOutAgg[, pos:= xDecompAgg>=0]
  
  decompCollect <- list(xDecompVec= xDecompOut, xDecompVec.scaled=xDecompOut.scaled, xDecompAgg = decompOutAgg, coefsOutCat=coefsOutCat)
  
  return(decompCollect)
} ## decomp end

################################################
#### Define lift calibration function

calibrate_mmm <- function(decompCollect, calibration_input=InputCollect$calibration_input, paid_media_vars=InputCollect$paid_media_vars) {
  
  check_set_lift <- any(sapply(calibration_input$channel, function(x) any(str_detect(x, paid_media_vars)))==FALSE) #check if any lift channel doesnt have media var
  if (check_set_lift) {stop("calibration_input channels must have media variable")}
  ## prep lift input
  getLiftMedia <- unique(calibration_input$channel)
  getDecompVec <- decompCollect$xDecompVec
  
  ## loop all lift input
  liftCollect <- list()
  for (m in 1:length(getLiftMedia)) { # loop per lift channel
    
    liftWhich <- str_which(calibration_input$channel, getLiftMedia[m])
    
    liftCollect2 <- list()
    for (lw in 1:length(liftWhich)) { # loop per lift test per channel
      
      ## get lift period subset
      liftStart <- calibration_input[liftWhich[lw], liftStartDate]
      liftEnd <- calibration_input[liftWhich[lw], liftEndDate]
      liftPeriodVec <- getDecompVec[ds >= liftStart & ds <= liftEnd, c("ds", getLiftMedia[m]), with = FALSE]
      liftPeriodVecDependent <- getDecompVec[ds >= liftStart & ds <= liftEnd, c("ds", "y"), with = FALSE]
      
      ## scale decomp
      mmmDays <- nrow(liftPeriodVec) * 7
      liftDays <- as.integer(liftEnd- liftStart + 1)
      y_hatLift <- sum(unlist(getDecompVec[, -1])) # total pred sales
      x_decompLift <- sum(liftPeriodVec[,2])
      x_decompLiftScaled <- x_decompLift / mmmDays * liftDays
      y_scaledLift <- liftPeriodVecDependent[, sum(y)] / mmmDays * liftDays
      
      ## output
      liftCollect2[[lw]] <- data.table(liftMedia = getLiftMedia[m] ,
                                       liftStart = liftStart,
                                       liftEnd = liftEnd,
                                       liftAbs = calibration_input[liftWhich[lw], liftAbs],
                                       decompAbsScaled = x_decompLiftScaled,
                                       dependent = y_scaledLift)
    }
    liftCollect[[m]] <- rbindlist(liftCollect2)
  }
  
  ## get mape_lift
  liftCollect <- rbindlist(liftCollect)[, mape_lift := abs((decompAbsScaled - liftAbs) / liftAbs)]
  return(liftCollect)
}


########################################################################
###### Major MMM function
########################################################################


#####################################
#### Define refit function
model_refit <- function(x_train, y_train, lambda, lower.limits, upper.limits) {
  mod <- glmnet(
    x_train
    ,y_train
    ,family = "gaussian"
    ,alpha = 0 #0 for ridge regression
    ,lambda = lambda # https://stats.stackexchange.com/questions/138569/why-is-lambda-within-one-standard-error-from-the-minimum-is-a-recommended-valu
    ,lower.limits = lower.limits
    ,upper.limits = upper.limits
  ) # coef(mod)
  
  ## drop intercept if negative
  if (coef(mod)[1] <0) {
    mod <- glmnet(
      x_train
      ,y_train
      ,family = "gaussian"
      ,alpha = 0 #0 for ridge regression
      ,lambda = lambda
      ,lower.limits = lower.limits
      ,upper.limits = upper.limits
      ,intercept = FALSE
    ) # coef(mod)
  } #; plot(mod); print(mod)
  
  df.int <- ifelse(coef(mod)[1] < 0, 0, 1)
  
  y_trainPred <- predict(mod, s = lambda, newx = x_train)
  rsq_train<- get_rsq(true = y_train, predicted = y_trainPred, p = ncol(x_train), df.int = df.int); rsq_train
  
  #y_testPred <- predict(mod, s = lambda, newx = x_test)
  #rsq_test <- get_rsq(true = y_test, predicted = y_testPred); rsq_test
  
  #mape_mod<- mean(abs((y_test - y_testPred)/y_test)* 100); mape_mod
  coefs <- as.matrix(coef(mod))
  #y_pred <- c(y_trainPred, y_testPred)
  
  nrmse_train <- sqrt(mean((y_train - y_trainPred)^2)) / (max(y_train) - min(y_train)) # mean(y_train) sd(y_train)
  #nrmse_test <- sqrt(mean(sum((y_test - y_testPred)^2))) / (max(y_test) - min(y_test)) # mean(y_test) sd(y_test)
  
  mod_out <- list(rsq_train = rsq_train
                  #,rsq_test = rsq_test
                  ,nrmse_train = nrmse_train
                  #,nrmse_test = nrmse_test
                  #,mape_mod = mape_mod
                  ,coefs = coefs
                  ,y_pred = y_trainPred
                  ,mod=mod
                  ,df.int = df.int)
  
  return(mod_out)
}

################################################################
#### Define major mmm function

robyn_mmm <- function(hyper_collect
                      , InputCollect
                      , iterations = InputCollect$iterations
                      , lambda.n = 100
                      , lambda_fixed = NULL
                      , refresh = FALSE
) {
  
  ################################################
  #### Collect hyperparameters
  
  hypParamSamName <- hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
  hyper_fixed <- FALSE
  
  # hyper_collect <- unlist(list(...), recursive = FALSE) # hyper_collect <- InputCollect$hyperparameters; hyper_collect <- hyperparameters_fixed
  
  # sort hyperparameter list by name
  hyper_bound_list <- list()
  for (i in 1:length(hypParamSamName)) {
    hyper_bound_list[i] <- hyper_collect[hypParamSamName[i]]
    names(hyper_bound_list)[i] <- hypParamSamName[i]
  }
  
  # get hyperparameters for Nevergrad
  hyper_which <- which(sapply(hyper_bound_list, length)==2)
  hyper_bound_list_updated <- hyper_bound_list[hyper_which]
  hyper_bound_list_updated_name <- names(hyper_bound_list_updated)
  hyper_count <- length(hyper_bound_list_updated)
  if (hyper_count == 0) {
    hyper_fixed <- TRUE
    if (is.null(lambda_fixed)) {stop("when hyperparameters are fixed, lambda_fixed must be provided from the selected lambda in old model")}
  }
  
  # get fixed hyperparameters
  hyper_fixed_which <- which(sapply(hyper_bound_list, length)==1)
  hyper_bound_list_fixed <- hyper_bound_list[hyper_fixed_which]
  hyper_bound_list_fixed_name <- names(hyper_bound_list_fixed)  
  hyper_count_fixed <- length(hyper_bound_list_fixed)
  
  #hyper_bound_list_fixed <- list(print_S_alphas = 1 , print_S_gammas = 0.5)
  if (InputCollect$cores >1) {
    dt_hyperFixed <- data.table(sapply(hyper_bound_list_fixed, function(x) rep(x, InputCollect$cores)))
  } else {
    dt_hyperFixed <- as.data.table(matrix(hyper_bound_list_fixed, nrow = 1))
    names(dt_hyperFixed) <- hyper_bound_list_fixed_name
  }
  
  ################################################
  #### Setup environment
  
  if (is.null(InputCollect$dt_mod)) {stop("Run InputCollect$dt_mod <- robyn_engineering() first to get the dt_mod")}
  
  ## get environment for parallel backend
  dt_mod <- copy(InputCollect$dt_mod)
  xDecompAggPrev <- InputCollect$xDecompAggPrev
  rollingWindowStartWhich <- InputCollect$rollingWindowStartWhich
  rollingWindowEndWhich <- InputCollect$rollingWindowEndWhich
  refreshAddedStart <- InputCollect$refreshAddedStart
  dt_modRollWind <- copy(InputCollect$dt_modRollWind)
  refresh_steps <- InputCollect$refresh_steps
  rollingWindowLength <- InputCollect$rollingWindowLength
  
  paid_media_vars <- InputCollect$paid_media_vars
  paid_media_spends <- InputCollect$paid_media_spends
  organic_vars <- InputCollect$organic_vars
  context_vars <- InputCollect$context_vars
  prophet_vars <- InputCollect$prophet_vars
  adstock <- InputCollect$adstock
  context_signs <- InputCollect$context_signs
  paid_media_signs <- InputCollect$paid_media_signs
  prophet_signs <- InputCollect$prophet_signs
  organic_signs <- InputCollect$organic_signs
  all_media <- InputCollect$all_media
  #factor_vars <- InputCollect$factor_vars
  calibration_input <- InputCollect$calibration_input
  optimizer_name <- InputCollect$nevergrad_algo
  cores <- InputCollect$cores
  
  ng <- reticulate::py_suppress_warnings(reticulate::import("nevergrad"))
  
  ################################################
  #### Get spend share
  
  dt_inputTrain <- InputCollect$dt_input[rollingWindowStartWhich:rollingWindowEndWhich]
  dt_spendShare <- dt_inputTrain[, .(rn = paid_media_vars,
                                     total_spend = sapply(.SD, sum),
                                     mean_spend = sapply(.SD, function(x) ifelse(is.na(mean(x[x>0])),0, mean(x[x>0])))), .SDcols=paid_media_spends]
  dt_spendShare[, ':='(spend_share = total_spend / sum(total_spend))]
  
  refreshAddedStartWhich <- which(dt_modRollWind$ds==refreshAddedStart)
  dt_spendShareRF <- dt_inputTrain[refreshAddedStartWhich:rollingWindowLength, 
                                   .(rn = paid_media_vars,
                                     total_spend = sapply(.SD, sum),
                                     mean_spend = sapply(.SD, function(x) ifelse(is.na(mean(x[x>0])),0, mean(x[x>0])))) 
                                   ,.SDcols=paid_media_spends]
  dt_spendShareRF[, ':='(spend_share = total_spend / sum(total_spend))]
  dt_spendShare[, ':='(total_spend_refresh = dt_spendShareRF$total_spend
                       ,mean_spend_refresh = dt_spendShareRF$mean_spend
                       ,spend_share_refresh = dt_spendShareRF$spend_share)]
  
  
  ################################################
  #### Start Nevergrad loop
  
  t0 <- Sys.time()
  
  ## set iterations
  
  if (hyper_fixed == FALSE) {
    iterTotal <- iterations
    iterPar <- cores
  } else {
    iterTotal <- 1
    iterPar <- 1
  }
  
  iterNG <-  ifelse(hyper_fixed == FALSE, ceiling(iterations/cores), 1)
  
  # cat("\nRunning", iterTotal,"iterations with evolutionary algorithm on",adstock, "adstocking,", length(hyper_bound_list_updated),"hyperparameters,",lambda.n,"-fold ridge x-validation using", cores,"cores...\n")
  
  ## start Nevergrad optimiser
  
  if (length(hyper_bound_list_updated) !=0) {
    my_tuple <- tuple(hyper_count)
    instrumentation <- ng$p$Array(shape=my_tuple, lower=0., upper=1.)
    #instrumentation$set_bounds(0., 1.)
    optimizer <-  ng$optimizers$registry[optimizer_name](instrumentation, budget=iterTotal, num_workers=cores)
    if (is.null(calibration_input)) {
      optimizer$tell(ng$p$MultiobjectiveReference(), tuple(1.0, 1.0))
    } else {
      optimizer$tell(ng$p$MultiobjectiveReference(), tuple(1.0, 1.0, 1.0))
    }
    # Creating a hyperparameter vector to be used in the next learning.
  }
  
  ## start loop
  
  resultCollectNG <- list()
  cnt <- 0
  cat('\n',"Nevergrad algorithm: ", optimizer_name,'\n')
  if(hyper_fixed==FALSE) {pb <- txtProgressBar(max = iterTotal, style = 3)}
  assign("InputCollect", InputCollect, envir = .GlobalEnv) # adding this to enable InputCollect reading during parallel
  #opts <- list(progress = function(n) setTxtProgressBar(pb, n))
  sysTimeDopar <- system.time({
    for (lng in 1:iterNG) {
      
      nevergrad_hp <- list()
      nevergrad_hp_val <- list()
      hypParamSamList <- list()
      hypParamSamNG <- c()
      
      if (hyper_fixed == FALSE) {
        for (co in 1:iterPar) {
          
          ## get hyperparameter sample with ask
          nevergrad_hp[[co]] <- optimizer$ask()
          nevergrad_hp_val[[co]] <- nevergrad_hp[[co]]$value
          
          ## scale sample to given bounds
          for (hypNameLoop in hyper_bound_list_updated_name) { # hypNameLoop <- local_name.all[1]
            index <- which(hypNameLoop == hyper_bound_list_updated_name)
            channelBound <- unlist(hyper_bound_list_updated[hypNameLoop])
            hyppar_for_qunif <- nevergrad_hp_val[[co]][index]  
            hyppar_scaled <- qunif(hyppar_for_qunif, min(channelBound), max(channelBound))  
            hypParamSamNG[hypNameLoop] <- hyppar_scaled 
          }
          hypParamSamList[[co]] <- transpose(data.table(hypParamSamNG))
        }
        
        hypParamSamNG<- rbindlist(hypParamSamList)
        hypParamSamNG <- setnames(hypParamSamNG, names(hypParamSamNG), hyper_bound_list_updated_name)
        
        ## add fixed hyperparameters
        
        if (hyper_count_fixed != 0) {
          hypParamSamNG <- cbind(hypParamSamNG, dt_hyperFixed)
          hypParamSamNG <- setcolorder(hypParamSamNG, hypParamSamName)
        }
      } else {
        hypParamSamNG <- as.data.table(matrix(unlist(hyper_bound_list), nrow = 1))
        setnames(hypParamSamNG, names(hypParamSamNG), hypParamSamName)
      }
      
      ## Parallel start
      
      nrmse.collect <- c()
      decomp.rssd.collect <- c()
      best_mape <- Inf
      # registerDoParallel(cores)  #registerDoParallel(cores=InputCollect$cores)

      registerDoFuture()
      if (.Platform$OS.type == "unix") {
        plan(multicore(workers = InputCollect$cores))
      } else {
        plan(sequential)
      }
      
      # nbrOfWorkers()
      
      getDoParWorkers()
      doparCollect <- foreach (
        i = 1:iterPar
        , .export = c('adstock_geometric'
                      , 'adstock_weibull'
                      , 'saturation_hill'
                      , 'get_rsq'
                      , 'model_decomp'
                      , 'calibrate_mmm'
                      #, 'ridge_lambda'
                      , 'model_refit')
        , .packages = c('glmnet'
                        ,'stringr'
                        ,'data.table'
        )
        #, .options.snow = opts
      )  %dorng%  {
        
        t1 <- Sys.time()
        
        #####################################
        #### Get hyperparameter sample
        
        hypParamSam <- unlist(hypParamSamNG[i])
        
        #### Tranform media with hyperparameters
        dt_modAdstocked <- dt_mod[, .SD, .SDcols = setdiff(names(dt_mod), "ds")]
        mediaAdstocked <- list()
        mediaVecCum <- list()
        mediaSaturated <- list()
        for (v in 1:length(all_media)) {
          
          m <- dt_modAdstocked[, get(all_media[v])]
          
          ## adstocking
          
          if (adstock == "geometric") {
            
            theta = hypParamSam[paste0(all_media[v],"_thetas")]
            x_list <- adstock_geometric(x=m, theta = theta)
            
          } else if (adstock == "weibull") {
            
            shape = hypParamSam[paste0(all_media[v],"_shapes")]
            scale = hypParamSam[paste0(all_media[v],"_scales")]
            x_list <- adstock_weibull(x=m, shape = shape, scale=scale)
            
          } else {break; print("adstock parameter must be geometric or weibull")}
          
          m_adstocked <- x_list$x_decayed
          mediaAdstocked[[v]] <- m_adstocked
          mediaVecCum[[v]] <- x_list$thetaVecCum
          
          ## saturation
          m_adstockedRollWind <- m_adstocked[rollingWindowStartWhich:rollingWindowEndWhich]
          
          alpha = hypParamSam[paste0(all_media[v],"_alphas")]
          gamma = hypParamSam[paste0(all_media[v],"_gammas")]
          mediaSaturated[[v]] <- saturation_hill(m_adstockedRollWind, alpha = alpha, gamma = gamma)
        }
        
        names(mediaAdstocked) <- all_media
        dt_modAdstocked[, (all_media) := mediaAdstocked]
        dt_mediaVecCum <- data.table()[, (all_media):= mediaVecCum]
        
        names(mediaSaturated) <- all_media
        dt_modSaturated <- dt_modAdstocked[rollingWindowStartWhich:rollingWindowEndWhich]
        dt_modSaturated[, (all_media) := mediaSaturated]
        
        #####################################
        #### Split and prepare data for modelling
        
        #trainSize <- round(nrow(dt_modSaturated)* set_modTrainSize)
        #dt_train <- dt_modSaturated[1:trainSize]
        #dt_test <- dt_modSaturated[(trainSize+1):nrow(dt_modSaturated)]
        #trainStartWhich <- which.min(abs(difftime(as.Date(dt_mod$ds), as.Date(InputCollect$window_start), units = "days")))
        dt_train <- copy(dt_modSaturated)
        
        ## contrast matrix because glmnet does not treat categorical variables
        y_train <- dt_train$dep_var
        x_train <- model.matrix(dep_var ~., dt_train)[, -1]
        #y_test <- dt_test$dep_var
        #x_test <- model.matrix(dep_var ~., dt_test)[, -1]
        #y <- c(y_train, y_test)
        #x <- rbind(x_train, x_test)
        
        ## create lambda sequence with x and y
        # lambda_seq <- ridge_lambda(x=x_train, y=y_train, seq_len = lambda.n, lambda_min_ratio = 0.0001)
        
        ## define sign control
        dt_sign <- dt_modSaturated[, !"dep_var"] #names(dt_sign)
        #x_sign <- if (activate_prophet) {c(prophet_signs, context_signs, paid_media_signs)} else {c(context_signs, paid_media_signs)}
        x_sign <- c(prophet_signs, context_signs, paid_media_signs, organic_signs)
        names(x_sign) <- c(prophet_vars, context_vars, paid_media_vars, organic_vars)
        check_factor <- sapply(dt_sign, is.factor)
        
        lower.limits <- c(); upper.limits <- c()
        
        for (s in 1:length(check_factor)) {
          
          if (check_factor[s]==TRUE) {
            level.n <- length(levels(unlist(dt_sign[, s, with=FALSE])))
            if (level.n <=1) {stop("factor variables must have more than 1 level")}
            lower_vec <- if(x_sign[s] == "positive") {rep(0, level.n-1)} else {rep(-Inf, level.n-1)}
            upper_vec <- if(x_sign[s] == "negative") {rep(0, level.n-1)} else {rep(Inf, level.n-1)}
            lower.limits <- c(lower.limits, lower_vec)
            upper.limits <- c(upper.limits, upper_vec)
          } else {
            lower.limits <- c(lower.limits, ifelse(x_sign[s] == "positive", 0, -Inf))
            upper.limits <- c(upper.limits ,ifelse(x_sign[s] == "negative", 0, Inf))
          }
        }
        
        #####################################
        #### fit ridge regression with x-validation
        cvmod <- cv.glmnet(x_train
                           ,y_train
                           ,family = "gaussian"
                           ,alpha = 0 #0 for ridge regression
                           #,lambda = lambda_seq
                           ,lower.limits = lower.limits
                           ,upper.limits = upper.limits
                           ,type.measure = "mse"
                           #,penalty.factor = c(1,1,1,1,1,1,1,1,1)
                           #,nlambda = 100
                           #,intercept = FALSE
        ) # plot(cvmod) coef(cvmod)
        # head(predict(cvmod, newx=x_train, s="lambda.1se"))
        # cbind(coef(cvmod1, s = "lambda.min"), coef(cvmod2, s = "lambda.min"), coef(cvmod3, s = "lambda.min"), coef(cvmod4, s = "lambda.min"))
        
        #####################################
        #### refit ridge regression with selected lambda from x-validation
        
        
        ## if no lift calibration, refit using best lambda
        if (hyper_fixed == FALSE) {
          mod_out <- model_refit(x_train, y_train, lambda=cvmod$lambda.1se, lower.limits, upper.limits)
          lambda <- cvmod$lambda.1se
        } else {
          mod_out <- model_refit(x_train, y_train, lambda=lambda_fixed[i], lower.limits, upper.limits)
          lambda <- lambda_fixed[i]
        }
        
        #hypParamSam["lambdas"] <- cvmod$lambda.1se
        #hypParamSamName <- names(hypParamSam)
        
        decompCollect <- model_decomp(coefs=mod_out$coefs, dt_modSaturated=dt_modSaturated, x=x_train, y_pred=mod_out$y_pred, i=i, dt_modRollWind=dt_modRollWind, refreshAddedStart = refreshAddedStart)
        nrmse <- mod_out$nrmse_train
        mape <- 0
        df.int <- mod_out$df.int
        
        
        #####################################
        #### get calibration mape
        
        if (!is.null(calibration_input)) {
          
          liftCollect <- calibrate_mmm(decompCollect=decompCollect, calibration_input=calibration_input)
          mape <- liftCollect[, mean(mape_lift)]
          
        }
        
        #####################################
        #### calculate multi-objectives for pareto optimality
        
        ## decomp objective: sum of squared distance between decomp share and spend share to be minimised
        dt_decompSpendDist <- decompCollect$xDecompAgg[rn %in% paid_media_vars, .(rn, xDecompPerc, xDecompMeanNon0Perc, xDecompMeanNon0, xDecompPercRF, xDecompMeanNon0PercRF, xDecompMeanNon0RF)]
        dt_decompSpendDist <- dt_decompSpendDist[dt_spendShare[, .(rn, spend_share, spend_share_refresh, mean_spend, total_spend)], on = "rn"]
        dt_decompSpendDist[, ':='(effect_share = xDecompPerc/sum(xDecompPerc)
                                  ,effect_share_refresh = xDecompPercRF/sum(xDecompPercRF))]
        decompCollect$xDecompAgg[dt_decompSpendDist[, .(rn, spend_share_refresh, effect_share_refresh)], 
                                 ':='(spend_share_refresh = i.spend_share_refresh
                                      ,effect_share_refresh = i.effect_share_refresh), on = "rn"]
        
        if (!refresh) {
          decomp.rssd <- dt_decompSpendDist[, sqrt(sum((effect_share-spend_share)^2))]
        } else {
          dt_decompRF <- decompCollect$xDecompAgg[, .(rn, decomp_perc = xDecompPerc)][xDecompAggPrev[, .(rn, decomp_perc_prev=xDecompPerc)], on = "rn"]
          decomp.rssd.nonmedia <- dt_decompRF[!(rn %in% paid_media_vars), sqrt(mean((decomp_perc - decomp_perc_prev)^2))]
          decomp.rssd.media <- dt_decompSpendDist[, sqrt(mean((effect_share_refresh-spend_share_refresh)^2))]
          decomp.rssd <- decomp.rssd.media + decomp.rssd.nonmedia / (1- refresh_steps / rollingWindowLength)
        }
        
        if (is.nan(decomp.rssd)) {
          message("all media in this iteration have 0 coefficients")
          decomp.rssd <- Inf
          dt_decompSpendDist[, effect_share:=0]
        }
        
        ## adstock objective: sum of squared infinite sum of decay to be minimised - deprecated
        # dt_decaySum <- dt_mediaVecCum[,  .(rn = all_media, decaySum = sapply(.SD, sum)), .SDcols = all_media]
        # adstock.ssisd <- dt_decaySum[, sum(decaySum^2)]
        
        ## calibration objective: not calibration: mse, decomp.rssd, if calibration: mse, decom.rssd, mape_lift
        
        #####################################
        #### Collect output
        
        resultHypParam <- data.table()[, (hypParamSamName):= lapply(hypParamSam[1:length(hypParamSamName)], function(x) x)]
        
        resultCollect <- list(
          resultHypParam = resultHypParam[, ':='(mape = mape
                                                 ,nrmse = nrmse
                                                 ,decomp.rssd = decomp.rssd
                                                 #,adstock.ssisd = adstock.ssisd
                                                 ,rsq_train = mod_out$rsq_train
                                                 #,rsq_test = mod_out$rsq_test
                                                 ,pos = prod(decompCollect$xDecompAgg$pos)
                                                 ,lambda=lambda
                                                 #,Score = -mape
                                                 ,Elapsed = as.numeric(difftime(Sys.time(),t1, units = "secs"))
                                                 ,ElapsedAccum = as.numeric(difftime(Sys.time(),t0, units = "secs"))
                                                 ,iterPar= i
                                                 ,iterNG = lng
                                                 ,df.int = df.int)],
          xDecompVec = if (hyper_fixed == TRUE) {decompCollect$xDecompVec[, ':='(intercept = decompCollect$xDecompAgg[rn=="(Intercept)", xDecompAgg]
                                                                                 ,mape = mape
                                                                                 ,nrmse = nrmse
                                                                                 ,decomp.rssd = decomp.rssd
                                                                                 #,adstock.ssisd = adstock.ssisd
                                                                                 ,rsq_train = mod_out$rsq_train
                                                                                 #,rsq_test = mod_out$rsq_test
                                                                                 ,lambda=lambda
                                                                                 ,iterPar= i
                                                                                 ,iterNG = lng
                                                                                 ,df.int = df.int)]} else{NULL} ,
          xDecompAgg = decompCollect$xDecompAgg[, ':='(mape = mape
                                                       ,nrmse = nrmse
                                                       ,decomp.rssd = decomp.rssd
                                                       #,adstock.ssisd = adstock.ssisd
                                                       ,rsq_train = mod_out$rsq_train
                                                       #,rsq_test = mod_out$rsq_test
                                                       ,lambda=lambda
                                                       ,iterPar= i
                                                       ,iterNG = lng
                                                       ,df.int = df.int)] ,
          liftCalibration = if (!is.null(calibration_input)) {liftCollect[, ':='(mape = mape
                                                                               ,nrmse = nrmse
                                                                               ,decomp.rssd = decomp.rssd
                                                                               #,adstock.ssisd = adstock.ssisd
                                                                               ,rsq_train = mod_out$rsq_train
                                                                               #,rsq_test = mod_out$rsq_test
                                                                               ,lambda=lambda
                                                                               ,iterPar= i
                                                                               ,iterNG = lng)] } else {NULL},
          decompSpendDist = dt_decompSpendDist[, ':='(mape = mape
                                                      ,nrmse = nrmse
                                                      ,decomp.rssd = decomp.rssd
                                                      #,adstock.ssisd = adstock.ssisd
                                                      ,rsq_train = mod_out$rsq_train
                                                      #,rsq_test = mod_out$rsq_test
                                                      ,lambda=lambda
                                                      ,iterPar= i
                                                      ,iterNG = lng
                                                      ,df.int = df.int)],
          mape.lift = mape,
          nrmse = nrmse,
          decomp.rssd = decomp.rssd,
          iterPar = i,
          iterNG = lng,
          df.int = df.int
          #,cvmod = cvmod
        )
        
        best_mape <- min(best_mape, mape)
        if (cnt == iterTotal) {
          print(" === ")
          print(paste0("Optimizer_name: ",optimizer_name, ";  Total_iterations: ", cnt, ";   best_mape: ",best_mape))
        }
        return(resultCollect)
      } # end dopar
      ## end parallel
      
      # stopImplicitCluster()
      
      nrmse.collect <- sapply(doparCollect, function(x) x$nrmse)
      decomp.rssd.collect <- sapply(doparCollect, function(x) x$decomp.rssd)
      mape.lift.collect <- sapply(doparCollect, function(x) x$mape.lift)
      
      
      #####################################
      #### Nevergrad tells objectives
      
      if (hyper_fixed == FALSE) {
        if (is.null(calibration_input)) {
          for (co in 1:iterPar) {
            optimizer$tell(nevergrad_hp[[co]], tuple(nrmse.collect[co], decomp.rssd.collect[co])) 
          }
        } else {
          for (co in 1:iterPar) {
            optimizer$tell(nevergrad_hp[[co]], tuple(nrmse.collect[co], decomp.rssd.collect[co], mape.lift.collect[co])) 
          }
        }
      }
      
      resultCollectNG[[lng]] <- doparCollect
      cnt <- cnt + iterPar
      if(hyper_fixed==FALSE) {setTxtProgressBar(pb, cnt)}
      
    } ## end NG loop
  }) # end system.time
  
  cat("\n Finished in",sysTimeDopar[3]/60,"mins\n")
  if(hyper_fixed==FALSE) {close(pb)}
  registerDoSEQ(); getDoParWorkers()
  
  #####################################
  #### Get nevergrad pareto results 
  
  # if (hyper_fixed == FALSE) {
  #   pareto_results <- data.table::transpose(rbind(as.data.table(sapply(optimizer$pareto_front(997, subset="domain-covering", subset_tentatives=500), function(p) round(p$value[],4))),
  #                                                 as.data.table(sapply(optimizer$pareto_front(997, subset="domain-covering", subset_tentatives=500), function(p) round(p$losses[],4)))))
  #   if (is.null(calibration_input)) {
  #     pareto_results_names<-setnames(pareto_results, old=names(pareto_results), new=c(hyper_bound_list_updated_name,"nrmse", "decomp.rssd") )
  #     pareto_results_ordered<-setorder(pareto_results_names, "nrmse", "decomp.rssd")
  #   } else {
  #     pareto_results_names<-setnames(pareto_results, old=names(pareto_results), new=c(hyper_bound_list_updated_name,"nrmse", "decomp.rssd", "mape.lift") )
  #     pareto_results_ordered<-setorder(pareto_results_names, "nrmse", "decomp.rssd", "mape.lift")
  #   }
  #   #print(pareto_results_ordered)
  # } else {
  #   pareto_results_ordered <- NULL
  # }
  
  #####################################
  #### Final result collect
  
  resultCollect <- list(
    resultHypParam = rbindlist(lapply(resultCollectNG, function(x) {rbindlist(lapply(x, function(y) y$resultHypParam))}))[order(nrmse)]
    ,xDecompVec = if (hyper_fixed==TRUE) {rbindlist(lapply(resultCollectNG, function(x) {rbindlist(lapply(x, function(y) y$xDecompVec))}))[order(nrmse, ds)]} else {NULL}
    ,xDecompAgg =   rbindlist(lapply(resultCollectNG, function(x) {rbindlist(lapply(x, function(y) y$xDecompAgg))}))[order(nrmse)]
    ,liftCalibration = if(!is.null(calibration_input)) {rbindlist(lapply(resultCollectNG, function(x) {rbindlist(lapply(x, function(y) y$liftCalibration))}))[order(mape, liftMedia, liftStart)]} else {NULL}
    ,decompSpendDist = rbindlist(lapply(resultCollectNG, function(x) {rbindlist(lapply(x, function(y) y$decompSpendDist))}))[order(nrmse)]
    # ,mape = unlist(lapply(doparCollect, function(x) x$mape))
    # ,iterRS = unlist(lapply(doparCollect, function(x) x$iterRS))
    # ,paretoFront= as.data.table(pareto_results_ordered)
    # ,cvmod = lapply(doparCollect, function(x) x$cvmod)
  )
  resultCollect$iter <- length(resultCollect$mape)
  # resultCollect$best.iter <- resultCollect$resultHypParam$iterRS[1]
  resultCollect$elapsed.min <- sysTimeDopar[3]/60
  resultCollect$resultHypParam[, ElapsedAccum:= ElapsedAccum - min(ElapsedAccum) + resultCollect$resultHypParam[which.min(ElapsedAccum), Elapsed]] # adjust accummulated time
  resultCollect$resultHypParam
  # print(optimizer_name)
  # print(" get ")
  # please_stop_here()
  
  return(list(#Score =  -resultCollect$mape[iterRS], # score for BO
    resultCollect = resultCollect
    ,hyperBoundNG = hyper_bound_list_updated
    ,hyperBoundFixed = hyper_bound_list_fixed))
}



#####################################
#### Define robyn_run, the main trial looping and plotting function

robyn_run <- function(InputCollect
                      ,plot_folder = getwd() 
                      ,dt_hyper_fixed = NULL
                      ,pareto_fronts = 1
                      ,plot_pareto = TRUE
                      ,refresh = FALSE) {
  
  #####################################
  #### Set local environment
  
  if (is.null(InputCollect$dt_mod)) {stop("\nmust provide hyperparameters in  robyn_inputs() first\n")}
  
  t0 <- Sys.time()
  
  # check path 
  if (tolower(substr(plot_folder, start = nchar(plot_folder)-5, stop = nchar(plot_folder)))==".rdata") {
    plot_folder <- substr(plot_folder, start = 1, stop = max(gregexpr("/|\\\\", plot_folder)[[1]])-1)
  } else if (grepl("^/$|^\\\\$", substr(plot_folder, start=nchar(plot_folder), stop=nchar(plot_folder)))) {
    plot_folder <- substr(plot_folder, start = 1, stop = max(gregexpr("/|\\\\", plot_folder)[[1]])-1)
  } 
  
  if (!dir.exists(plot_folder)) {
    plot_folder <- getwd()
    message("provided plot_folder doesn't exist. Using default plot_folder = getwd(): ", getwd())
  }
  
  dt_mod <- copy(InputCollect$dt_mod)
  dt_modRollWind <- copy(InputCollect$dt_modRollWind)
  
  message("Input data has ", nrow(dt_mod), " ", InputCollect$intervalType, "s in total: "
          , dt_mod[, min(ds)], " to ", dt_mod[, max(ds)])
  message(ifelse(!refresh, "Initial", "Refresh")," model is built on rolling window of ", InputCollect$rollingWindowLength, " ", InputCollect$intervalType,"s: "
          , InputCollect$window_start, " to ", InputCollect$window_end)
  if (refresh) {message("Rolling window moving forward: ", InputCollect$refresh_steps, " ", InputCollect$intervalType)}
  
  
  #####################################
  #### Run robyn_mmm on set_trials
  
  hyper_fixed <- all(sapply(InputCollect$hyperparameters, length)==1)
  if (hyper_fixed & is.null(dt_hyper_fixed)) {
    stop("hyperparameters can't be all fixed for hyperparameter optimisation. If you want to get old model result, pleaseprovide only 1 model / 1 row from OutputCollect$resultHypParam or pareto_hyperparameters.csv from previous runs")
  }
  hypParamSamName <- hyper_names(adstock = InputCollect$adstock, all_media=InputCollect$all_media)
  
  if (!is.null(dt_hyper_fixed)) {
    
    ## Run robyn_mmm if using old model result tables
    dt_hyper_fixed <- as.data.table(dt_hyper_fixed)
    if (nrow(dt_hyper_fixed) != 1) {stop("Provide only 1 model / 1 row from OutputCollect$resultHypParam or pareto_hyperparameters.csv from previous runs")}
    if (!all(c(hypParamSamName, "lambda") %in% names(dt_hyper_fixed))) {stop("dt_hyper_fixed is provided with wrong input. please provide the table OutputCollect$resultHypParam from previous runs or pareto_hyperparameters.csv with desired model ID")}
    
    hyper_fixed <- TRUE
    hyperparameters_fixed <- lapply(dt_hyper_fixed[, hypParamSamName, with = FALSE], unlist)
    
    model_output_collect <- list()
    model_output_collect[[1]] <- robyn_mmm(hyper_collect = hyperparameters_fixed
                                           ,InputCollect = InputCollect
                                           #,iterations = iterations
                                           #,cores = cores
                                           ,optimizer_name = InputCollect$nevergrad_algo
                                           ,lambda_fixed = dt_hyper_fixed$lambda)
    
    model_output_collect[[1]]$trial <- 1
    model_output_collect[[1]]$resultCollect$resultHypParam <- model_output_collect[[1]]$resultCollect$resultHypParam[order(iterPar)]
    
    dt_IDs <- data.table(solID = dt_hyper_fixed$solID, 
                         iterPar = model_output_collect[[1]]$resultCollect$resultHypParam$iterPar)
    
    model_output_collect[[1]]$resultCollect$resultHypParam[dt_IDs, on =.(iterPar), "solID" := .(i.solID)]
    model_output_collect[[1]]$resultCollect$xDecompAgg[dt_IDs, on =.(iterPar), "solID" := .(i.solID)]
    model_output_collect[[1]]$resultCollect$xDecompVec[dt_IDs, on =.(iterPar), "solID" := .(i.solID)]
    model_output_collect[[1]]$resultCollect$decompSpendDist[dt_IDs, on =.(iterPar), "solID" := .(i.solID)]
    
  } else {
    
    ## Run robyn_mmm on set_trials if hyperparameters are not all fixed
    
    t0 <- Sys.time()
    
    # ng_collect <- list()
    cat("\nUsing",InputCollect$adstock, "adstocking with", length(InputCollect$hyperparameters),"hyperparameters & 100-fold ridge x-validation on", InputCollect$cores,"cores...\n")
    model_output_collect <- list()
    
    for (ngt in 1:InputCollect$trials) { 
      
      # if (is.null(InputCollect$calibration_input)) {
      #   cat("\nRunning trial nr.", ngt,"out of",InputCollect$trials,"...\n")
      # } else {
      cat("\nRunning trial nr.", ngt,"out of",InputCollect$trials,"with",InputCollect$iterations, "iterations per trial", ifelse(is.null(InputCollect$calibration_input), "...\n","with calibration...\n"))
      #}

      model_output <- robyn_mmm(hyper_collect = InputCollect$hyperparameters
                                ,InputCollect = InputCollect
                                ,refresh = refresh)
      
      check_coef0 <- any(model_output$resultCollect$decompSpendDist$decomp.rssd == Inf)
      if (check_coef0) {
        num_coef0_mod <- model_output$resultCollect$decompSpendDist[decomp.rssd == Inf, uniqueN(paste0(iterNG,"_",iterPar))]
        num_coef0_mod <- ifelse(num_coef0_mod>InputCollect$iterations, InputCollect$iterations, num_coef0_mod)
        message("\nThis trial contains ", num_coef0_mod," iterations with all 0 media coefficient. Please reconsider your media variable choice if the pareto choices are unreasonable.
                  \nRecommendations are: \n1. increase hyperparameter ranges for 0-coef channels on theta (max.reco. c(0, 0.9) ) and gamma (max.reco. c(0.1, 1) ) to give Robyn more freedom\n2. split media into sub-channels, and/or aggregate similar channels, and/or introduce other media\n3. increase trials to get more samples\n")
      }
      
      model_output["trial"] <- ngt
      model_output_collect[[ngt]] <- model_output
    }
    # ng_collect <- rbindlist(ng_collect)
    # px <- low(ng_collect$nrmse) * low(ng_collect$decomp.rssd)
    # ng_collect <- psel(ng_collect, px, top = nrow(ng_collect))[order(trial, nrmse)]
    # ng_out[[which(ng_algos==optmz)]] <- ng_collect
    # }
    # ng_out <- rbindlist(ng_out)
    # setnames(ng_out, ".level", "manual_pareto")
  }
  
  #####################################
  #### Collect results for plotting
  
  ## collect hyperparameter results
  resultHypParam <- rbindlist(lapply(model_output_collect, function (x) x$resultCollect$resultHypParam[, trial:= x$trial]))
  resultHypParam[, iterations:=(iterNG-1)*InputCollect$cores + iterPar]
  xDecompAgg <- rbindlist(lapply(model_output_collect, function (x) x$resultCollect$xDecompAgg[, trial:= x$trial]))
  xDecompAgg[, iterations:=(iterNG-1)*InputCollect$cores + iterPar]
  
  #if (hyper_fixed == FALSE) {
  resultHypParam[, solID:= (paste(trial,iterNG, iterPar, sep = "_"))]
  xDecompAgg[, solID:= (paste(trial,iterNG, iterPar, sep = "_"))]
  #}
  xDecompAggCoef0 <- xDecompAgg[rn %in% InputCollect$paid_media_vars, .(coef0=min(coef)==0), by = "solID"]
  
  if (!hyper_fixed) {
    mape_lift_quantile10 <- quantile(resultHypParam$mape, probs = 0.10)
    nrmse_quantile90 <- quantile(resultHypParam$nrmse, probs = 0.90)
    decomprssd_quantile90 <- quantile(resultHypParam$decomp.rssd, probs = 0.90)
    resultHypParam <- resultHypParam[xDecompAggCoef0, on = "solID"]
    resultHypParam[, mape.qt10:= mape <= mape_lift_quantile10 & nrmse <= nrmse_quantile90 & decomp.rssd <= decomprssd_quantile90]
    
    
    resultHypParamPareto <- resultHypParam[mape.qt10==TRUE]
    px <- low(resultHypParamPareto$nrmse) * low(resultHypParamPareto$decomp.rssd)
    resultHypParamPareto <- psel(resultHypParamPareto, px, top = nrow(resultHypParamPareto))[order(iterNG, iterPar, nrmse)]
    setnames(resultHypParamPareto, ".level", "robynPareto")
    
    setkey(resultHypParam,solID)
    setkey(resultHypParamPareto,solID)
    resultHypParam <- merge(resultHypParam,resultHypParamPareto[, .(solID, robynPareto)], all.x=TRUE)
    
  } else {
    resultHypParam[, ':='(mape.qt10 = TRUE, robynPareto =1, coef0=NA)]
  }
  
  xDecompAgg <- xDecompAgg[resultHypParam, robynPareto := i.robynPareto, on = c("iterNG", "iterPar", "trial")]
  
  decompSpendDist <- rbindlist(lapply(model_output_collect, function (x) x$resultCollect$decompSpendDist[, trial:= x$trial]))
  decompSpendDist <- decompSpendDist[resultHypParam, robynPareto := i.robynPareto, on = c("iterNG", "iterPar", "trial")]
  if (hyper_fixed == FALSE) {
    decompSpendDist[, solID:= (paste(trial,iterNG, iterPar, sep = "_"))]
  } else {
    xDecompAgg[, solID:=unique(decompSpendDist$solID)]
    resultHypParam[, solID:=unique(decompSpendDist$solID)]
  }
  decompSpendDist <- decompSpendDist[xDecompAgg[rn %in% InputCollect$paid_media_vars, .(rn, xDecompAgg, solID)], on = c("rn", "solID")]
  resp_collect <- c()
  for (n in 1:length(decompSpendDist$rn)) {
    resp_collect[n] <- robyn_response(paid_media_var = decompSpendDist$rn[n]
                                      ,select_model = decompSpendDist[n, solID]
                                      ,Spend = decompSpendDist[n, mean_spend]
                                      ,dt_hyppar = resultHypParam
                                      ,dt_coef = xDecompAgg
                                      ,InputCollect = InputCollect)
  }
  decompSpendDist[, mean_response:=resp_collect]
  decompSpendDist[, ':='(roi_mean = mean_response/mean_spend
                         ,roi_total = xDecompAgg/total_spend
                         ,cpa_mean = if(InputCollect$dep_var_type=="conversion") {mean_spend/mean_response} else {NA}
                         ,cpa_total = if(InputCollect$dep_var_type=="conversion") {total_spend/xDecompAgg} else {NA})]
  #decompSpendDist[, roi := xDecompMeanNon0/mean_spend]
  
  setkey(xDecompAgg,solID, rn)
  setkey(decompSpendDist,solID, rn)
  xDecompAgg <- merge(xDecompAgg,decompSpendDist[, .(rn, solID, total_spend, mean_spend, spend_share, effect_share, roi_mean, roi_total, cpa_total)], all.x=TRUE)
  
  
  #####################################
  #### Plot overview
  
  ## set folder to save plat
  if (!exists("plot_folder_sub")) {
    folder_var <- ifelse(!refresh, "init", paste0("rf",InputCollect$refreshCounter))
    plot_folder_sub <- paste0(format(Sys.time(), "%Y-%m-%d %H.%M"), " ", folder_var)
    plotPath <- dir.create(file.path(plot_folder, plot_folder_sub))
  }
  
  #pareto_fronts_vec <- ifelse(!hyper_fixed, c(1,2,3), 1)
  if (!hyper_fixed ) {
    pareto_fronts_vec <- 1:pareto_fronts
    num_pareto123 <- resultHypParam[robynPareto %in% pareto_fronts_vec, .N]
  } else {
    pareto_fronts_vec <- 1
    num_pareto123 <- nrow(resultHypParam)
  }
  
  
  cat("\nPlotting summary charts in to folder",paste0(plot_folder, "/", plot_folder_sub,"/"),"...\n")
  
  if (!hyper_fixed) {
    
    ## plot prophet
    
    if (!is.null(InputCollect$prophet_vars)) {
      # pProphet <- prophet_plot_components(InputCollect$modelRecurrance, InputCollect$forecastRecurrance, render_plot = TRUE)
      
      dt_plotProphet <- InputCollect$dt_mod[, c('ds','dep_var', InputCollect$prophet_vars, InputCollect$factor_vars), with = FALSE]
      dt_plotProphet <- melt.data.table(dt_plotProphet, id.vars = 'ds')
      pProphet <- ggplot(dt_plotProphet, aes(x=ds, y=value)) + 
        geom_line(color='steelblue')+ 
        facet_wrap(~ variable, scales="free", ncol = 1) +
        labs(title = 'Prophet decomposition') + xlab(NULL) + ylab(NULL)
      print(pProphet)
      ggsave(paste0(plot_folder, "/", plot_folder_sub,"/", "prophet_decomp.png")
             , plot = pProphet
             , dpi = 600, width = 12, height = 3*length(levels(dt_plotProphet$variable)))
      
    }
    
    
    ## plot spend exposure model
    
    if(any(InputCollect$costSelector)) {
      pSpendExposure <- arrangeGrob(grobs = InputCollect$plotNLSCollect
                                    ,ncol= ifelse(length(InputCollect$plotNLSCollect)<=3, length(InputCollect$plotNLSCollect), 3)
                                    ,top = "Spend-exposure fitting with Michaelis-Menten model")
      #grid.draw(pSpendExposure)
      ggsave(paste0(plot_folder, "/", plot_folder_sub,"/", "spend_exposure_fitting.png")
             , plot = pSpendExposure
             , dpi = 600, width = 12, height = ceiling(length(InputCollect$plotNLSCollect)/3)*7)
      
    } else {
      message("\nno spend-exposure modelling needed. all media variables used for mmm are spend variables ")
    }
    
    
    ## plot hyperparameter sampling distribution
    
    resultHypParam.melted <- melt.data.table(resultHypParam[, c(InputCollect$local_name,"robynPareto"), with = FALSE], id.vars = c("robynPareto"))
    
    pSamp <- ggplot(data = resultHypParam.melted,  aes( x = value, y=variable, color = variable, fill = variable) ) +
      geom_violin(alpha = .5, size = 0) +
      geom_point(size = 0.2) +
      theme(legend.position = "none") +
      labs(title="Model selection", 
           subtitle=paste0("Hyperparameter pareto sample distribution", ", iterations = ", InputCollect$iterations, " * ", InputCollect$trials, " trial"),
           x="Hyperparameter space",
           y="")
    print(pSamp)
    ggsave(paste0(plot_folder, "/", plot_folder_sub,"/", "hypersampling.png")
           , plot = pSamp
           , dpi = 600, width = 12, height = 7)
    
    
    ## plot Pareto front
    if (!is.null(InputCollect$calibration_input)) {resultHypParam[, iterations:= ifelse(is.na(robynPareto), NA, iterations)]}
    pParFront <- ggplot(data = resultHypParam, aes(x=nrmse, y=decomp.rssd, color = iterations)) +
      geom_point(size = 0.5) +
      #stat_smooth(data = resultHypParam, method = 'gam', formula = y ~ s(x, bs = "cs"), size = 0.2, fill = "grey100", linetype="dashed")+
      geom_line(data = resultHypParam[robynPareto ==1], aes(x=nrmse, y=decomp.rssd), colour = "coral4")+
      #geom_line(data = resultHypParam[robynPareto ==2], aes(x=nrmse, y=decomp.rssd), colour = "coral3")+
      #geom_line(data = resultHypParam[robynPareto ==3], aes(x=nrmse, y=decomp.rssd), colour = "coral")+
      scale_colour_gradient(low = "navyblue", high = "skyblue") +
      labs(title=ifelse(is.null(InputCollect$calibration_input), "Model selection", "Model selection with top 10% calibration"),
           subtitle=paste0("2D Pareto front 1-3 with ",InputCollect$nevergrad_algo,", iterations = ", InputCollect$iterations , " * ", InputCollect$trials, " trial"),
           x="NRMSE",
           y="DECOMP.RSSD")
    
    if (length(pareto_fronts_vec)>1) {
      for (pfs in 2:max(pareto_fronts_vec)) {
        if (pfs ==2) {
          pf_color <- "coral3"} else if (pfs ==3) {pf_color <- "coral2"} else {pf_color <- "coral"}
        pParFront <- pParFront + geom_line(data = resultHypParam[robynPareto == pfs], aes(x=nrmse, y=decomp.rssd), colour = pf_color)
      }
    }
    
    print(pParFront)
    ggsave(paste0(plot_folder, "/", plot_folder_sub,"/", "pareto_front.png")
           , plot = pParFront
           , dpi = 600, width = 12, height = 7)
  }
  
  
  #####################################
  #### Plot each pareto solution
  
  if (plot_pareto) {
    cat("\nPlotting", num_pareto123,"pareto optimum models in to folder",paste0(plot_folder, "/", plot_folder_sub,"/"),"...\n")
    pbplot <- txtProgressBar(max = num_pareto123, style = 3)
  }
  
  cnt <- 0
  mediaVecCollect <- list()
  xDecompVecCollect <- list()
  meanResponseCollect <- list()
  for (pf in pareto_fronts_vec) {
    
    #if (!hyper_fixed) {
    plotMediaShare <- xDecompAgg[robynPareto == pf & rn %in% InputCollect$paid_media_vars]
    plotWaterfall <- xDecompAgg[robynPareto == pf]
    uniqueSol <- plotMediaShare[, unique(solID)]
    # } else {
    #   plotMediaShare <- xDecompAgg[rn %in% InputCollect$paid_media_vars]
    #   plotWaterfall <- copy(xDecompAgg)
    #   uniqueSol <- plotMediaShare[, unique(solID)]
    # }
    
    for (j in 1:length(uniqueSol)) {
      
      cnt <- cnt+1
      
      ## plot spend x effect share comparison
      plotMediaShareLoop <- plotMediaShare[solID == uniqueSol[j]]
      rsq_train_plot <- plotMediaShareLoop[, round(unique(rsq_train),4)]
      nrmse_plot <- plotMediaShareLoop[, round(unique(nrmse),4)]
      decomp_rssd_plot <- plotMediaShareLoop[, round(unique(decomp.rssd),4)]
      mape_lift_plot <- ifelse(!is.null(InputCollect$calibration_input), plotMediaShareLoop[, round(unique(mape),4)], NA)
      
      suppressWarnings(plotMediaShareLoop <- melt.data.table(plotMediaShareLoop, id.vars = c("rn", "nrmse", "decomp.rssd", "rsq_train" ), measure.vars = c("spend_share", "effect_share", "roi_total", "cpa_total")))
      plotMediaShareLoop[, rn:= factor(rn, levels = sort(InputCollect$paid_media_vars))]
      plotMediaShareLoopBar <- plotMediaShareLoop[variable %in% c("spend_share", "effect_share")]
      #plotMediaShareLoopBar[, variable:= ifelse(variable=="spend_share", "total spend share", "total effect share")]
      plotMediaShareLoopLine <- plotMediaShareLoop[variable ==ifelse(InputCollect$dep_var_type == "conversion", "cpa_total", "roi_total")]
      #plotMediaShareLoopLine[, variable:= "roi_total"]
      ySecScale <- max(plotMediaShareLoopLine$value)/max(plotMediaShareLoopBar$value)*1.1
      
      p1 <- ggplot(plotMediaShareLoopBar, aes(x=rn, y=value, fill = variable)) +
        geom_bar(stat = "identity", width = 0.5, position = "dodge") +
        geom_text(aes(label=paste0(round(value*100,2),"%")), color = "darkblue",  position=position_dodge(width=0.5), fontface = "bold") +
        
        geom_line(data = plotMediaShareLoopLine, aes(x = rn, y=value/ySecScale, group = 1, color = variable), inherit.aes = FALSE) +
        geom_point(data = plotMediaShareLoopLine, aes(x = rn, y=value/ySecScale, group = 1, color = variable), inherit.aes = FALSE, size=4) +
        geom_text(data = plotMediaShareLoopLine, aes(label=round(value,2), x = rn, y=value/ySecScale, group = 1, color = variable)
                  , fontface = "bold", inherit.aes = FALSE, hjust = -1, size = 6) +
        scale_y_continuous(sec.axis = sec_axis(~.* ySecScale)) +          
        coord_flip() +
        theme( legend.title = element_blank(), legend.position = c(0.9, 0.2) ,axis.text.x = element_blank()) +
        scale_fill_brewer(palette = "Paired") +
        labs(title = paste0("Share of Spend VS Share of Effect with total ", ifelse(InputCollect$dep_var_type == "conversion", "CPA", "ROI"))
             ,subtitle = paste0("rsq_train: ", rsq_train_plot, 
                                ", nrmse = ", nrmse_plot, 
                                ", decomp.rssd = ", decomp_rssd_plot,
                                ", mape.lift = ", mape_lift_plot)
             ,y="", x="")
      
      ## plot waterfall
      plotWaterfallLoop <- plotWaterfall[solID == uniqueSol[j]][order(xDecompPerc)]
      plotWaterfallLoop[, end := cumsum(xDecompPerc)]
      plotWaterfallLoop[, end := 1-end]
      plotWaterfallLoop[, ':='(start =shift(end, fill = 1, type = "lag")
                               ,id = 1:nrow(plotWaterfallLoop)
                               ,rn = as.factor(rn)
                               ,sign = as.factor(ifelse(xDecompPerc>=0, "pos", "neg")))]
      
      p2 <- suppressWarnings(ggplot(plotWaterfallLoop, aes(x= id, fill = sign)) +
                               geom_rect(aes(x = rn, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start), stat="identity") +
                               scale_x_discrete("", breaks = levels(plotWaterfallLoop$rn), labels = plotWaterfallLoop$rn)+
                               theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position = c(0.1, 0.1))  +
                               geom_text(mapping = aes(label = paste0(format_unit(xDecompAgg),"\n", round(xDecompPerc*100, 2), "%")
                                                       ,y = rowSums(cbind(end,xDecompPerc/2))), fontface = "bold") +
                               coord_flip() +
                               labs(title="Response decomposition waterfall by predictor"
                                    ,subtitle = paste0("rsq_train: ", rsq_train_plot, 
                                                       ", nrmse = ", nrmse_plot, 
                                                       ", decomp.rssd = ", decomp_rssd_plot,
                                                       ", mape.lift = ", mape_lift_plot)
                                    ,x=""
                                    ,y=""))
      
      ## plot adstock rate
      
      resultHypParamLoop <- resultHypParam[solID == uniqueSol[j]]
      
      hypParam <- unlist(resultHypParamLoop[, InputCollect$local_name, with =FALSE])
      dt_transformPlot <- dt_mod[, c("ds", InputCollect$all_media), with =FALSE] # independent variables
      dt_transformSpend <- cbind(dt_transformPlot[,.(ds)], InputCollect$dt_input[, c(InputCollect$paid_media_spends), with =FALSE]) # spends of indep vars
      setnames(dt_transformSpend, names(dt_transformSpend), c("ds", InputCollect$paid_media_vars))
      
      # update non-spend variables
      dt_transformSpendMod <- dt_transformPlot[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich, c("ds", InputCollect$paid_media_vars), with=FALSE ]
      if (length(InputCollect$exposureVarName)>0) {
        for (expo in InputCollect$exposureVarName) {
          sel_nls <- ifelse(InputCollect$modNLSCollect[channel == expo, rsq_nls>rsq_lm],"nls","lm")
          dt_transformSpendMod[, (expo):= InputCollect$yhatNLSCollect[channel==expo & models == sel_nls, yhat]]
        }
      }
      
      dt_transformAdstock <- copy(dt_transformPlot)
      dt_transformSaturation <- dt_transformPlot[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
      #chnl_non_spend <- InputCollect$paid_media_vars[!(InputCollect$paid_media_vars==InputCollect$paid_media_spends)]
      
      m_decayRate <- list()
      
      for (med in 1:length(InputCollect$all_media)) {
        
        med_select <- InputCollect$all_media[med]
        m <- dt_transformPlot[, get(med_select)]
        
        
        ## adstocking
        if (InputCollect$adstock == "geometric") {
          
          theta <- hypParam[paste0(InputCollect$all_media[med], "_thetas")]
          x_list <- adstock_geometric(x=m, theta = theta)
          
        } else if (InputCollect$adstock == "weibull") {
          
          shape <- hypParam[paste0(InputCollect$all_media[med], "_shapes")]
          scale <- hypParam[paste0(InputCollect$all_media[med], "_scales")]
          x_list <- adstock_weibull(x=m, shape = shape, scale = scale)
          
        }
        m_adstocked <- x_list$x_decayed
        dt_transformAdstock[, (med_select):= m_adstocked]
        m_adstockedRollWind <- m_adstocked[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
        
        ## saturation
        alpha <- hypParam[paste0(InputCollect$all_media[med], "_alphas")]
        gamma <- hypParam[paste0(InputCollect$all_media[med], "_gammas")]
        dt_transformSaturation[, (med_select):= saturation_hill(x=m_adstockedRollWind, alpha = alpha, gamma = gamma)] 
        
        m_decayRate[[med]] <- data.table(x_list$thetaVecCum)
        setnames(m_decayRate[[med]], "V1", paste0(InputCollect$all_media[med], "_decayRate"))
        
      } 
      
      m_decayRate <- data.table(cbind(sapply(m_decayRate, function(x) sapply(x, function(y)y))))
      setnames(m_decayRate, names(m_decayRate), InputCollect$all_media)
      m_decayRateSum <- m_decayRate[, lapply(.SD, sum), .SDcols = InputCollect$all_media]
      
      decayRate.melt <- suppressWarnings(melt.data.table(m_decayRateSum))
      
      #decayRate.melt[, channel:=str_extract(decayRate.melt$variable, paste0(InputCollect$paid_media_vars, collapse = "|"))]
      #decayRate.melt[, variable:=str_replace(decayRate.melt$variable, paste0(paste0(InputCollect$paid_media_vars,"_"), collapse = "|"), "")]
      
      ## get geometric reference
      decayVec <- seq(0, 0.9, by = 0.001)
      decayInfSum <- c()
      for (i in 1:length(decayVec)) {
        decayInfSum[i] <- 1 / (1 - decayVec[i])-1
      }
      
      decayOut <- c()
      for (i in 1:nrow(decayRate.melt)) {
        decayOut[i] <- decayVec[which.min(abs(decayRate.melt$value[i] - decayInfSum))]
      }
      decayRate.melt[, avg_decay_rate:= decayOut]
      decayRate.melt[, variable:= factor(variable, levels = sort(InputCollect$all_media))]
      
      p3 <- ggplot(decayRate.melt, aes(x=variable, y=avg_decay_rate, fill = "coral")) +
        geom_bar(stat = "identity", width = 0.5) +
        theme(legend.position = "none") +
        coord_flip() +
        geom_text(aes(label=paste0(round(avg_decay_rate*100,1), "%")),  position=position_dodge(width=0.5), fontface = "bold") +
        ylim(0,1) +
        labs(title = "Average adstock decay rate"
             ,subtitle = paste0("rsq_train: ", rsq_train_plot, 
                                ", nrmse = ", nrmse_plot, 
                                ", decomp.rssd = ", decomp_rssd_plot,
                                ", mape.lift = ", mape_lift_plot)
             ,y="", x="")
      
      ## plot response curve
      
      dt_transformSaturationDecomp <- copy(dt_transformSaturation)
      for (i in 1:InputCollect$mediaVarCount) {
        coef <- plotWaterfallLoop[rn == InputCollect$all_media[i], coef]
        dt_transformSaturationDecomp[, (InputCollect$all_media[i]):= .SD * coef, .SDcols = InputCollect$all_media[i]]
      }
      
      #mediaAdstockFactorPlot <- dt_transformPlot[, lapply(.SD, sum), .SDcols = InputCollect$paid_media_vars]  / dt_transformAdstock[, lapply(.SD, sum), .SDcols = InputCollect$paid_media_vars]
      #dt_transformSaturationAdstockReverse <- data.table(mapply(function(x, y) {x*y},x= dt_transformAdstock[, InputCollect$paid_media_vars, with=FALSE], y= mediaAdstockFactorPlot))
      dt_transformSaturationSpendReverse <- copy(dt_transformAdstock[, c("ds", InputCollect$all_media), with=FALSE])
      
      for (i in 1:InputCollect$mediaVarCount) {
        chn <- InputCollect$paid_media_vars[i]
        if (chn %in% InputCollect$paid_media_vars[InputCollect$costSelector]) {
          
          # get Michaelis Menten nls fitting param
          get_chn <- dt_transformSaturationSpendReverse[, chn, with =FALSE]
          Vmax <- InputCollect$modNLSCollect[channel == chn, Vmax]
          Km <- InputCollect$modNLSCollect[channel == chn, Km]
          
          # reverse exposure to spend
          dt_transformSaturationSpendReverse[, (chn):= mic_men(x=.SD, Vmax = Vmax, Km = Km, reverse = TRUE), .SDcols = chn] # .SD * Km / (Vmax - .SD) exposure to spend, reverse Michaelis Menthen: x = y*Km/(Vmax-y)
          
        } else if (chn %in% InputCollect$exposureVarName) {
          coef_lm <- InputCollect$modNLSCollect[channel == chn, coef_lm]
          dt_transformSaturationSpendReverse[, (chn):= .SD/coef_lm, .SDcols = chn] 
        } 
        # spendRatioFitted <- xDecompAgg[rn == chn, mean(total_spend)] / dt_transformSaturationSpendReverse[, sum(.SD), .SDcols = chn]
        # dt_transformSaturationSpendReverse[, (chn):= .SD * spendRatioFitted, .SDcols = chn]
      }
      
      dt_transformSaturationSpendReverse <- dt_transformSaturationSpendReverse[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
      
      dt_scurvePlot <- cbind(melt.data.table(dt_transformSaturationDecomp[, c("ds", InputCollect$all_media), with=FALSE], id.vars = "ds", variable.name = "channel",value.name = "response"),
                             melt.data.table(dt_transformSaturationSpendReverse, id.vars = "ds", value.name = "spend")[, .(spend)])
      dt_scurvePlot <- dt_scurvePlot[spend>=0] # remove outlier introduced by MM nls fitting
      
      
      dt_scurvePlotMean <- dt_transformSpend[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich, !"ds"][, lapply(.SD, function(x) ifelse(is.na(mean(x[x>0])),0,mean(x[x>0])) ), .SDcols = InputCollect$paid_media_vars]
      dt_scurvePlotMean <- melt.data.table(dt_scurvePlotMean, measure.vars = InputCollect$paid_media_vars, value.name = "mean_spend", variable.name = "channel")
      dt_scurvePlotMean[, ':='(mean_spend_scaled=0, mean_response=0, next_unit_response=0)]
      
      for (med in 1:InputCollect$mediaVarCount) {
        
        get_med <- InputCollect$paid_media_vars[med]
        get_spend <- dt_scurvePlotMean[channel == get_med, mean_spend]
        
        if (get_med %in% InputCollect$paid_media_vars[InputCollect$costSelector]) {
          Vmax <- InputCollect$modNLSCollect[channel == get_med, Vmax]
          Km <- InputCollect$modNLSCollect[channel == get_med, Km]
          get_spend_mm <- mic_men(x = get_spend, Vmax = Vmax, Km = Km) # Vmax * get_spend/(Km + get_spend)
        } else if (get_med %in% InputCollect$exposureVarName) {
          coef_lm <- InputCollect$modNLSCollect[channel == get_med, coef_lm]
          get_spend_mm <- get_spend*coef_lm
        } else {
          get_spend_mm <- get_spend
        }
        
        m <- dt_transformAdstock[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich, get(get_med)]
        # m <- m[m>0] # remove outlier introduced by MM nls fitting
        alpha <- hypParam[which(paste0(get_med, "_alphas")==names(hypParam))]
        gamma <- hypParam[which(paste0(get_med, "_gammas")==names(hypParam))]
        #gammaTrans <- round(quantile(seq(range(m)[1], range(m)[2], length.out = 100), gamma),4)
        #get_response <-  get_spend_mm**alpha / (get_spend_mm**alpha + gammaTrans**alpha)
        #get_response_marginal <- (get_spend_mm+1)**alpha / ((get_spend_mm+1)**alpha + gammaTrans**alpha)
        get_response <- saturation_hill(x=m, alpha = alpha, gamma = gamma, x_marginal = get_spend_mm)
        get_response_marginal <- saturation_hill(x=m, alpha = alpha, gamma = gamma, x_marginal = get_spend_mm+1)
        
        coef <- plotWaterfallLoop[rn == get_med, coef]
        dt_scurvePlotMean[channel == get_med, mean_spend_scaled := get_spend_mm]
        dt_scurvePlotMean[channel == get_med, mean_response := get_response * coef]
        dt_scurvePlotMean[channel == get_med, next_unit_response := get_response_marginal * coef - mean_response]
        
      }
      dt_scurvePlotMean[, solID:= uniqueSol[j]]
      
      p4 <- ggplot(data= dt_scurvePlot, aes(x=spend, y=response, color = channel)) +
        geom_line() +
        geom_point(data = dt_scurvePlotMean, aes(x=mean_spend, y=mean_response, color = channel)) +
        geom_text(data = dt_scurvePlotMean, aes(x=mean_spend, y=mean_response,  label = round(mean_spend,0)), show.legend = FALSE, hjust = -0.2)+
        theme(legend.position = c(0.9, 0.2)) +
        labs(title="Response curve and mean spend by channel"
             ,subtitle = paste0("rsq_train: ", rsq_train_plot, 
                                ", nrmse = ", nrmse_plot, 
                                ", decomp.rssd = ", decomp_rssd_plot,
                                ", mape.lift = ", mape_lift_plot)
             ,x="Spend" ,y="response")
      
      ## plot fitted vs actual
      
      if(!is.null(InputCollect$prophet_vars)) {
        dt_transformDecomp <- cbind(dt_modRollWind[, c("ds", "dep_var", InputCollect$prophet_vars, InputCollect$context_vars), with=FALSE], dt_transformSaturation[, InputCollect$all_media, with=FALSE])
      } else {
        dt_transformDecomp <- cbind(dt_modRollWind[, c("ds", "dep_var", InputCollect$context_vars), with=FALSE], dt_transformSaturation[, InputCollect$all_media, with=FALSE])
      }
      col_order <- c("ds", "dep_var", InputCollect$all_ind_vars)
      setcolorder(dt_transformDecomp, neworder = col_order)
      
      xDecompVec <- dcast.data.table(xDecompAgg[solID==uniqueSol[j], .(rn, coef, solID)],  solID ~ rn, value.var = "coef")
      if (!("(Intercept)" %in% names(xDecompVec))) {xDecompVec[, "(Intercept)":= 0]}
      setcolorder(xDecompVec, neworder = c("solID", "(Intercept)",col_order[!(col_order %in% c("ds", "dep_var"))]))
      intercept <- xDecompVec$`(Intercept)`
      
      xDecompVec <- data.table(mapply(function(scurved,coefs) { scurved * coefs}, 
                                      scurved=dt_transformDecomp[, !c("ds", "dep_var"), with=FALSE] , 
                                      coefs = xDecompVec[, !c("solID", "(Intercept)")]))
      xDecompVec[, intercept:= intercept]
      xDecompVec[, ':='(depVarHat=rowSums(xDecompVec), solID = uniqueSol[j])]
      xDecompVec <- cbind(dt_transformDecomp[, .(ds, dep_var)], xDecompVec)
      
      xDecompVecPlot <- xDecompVec[, .(ds, dep_var, depVarHat)]
      setnames(xDecompVecPlot, old = c("ds", "dep_var", "depVarHat"), new = c("ds", "actual", "predicted"))
      suppressWarnings(xDecompVecPlotMelted <- melt.data.table(xDecompVecPlot, id.vars = "ds"))
      
      p5 <- ggplot(xDecompVecPlotMelted, aes(x=ds, y = value, color = variable)) +
        geom_line()+
        theme(legend.position = c(0.9, 0.9)) +
        labs(title="Actual vs. predicted response"
             ,subtitle = paste0("rsq_train: ", rsq_train_plot, 
                                ", nrmse = ", nrmse_plot, 
                                ", decomp.rssd = ", decomp_rssd_plot,
                                ", mape.lift = ", mape_lift_plot)
             ,x="date" ,y="response")
      
      ## plot diagnostic: fitted vs residual
      
      p6 <- qplot(x=predicted, y = actual - predicted, data = xDecompVecPlot) +
        geom_hline(yintercept = 0) +
        geom_smooth(se = TRUE, method = 'loess', formula = 'y ~ x') + 
        xlab("fitted") + ylab("resid") + ggtitle("fitted vs. residual")
      
      
      ## save and aggregate one-pager plots
      
      onepagerTitle <- paste0("Model one-pager, on pareto front ", pf,", ID: ", uniqueSol[j])
      
      pg <- arrangeGrob(p2,p5,p1, p4, p3, p6, ncol=2, top = text_grob(onepagerTitle, size = 15, face = "bold"))
      # grid.draw(pg)
      if (plot_pareto) {
        ggsave(filename=paste0(plot_folder, "/", plot_folder_sub,"/", uniqueSol[j],".png")
               , plot = pg
               , dpi = 600, width = 18, height = 18)
        
        setTxtProgressBar(pbplot, cnt)
      }
      
      ## prepare output
      if (!is.null(InputCollect$organic_vars)) {
        dt_transformSpend[, (InputCollect$organic_vars):= NA]
        dt_transformSpendMod[, (InputCollect$organic_vars):= NA]
        dt_transformSaturationSpendReverse[, (InputCollect$organic_vars):= NA]
      }
      
      
      mediaVecCollect[[cnt]] <- rbind(dt_transformPlot[, ':='(type="rawMedia", solID=uniqueSol[j])]
                                      ,dt_transformSpend[, ':='(type="rawSpend", solID=uniqueSol[j])]
                                      ,dt_transformSpendMod[, ':='(type="predictedExposure", solID=uniqueSol[j])]
                                      ,dt_transformAdstock[, ':='(type="adstockedMedia", solID=uniqueSol[j])]
                                      ,dt_transformSaturation[, ':='(type="saturatedMedia", solID=uniqueSol[j])]
                                      ,dt_transformSaturationSpendReverse[, ':='(type="saturatedSpendReversed", solID=uniqueSol[j])]
                                      ,dt_transformSaturationDecomp[, ':='(type="decompMedia", solID=uniqueSol[j])])
      
      xDecompVecCollect[[cnt]] <- xDecompVec
      meanResponseCollect[[cnt]] <- dt_scurvePlotMean
      
    } # end solution loop
  } # end pareto front loop
  mediaVecCollect <- rbindlist(mediaVecCollect)
  xDecompVecCollect <- rbindlist(xDecompVecCollect)
  meanResponseCollect <- rbindlist(meanResponseCollect)
  
  setnames(meanResponseCollect, old = "channel", new = "rn")
  setkey(meanResponseCollect, solID, rn)
  xDecompAgg <- merge(xDecompAgg,meanResponseCollect[, .(rn, solID, mean_response, next_unit_response)], all.x=TRUE)
  
  totalTime <- difftime(Sys.time(),t0, units = "mins")
  cat("\nTotal time: ",totalTime, "mins\n")
  
  #####################################
  #### Collect results for output
  
  allSolutions <- xDecompVecCollect[, unique(solID)]
  
  fwrite(resultHypParam[solID %in% allSolutions], paste0(plot_folder, "/", plot_folder_sub,"/", "pareto_hyperparameters.csv"))
  fwrite(xDecompAgg[solID %in% allSolutions], paste0(plot_folder, "/", plot_folder_sub,"/", "pareto_aggregated.csv"))
  fwrite(mediaVecCollect, paste0(plot_folder, "/", plot_folder_sub,"/", "pareto_media_transform_matrix.csv"))
  fwrite(xDecompVecCollect, paste0(plot_folder, "/", plot_folder_sub,"/", "pareto_alldecomp_matrix.csv"))
  
  OutputCollect <- list(resultHypParam=resultHypParam[solID %in% allSolutions],
                        xDecompAgg=xDecompAgg[solID %in% allSolutions],
                        mediaVecCollect=mediaVecCollect,
                        xDecompVecCollect=xDecompVecCollect,
                        model_output_collect=model_output_collect,
                        allSolutions = allSolutions,
                        totalTime = totalTime,
                        plot_folder= paste0(plot_folder, "/", plot_folder_sub,"/"))
  
  # if (!refresh) {
  #   assign("OutputCollect", OutputCollect, envir = .GlobalEnv)
  # } else {
  #   assign("OutputCollectRF", OutputCollect, envir = .GlobalEnv)
  # }
  return(OutputCollect)
  
}


#####################################
#### Define robyn_response function

robyn_response <- function(robyn_object = NULL
                           , select_run = NULL
                           , paid_media_var = NULL
                           , select_model = NULL
                           , Spend = NULL
                           , dt_hyppar = NULL # OutputCollect$resultHypParam
                           , dt_coef = NULL # OutputCollect$xDecompAgg
                           , InputCollect = NULL# InputCollect
                           
) {
  
  ## get input
  if (!is.null(robyn_object)) {
    
    load(robyn_object)
    objectName <-  substr(robyn_object, start = max(gregexpr("/|\\\\", robyn_object)[[1]])+1, stop = max(gregexpr("RData", robyn_object)[[1]])-2)
    objectPath <- substr(robyn_object, start = 1, stop = max(gregexpr("/|\\\\", robyn_object)[[1]]))
    Robyn <- get(objectName) 
    
    select_run_all <- 0:(length(Robyn)-1)
    if (is.null(select_run)) {
      select_run <- max(select_run_all)
      message("Using latest model: ", ifelse(select_run==0, "initial model",paste0("refresh model nr.",select_run))," for the response function. Use parameter select_run to specify which run to use")
    }
    
    if (!(select_run %in% select_run_all) | length(select_run) !=1) {stop("select_run must be one value of ", paste(select_run_all, collapse = ", "))}
    
    listName <- ifelse(select_run == 0, "listInit", paste0("listRefresh",select_run))
    InputCollect <- Robyn[[listName]][["InputCollect"]]
    OutputCollect <- Robyn[[listName]][["OutputCollect"]]
    dt_hyppar <- OutputCollect$resultHypParam
    dt_coef <- OutputCollect$xDecompAgg
    select_model <- OutputCollect$selectID
    
  } else if (any(is.null(dt_hyppar), is.null(dt_coef), is.null(InputCollect))) {
    stop("when robyn_object is not provided, then dt_hyppar = OutputCollect$resultHypParam, dt_coef = OutputCollect$xDecompAgg and InputCollect must be provided")
  }
  
  dt_input = InputCollect$dt_input
  paid_media_vars = InputCollect$paid_media_vars
  paid_media_spends = InputCollect$paid_media_spends
  startRW = InputCollect$rollingWindowStartWhich
  endRW = InputCollect$rollingWindowEndWhich
  adstock = InputCollect$adstock
  allSolutions = dt_hyppar[, unique(solID)]
  spendExpoMod = InputCollect$modNLSCollect
  
  ## check inputs
  if (is.null(paid_media_var)) {
    stop(paste0("paid_media_var must be one of these values: ", paste(paid_media_vars, collapse = ", ")))
  } else if ( !(paid_media_var %in% paid_media_vars) | length(paid_media_var)!=1) {
    stop(paste0("paid_media_var must be one of these values: ", paste(paid_media_vars, collapse = ", ")))
  } 
  
  if (!(select_model %in% allSolutions)) {
    stop(paste0("select_model must be one of these values: ", paste(allSolutions, collapse = ", ")))
  }
  
  mediaVar <- dt_input[, get(paid_media_var)]
  
  if (!is.null(Spend)) {
    if (length(Spend) !=1 | Spend <= 0 | !is.numeric(Spend)) {stop("Spend must be a positive number")} 
  }
  
  
  
  ## transform spend to exposure if necessary
  if (paid_media_var %in% InputCollect$exposureVarName) {
    
    # use non-0 mean spend as marginal level if Spend not provided
    if (is.null(Spend)) {
      mediaSpend <- dt_input[startRW:endRW, get(paid_media_spends[which(paid_media_vars == paid_media_var)])]
      Spend <- mean(mediaSpend[mediaSpend>0])
      message("Spend not provided. using mean of ", paid_media_var," as marginal levl instead")
    }
    
    # fit spend to exposure
    nls_select <- spendExpoMod[channel == paid_media_var, rsq_nls > rsq_lm]
    if (nls_select) {
      Vmax <- spendExpoMod[channel == paid_media_var, Vmax]
      Km <- spendExpoMod[channel == paid_media_var, Km]
      Spend <- mic_men(x=Spend, Vmax = Vmax, Km = Km, reverse = FALSE) 
    } else {
      coef_lm <- spendExpoMod[channel == paid_media_var, coef_lm]
      Spend <- Spend * coef_lm
    }
  } else {
    
    # use non-0 mean spend as marginal level if Spend not provided
    if (is.null(Spend)) {
      mediaSpend <- dt_input[startRW:endRW, get(paid_media_var)]
      Spend <- mean(mediaSpend[mediaSpend>0])
      message("Spend not provided. using mean of ", paid_media_var," as marginal levl instead")
    }
  }
  
  
  ## adstocking
  if (adstock == "geometric") {
    theta <- dt_hyppar[solID == select_model, get(paste0(paid_media_var,"_thetas"))]
    x_list <- adstock_geometric(x=mediaVar, theta = theta)
  } else if (adstock == "weibull") {
    shape <- dt_hyppar[solID == select_model, get(paste0(paid_media_var,"_shapes"))]
    scale <- dt_hyppar[solID == select_model, get(paste0(paid_media_var,"_scales"))]
    x_list <- adstock_weibull(x=mediaVar, shape = shape, scale = scale)
  }
  m_adstocked <- x_list$x_decayed
  
  ## saturation
  m_adstockedRW <- m_adstocked[startRW:endRW]
  alpha <- dt_hyppar[solID == select_model, get(paste0(paid_media_var,"_alphas"))]
  gamma <- dt_hyppar[solID == select_model, get(paste0(paid_media_var,"_gammas"))]
  Saturated <- saturation_hill(x=m_adstockedRW, alpha = alpha, gamma = gamma, x_marginal = Spend)
  
  ## decomp
  coeff <- dt_coef[solID == select_model & rn == paid_media_var, coef]
  Response <- Saturated * coeff
  
  return(as.numeric(Response))
}


#####################################
#### Define robyn_save function

robyn_save <- function(robyn_object
                       ,select_model
                       ,InputCollect
                       ,OutputCollect
) {
  
  
  if (!(select_model %in% OutputCollect$resultHypParam$solID)) {
    stop(paste0("select_model must be one of these values: ", paste(OutputCollect$resultHypParam$solID, collapse = ", ")))}
  
  #initModPathFull <- paste0(robyn_object, "/")
  if (file.exists(robyn_object)) {
    answer <- askYesNo(paste0(robyn_object, " already exists. Are you certain to overwrite it?")) 
    if(answer==FALSE | is.na(answer)) {
      stop("stopped")
    }
  }
  
  OutputCollect$resultHypParam = OutputCollect$resultHypParam[solID == select_model]
  OutputCollect$xDecompAgg = OutputCollect$xDecompAgg[solID == select_model]
  OutputCollect$mediaVecCollect = OutputCollect$mediaVecCollect[solID == select_model]
  OutputCollect$xDecompVecCollect = OutputCollect$xDecompVecCollect[solID == select_model]
  OutputCollect$selectID = select_model
  
  InputCollect$refreshCounter <- 0
  #listParamInit <- listParam
  listInit <- list(OutputCollect=OutputCollect, InputCollect=InputCollect)
  Robyn <- list(listInit=listInit)
  
  save(Robyn, file = robyn_object)
  #listOutputInit <- NULL;  listParamInit <- NULL 
  #load("/Users/gufengzhou/Documents/GitHub/plots/listInit.RData")
  
}


#####################################
#### Define robyn_refresh function

robyn_refresh <- function(robyn_object
                          ,dt_input = dt_input
                          ,dt_holidays= dt_holidays
                          ,refresh_steps = 4
                          ,refresh_mode = "manual" # "auto", "manual"
                          ,refresh_iters = 100
                          ,refresh_trials = 1
                          ,plot_pareto = TRUE
                          
) {
  
  refreshControl <- TRUE
  while (refreshControl) {
    
    ## load inital model
    #if(exists("OutputCollectRF")) {rm(OutputCollectRF, listParamRefresh, listDTRefresh)}
    load(robyn_object) 
    objectName <-  substr(robyn_object, start = max(gregexpr("/|\\\\", robyn_object)[[1]])+1, stop = max(gregexpr("RData", robyn_object)[[1]])-2)
    objectPath <- substr(robyn_object, start = 1, stop = max(gregexpr("/|\\\\", robyn_object)[[1]]))
    Robyn <- get(objectName) 
    
    ## count refresh
    refreshCounter <- length(Robyn); refreshCounter
    objectCheck <- if (refreshCounter==1) {c("listInit")} else {c("listInit", paste0("listRefresh", 1:(refreshCounter-1)))} 
    if (!all(objectCheck %in% names(Robyn))) {stop("Saved Robyn object is corrupted. It should contain ", paste(objectCheck, collapse = ",", ". Please rerun model."))}
    
    ## get previous data
    if(refreshCounter==1) {
      InputCollectRF <- Robyn$listInit$InputCollect
      listOutputPrev <- Robyn$listInit$OutputCollect
      InputCollectRF$xDecompAggPrev <- listOutputPrev$xDecompAgg
      
      message("\n###### initial model loaded ... ######")
      if (length(unique(Robyn$listInit$OutputCollect$resultHypParam$solID))>1) {stop("Run robyn_save first to select one initial model")}
      
    } else {
      listName <- paste0("listRefresh",refreshCounter-1)
      InputCollectRF <- Robyn[[listName]][["InputCollect"]]
      listOutputPrev <- Robyn[[listName]][["OutputCollect"]]
      listReportPrev <- Robyn[[listName]][["ReportCollect"]]
      
      message(paste0("\n###### refresh model nr.",refreshCounter-1," loaded ... ######"))
      
      ## model selection from previous build
      listOutputPrev$resultHypParam <- listOutputPrev$resultHypParam[bestModRF==TRUE]
      listOutputPrev$xDecompAgg <- listOutputPrev$xDecompAgg[bestModRF==TRUE]
      listOutputPrev$mediaVecCollect <- listOutputPrev$mediaVecCollect[bestModRF==TRUE]
      listOutputPrev$xDecompVecCollect <- listOutputPrev$xDecompVecCollect[bestModRF==TRUE]
    }
    
    InputCollectRF$refreshCounter <- refreshCounter
    InputCollectRF$refresh_steps <- refresh_steps
    if (refresh_steps >= InputCollectRF$rollingWindowLength) {stop("Refresh input data is completely new. Please rebuild model using robyn_run")}
    
    
    ## load new data
    
    InputCollectRF$dt_input <- dt_input
    InputCollectRF$dt_holidays <- dt_holidays
    
    #### update refresh model parameters
    
    ## refresh rolling window
    totalDates <- as.Date(dt_input[, get(InputCollectRF$date_var)])
    refreshStart <- as.Date(InputCollectRF$window_start) + InputCollectRF$dayInterval * refresh_steps
    refreshEnd <- as.Date(InputCollectRF$window_end) + InputCollectRF$dayInterval * refresh_steps
    InputCollectRF$refreshAddedStart <- as.Date(InputCollectRF$window_end) + InputCollectRF$dayInterval 
    InputCollectRF$window_start <- refreshStart
    InputCollectRF$window_end <- refreshEnd
    
    refreshStartWhich <- which.min(abs(difftime(totalDates, as.Date(refreshStart), units = "days")))
    refreshEndWhich <- which.min(abs(difftime(totalDates, as.Date(refreshEnd), units = "days")))
    InputCollectRF$rollingWindowStartWhich <- refreshStartWhich
    InputCollectRF$rollingWindowEndWhich <- refreshEndWhich
    InputCollectRF$rollingWindowLength <- refreshEndWhich - refreshStartWhich +1
    
    if (refreshEnd > max(totalDates)) {stop("Not enough data for this refresh. Input data from date ", refreshEnd, " or later required")}
    
    if (refresh_mode == "manual") {
      refreshLooper <- 1
      message("###### refreshing model nr.",refreshCounter, " in ", refresh_mode, " mode ... ######")
      refreshControl <- FALSE
    } else {
      refreshLooper <- floor(as.numeric(difftime(max(totalDates), refreshEnd, units = "days")) / InputCollectRF$dayInterval / refresh_steps)
      message("###### refreshing model nr.",refreshCounter, " in ",refresh_mode," mode. ",refreshLooper," more to go ... ######")
    }
    
    #### update refresh model parameters
    
    
    ## refresh hyperparameter bounds
    initBounds <- Robyn$listInit$InputCollect$hyperparameters
    initBoundsDis <- sapply(initBounds, function(x) return(x[2]-x[1]))
    newBoundsFreedom <- refresh_steps/InputCollectRF$rollingWindowLength
    
    hyperparameters <- InputCollectRF$hyperparameters
    hypNames <- names(hyperparameters)
    for (h in 1:length(hypNames))  {
      getHyp <- listOutputPrev$resultHypParam[, get(hypNames[h])]
      getDis <- initBoundsDis[hypNames[h]]
      newLowB <- getHyp - getDis*newBoundsFreedom
      if (newLowB< initBounds[hypNames[h]][[1]][1]) {newLowB <- initBounds[hypNames[h]][[1]][1]}
      newUpB <- getHyp + getDis*newBoundsFreedom
      if (newUpB> initBounds[hypNames[h]][[1]][2]) {newUpB <- initBounds[hypNames[h]][[1]][2]}
      newBounds <- unname(c(newLowB, newUpB))
      hyperparameters[hypNames[h]][[1]] <- newBounds
    }
    InputCollectRF$hyperparameters <- hyperparameters
    
    ## refresh iterations and trial
    InputCollectRF$iterations <- refresh_iters
    InputCollectRF$trials <- refresh_trials
    
    #### update refresh model parameters
    
    ## feature engineering for refreshed data
    InputCollectRF <- robyn_engineering(InputCollect = InputCollectRF)
    
    ## refresh model with adjusted decomp.rssd
    
    OutputCollectRF <- robyn_run(InputCollect = InputCollectRF
                                 , plot_folder = objectPath
                                 , pareto_fronts = 1
                                 , refresh = TRUE
                                 , plot_pareto = plot_pareto)
    
    ## select winner model for current refresh
    # selectID <- OutputCollectRF$resultHypParam[which.min(decomp.rssd), solID] # min decomp.rssd selection
    OutputCollectRF$resultHypParam[, error_dis:= sqrt(nrmse^2 + decomp.rssd^2)] # min error distance selection
    selectID <- OutputCollectRF$resultHypParam[which.min(error_dis), solID]
    OutputCollectRF$selectID <- selectID
    message("\nSelected model ID: ", selectID, " for refresh model nr.",refreshCounter," based on the smallest combined error of nrmse & decomp.rssd")
    
    OutputCollectRF$resultHypParam[, bestModRF:= solID == selectID]
    OutputCollectRF$xDecompAgg[, bestModRF:= solID == selectID]
    OutputCollectRF$mediaVecCollect[, bestModRF:= solID == selectID]
    OutputCollectRF$xDecompVecCollect[, bestModRF:= solID == selectID]
    
    
    #### result collect & save
    if (refreshCounter==1) {
      listOutputPrev$resultHypParam[, ':='(error_dis = sqrt(nrmse^2 + decomp.rssd^2), bestModRF = TRUE, refreshStatus=refreshCounter-1)]
      listOutputPrev$xDecompAgg[, ':='(bestModRF=TRUE, refreshStatus=refreshCounter-1)]
      listOutputPrev$mediaVecCollect[, ':='(bestModRF=TRUE, refreshStatus=refreshCounter-1)]
      listOutputPrev$xDecompVecCollect[, ':='(bestModRF=TRUE, refreshStatus=refreshCounter-1)]
      
      
      resultHypParamReport <- rbind(listOutputPrev$resultHypParam[bestModRF==TRUE]
                                    ,OutputCollectRF$resultHypParam[bestModRF==TRUE][, refreshStatus:=refreshCounter])
      xDecompAggReport <- rbind(listOutputPrev$xDecompAgg[bestModRF==TRUE]
                                ,OutputCollectRF$xDecompAgg[bestModRF==TRUE][, refreshStatus:=refreshCounter])
      mediaVecReport <- rbind(listOutputPrev$mediaVecCollect[bestModRF==TRUE & ds>=(refreshStart- InputCollectRF$dayInterval * refresh_steps) & ds <= (refreshEnd- InputCollectRF$dayInterval * refresh_steps)]
                              ,OutputCollectRF$mediaVecCollect[bestModRF==TRUE & ds>=InputCollectRF$refreshAddedStart & ds<=refreshEnd][, refreshStatus:=refreshCounter])
      mediaVecReport <- mediaVecReport[order(type, ds, refreshStatus)]
      xDecompVecReport <- rbind(listOutputPrev$xDecompVecCollect[bestModRF==TRUE]
                                ,OutputCollectRF$xDecompVecCollect[bestModRF==TRUE & ds>=InputCollectRF$refreshAddedStart & ds<=refreshEnd][, refreshStatus:=refreshCounter])
    } else {
      
      resultHypParamReport <- rbind(listReportPrev$resultHypParamReport, OutputCollectRF$resultHypParam[bestModRF==TRUE][, refreshStatus:=refreshCounter])
      xDecompAggReport <- rbind(listReportPrev$xDecompAggReport, OutputCollectRF$xDecompAgg[bestModRF==TRUE][, refreshStatus:=refreshCounter])
      mediaVecReport <- rbind(listReportPrev$mediaVecReport
                              , OutputCollectRF$mediaVecCollect[bestModRF==TRUE & ds>=InputCollectRF$refreshAddedStart & ds<=refreshEnd][, refreshStatus:=refreshCounter])
      mediaVecReport <- mediaVecReport[order(type, ds, refreshStatus)]
      xDecompVecReport <- rbind(listReportPrev$xDecompVecReport
                                ,OutputCollectRF$xDecompVecCollect[bestModRF==TRUE & ds>=InputCollectRF$refreshAddedStart & ds<=refreshEnd][, refreshStatus:=refreshCounter])
    }
    
    fwrite(resultHypParamReport, paste0(OutputCollectRF$plot_folder, "report_hyperparameters.csv"))
    fwrite(xDecompAggReport, paste0(OutputCollectRF$plot_folder, "report_aggregated.csv"))
    fwrite(mediaVecReport, paste0(OutputCollectRF$plot_folder, "report_media_transform_matrix.csv"))
    fwrite(xDecompVecReport, paste0(OutputCollectRF$plot_folder, "report_alldecomp_matrix.csv"))
    
    
    #### reporting plots 
    ## actual vs fitted
    
    xDecompVecReportPlot <- copy(xDecompVecReport)
    xDecompVecReportPlot[, ':='(refreshStart = min(ds)
                                ,refreshEnd = max(ds)), by = "refreshStatus"]
    xDecompVecReportPlot[, duration:= as.numeric((refreshEnd-refreshStart+InputCollectRF$dayInterval)/InputCollectRF$dayInterval)]
    getRefreshStarts <- sort(unique(xDecompVecReportPlot$refreshStart))[-1]
    dt_refreshDates <- unique(xDecompVecReportPlot[, .(refreshStatus=as.factor(refreshStatus), refreshStart, refreshEnd, duration)])
    dt_refreshDates[, label:= ifelse(dt_refreshDates$refreshStatus==0
                                     , paste0("initial: ", dt_refreshDates$refreshStart, ", ", dt_refreshDates$duration, InputCollectRF$intervalType, "s")
                                     , paste0("refresh nr.", dt_refreshDates$refreshStatus,": ",dt_refreshDates$refreshStart, ", ", dt_refreshDates$duration, InputCollectRF$intervalType, "s"))]
    dt_refreshDates
    # xDecompVecReportPlot <- fread("/Users/gufengzhou/Documents/GitHub/plots/2021-06-18 11.51 rf2/report_alldecomp_matrix.csv")
    
    xDecompVecReportMelted <- melt.data.table(xDecompVecReportPlot[, .(ds, refreshStart, refreshEnd, refreshStatus, actual=dep_var, predicted=depVarHat)] , id.vars = c("ds", "refreshStatus","refreshStart", "refreshEnd"))
    pFitRF <- ggplot(data=xDecompVecReportMelted) +
      geom_line(aes(x = ds, y = value, color = variable))+
      geom_rect(data = dt_refreshDates, aes(xmin = refreshStart, xmax = refreshEnd, fill = refreshStatus)
                , ymin = -Inf, ymax = Inf, alpha = 0.2) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank() ,# legend.position = c(0.1, 0.8), 
            legend.background = element_rect(fill=alpha('white', 0.4)),
      ) +
      scale_fill_brewer(palette = 'BuGn') +
      geom_text(data = dt_refreshDates, mapping=aes(x=refreshStart, y=max(xDecompVecReportMelted$value), label=label
                                                    , angle=270, hjust=-0.1, vjust=-0.2), color = "gray40" )+
      #geom_vline(xintercept = getRefreshStarts, linetype="dotted") + 
      labs(title="Model refresh: actual vs. predicted response"
           ,subtitle = paste0("Assembled rsq: ", round(get_rsq(true = xDecompVecReportPlot$dep_var, predicted = xDecompVecReportPlot$depVarHat),2)
                              #,"\nRefresh dates: ", paste(getRefreshStarts, collapse = ", ")
           )
           ,x="date" ,y="response")
    print(pFitRF)
    
    ggsave(filename=paste0(OutputCollectRF$plot_folder,"report_actual_fitted.png")
           , plot = pFitRF
           , dpi = 900, width = 12, height = 8)
    
    ## stacked bar plot
    
    xDecompAggReportPlotBase <- xDecompAggReport[rn %in% c(InputCollectRF$prophet_vars,"(Intercept)"), .(rn, perc = ifelse(refreshStatus==0, xDecompPerc, xDecompPercRF), refreshStatus)]
    xDecompAggReportPlotBase <- xDecompAggReportPlotBase[, .(variable = "baseline", percentage = sum(perc)), by = refreshStatus][, roi_total := NA]
    xDecompAggReportPlot <- xDecompAggReport[!(rn %in% c(InputCollectRF$prophet_vars,"(Intercept)")), .(refreshStatus, variable=rn, percentage = ifelse(refreshStatus==0, xDecompPerc, xDecompPercRF), roi_total )]
    xDecompAggReportPlot <- rbind(xDecompAggReportPlot, xDecompAggReportPlotBase)[order(refreshStatus, -variable)]
    xDecompAggReportPlot[, refreshStatus:=ifelse(refreshStatus==0, "init.mod", paste0("refresh",refreshStatus))]
    ySecScale <- max(na.omit(xDecompAggReportPlot$roi_total))/max(xDecompAggReportPlot$percentage)*0.75
    ymax <- max(c(na.omit(xDecompAggReportPlot$roi_total)/ySecScale, xDecompAggReportPlot$percentage))*1.1
    
    pBarRF <- ggplot(data=xDecompAggReportPlot, mapping=aes(x=variable, y= percentage, fill=variable)) +
      geom_bar(alpha=0.8, position="dodge", stat="identity") +
      facet_wrap(~refreshStatus,scales = "free") +
      scale_fill_brewer(palette = 'BrBG') +
      geom_text(aes(label = paste0(round(percentage*100,1),"%")), size = 3 
                ,position=position_dodge(width=0.9), hjust=-0.25) +
      geom_point(aes(x = variable, y = roi_total/ySecScale, color = variable)
                 , size=4, shape = 17, na.rm=TRUE
                 , data= xDecompAggReportPlot ) +
      geom_text(aes(label = round(roi_total,2), x = variable, y = roi_total/ySecScale)
                , size = 3, na.rm=TRUE, hjust=-0.4, fontface = "bold"
                , position=position_dodge(width=0.9)
                , data= xDecompAggReportPlot) +
      scale_color_brewer(palette = 'BrBG') +
      scale_y_continuous(sec.axis = sec_axis(~.* ySecScale), breaks = seq(0, ymax, 0.2), limits = c(0,ymax), name = "roi_total") +
      coord_flip() +
      theme(legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      labs(title="Model refresh: Decomposition & paid media ROI"
           ,subtitle = paste0("baseline includes intercept and all prophet vars: ", paste(InputCollectRF$prophet_vars, collapse = ", ")))
    
    print(pBarRF)
    
    # pReport <- arrangeGrob(pFitRF,pBarRF, ncol=1, top = text_grob("Robyn report onepaper", size = 15, face = "bold"))
    ggsave(filename=paste0(OutputCollectRF$plot_folder,"report_decomposition.png")
           , plot = pBarRF
           , dpi = 900, width = 12, height = 8)
    
    
    #### save result objects
    
    ReportCollect <- list(resultHypParamReport=resultHypParamReport
                          ,xDecompAggReport=xDecompAggReport
                          ,mediaVecReport=mediaVecReport
                          ,xDecompVecReport=xDecompVecReport)
    #assign("ReportCollect", ReportCollect)
    
    listHolder<- list(InputCollect=InputCollectRF
                      ,OutputCollect=OutputCollectRF
                      ,ReportCollect=ReportCollect)
    
    
    listNameUpdate <- paste0("listRefresh",refreshCounter)
    #assign(listNameUpdate, listHolder)
    Robyn[[listNameUpdate]] <- listHolder
    
    save(Robyn, file = robyn_object)
    
    if(refreshLooper==0) {
      refreshControl <- FALSE
      message("reached maximum available date. no further refresh possible")}
  }
  
  #save(listDTRefresh, listDTRefresh, file = robyn_object)
  invisible(Robyn)
}






