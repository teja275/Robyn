# Copyright (c) Facebook, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

########################################################################
###### Input and setup functions
########################################################################

f.inputDT <- function(data_csv_name = "de_simulated_data.csv"
                      ,holiday_csv_name = "holidays.csv"
                      ,csv_path = script_path
                      ,plot_corrmat = F
) {
  
  dt_input <- fread(paste0(csv_path, data_csv_name))
  dt_holidays <- fread(paste0(csv_path, holiday_csv_name))
  
  if (plot_corrmat) {
    cor_mat <- cor(dt_input[, sapply(dt_input, is.numeric), with =F])
    corrplot(cor_mat, type = "upper", order = "hclust", sig.level = 0.05, insig = "blank",tl.cex = 0.8)
  }
  
  listDT <- list(dt_input=dt_input, dt_holidays=dt_holidays, dt_mod=NULL, dt_modRollWind=NULL)
  assign("listDT", listDT, envir = .GlobalEnv)
  #return(listDT)
}


f.inputParam <- function(listDT = parent.frame()$listDT
                         ,set_dateVarName = NULL # date format must be "2020-01-01"
                         ,set_depVarName = NULL # there should be only one dependent variable
                         #,set_depVarType = "revenue" # "revenue" or "conversion" are allowed
                         
                         ,set_prophet = NULL # "trend","season", "weekday", "holiday" are provided and case-sensitive. Recommended to at least keep Trend & Holidays
                         ,set_prophetVarSign = NULL # c("default", "positive", and "negative"). Recommend as default. Must be same length as set_prophet
                         ,set_prophetCountry = NULL # only one country allowed once. Including national holidays for 59 countries, whose list can be found on our githut guide 
                         
                         ,set_baseVarName = NULL # typically competitors, price & promotion, temperature,  unemployment rate etc
                         ,set_baseVarSign = NULL # c("default", "positive", and "negative"), control the signs of coefficients for baseline variables
                         
                         ,set_mediaVarName = NULL # c("tv_S"	,"ooh_S",	"print_S"	,"facebook_I", "facebook_S"	,"search_clicks_P"	,"search_S") we recommend to use media exposure metrics like impressions, GRP etc for the model. If not applicable, use spend instead
                         ,set_mediaVarSign = NULL # c("default", "positive", and "negative"), control the signs of coefficients for media variables
                         ,set_mediaSpendName = NULL # spends must have same order and same length as set_mediaVarName
                         
                         ,set_factorVarName = NULL # please specify which variable above should be factor, otherwise leave empty c()
                         
                         ################################################################
                         #### set global model parameters
                         
                         ## set cores for parallel computing
                         ,set_cores = 1 # I am using 6 cores from 8 on my local machine. Use detectCores() to find out cores
                         
                         ## set rolling window start (only works for whole dataset for now)
                         ,set_rollingWindowStartDate = NULL 
                         ,set_rollingWindowEndDate = NULL
                         
                         ## set model core features
                         ,adstock = "geometric" # geometric or weibull. weibull is more flexible, yet has one more parameter and thus takes longer
                         ,set_iter = 500  # number of allowed iterations per trial. 500 is recommended
                         
                         ,set_hyperOptimAlgo = "DiscreteOnePlusOne" # selected algorithm for Nevergrad, the gradient-free optimisation library https://facebookresearch.github.io/nevergrad/index.html
                         ,set_trial = 40 # number of allowed iterations per trial. 40 is recommended without calibration, 100 with calibration.
                         ## Time estimation: with geometric adstock, 500 iterations * 40 trials and 6 cores, it takes less than 1 hour. Weibull takes at least twice as much time.
                         
                         ,set_hyperBoundLocal = NULL
                         ,set_lift = data.table(channel = character(), # channel names, allow multiple studies for one channel
                                                liftStartDate = Date(), # must be date format '2020-12-31'
                                                liftEndDate = Date(), # must be date format '2020-12-31'
                                                liftAbs = numeric()) # causal result
                         
) {
  
  ## check listDT existence
  if (is.null(listDT)) {stop("Object listDT is missing. Must run f.inputDT first")}
  
  ## check date input
  inputLen <- length(listDT$dt_input[, get(set_dateVarName)])
  inputLenUnique <- length(unique(listDT$dt_input[, get(set_dateVarName)]))
  
  if (is.null(set_dateVarName) | !(set_dateVarName %in% names(listDT$dt_input)) | length(set_dateVarName)>1) {
    stop("Must provide correct only 1 date variable name for set_dateVarName")
  } else if (any(is.na(as.Date(as.character(listDT$dt_input[, get(set_dateVarName)]), "%Y-%m-%d")))) {
    stop("Date variable in set_dateVarName must have format '2020-12-31'")
  } else if (inputLen != inputLenUnique) {
    stop("Date variable has duplicated dates. Please clean data first")
  } else if (any(apply(listDT$dt_input, 2, function(x) any(is.na(x) | is.infinite(x))))) {
    stop("listDT$dt_input has NA or Inf. Please clean data first")
  }

  listDT$dt_input <- listDT$dt_input[order(as.Date(listDT$dt_input[, get(set_dateVarName)]))]
  dayInterval <- as.integer(difftime(as.Date(listDT$dt_input[, get(set_dateVarName)])[2], as.Date(listDT$dt_input[, get(set_dateVarName)])[1], units = "days"))
  intervalType <- if(dayInterval==1) {"day"} else if (dayInterval==7) {"week"} else if (dayInterval %in% 28:31) {"month"} else {stop("input data has to be daily, weekly or monthly")}
  
  ## check dependent var
  if (is.null(set_depVarName) | !(set_depVarName %in% names(listDT$dt_input)) | length(set_depVarName)>1) {
    stop("Must provide only 1 correct dependent variable name for set_depVarName")
  } else if ( !(is.numeric(listDT$dt_input[, get(set_depVarName)]) | is.integer(listDT$dt_input[, get(set_depVarName)]))) {
    stop("set_depVarName must be numeric or integer")
  }
  
  ## check prophet
  if (is.null(set_prophet)) {
    set_prophetVarSign <- NULL; set_prophetCountry <- NULL
  } else if (!all(set_prophet %in% c("trend","season", "weekday", "holiday"))) {
    stop("allowed values for set_prophet are 'trend', 'season', 'weekday' and 'holiday'")
  } else if (is.null(set_prophetCountry) | length(set_prophetCountry) >1) {
    stop("1 country code must be provided in set_prophetCountry. ",listDT$dt_holidays[, uniqueN(country)], " countries are included: ", paste(listDT$dt_holidays[, unique(country)], collapse = ", "), ". If your country is not available, please add it to the holidays.csv first")
  } else if (is.null(set_prophetVarSign)) {
    set_prophetVarSign <- rep("default", length(set_prophet))
    message("set_prophetVarSign is not provided. 'default' is used")
  } else if (length(set_prophetVarSign) != length(set_prophet) | !all(set_prophetVarSign %in% c("positive", "negative", "default"))) {
    stop("set_prophetVarSign must have same length as set_prophet. allowed values are 'positive', 'negative', 'default'")
  }
  
  ## check baseline variables
  if (is.null(set_baseVarName)) {
    set_baseVarSign <- NULL
  } else if ( !all(set_baseVarName %in% names(listDT$dt_input)) ) {
    stop("Provided set_baseVarName is not included in input data")
  } else if (is.null(set_baseVarSign)) {
    set_baseVarSign <- rep("default", length(set_baseVarName))
    message("set_baseVarSign is not provided. 'default' is used")
  } else if (length(set_baseVarSign) != length(set_baseVarName) | !all(set_baseVarSign %in% c("positive", "negative", "default"))) {
    stop("set_baseVarSign must have same length as set_baseVarName. allowed values are 'positive', 'negative', 'default'")
  }
  
  ## check media variables
  mediaVarCount <- length(set_mediaVarName)
  spendVarCount <- length(set_mediaSpendName)
  if (is.null(set_mediaVarName) | is.null(set_mediaSpendName)) {
    stop("Must provide set_mediaVarName and set_mediaSpendName")
  } else if ( !all(set_mediaVarName %in% names(listDT$dt_input)) ) {
    stop("Provided set_mediaVarName is not included in input data")
  } else if (is.null(set_mediaVarSign)) {
    set_mediaVarSign <- rep("positive", mediaVarCount)
    message("set_mediaVarSign is not provided. 'positive' is used")
  } else if (length(set_mediaVarSign) != mediaVarCount | !all(set_mediaVarSign %in% c("positive", "negative", "default"))) {
    stop("set_mediaVarSign must have same length as set_mediaVarName. allowed values are 'positive', 'negative', 'default'")
  } else if (!all(set_mediaSpendName %in% names(listDT$dt_input))) {
    stop("Provided set_mediaSpendName is not included in input data")
  } else if (spendVarCount != mediaVarCount) {
    stop("set_mediaSpendName must have same length as set_mediaVarName.")
  } 
  
  exposureVarName <- set_mediaVarName[!(set_mediaVarName==set_mediaSpendName)]
  
  ## check set_rollingWindowStartDate & set_rollingWindowEndDate
  if (is.null(set_rollingWindowStartDate)) {
    set_rollingWindowStartDate <- min(as.character(listDT$dt_input[, get(set_dateVarName)]))
  } else if (is.na(as.Date(set_rollingWindowStartDate, "%Y-%m-%d"))) {
    stop("set_rollingWindowStartDate must have format '2020-12-31'")
  } else if (set_rollingWindowStartDate < min(as.character(listDT$dt_input[, get(set_dateVarName)]))) {
    set_rollingWindowStartDate <- min(as.character(listDT$dt_input[, get(set_dateVarName)]))
    message("set_rollingWindowStartDate is smaller than the earliest date in input data. It's set to the earliest date")
  } else if (set_rollingWindowStartDate > max(as.character(listDT$dt_input[, get(set_dateVarName)]))) {
    stop("set_rollingWindowStartDate can't be larger than the the latest date in input data")
  }  
  
  initRollWindStartWhich <- which.min(abs(difftime(as.Date(listDT$dt_input[, get(set_dateVarName)]), as.Date(set_rollingWindowStartDate), units = "days")))
  if (!(as.Date(set_rollingWindowStartDate) %in% listDT$dt_input[, get(set_dateVarName)])) {
    set_rollingWindowStartDate <- listDT$dt_input[initRollWindStartWhich, get(set_dateVarName)]
    message("set_rollingWindowStartDate is adapted to the closest date contained in input data: ", set_rollingWindowStartDate)
  }
  
  if (is.null(set_rollingWindowEndDate)) {
    set_rollingWindowEndDate <- max(as.character(listDT$dt_input[, get(set_dateVarName)]))
  } else if (is.na(as.Date(set_rollingWindowEndDate, "%Y-%m-%d"))) {
    stop("set_rollingWindowEndDate must have format '2020-12-31'")
  } else if (set_rollingWindowEndDate > max(as.character(listDT$dt_input[, get(set_dateVarName)]))) {
    set_rollingWindowEndDate <- max(as.character(listDT$dt_input[, get(set_dateVarName)]))
    message("set_rollingWindowEndDate is larger than the latest date in input data. It's set to the latest date")
  } else if (set_rollingWindowEndDate < set_rollingWindowStartDate) {
    set_rollingWindowEndDate <- max(as.character(listDT$dt_input[, get(set_dateVarName)]))
    message("set_rollingWindowEndDate must be >= set_rollingWindowStartDate. It's set to latest date in input data")
  }
  
  initRollWindEndWhich <- which.min(abs(difftime(as.Date(listDT$dt_input[, get(set_dateVarName)]), as.Date(set_rollingWindowEndDate), units = "days")))
  if (!(as.Date(set_rollingWindowEndDate) %in% listDT$dt_input[, get(set_dateVarName)])) {
    set_rollingWindowEndDate <- listDT$dt_input[initRollWindEndWhich, get(set_dateVarName)]
    message("set_rollingWindowEndDate is adapted to the closest date contained in input data: ", set_rollingWindowEndDate)
  }
  
  initRollWindLength <- initRollWindEndWhich - initRollWindStartWhich +1
  dt_init <- listDT$dt_input[initRollWindStartWhich:initRollWindEndWhich, set_mediaVarName, with =F]
  init_all0 <- colSums(dt_init)==0
  if(any(init_all0)) {
    stop("These media channels contains only 0 within training period ",listDT$dt_input[initRollWindStartWhich, get(set_dateVarName)], " to ", listDT$dt_input[initRollWindEndWhich, get(set_dateVarName)], ": ", paste(names(dt_init)[init_all0], collapse = ", ")
         , " \nRecommendation: adapt listParam$set_rollingWindowStartDate, remove or combine these channels")
  }
  
  ## check adstock
  
  if((adstock %in% c("geometric", "weibull")) == F) {stop("adstock must be 'geometric' or 'weibull'")}
  
  ## check hyperparameter names in set_hyperBoundLocal
  global_name <- c("thetas",  "shapes",  "scales",  "alphas",  "gammas",  "lambdas")
  if (adstock == "geometric") {
    local_name <- sort(apply(expand.grid(set_mediaVarName, global_name[global_name %like% 'thetas|alphas|gammas']), 1, paste, collapse="_"))
  } else if (adstock == "weibull") {
    local_name <- sort(apply(expand.grid(set_mediaVarName, global_name[global_name %like% 'shapes|scales|alphas|gammas']), 1, paste, collapse="_"))
  }
  
  if (is.null(set_hyperBoundLocal) | !identical(sort(names(set_hyperBoundLocal)), local_name)) {
    stop("set_hyperBoundLocal must be a list and contain vectors or values named as followed: ", paste(local_name, collapse = ", "))
  }
  
  
  ## check calibration
  
  if(nrow(set_lift)>0) {
    if ((min(set_lift$liftStartDate) < min(listDT$dt_input[, get(set_dateVarName)])) | (max(set_lift$liftEndDate) >  (max(listDT$dt_input[, get(set_dateVarName)]) + dayInterval-1))) {
      stop("we recommend you to only use lift results conducted within your MMM input data date range")
    } else if (set_iter < 500 | set_trial < 80) {
      message("you are calibrating MMM. we recommend to run at least 500 iterations per trial and at least 80 trials at the beginning")
    }
  } else {
    if (set_iter < 500 | set_trial < 40) {message("\nwe recommend to run at least 500 iterations per trial and at least 40 trials at the beginning")}
  }
  
  ## 
  
  listParam <- list(set_dateVarName=set_dateVarName
                    ,dayInterval=dayInterval
                    ,intervalType=intervalType
                    
                    ,set_depVarName=set_depVarName
                    #,set_depVarType=set_depVarType
                    
                    #,activate_prophet=activate_prophet
                    ,set_prophet=set_prophet
                    ,set_prophetVarSign=set_prophetVarSign 
                    ,set_prophetCountry=set_prophetCountry
                    
                    #,activate_baseline=activate_baseline 
                    ,set_baseVarName=set_baseVarName 
                    ,set_baseVarSign=set_baseVarSign
                    
                    ,set_mediaVarName=set_mediaVarName
                    ,set_mediaVarSign=set_mediaVarSign
                    ,set_mediaSpendName=set_mediaSpendName
                    ,mediaVarCount=mediaVarCount
                    ,exposureVarName=exposureVarName
                    
                    ,set_factorVarName=set_factorVarName
                    
                    ,set_cores=set_cores
                    
                    ,set_rollingWindowStartDate=set_rollingWindowStartDate
                    ,initRollWindStartWhich=initRollWindStartWhich
                    ,set_rollingWindowEndDate=set_rollingWindowEndDate
                    ,initRollWindEndWhich=initRollWindEndWhich
                    ,initRollWindLength=initRollWindLength

                    ,adstock=adstock
                    ,set_iter=set_iter
                    
                    ,set_hyperOptimAlgo=set_hyperOptimAlgo 
                    ,set_trial=set_trial 
                    
                    ,set_hyperBoundLocal=set_hyperBoundLocal 
                    ,local_name=local_name
                    #,activate_calibration=activate_calibration 
                    ,set_lift=set_lift
  )
  
  assign("listParam", listParam, envir = .GlobalEnv)
  assign("listDT", listDT, envir = .GlobalEnv)
}



########################################################################
###### Data transformation and helper functions
########################################################################

################################################################
#### Define training size guidance plot using Bhattacharyya coefficient

f.plotAdstockCurves <- function(plotAdstockCurves, adstock=listParam$adstock) {
  if (plotAdstockCurves) {
    if (adstock == "weibull") {
      weibullCollect <- list()
      shapeVec <- c(2, 2, 2, 2, 2, 2, 0.01, 0.1, 0.5, 1, 1.5, 2)
      scaleVec <- c(0.01, 0.05, 0.1, 0.15, 0.2, 0.5, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)
      paramRotate <- c(rep("scale",6), rep("shape",6))
      
      for (v in 1:length(shapeVec)) {
        dt_weibull<- data.table(x=1:100,
                                decay_accumulated=f.adstockWeibull(rep(1, 100), shape=shapeVec[v], scale=scaleVec[v])$thetaVecCum
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
      
      grid.arrange(p1,p2)
      
    } else if (adstock == "geometric") {
      
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
      print(p3)
    }
  }
}

f.plotResponseCurves <- function(plotResponseCurves) {
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

f.unit_format <- function(x_in) {
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
  }, simplify = T) 
  return(x_out)
}

################################################################
#### Define major input data transformation function

f.featureEngineering <- function(dt_transform = listDT$dt_input, listParam = parent.frame()$listParam) {
  
  dt_input <- copy(dt_transform) # dt_input <- copy(listDT$dt_input)
  dt_inputRollWind <- dt_input[listParam$initRollWindStartWhich:listParam$initRollWindEndWhich] # dt_inputRollWind <- listDT$dt_input[listParam$initRollWindStartWhich:listParam$initRollWindEndWhich]
  
  dt_transform <- copy(dt_transform) # dt_transform <- copy(listDT$dt_input)
  setnames(dt_transform, listParam$set_dateVarName, "ds", skip_absent = T)
  dt_transform <- dt_transform[, ':='(ds= as.Date(ds))][order(ds)]
  dt_transformRollWind <- dt_transform[listParam$initRollWindStartWhich:listParam$initRollWindEndWhich]
  
  setnames(dt_transform, listParam$set_depVarName, "depVar", skip_absent = T) #; listParam$set_depVarName <- "depVar"
  #indepName <- c(listParam$set_prophet, listParam$set_baseVarName, listParam$set_mediaVarName)
  
  ## check date format
  # tryCatch({
  #   dateCheck <- as.Date(dt_transform$ds)
  #   dateCheckStart <- as.Date(listParam$set_rollingWindowStartDate)
  # },
  # error= function(cond) {
  #   stop("input date variable and listParam$set_rollingWindowStartDate should have format '2020-01-01'")
  # })
  
  # if (any(dateCheckStart< min(dt_transform$ds), dateCheckStart> max(dt_transform$ds))) {
  #   stop("listParam$set_rollingWindowStartDate must be between ", min(dt_transform$ds) ," and ",max(dt_transform$ds))
  # }
  
  ## check variables existence
  
  # if (is.null(listParam$set_mediaSpendName)) {stop("listParam$set_mediaSpendName must be specified")
  # } else if(length(listParam$set_mediaVarName) != length(listParam$set_mediaSpendName)) {
  #   stop("listParam$set_mediaSpendName and listParam$set_mediaVarName have to be the same length and same order")}
  
  
  #trainSize <- round(nrow(dt_transform)* set_modTrainSize)
  #dt_train <- dt_transform[1:trainSize, listParam$set_mediaVarName, with =F]
  # trainStartWhich <- which.min(abs(difftime(as.Date(dt_transform$ds), as.Date(listParam$set_rollingWindowStartDate), units = "days")))
  # dt_train <- dt_transform[trainStartWhich:nrow(dt_transform), listParam$set_mediaVarName, with =F]
  # train_all0 <- colSums(dt_train)==0
  # if(any(train_all0)) {
  #   stop("These media channels contains only 0 within training period ",dt_transform$ds[trainStartWhich], " to ", max(dt_transform$ds), ": ", paste(names(dt_train)[train_all0], collapse = ", ")
  #        , " \nRecommendation: adapt listParam$set_rollingWindowStartDate, remove or combine these channels")
  # }
  
  
  #hypName <- c("thetas", "shapes", "scales", "alphas", "gammas", "lambdas") # defind hyperparameter names
  #dayInterval <- as.integer(difftime(sort(unique(dt_transform$ds))[2], sort(unique(dt_transform$ds))[1], units = "days"))
  #intervalType <- if(dayInterval==1) {"day"} else if (dayInterval==7) {"week"} else if (dayInterval %in% 28:31) {"month"} else {stop("input data has to be daily, weekly or monthly")}
  #mediaVarCount <- length(listParam$set_mediaVarName)
  
  ################################################################
  #### model exposure metric from spend
  
  mediaCostFactor <- unlist(dt_inputRollWind[, lapply(.SD, sum), .SDcols = listParam$set_mediaSpendName] / dt_inputRollWind[, lapply(.SD, sum), .SDcols = listParam$set_mediaVarName])
  names(mediaCostFactor) <- listParam$set_mediaVarName
  costSelector <- !(listParam$set_mediaSpendName == listParam$set_mediaVarName)
  names(costSelector) <- listParam$set_mediaVarName
  
  if (any(costSelector)) {
    modNLSCollect <- list()
    yhatCollect <- list()
    plotNLSCollect <- list()
    for (i in 1:listParam$mediaVarCount) {
      if (costSelector[i]) {
        dt_spendModInput <- dt_inputRollWind[, c(listParam$set_mediaSpendName[i],listParam$set_mediaVarName[i]), with =F]
        setnames(dt_spendModInput, names(dt_spendModInput), c("spend", "exposure"))
        #dt_spendModInput <- dt_spendModInput[spend !=0 & exposure != 0]
        
        # scale 0 spend and exposure to a tiny number
        dt_spendModInput[, spend:=as.numeric(spend)][spend==0, spend:=0.01] # remove spend == 0 to avoid DIV/0 error
        dt_spendModInput[, exposure:=as.numeric(exposure)][exposure==0, exposure:=spend / mediaCostFactor[i]] # adapt exposure with avg when spend == 0
        
        # mod_nls <- nls(exposure ~ SSmicmen(spend, Vmax, Km)
        #                ,data = dt_spendModInput
        #                ,control = nls.control(minFactor=1/2048, warnOnly = T))
        
        # estimate starting values for nls
        # modLM <- lm(log(exposure) ~ spend, dt_spendModInput)
        # nlsStartVal <- list(Vmax = exp(coef(modLM)[1]), Km = coef(modLM)[2])
        # nlsStartVal <- list(Vmax = dt_spendModInput[, max(exposure)/2], Km = dt_spendModInput[, max(exposure)])
        # run nls model
        # modNLS <- nlsLM(exposure ~ Vmax * spend/(Km + spend), #Michaelis-Menten model Vmax * spend/(Km + spend)
        #                data = dt_spendModInput,
        #                start = nlsStartVal
        #                ,control = nls.control(warnOnly = T)
        # )
        
        tryCatch(
          {
            #dt_spendModInput[, exposure:= rep(0,(nrow(dt_spendModInput)))]
            nlsStartVal <- list(Vmax = dt_spendModInput[, max(exposure)], Km = dt_spendModInput[, max(exposure)/2])
            suppressWarnings(modNLS <- nlsLM(exposure ~ Vmax * spend/(Km + spend), #Michaelis-Menten model Vmax * spend/(Km + spend)
                                      data = dt_spendModInput,
                                      start = nlsStartVal
                                      ,control = nls.control(warnOnly = T)))
            
            yhatNLS <- predict(modNLS)
            modNLSSum <- summary(modNLS)
            
            # QA nls model prediction
            yhatNLSQA <- modNLSSum$coefficients[1,1] * dt_spendModInput$spend / (modNLSSum$coefficients[2,1] + dt_spendModInput$spend) #exposure = v  * spend / (k + spend)
            identical(yhatNLS, yhatNLSQA)
            
            rsq_nls <- f.rsq(dt_spendModInput$exposure, yhatNLS) 
          },
          
          error=function(cond) {
            # nlsStartVal <- list(Vmax=1, Km=1)
            # suppressWarnings(modNLS <- nlsLM(exposure ~ Vmax * spend/(Km + spend), #Michaelis-Menten model Vmax * spend/(Km + spend)
            #                                  data = dt_spendModInput,
            #                                  start = nlsStartVal
            #                                  ,control = nls.control(warnOnly = T)))
            # warning("default start value for nls out of range. using c(1,1) instead")
            # return(modNLS)
            modNLS <- NULL; yhatNLS <- NULL; modNLSSum <- NULL; rsq_nls <- NULL
            message("michaelis menten fitting for ", listParam$set_mediaVarName[i]," out of range. using lm instead")

          }
        )
        
        # build lm comparison model
        modLM <- lm(exposure ~ spend-1, data = dt_spendModInput)
        yhatLM <- predict(modLM)
        modLMSum <- summary(modLM)
        rsq_lm <- f.rsq(dt_spendModInput$exposure, yhatLM) 
        if (is.na(rsq_lm)) {stop("please check if ",listParam$set_mediaVarName[i]," constains only 0")}
        
        # compare NLS & LM, takes LM if NLS fits worse
        costSelector[i] <- if(is.null(rsq_nls)) {return(FALSE)} else {rsq_nls > rsq_lm}
        
        modNLSCollect[[listParam$set_mediaVarName[i]]] <- data.table(channel = listParam$set_mediaVarName[i],
                                                           Vmax = modNLSSum$coefficients[1,1],
                                                           Km = modNLSSum$coefficients[2,1],
                                                           aic_nls = AIC(modNLS),
                                                           aic_lm = AIC(modLM),
                                                           bic_nls = BIC(modNLS),
                                                           bic_lm = BIC(modLM),
                                                           rsq_nls = rsq_nls,
                                                           rsq_lm = rsq_lm,
                                                           coef_lm = coef(modLMSum)[1]
        )
        
        dt_plotNLS <- data.table(channel = listParam$set_mediaVarName[i],
                                 yhatNLS = if(costSelector[i]) {yhatNLS} else {yhatLM},
                                 yhatLM = yhatLM,
                                 y = dt_spendModInput$exposure,
                                 x = dt_spendModInput$spend)
        dt_plotNLS <- melt.data.table(dt_plotNLS, id.vars = c("channel", "y", "x"), variable.name = "models", value.name = "yhat")
        dt_plotNLS[, models:= str_remove(tolower(models), "yhat")]
        
        yhatCollect[[listParam$set_mediaVarName[i]]] <- dt_plotNLS
        
        # create plot
        plotNLSCollect[[listParam$set_mediaVarName[i]]] <- ggplot(dt_plotNLS, aes(x=x, y=y, color = models)) +
          geom_point() +
          geom_line(aes(y=yhat, x=x, color = models)) +
          labs(subtitle = paste0("y=",listParam$set_mediaVarName[i],", x=", listParam$set_mediaSpendName[i],
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
  
  getSpendSum <- listDT$dt_input[, lapply(.SD, sum), .SDcols=listParam$set_mediaSpendName]
  names(getSpendSum) <- listParam$set_mediaVarName
  getSpendSum <- suppressWarnings(melt.data.table(getSpendSum, measure.vars= listParam$set_mediaVarName, variable.name = "rn", value.name = "spend"))
  
  ################################################################
  #### clean & aggregate data
  
  all_name <- unique(c("ds", "depVar", listParam$set_prophet, listParam$set_baseVarName, listParam$set_mediaVarName #, set_keywordsVarName, listParam$set_mediaSpendName
  ))
  all_mod_name <- c("ds", "depVar", listParam$set_prophet, listParam$set_baseVarName, listParam$set_mediaVarName)
  if(!identical(all_name, all_mod_name)) {stop("Input variables must have unique names")}
  
  ## transform all factor variables

  if (length(listParam$set_factorVarName)>0) {
    dt_transform[, (listParam$set_factorVarName):= as.factor(get(listParam$set_factorVarName)) ]
  } 
  
  ################################################################
  #### Obtain prophet trend, seasonality and changepoints
  
  if ( !is.null(listParam$set_prophet) ) {
    
    if(length(listParam$set_prophet) != length(listParam$set_prophetVarSign)) {stop("listParam$set_prophet and listParam$set_prophetVarSign have to be the same length")}
    if(any(length(listParam$set_prophet)==0, length(listParam$set_prophetVarSign)==0)) {stop("listParam$set_prophet and listParam$set_prophetVarSign must be both specified")}
    if(!(listParam$set_prophetCountry %in% listDT$dt_holidays$country)) {stop("listParam$set_prophetCountry must be already included in the holidays.csv and as ISO 3166-1 alpha-2 abbreviation")}
    
    recurrance <- dt_transform[, .(ds = ds, y = depVar)]
    use_trend <- any(str_detect("trend", listParam$set_prophet))
    use_season <- any(str_detect("season", listParam$set_prophet))
    use_weekday <- any(str_detect("weekday", listParam$set_prophet))
    use_holiday <- any(str_detect("holiday", listParam$set_prophet))
    
    if (listParam$intervalType == "day") {
      
      holidays <- listDT$dt_holidays
      
    } else if (listParam$intervalType == "week") {
      
      weekStartInput <- weekdays(dt_transform[1, ds])
      weekStartMonday <- if(weekStartInput=="Monday") {TRUE} else if (weekStartInput=="Sunday") {FALSE} else {stop("week start has to be Monday or Sunday")}
      listDT$dt_holidays[, dsWeekStart:= cut(as.Date(ds), breaks = listParam$intervalType, start.on.monday = weekStartMonday)]
      holidays <- listDT$dt_holidays[, .(ds=dsWeekStart, holiday, country, year)]
      holidays <- holidays[, lapply(.SD, paste0, collapse="#"), by = c("ds", "country", "year"), .SDcols = "holiday"]
      
    } else if (listParam$intervalType == "month") {
      
      monthStartInput <- all(day(dt_transform[, ds]) ==1)
      if (monthStartInput==FALSE) {stop("monthly data should have first day of month as datestampe, e.g.'2020-01-01' ")}
      listDT$dt_holidays[, dsMonthStart:= cut(as.Date(ds), listParam$intervalType)]
      holidays <- listDT$dt_holidays[, .(ds=dsMonthStart, holiday, country, year)]
      holidays <- holidays[, lapply(.SD, paste0, collapse="#"), by = c("ds", "country", "year"), .SDcols = "holiday"]
      
    }
    
    modelRecurrance<- prophet(recurrance
                             ,holidays = if(use_holiday) {holidays[country==listParam$set_prophetCountry]} else {NULL}
                             ,yearly.seasonality = use_season
                             ,weekly.seasonality = use_weekday
                             ,daily.seasonality= F
                             #,changepoint.range = 0.8
                             #,seasonality.mode = 'multiplicative'
                             #,changepoint.prior.scale = 0.1
    )
    
    #futureDS <- make_future_dataframe(modelRecurrance, periods=1, freq = listParam$intervalType)
    forecastRecurrance <- predict(modelRecurrance, dt_transform[, "ds", with =F])
    
    # if (use_regressor) {
    #   m.recurrance <- cbind(recurrance, dt_transform[, c(listParam$set_baseVarName, listParam$set_mediaVarName), with =F])
    #   modelRecurrance <- prophet(holidays = if(use_holiday) {holidays[country==listParam$set_prophetCountry]} else {NULL}
    #                 ,yearly.seasonality = use_season
    #                 ,weekly.seasonality = use_weekday 
    #                 ,daily.seasonality= F)
    #   for (addreg in c(listParam$set_baseVarName, listParam$set_mediaVarName)) {
    #     modelRecurrance <- add_regressor(modelRecurrance, addreg)
    #   }
    #   modelRecurrance <- fit.prophet(modelRecurrance, m.recurrance)
    #   forecastRecurrance <- predict(modelRecurrance, dt_transform[, c("ds",listParam$set_baseVarName, listParam$set_mediaVarName), with =F])
    #   prophet_plot_components(modelRecurrance, forecastRecurrance)
    # }

    #plot(modelRecurrance, forecastRecurrance)
    #prophet_plot_components(modelRecurrance, forecastRecurrance, render_plot = T)
    
    if (use_trend) {
      fc_trend <- forecastRecurrance$trend[1:NROW(recurrance)]
      recurrance[, trend := scale(fc_trend, center = min(fc_trend), scale = F) + 1]
      dt_transform[, trend := recurrance$trend]
    }
    if (use_season) {
      fc_season <- forecastRecurrance$yearly[1:NROW(recurrance)]
      recurrance[, seasonal := scale(fc_season, center = min(fc_season), scale = F) + 1]
      dt_transform[, season := recurrance$seasonal]
    }
    if (use_weekday) {
      fc_weekday <- forecastRecurrance$weekly[1:NROW(recurrance)]
      recurrance[, weekday := scale(fc_weekday, center = min(fc_weekday), scale = F) + 1]
      dt_transform[, weekday := recurrance$weekday]
    }
    if (use_holiday) {
      fc_holiday <- forecastRecurrance$holidays[1:NROW(recurrance)]
      recurrance[, holidays := scale(fc_holiday, center = min(fc_holiday), scale = F) + 1]
      dt_transform[, holiday := recurrance$holidays]
    }
  }
  
  ################################################################
  #### Finalize input
  
  #dt <- dt[, all_name, with = F]
  dt_transform <- dt_transform[, all_mod_name, with = F]
  
  listDT$dt_mod <- dt_transform
  listDT$dt_modRollWind <- dt_transform[listParam$initRollWindStartWhich:listParam$initRollWindEndWhich]
  listDT$dt_inputRollWind <- dt_inputRollWind
  
  listParam[['modNLSCollect']] <- modNLSCollect
  listParam[['plotNLSCollect']] <- plotNLSCollect  
  listParam[['yhatNLSCollect']] <- yhatNLSCollect  
  listParam[['costSelector']] <- costSelector  
  listParam[['mediaCostFactor']] <- mediaCostFactor  

  #f.checkConditions(dt_transform = dt_transform, listParam = listParam)
  
  assign("listDT", listDT, envir = .GlobalEnv)
  assign("listParam", listParam, envir = .GlobalEnv)
  #return(dt_transform)
}

################################################################
#### Define hyperparameter names extraction function

f.getHyperNames <- function(adstock = parent.frame()$listParam$adstock, set_mediaVarName=parent.frame()$listParam$set_mediaVarName) {
  global_name <- c("thetas",  "shapes",  "scales",  "alphas",  "gammas",  "lambdas")
  if (adstock == "geometric") {
    local_name <- sort(apply(expand.grid(set_mediaVarName, global_name[global_name %like% 'thetas|alphas|gammas']), 1, paste, collapse="_"))
  } else if (adstock == "weibull") {
    local_name <- sort(apply(expand.grid(set_mediaVarName, global_name[global_name %like% 'shapes|scales|alphas|gammas']), 1, paste, collapse="_"))
  }
  return(local_name)
}

################################################
#### Define Michaelis Menten function

f.micMen <- function(x, Vmax, Km, reverse = F) {
  if (!reverse) {
    mm_out <- exposure <- Vmax * x/(Km + x)
  } else {
    mm_out <- spend <- x * Km / (Vmax - x)
  }
  return(mm_out)
}

################################################
#### Define adstock geometric function

f.adstockGeometric <- function(x, theta) {
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

f.adstockWeibull <- function(x, shape , scale) {
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

f.hill <- function(x, alpha, gamma, x_marginal = NULL) {
  
  gammaTrans <- round(quantile(seq(range(x)[1], range(x)[2], length.out = 100), gamma),4)
  
  if (is.null(x_marginal)) {
    x_scurve <-  x**alpha / (x**alpha + gammaTrans**alpha) # plot(x_scurve) summary(x_scurve)
  } else {
    x_scurve <-  x_marginal**alpha / (x_marginal**alpha + gammaTrans**alpha)
  }
  return(x_scurve)
}

# ################################################
# #### Define transformation function
# 
# f.transformation <- function (x, theta= NULL, shape= NULL, scale= NULL, alpha=NULL, gamma=NULL, alternative = listParam$adstock, stage = 3) {
#   
#   ## step 1: add decay rate
#   
#   if (alternative == "geometric") {
#     x_list <- f.adstockGeometric(x, theta)
#     x_decayed <- x_list$x_decayed
#     
#     if (stage == "thetaVecCum") {
#       #thetaVecCum <- theta
#       #for (t in 2:length(x)) {thetaVecCum[t] <- thetaVecCum[t-1]*theta} # plot(thetaVecCum)
#       thetaVecCum <- x_list$thetaVecCum
#     }
#     
#   } else if (alternative == "weibull") {
#     x_list <- f.adstockWeibull(x, shape, scale)
#     x_decayed <- x_list$x_decayed # plot(x_decayed)
#     
#     if (stage == "thetaVecCum") {
#       thetaVecCum <- x_list$thetaVecCum # plot(thetaVecCum)
#     }
#     
#   } else {
#     print("alternative must be geometric or weibull")
#   }
#   
#   ## step 2: normalize decayed independent variable ############ deprecated
#   #x_normalized <- scale(x_decayed, center =F) # plot(x_normalized) summary(x_normalized)
#   x_normalized <- x_decayed
#   
#   ## step 3: s-curve transformation
#   gammaTrans <- round(quantile(seq(range(x_normalized)[1], range(x_normalized)[2], length.out = 100), gamma),4)
#   x_scurve <-  x_normalized**alpha / (x_normalized**alpha + gammaTrans**alpha) # plot(x_scurve) summary(x_scurve)
#   
#   if (stage == 1) {
#     x_out <- x_decayed
#   } else if (stage == 2) {
#     x_out <- x_normalized
#   } else if (stage ==3) {
#     x_out <- x_scurve
#   } else if (stage == "thetaVecCum") {
#     x_out <- thetaVecCum
#   } else {stop("stage must be 1, 2 or 3, indicating adstock, normalization & scurve stages of transformation") }
#   
#   if (sum(is.nan(x_out))>0) {stop("hyperparameters out of range. theta range: 0-1 (excl.1), shape range: 0-5 (excl.0), alpha range: 0-5 (excl.0),  gamma range: 0-1 (excl.0)")}
#   return(x_out)
# }


################################################
#### Define r-squared function

f.rsq <- function(true, predicted) {
  sse <- sum((predicted - true)^2)
  sst <- sum((true - mean(true))^2)
  rsq <- 1 - sse / sst
  return(rsq)
}

################################################
#### Define ridge lambda sequence function

f.lambdaRidge <- function(x, y, seq_len = 100, lambda_min_ratio = 0.0001) {
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

f.decomp <- function(coefs, dt_modSaturated, x, y_pred, i, dt_modRollWind) {
  
  ## input for decomp
  y <- dt_modSaturated$depVar
  indepVar <- dt_modSaturated[, (setdiff(names(dt_modSaturated), "depVar")), with = F]
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
  
  xDecompOutAgg <- sapply(xDecompOut[, c("intercept", indepVarName), with =F], function(x) sum(x))
  xDecompOutAggPerc <- xDecompOutAgg / sum(y_hat)
  xDecompOutAggMeanNon0 <- sapply(xDecompOut[, c("intercept", indepVarName), with =F], function(x) mean(x[x>0]))
  xDecompOutAggMeanNon0[is.nan(xDecompOutAggMeanNon0)] <- 0
  xDecompOutAggMeanNon0Perc <- xDecompOutAggMeanNon0/sum(xDecompOutAggMeanNon0)
  #xDecompOutAggPerc.scaled <- abs(xDecompOutAggPerc)/sum(abs(xDecompOutAggPerc))
  #xDecompOutAgg.scaled <- sum(xDecompOutAgg)*xDecompOutAggPerc.scaled
  
  coefsOut <- data.table(coefs, keep.rownames = T)
  coefsOutCat <- copy(coefsOut)
  coefsOut[, rn := if (length(indepVarCat) == 0) {rn} else {sapply(indepVarCat, function(x) str_replace(coefsOut$rn, paste0(x,".*"), x))}]
  coefsOut <- coefsOut[, .(coef = mean(s0)), by = rn]
  
  decompOutAgg <- cbind(coefsOut, data.table(xDecompAgg = xDecompOutAgg
                                             ,xDecompPerc = xDecompOutAggPerc
                                             ,xDecompMeanNon0 = xDecompOutAggMeanNon0
                                             ,xDecompMeanNon0Perc = xDecompOutAggMeanNon0Perc
                                             #,xDecompAgg.scaled = xDecompOutAgg.scaled
                                             #,xDecompPerc.scaled = xDecompOutAggPerc.scaled
                                             ))
  decompOutAgg[, pos:= xDecompAgg>=0]
  
  decompCollect <- list(xDecompVec= xDecompOut, xDecompVec.scaled=xDecompOut.scaled, xDecompAgg = decompOutAgg, coefsOutCat=coefsOutCat)
  
  return(decompCollect)
} ## decomp end

################################################
#### Define lift calibration function

f.calibrateLift <- function(decompCollect, set_lift=listParam$set_lift) {
  
  check_set_lift <- any(sapply(set_lift$channel, function(x) any(str_detect(x, listParam$set_mediaVarName)))==F) #check if any lift channel doesnt have media var
  if (check_set_lift) {stop("set_lift channels must have media variable")}
  ## prep lift input
  getLiftMedia <- unique(set_lift$channel)
  getDecompVec <- decompCollect$xDecompVec
  
  ## loop all lift input
  liftCollect <- list()
  for (m in 1:length(getLiftMedia)) { # loop per lift channel
    
    liftWhich <- str_which(set_lift$channel, getLiftMedia[m])
    
    liftCollect2 <- list()
    for (lw in 1:length(liftWhich)) { # loop per lift test per channel
      
      ## get lift period subset
      liftStart <- set_lift[liftWhich[lw], liftStartDate]
      liftEnd <- set_lift[liftWhich[lw], liftEndDate]
      liftPeriodVec <- getDecompVec[ds >= liftStart & ds <= liftEnd, c("ds", getLiftMedia[m]), with = F]
      liftPeriodVecDependent <- getDecompVec[ds >= liftStart & ds <= liftEnd, c("ds", "y"), with = F]
      
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
                                       liftAbs = set_lift[liftWhich[lw], liftAbs],
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
f.refit <- function(x_train, y_train, lambda, lower.limits, upper.limits) {
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
  
  y_trainPred <- predict(mod, s = lambda, newx = x_train)
  rsq_train<- f.rsq(true = y_train, predicted = y_trainPred); rsq_train
  
  #y_testPred <- predict(mod, s = lambda, newx = x_test)
  #rsq_test <- f.rsq(true = y_test, predicted = y_testPred); rsq_test
  
  #mape_mod<- mean(abs((y_test - y_testPred)/y_test)* 100); mape_mod
  coefs <- as.matrix(coef(mod))
  #y_pred <- c(y_trainPred, y_testPred)
  
  nrmse_train <- sqrt(mean(sum((y_train - y_trainPred)^2))) / (max(y_train) - min(y_train)) # mean(y_train) sd(y_train)
  #nrmse_test <- sqrt(mean(sum((y_test - y_testPred)^2))) / (max(y_test) - min(y_test)) # mean(y_test) sd(y_test)
  
  mod_out <- list(rsq_train = rsq_train
                  #,rsq_test = rsq_test
                  ,nrmse_train = nrmse_train
                  #,nrmse_test = nrmse_test
                  #,mape_mod = mape_mod
                  ,coefs = coefs
                  ,y_pred = y_trainPred
                  ,mod=mod)
  
  return(mod_out)
}

################################################################
#### Define major mmm function

f.mmm <- function(...
                  , listParam = parent.frame()$listParam
                  , listDT = parent.frame()$listDT
                  , set_iter = parent.frame()$listParam$set_iter
                  , lambda.n = 100
                  , fixed.out = F
                  , fixed.lambda = NULL
) {
  
  ################################################
  #### Collect hyperparameters
  
  hypParamSamName <- f.getHyperNames()
  
  if (fixed.out==F) {
    input.collect <- unlist(list(...), recursive = F) # input.collect <- listParam$set_hyperBoundLocal
    
    # sort hyperparameter list by name
    hyper_bound_local <- list()
    for (i in 1:length(hypParamSamName)) {
      hyper_bound_local[i] <- input.collect[hypParamSamName[i]]
      names(hyper_bound_local)[i] <- hypParamSamName[i]
    }
    
    # get hyperparameters for Nevergrad
    bounds_ng <- which(sapply(hyper_bound_local, length)==2)
    hyper_bound_local_ng <- hyper_bound_local[bounds_ng]
    hyper_bound_local_ng_name <- names(hyper_bound_local_ng)
    num_hyppar_ng <- length(hyper_bound_local_ng)
    if (num_hyppar_ng == 0) {fixed.out <- T}
    
    # get fixed hyperparameters
    bounds_fixed <- which(sapply(hyper_bound_local, length)==1)
    hyper_bound_local_fixed <- hyper_bound_local[bounds_fixed]
    hyper_bound_local_fixed_name <- names(hyper_bound_local_fixed)  
    num_hyppar_fixed <- length(hyper_bound_local_fixed)
    
    #hyper_bound_local_fixed <- list(print_S_alphas = 1 , print_S_gammas = 0.5)
    if (listParam$set_cores >1) {
      hyper_bound_local_fixed_dt <- data.table(sapply(hyper_bound_local_fixed, function(x) rep(x, listParam$set_cores)))
    } else {
      hyper_bound_local_fixed_dt <- as.data.table(matrix(hyper_bound_local_fixed, nrow = 1))
      names(hyper_bound_local_fixed_dt) <- hyper_bound_local_fixed_name
    }
    
  } else {
    #input.collect <- listParam$set_hyperBoundLocal
    #input.fixed <- dt_hyperResult
    input.fixed <- list(...)[[1]]
    num_hyppar_ng <- length(hypParamSamName)
    hyper_bound_local_ng <- NULL
    hyper_bound_local_ng_name <- NULL
    hyper_bound_local_fixed <- NULL
    fixed.lambda
  }
  
  ################################################
  #### Setup environment
  
  if (is.null(listDT$dt_mod)) {stop("Run listDT$dt_mod <- f.featureEngineering() first to get the dt_mod")}
  
  ## get environment for parallel backend
  dt_mod <- copy(listDT$dt_mod)
  initRollWindStartWhich <- listParam$initRollWindStartWhich
  initRollWindEndWhich <- listParam$initRollWindEndWhich
  dt_modRollWind <- copy(listDT$dt_modRollWind)
  
  set_mediaVarName <- listParam$set_mediaVarName
  adstock <- listParam$adstock
  set_baseVarSign <- listParam$set_baseVarSign
  set_mediaVarSign <- listParam$set_mediaVarSign
  set_prophetVarSign <- listParam$set_prophetVarSign
  #set_factorVarName <- listParam$set_factorVarName
  set_lift <- listParam$set_lift
  optimizer_name <- listParam$set_hyperOptimAlgo
  
  ng <- import("nevergrad")
  
  # available optimizers in ng
  # optimizer_name <- "DoubleFastGADiscreteOnePlusOne"
  # optimizer_name <- "OnePlusOne"
  # optimizer_name <- "DE"
  # optimizer_name <- "RandomSearch"
  # optimizer_name <- "TwoPointsDE"
  # optimizer_name <- "Powell"
  # optimizer_name <- "MetaModel"  CRASH !!!!
  # optimizer_name <- "SQP"
  # optimizer_name <- "Cobyla"
  # optimizer_name <- "NaiveTBPSA"
  # optimizer_name <- "DiscreteOnePlusOne"
  # optimizer_name <- "cGA"
  # optimizer_name <- "ScrHammersleySearch"
  
  ################################################
  #### Get spend share
  
  #trainStartWhich <- which.min(abs(difftime(as.Date(dt_mod$ds), as.Date(listParam$set_rollingWindowStartDate), units = "days")))
  #dt_inputTrain <- listDT$dt_input[listDT$dt_input[, rank(.SD), .SDcols = listParam$set_dateVarName]]
  #dt_inputTrain <- dt_inputTrain[trainStartWhich:nrow(dt_inputTrain)]
  dt_inputTrain <- listDT$dt_input[listParam$initRollWindStartWhich:listParam$initRollWindEndWhich]
  dt_spendShare <- dt_inputTrain[, .(rn = listParam$set_mediaVarName,
                                     total_spend = sapply(.SD, sum),
                                     mean_spend = sapply(.SD, function(x) mean(x[x>0]))), .SDcols=listParam$set_mediaSpendName]
  dt_spendShare[, ':='(spend_share = mean_spend / sum(mean_spend))]
  
  ################################################
  #### Start Nevergrad loop
  
  t0 <- Sys.time()
  
  ## set iterations
  if (fixed.out == F) {
    iterTotal <- set_iter
    iterPar <- listParam$set_cores
  } else if (num_hyppar_ng==0 & fixed.out == T) {
    iterTotal <- 1
    iterPar <- 1
  } else {
    iterTotal <- nrow(input.fixed)
    iterPar <- nrow(input.fixed)
  }
  iterNG <-  ifelse(fixed.out == F, ceiling(set_iter/listParam$set_cores), 1)
  
  cat("\nRunning", iterTotal,"iterations with evolutionary algorithm on",adstock, "adstocking,", length(hyper_bound_local_ng),"hyperparameters,",lambda.n,"-fold ridge x-validation using",listParam$set_cores,"cores...\n")
  
  ## start Nevergrad optimiser
  
  if (length(hyper_bound_local_ng) !=0) {
    my_tuple <- tuple(num_hyppar_ng)
    instrumentation <- ng$p$Array(shape=my_tuple)
    instrumentation$set_bounds(0., 1.)
    optimizer <-  ng$optimizers$registry[optimizer_name](instrumentation, budget=iterTotal, num_workers=listParam$set_cores)
    if (nrow(set_lift)==0) {
      optimizer$tell(ng$p$MultiobjectiveReference(), tuple(1.0, 1.0))
    } else {
      optimizer$tell(ng$p$MultiobjectiveReference(), tuple(1.0, 1.0, 1.0))
    }
    # Creating a hyperparameter vector to be used in the next learning.
  }
  
  ## start loop
  
  resultCollectNG <- list()
  cnt <- 0
  cat('\n',"Working with: ", optimizer_name,'\n')
  if(fixed.out==F) {pb <- txtProgressBar(max = iterTotal, style = 3)}
  #opts <- list(progress = function(n) setTxtProgressBar(pb, n))
  sysTimeDopar <- system.time({
    for (lng in 1:iterNG) {
      
      nevergrad_hp <- list()
      nevergrad_hp_val <- list()
      hypParamSamList <- list()
      hypParamSamNG <- c()
      
      if (fixed.out == F) {
        for (co in 1:iterPar) {
          
          ## get hyperparameter sample with ask
          nevergrad_hp[[co]] <- optimizer$ask()
          nevergrad_hp_val[[co]] <- nevergrad_hp[[co]]$value
          
          ## scale sample to given bounds
          for (hypNameLoop in hyper_bound_local_ng_name) { # hypNameLoop <- local_name.all[1]
            index <- which(hypNameLoop == hyper_bound_local_ng_name)
            channelBound <- unlist(hyper_bound_local_ng[hypNameLoop])
            hyppar_for_qunif <- nevergrad_hp_val[[co]][index]  
            hyppar_scaled <- qunif(hyppar_for_qunif, min(channelBound), max(channelBound))  
            hypParamSamNG[hypNameLoop] <- hyppar_scaled 
          }
          hypParamSamList[[co]] <- transpose(data.table(hypParamSamNG))
        }
        
        hypParamSamNG<- rbindlist(hypParamSamList)
        hypParamSamNG <- setnames(hypParamSamNG, names(hypParamSamNG), hyper_bound_local_ng_name)
        
        ## add fixed hyperparameters
        
        if (num_hyppar_fixed != 0) {
          hypParamSamNG <- cbind(hypParamSamNG, hyper_bound_local_fixed_dt)
          hypParamSamNG <- setcolorder(hypParamSamNG, hypParamSamName)
        }
      } else if (num_hyppar_ng==0 & fixed.out == T) {
        hypParamSamNG <- as.data.table(matrix(unlist(hyper_bound_local), nrow = 1))
        setnames(hypParamSamNG, names(hypParamSamNG), hypParamSamName)
      } else {
        hypParamSamNG <- input.fixed[, hypParamSamName, with = F]
      }
      
      ## Parallel start
      
      nrmse.collect <- c()
      decomp.rssd.collect <- c()
      best_mape <- Inf
      closeAllConnections()
      registerDoParallel(listParam$set_cores)  #registerDoParallel(cores=listParam$set_cores)
      getDoParWorkers()
      doparCollect <- foreach (
        i = 1:iterPar
        , .export = c('f.adstockGeometric'
                      , 'f.adstockWeibull'
                      , 'f.hill'
                      #, 'f.transformation'
                      , 'f.rsq'
                      , 'f.decomp'
                      , 'f.calibrateLift'
                      , 'f.lambdaRidge'
                      , 'f.refit')
        , .packages = c('glmnet'
                        ,'stringr'
                        ,'data.table'
        )
        #, .options.snow = opts
      )  %dopar%  {
        
        t1 <- Sys.time()
        
        #####################################
        #### Get hyperparameter sample
        
        # f.transformation <- function(hypParamSam, dt_mod, set_mediaVarName, initRollWindStartWhich, initRollWindEndWhich) {
        #   
        # }

        hypParamSam <- unlist(hypParamSamNG[i])
        
        #### Tranform media with hyperparameters
        dt_modAdstocked <- dt_mod[, .SD, .SDcols = setdiff(names(dt_mod), "ds")]
        mediaAdstocked <- list()
        mediaVecCum <- list()
        mediaSaturated <- list()
        for (v in 1:length(set_mediaVarName)) {
          
          m <- dt_modAdstocked[, get(set_mediaVarName[v])]
          
          ## adstocking
          
          if (adstock == "geometric") {
            
            theta = hypParamSam[paste0(set_mediaVarName[v],"_thetas")]
            x_list <- f.adstockGeometric(x=m, theta = theta)
            
          } else if (adstock == "weibull") {
            
            shape = hypParamSam[paste0(set_mediaVarName[v],"_shapes")]
            scale = hypParamSam[paste0(set_mediaVarName[v],"_scales")]
            x_list <- f.adstockWeibull(x=m, shape = shape, scale=scale)
            
          } else {break; print("adstock parameter must be geometric or weibull")}
          
          m_adstocked <- x_list$x_decayed
          mediaAdstocked[[v]] <- m_adstocked
          mediaVecCum[[v]] <- x_list$thetaVecCum
          
          ## saturation
          m_adstockedRollWind <- m_adstocked[initRollWindStartWhich:initRollWindEndWhich]
          
          alpha = hypParamSam[paste0(set_mediaVarName[v],"_alphas")]
          gamma = hypParamSam[paste0(set_mediaVarName[v],"_gammas")]
          mediaSaturated[[v]] <- f.hill(m_adstockedRollWind, alpha = alpha, gamma = gamma)
        }
        
        names(mediaAdstocked) <- set_mediaVarName
        dt_modAdstocked[, (set_mediaVarName) := mediaAdstocked]
        dt_mediaVecCum <- data.table()[, (set_mediaVarName):= mediaVecCum]
        
        names(mediaSaturated) <- set_mediaVarName
        dt_modSaturated <- dt_modAdstocked[initRollWindStartWhich:initRollWindEndWhich]
        dt_modSaturated[, (set_mediaVarName) := mediaSaturated]

        #####################################
        #### Split and prepare data for modelling
        
        #trainSize <- round(nrow(dt_modSaturated)* set_modTrainSize)
        #dt_train <- dt_modSaturated[1:trainSize]
        #dt_test <- dt_modSaturated[(trainSize+1):nrow(dt_modSaturated)]
        #trainStartWhich <- which.min(abs(difftime(as.Date(dt_mod$ds), as.Date(listParam$set_rollingWindowStartDate), units = "days")))
        dt_train <- copy(dt_modSaturated)
        
        ## contrast matrix because glmnet does not treat categorical variables
        y_train <- dt_train$depVar
        x_train <- model.matrix(depVar ~., dt_train)[, -1]
        #y_test <- dt_test$depVar
        #x_test <- model.matrix(depVar ~., dt_test)[, -1]
        #y <- c(y_train, y_test)
        #x <- rbind(x_train, x_test)
        
        ## create lambda sequence with x and y
        # lambda_seq <- f.lambdaRidge(x=x_train, y=y_train, seq_len = lambda.n, lambda_min_ratio = 0.0001)
        
        ## define sign control
        dt_sign <- dt_modSaturated[, !"depVar"] #names(dt_sign)
        #x_sign <- if (activate_prophet) {c(set_prophetVarSign, set_baseVarSign, set_mediaVarSign)} else {c(set_baseVarSign, set_mediaVarSign)}
        x_sign <- c(set_prophetVarSign, set_baseVarSign, set_mediaVarSign)
        check_factor <- sapply(dt_sign, is.factor)
        
        lower.limits <- c(); upper.limits <- c()
        
        for (s in 1:length(check_factor)) {
          
          if (check_factor[s]==T) {
            level.n <- length(levels(unlist(dt_sign[, s, with=F])))
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
        #head(predict(cvmod, newx=x_train, s="lambda.1se"))
        #cbind(coef(cvmod1, s = "lambda.min"), coef(cvmod2, s = "lambda.min"), coef(cvmod3, s = "lambda.min"), coef(cvmod4, s = "lambda.min"))
        
        #####################################
        #### refit ridge regression with selected lambda from x-validation

          
          ## if no lift calibration, refit using best lambda
          if (fixed.out == F) {
            mod_out <- f.refit(x_train, y_train, lambda=cvmod$lambda.1se, lower.limits, upper.limits)
            lambda <- cvmod$lambda.1se
          } else {
            mod_out <- f.refit(x_train, y_train, lambda=fixed.lambda[i], lower.limits, upper.limits)
            lambda <- fixed.lambda[i]
          }

          #hypParamSam["lambdas"] <- cvmod$lambda.1se
          #hypParamSamName <- names(hypParamSam)
          
          decompCollect <- f.decomp(coefs=mod_out$coefs, dt_modSaturated=dt_modSaturated, x=x_train, y_pred=mod_out$y_pred, i=i, dt_modRollWind=dt_modRollWind)
          nrmse <- mod_out$nrmse_train
          mape <- 0
          
          
          #####################################
          #### get calibration mape
          
         if (nrow(set_lift)>0) {

            liftCollect <- f.calibrateLift(decompCollect=decompCollect, set_lift=set_lift)
            mape <- liftCollect[, mean(mape_lift)]
          
        }
        
        #####################################
        #### calculate multi-objectives for pareto optimality
        
        ## decomp objective: sum of squared distance between decomp share and spend share to be minimised
        dt_decompSpendDist <- decompCollect$xDecompAgg[rn %in% set_mediaVarName, .(rn, xDecompPerc, xDecompMeanNon0Perc, xDecompMeanNon0)]
        dt_decompSpendDist <- dt_decompSpendDist[dt_spendShare[, .(rn, spend_share, mean_spend, total_spend)], on = "rn"]
        dt_decompSpendDist[, effect_share:= xDecompMeanNon0Perc/sum(xDecompMeanNon0Perc)]
        decomp.rssd <- dt_decompSpendDist[, sqrt(sum((effect_share-spend_share)^2))]
        if (is.nan(decomp.rssd)) {
          message("all media in this iteration have 0 coefficients")
          decomp.rssd <- Inf
          dt_decompSpendDist[, effect_share:=0]
        }
        
        ## adstock objective: sum of squared infinite sum of decay to be minimised? maybe not necessary
        dt_decaySum <- dt_mediaVecCum[,  .(rn = set_mediaVarName, decaySum = sapply(.SD, sum)), .SDcols = set_mediaVarName]
        adstock.ssisd <- dt_decaySum[, sum(decaySum^2)]
        
        ## calibration objective: not calibration: mse, decomp.rssd, if calibration: mse, decom.rssd, mape_lift
        
        #####################################
        #### Collect output
        
        resultHypParam <- data.table()[, (hypParamSamName):= lapply(hypParamSam[1:length(hypParamSamName)], function(x) x)]
        
        resultCollect <- list(
          resultHypParam = resultHypParam[, ':='(mape = mape
                                                 ,nrmse = nrmse
                                                 ,decomp.rssd = decomp.rssd
                                                 ,adstock.ssisd = adstock.ssisd
                                                 ,rsq_train = mod_out$rsq_train
                                                 #,rsq_test = mod_out$rsq_test
                                                 ,pos = prod(decompCollect$xDecompAgg$pos)
                                                 ,lambda=lambda
                                                 #,Score = -mape
                                                 ,Elapsed = as.numeric(difftime(Sys.time(),t1, units = "secs"))
                                                 ,ElapsedAccum = as.numeric(difftime(Sys.time(),t0, units = "secs"))
                                                 ,iterPar= i
                                                 ,iterNG = lng)],
          xDecompVec = if (fixed.out == T) {decompCollect$xDecompVec[, ':='(mape = mape
                                                                      ,nrmse = nrmse
                                                                      ,decomp.rssd = decomp.rssd
                                                                      ,adstock.ssisd = adstock.ssisd
                                                                      ,rsq_train = mod_out$rsq_train
                                                                      #,rsq_test = mod_out$rsq_test
                                                                      ,lambda=lambda
                                                                      ,iterPar= i
                                                                      ,iterNG = lng)]} else{NULL} ,
          xDecompAgg = decompCollect$xDecompAgg[, ':='(mape = mape
                                                       ,nrmse = nrmse
                                                       ,decomp.rssd = decomp.rssd
                                                       ,adstock.ssisd = adstock.ssisd
                                                       ,rsq_train = mod_out$rsq_train
                                                       #,rsq_test = mod_out$rsq_test
                                                       ,lambda=lambda
                                                       ,iterPar= i
                                                       ,iterNG = lng)] ,
          liftCalibration = if (nrow(set_lift)>0) {liftCollect[, ':='(mape = mape
                                                                          ,nrmse = nrmse
                                                                          ,decomp.rssd = decomp.rssd
                                                                          ,adstock.ssisd = adstock.ssisd
                                                                          ,rsq_train = mod_out$rsq_train
                                                                          #,rsq_test = mod_out$rsq_test
                                                                          ,lambda=lambda
                                                                          ,iterPar= i
                                                                          ,iterNG = lng)] } else {NULL},
          decompSpendDist = dt_decompSpendDist[, ':='(mape = mape
                                                      ,nrmse = nrmse
                                                      ,decomp.rssd = decomp.rssd
                                                      ,adstock.ssisd = adstock.ssisd
                                                      ,rsq_train = mod_out$rsq_train
                                                      #,rsq_test = mod_out$rsq_test
                                                      ,lambda=lambda
                                                      ,iterPar= i
                                                      ,iterNG = lng)],
          mape.lift = mape,
          nrmse = nrmse,
          decomp.rssd = decomp.rssd,
          iterPar = i,
          iterNG = lng
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
      
      nrmse.coolect <- sapply(doparCollect, function(x) x$nrmse)
      decomp.rssd.coolect <- sapply(doparCollect, function(x) x$decomp.rssd)
      mape.lift.coolect <- sapply(doparCollect, function(x) x$mape.lift)
      
      
      #####################################
      #### Nevergrad tells objectives
      
      if (fixed.out == F) {
        if (nrow(set_lift)==0) {
          for (co in 1:iterPar) {
            optimizer$tell(nevergrad_hp[[co]], tuple(nrmse.coolect[co], decomp.rssd.coolect[co])) 
          }
        } else {
          for (co in 1:iterPar) {
            optimizer$tell(nevergrad_hp[[co]], tuple(nrmse.coolect[co], decomp.rssd.coolect[co], mape.lift.coolect[co])) 
          }
        }
      }
      
      resultCollectNG[[lng]] <- doparCollect
      cnt <- cnt + iterPar
      if(fixed.out==F) {setTxtProgressBar(pb, cnt)}
      
    } ## end NG loop
  }) # end system.time
  
  cat("\n Finished in",sysTimeDopar[3]/60,"mins\n")
  if(fixed.out==F) {close(pb)}
  registerDoSEQ(); getDoParWorkers()
  
  #####################################
  #### Get nevergrad pareto results 
  
  if (fixed.out == F) {
    pareto_results<-transpose(rbind(as.data.table(sapply(optimizer$pareto_front(997, subset="domain-covering", subset_tentatives=500), function(p) round(p$value[],4))),
                                    as.data.table(sapply(optimizer$pareto_front(997, subset="domain-covering", subset_tentatives=500), function(p) round(p$losses[],4)))))
    if (nrow(set_lift)==0) {
      pareto_results_names<-setnames(pareto_results, old=names(pareto_results), new=c(hyper_bound_local_ng_name,"nrmse", "decomp.rssd") )
      pareto_results_ordered<-setorder(pareto_results_names, "nrmse", "decomp.rssd")
    } else {
      pareto_results_names<-setnames(pareto_results, old=names(pareto_results), new=c(hyper_bound_local_ng_name,"nrmse", "decomp.rssd", "mape.lift") )
      pareto_results_ordered<-setorder(pareto_results_names, "nrmse", "decomp.rssd", "mape.lift")
    }
    #print(pareto_results_ordered)
  } else {
    pareto_results_ordered <- NULL
  }

  #####################################
  #### Final result collect
  
  resultCollect <- list(
    resultHypParam = rbindlist(lapply(resultCollectNG, function(x) {rbindlist(lapply(x, function(y) y$resultHypParam))}))[order(nrmse)],
    xDecompVec = if (fixed.out==T) {rbindlist(lapply(resultCollectNG, function(x) {rbindlist(lapply(x, function(y) y$xDecompVec))}))[order(nrmse, ds)]} else {NULL},
    xDecompAgg =   rbindlist(lapply(resultCollectNG, function(x) {rbindlist(lapply(x, function(y) y$xDecompAgg))}))[order(nrmse)],
    liftCalibration = if(nrow(set_lift)>0) {rbindlist(lapply(resultCollectNG, function(x) {rbindlist(lapply(x, function(y) y$liftCalibration))}))[order(mape, liftMedia, liftStart)]} else {NULL},
    decompSpendDist = rbindlist(lapply(resultCollectNG, function(x) {rbindlist(lapply(x, function(y) y$decompSpendDist))}))[order(nrmse)],
    #mape = unlist(lapply(doparCollect, function(x) x$mape)),
    #iterRS = unlist(lapply(doparCollect, function(x) x$iterRS)),
    paretoFront= as.data.table(pareto_results_ordered)
    #,cvmod = lapply(doparCollect, function(x) x$cvmod)
  )
  resultCollect$iter <- length(resultCollect$mape)
  #resultCollect$best.iter <- resultCollect$resultHypParam$iterRS[1]
  resultCollect$elapsed.min <- sysTimeDopar[3]/60
  resultCollect$resultHypParam[, ElapsedAccum:= ElapsedAccum - min(ElapsedAccum) + resultCollect$resultHypParam[which.min(ElapsedAccum), Elapsed]] # adjust accummulated time
  resultCollect$resultHypParam
  #print(optimizer_name)
  #print(" get ")
  #please_stop_here()
  
  return(list(#Score =  -resultCollect$mape[iterRS], # score for BO
    resultCollect = resultCollect
    ,hyperBoundNG = hyper_bound_local_ng
    ,hyperBoundFixed = hyper_bound_local_fixed))
}



#####################################
#### Define f.robyn, the main trial looping and plotting function


f.robyn <- function(listParam = parent.frame()$listParam
                    ,listDT = parent.frame()$listDT
                    ,plot_folder = getwd() # plot_folder = "~/Documents/GitHub/plots"
                    ,fixed.out = F
                    ,fixed.hyppar.dt = NULL
                    ,pareto_fronts = 3) {
  
  #####################################
  #### Set local environment
  
  t0 <- Sys.time()
  
  if (!dir.exists(plot_folder)) {
    plot_folder <- getwd()
    message("provided plot_folder doesn't exist. Using default plot_folder = getwd(): ", getwd())
  }
  
  dt_mod <- copy(listDT$dt_mod)
  dt_modRollWind <- copy(listDT$dt_modRollWind)
  
  message("Input data has ", nrow(dt_mod), " ", listParam$intervalType, "s in total: "
          , dt_mod[, min(ds)], " to ", dt_mod[, max(ds)])
  message("Initial model is built on rolling window of ", listParam$initRollWindLength, " ", listParam$intervalType,"s: "
          , listParam$set_rollingWindowStartDate, " to ", listParam$set_rollingWindowEndDate)
  
  #####################################
  #### Run f.mmm on set_trials

  hyperparameter_fixed <- all(sapply(listParam$set_hyperBoundLocal, length)==1)
  hypParamSamName <- f.getHyperNames()
  
  if (fixed.out == T) {
    
    ## Run f.mmm if using old model result tables
    
    if (is.null(fixed.hyppar.dt)) {stop("when fixed.out=T, please provide the table model_output_collect$resultHypParam from previous runs or pareto_hyperparameters.csv with desired model IDs")}
    if (!all(c(hypParamSamName, "lambda") %in% names(fixed.hyppar.dt))) {stop("fixed.hyppar.dt is provided with wrong input. please provide the table model_output_collect$resultHypParam from previous runs or pareto_hyperparameters.csv with desired model ID")}
    
    model_output_collect <- list()
    model_output_collect[[1]] <- f.mmm(fixed.hyppar.dt[, hypParamSamName, with = F]
                                       #,set_iter = set_iter
                                       #,set_cores = set_cores
                                       ,optimizer_name = listParam$set_hyperOptimAlgo
                                       ,fixed.out = T
                                       ,fixed.lambda = unlist(fixed.hyppar.dt$lambda))
    
    model_output_collect[[1]]$trials <- 1
    model_output_collect[[1]]$resultCollect$resultHypParam <- model_output_collect[[1]]$resultCollect$resultHypParam[order(iterPar)]
    
    dt_IDmatch <- data.table(solID = fixed.hyppar.dt$solID, 
                             iterPar = model_output_collect[[1]]$resultCollect$resultHypParam$iterPar)
    
    model_output_collect[[1]]$resultCollect$resultHypParam[dt_IDmatch, on =.(iterPar), "solID" := .(i.solID)]
    model_output_collect[[1]]$resultCollect$xDecompAgg[dt_IDmatch, on =.(iterPar), "solID" := .(i.solID)]
    model_output_collect[[1]]$resultCollect$xDecompVec[dt_IDmatch, on =.(iterPar), "solID" := .(i.solID)]
    model_output_collect[[1]]$resultCollect$decompSpendDist[dt_IDmatch, on =.(iterPar), "solID" := .(i.solID)]
    
    cat("\n######################\nHyperparameters are all fixed\n######################\n")
    print(model_output_collect[[1]]$resultCollect$xDecompAgg)
    
  } else if (hyperparameter_fixed) {
    
    ## Run f.mmm on set_trials if hyperparameters are all fixed
    model_output_collect <- list()
    model_output_collect[[1]] <- f.mmm(listParam$set_hyperBoundLocal
                                       ,set_iter = 1
                                       #,set_cores = 1
                                       #,optimizer_name = optimizer_name
    )
    model_output_collect[[1]]$trials <- 1
    
    cat("\n######################\nHyperparameters are all fixed\n######################\n")
    print(model_output_collect[[1]]$resultCollect$xDecompAgg)
    

  } else {
    
    ## Run f.mmm on set_trials if hyperparameters are not all fixed
    
    ng_out <- list()
    ng_algos <- listParam$set_hyperOptimAlgo # c("DoubleFastGADiscreteOnePlusOne", "DiscreteOnePlusOne", "TwoPointsDE", "DE")
    
    t0 <- Sys.time()
    for (optmz in ng_algos) {
      ng_collect <- list()
      model_output_collect <- list()
      
      for (ngt in 1:listParam$set_trial) { 
        
        if (nrow(listParam$set_lift)==0) {
          cat("\nRunning trial nr.", ngt,"out of",listParam$set_trial,"...\n")
        } else {
          cat("\nRunning trial nr.", ngt,"out of",listParam$set_trial,"with calibration...\n")
          
        }
        # rm(model_output)
        model_output <- f.mmm(listParam$set_hyperBoundLocal
                              #,set_iter = set_iter
                              #,set_cores = set_cores
                              #,optimizer_name = optmz
        )
        
        check_coef0 <- any(model_output$resultCollect$decompSpendDist$decomp.rssd == Inf)
        if (check_coef0) {
          num_coef0_mod <- model_output$resultCollect$decompSpendDist[decomp.rssd == Inf, uniqueN(paste0(iterNG,"_",iterPar))]
          num_coef0_mod <- ifelse(num_coef0_mod>listParam$set_iter, listParam$set_iter, num_coef0_mod)
          message("\nThis trial contains ", num_coef0_mod," iterations with all 0 media coefficient. Please reconsider your media variable choice if the pareto choices are unreasonable.
                  \nRecommendations are: \n1. increase hyperparameter ranges for 0-coef channels on theta (max.reco. c(0, 0.9) ) and gamma (max.reco. c(0.1, 1) ) to give Robyn more freedom\n2. split media into sub-channels, and/or aggregate similar channels, and/or introduce other media\n3. increase trials to get more samples\n")
        }
        
        model_output["trials"] <- ngt
        ng_collect[[ngt]] <- model_output$resultCollect$paretoFront[, ':='(trials=ngt, iters = listParam$set_iter, ng_optmz = optmz)]
        model_output_collect[[ngt]] <- model_output
        #model_output_pareto <- f.mmm(listParam$set_hyperBoundLocal, out = T)
      }
      ng_collect <- rbindlist(ng_collect)
      px <- low(ng_collect$nrmse) * low(ng_collect$decomp.rssd)
      ng_collect <- psel(ng_collect, px, top = nrow(ng_collect))[order(trials, nrmse)]
      ng_out[[which(ng_algos==optmz)]] <- ng_collect
    }
    ng_out <- rbindlist(ng_out)
    setnames(ng_out, ".level", "manual_pareto")
    
  }
    
  
  #####################################
  #### Collect results for plotting
  
  ## collect hyperparameter results
  resultHypParam <- rbindlist(lapply(model_output_collect, function (x) x$resultCollect$resultHypParam[, trials:= x$trials]))
  xDecompAgg <- rbindlist(lapply(model_output_collect, function (x) x$resultCollect$xDecompAgg[, trials:= x$trials]))
  
  if (fixed.out != T) {
    resultHypParam[, solID:= (paste(trials,iterNG, iterPar, sep = "_"))]
    xDecompAgg[, solID:= (paste(trials,iterNG, iterPar, sep = "_"))]
  }
  xDecompAggCoef0 <- xDecompAgg[rn %in% listParam$set_mediaVarName, .(coef0=min(coef)==0), by = "solID"]
  
  if (!hyperparameter_fixed) {
    mape_lift_quantile10 <- quantile(resultHypParam$mape, probs = 0.10)
    nrmse_quantile90 <- quantile(resultHypParam$nrmse, probs = 0.90)
    decomprssd_quantile90 <- quantile(resultHypParam$decomp.rssd, probs = 0.90)
    resultHypParam <- resultHypParam[xDecompAggCoef0, on = "solID"]
    resultHypParam[, mape.qt10:= mape <= mape_lift_quantile10 & nrmse <= nrmse_quantile90 & decomp.rssd <= decomprssd_quantile90]

    
    resultHypParamPareto <- resultHypParam[mape.qt10==T]
    px <- low(resultHypParamPareto$nrmse) * low(resultHypParamPareto$decomp.rssd)
    resultHypParamPareto <- psel(resultHypParamPareto, px, top = nrow(resultHypParamPareto))[order(iterNG, iterPar, nrmse)]
    setnames(resultHypParamPareto, ".level", "robynPareto")
    
    setkey(resultHypParam,solID)
    setkey(resultHypParamPareto,solID)
    resultHypParam <- merge(resultHypParam,resultHypParamPareto[, .(solID, robynPareto)], all.x=TRUE)
    
  } else {
    resultHypParam[, ':='(mape.qt10 = T, robynPareto =1)]
  }
  
  xDecompAgg <- xDecompAgg[resultHypParam, robynPareto := i.robynPareto, on = c("iterNG", "iterPar", "trials")]
  
  decompSpendDist <- rbindlist(lapply(model_output_collect, function (x) x$resultCollect$decompSpendDist[, trials:= x$trials]))
  decompSpendDist <- decompSpendDist[resultHypParam, robynPareto := i.robynPareto, on = c("iterNG", "iterPar", "trials")]
  if (fixed.out != T) {decompSpendDist[, solID:= (paste(trials,iterNG, iterPar, sep = "_"))]}
  decompSpendDist <- decompSpendDist[xDecompAgg[rn %in% listParam$set_mediaVarName, .(rn, xDecompAgg, solID)], on = c("rn", "solID")]
  decompSpendDist[, roi := xDecompMeanNon0/mean_spend]

  setkey(xDecompAgg,solID, rn)
  setkey(decompSpendDist,solID, rn)
  xDecompAgg <- merge(xDecompAgg,decompSpendDist[, .(rn, solID, total_spend, mean_spend, spend_share, effect_share, roi)], all.x=TRUE)

  
  #####################################
  #### Plot results
  
  ## set folder to save plat
    if (!exists("plot_folder_sub")) {
      plot_folder_sub <- format(Sys.time(), "%Y-%m-%d %H.%M")
      plotPath <- dir.create(file.path(plot_folder, plot_folder_sub))
    }
  
  #paretoFronts <- ifelse(!hyperparameter_fixed, c(1,2,3), 1)
  if (!hyperparameter_fixed & fixed.out ==F) {
    paretoFronts <- 1:pareto_fronts
    num_pareto123 <- resultHypParam[robynPareto %in% paretoFronts, .N]
  } else {
    paretoFronts <- 1
    num_pareto123 <- nrow(resultHypParam)
  }
  
  cat("\nPlotting", num_pareto123,"pareto optimum models in to folder",paste0(plot_folder, "/", plot_folder_sub,"/"),"...\n")
  pbplot <- txtProgressBar(max = num_pareto123, style = 3)

  ## plot overview plots
  
  if (!hyperparameter_fixed & fixed.out ==F) {
    
    ## plot prophet
    
    if (!is.null(listParam$set_prophet)) {
      # pProphet <- prophet_plot_components(listDT$modelRecurrance, listDT$forecastRecurrance, render_plot = T)

      dt_plotProphet <- listDT$dt_mod[, c('ds','depVar', listParam$set_prophet), with = F]
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
    
    if(any(listParam$costSelector)) {
      pSpendExposure <- arrangeGrob(grobs = listParam$plotNLSCollect
                                 ,ncol= ifelse(length(listParam$plotNLSCollect)<=3, length(listParam$plotNLSCollect), 3)
                                 ,top = "Spend-exposure fitting with Michaelis-Menten model")
      #grid.draw(pSpendExposure)
      ggsave(paste0(plot_folder, "/", plot_folder_sub,"/", "spend_exposure_fitting.png")
             , plot = pSpendExposure
             , dpi = 600, width = 12, height = ceiling(length(listParam$plotNLSCollect)/3)*7)
      
    } else {
      message("\nno spend-exposure modelling needed. all media variables used for mmm are spend variables ")
    }
    
    
    ## plot hyperparameter sampling distribution
    
    resultHypParam.melted <- melt.data.table(resultHypParam[, c(listParam$local_name,"robynPareto"), with = F], id.vars = c("robynPareto"))
    
    pSamp <- ggplot(data = resultHypParam.melted,  aes( x = value, y=variable, color = variable, fill = variable) ) +
      geom_violin(alpha = .5, size = 0) +
      geom_point(size = 0.2) +
      theme(legend.position = "none") +
      labs(title="Model selection", 
           subtitle=paste0("Hyperparameter pareto sample distribution", ", iterations = ", listParam$set_iter, " * ", listParam$set_trial, " trials"),
           x="Hyperparameter space",
           y="")
    print(pSamp)
    ggsave(paste0(plot_folder, "/", plot_folder_sub,"/", "hypersampling.png")
           , plot = pSamp
           , dpi = 600, width = 12, height = 7)
    

    ## plot Pareto front
    
    pParFront <- ggplot(data = resultHypParam, aes(x=nrmse, y=decomp.rssd, color = robynPareto)) +
      geom_point(size = 0.5) +
      #stat_smooth(data = resultHypParam, method = 'gam', formula = y ~ s(x, bs = "cs"), size = 0.2, fill = "grey100", linetype="dashed")+
      geom_line(data = resultHypParam[robynPareto ==1], aes(x=nrmse, y=decomp.rssd), colour = "coral4")+
      #geom_line(data = resultHypParam[robynPareto ==2], aes(x=nrmse, y=decomp.rssd), colour = "coral3")+
      #geom_line(data = resultHypParam[robynPareto ==3], aes(x=nrmse, y=decomp.rssd), colour = "coral")+
      scale_colour_gradient(low = "navyblue", high = "skyblue") +
      labs(title="Model selection",
           subtitle=paste0("2D Pareto front 1-3 with ",listParam$set_hyperOptimAlgo,", iterations = ", listParam$set_iter , " * ", listParam$set_trial, " trials"),
           x="NRMSE",
           y="DECOMP.RSSD")
    
    if (length(paretoFronts)>1) {
      for (pfs in 2:max(paretoFronts)) {
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

    
    ## plot each Pareto solution

    cnt <- 0
    mediaVecCollect <- list()
    xDecompVecCollect <- list()
    meanResponseCollect <- list()
    for (pf in paretoFronts) {
      
      if (!hyperparameter_fixed & fixed.out ==F) {
        plotMediaShare <- xDecompAgg[robynPareto == pf & rn %in% listParam$set_mediaVarName]
        plotWaterfall <- xDecompAgg[robynPareto == pf]
        uniqueSol <- plotMediaShare[, unique(solID)]
      } else {
        plotMediaShare <- xDecompAgg[rn %in% listParam$set_mediaVarName]
        plotWaterfall <- copy(xDecompAgg)
        uniqueSol <- plotMediaShare[, unique(solID)]
      }
      
      for (j in 1:length(uniqueSol)) {
        
        cnt <- cnt+1
        ## plot spend x effect share comparison
        plotMediaShareLoop <- plotMediaShare[solID == uniqueSol[j]]
        rsq_train_plot <- plotMediaShareLoop[, round(unique(rsq_train),4)]
        nrmse_plot <- plotMediaShareLoop[, round(unique(nrmse),4)]
        decomp_rssd_plot <- plotMediaShareLoop[, round(unique(decomp.rssd),4)]
        mape_lift_plot <- ifelse(!is.null(listParam$set_lift), plotMediaShareLoop[, round(unique(mape),4)], NA)
        
        plotMediaShareLoop <- melt.data.table(plotMediaShareLoop, id.vars = c("rn", "nrmse", "decomp.rssd", "rsq_train" ), measure.vars = c("spend_share", "effect_share", "roi"))
        plotMediaShareLoop[, rn:= factor(rn, levels = sort(listParam$set_mediaVarName))]
        plotMediaShareLoopBar <- plotMediaShareLoop[variable %in% c("spend_share", "effect_share")]
        plotMediaShareLoopBar[, variable:= ifelse(variable=="spend_share", "avg.spend share", "avg.effect share")]
        plotMediaShareLoopLine <- plotMediaShareLoop[variable =="roi"]
        plotMediaShareLoopLine[, variable:= "avg.roi"]
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
          labs(title = "Share of Spend VS Share of Effect"
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
                                 geom_text(mapping = aes(label = paste0(f.unit_format(xDecompAgg),"\n", round(xDecompPerc*100, 2), "%")
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
        
        hypParam <- unlist(resultHypParamLoop[, listParam$local_name, with =F])
        dt_transformPlot <- dt_mod[, c("ds", listParam$set_mediaVarName), with =F] # independent variables
        dt_transformSpend <- cbind(dt_transformPlot[,.(ds)], listDT$dt_input[, c(listParam$set_mediaSpendName), with =F]) # spends of indep vars
        setnames(dt_transformSpend, names(dt_transformSpend), c("ds", listParam$set_mediaVarName))
        
        # update non-spend variables
        dt_transformSpendMod <- dt_transformPlot[listParam$initRollWindStartWhich:listParam$initRollWindEndWhich]
        if (length(listParam$exposureVarName)>0) {
          for (expo in listParam$exposureVarName) {
            sel_nls <- ifelse(listParam$modNLSCollect[channel == expo, rsq_nls>rsq_lm],"nls","lm")
            dt_transformSpendMod[, (expo):= listParam$yhatNLSCollect[channel==expo & models == sel_nls, yhat]]
          }
        }

        dt_transformAdstock <- copy(dt_transformPlot)
        dt_transformSaturation <- dt_transformPlot[listParam$initRollWindStartWhich:listParam$initRollWindEndWhich]
        #chnl_non_spend <- listParam$set_mediaVarName[!(listParam$set_mediaVarName==listParam$set_mediaSpendName)]
        
        m_decayRate <- list()
        
        for (med in 1:listParam$mediaVarCount) {
          
          med_select <- listParam$set_mediaVarName[med]
          m <- dt_transformPlot[, get(med_select)]
          
          
          ## adstocking
          if (listParam$adstock == "geometric") {
            
            theta <- hypParam[paste0(listParam$set_mediaVarName[med], "_thetas")]
            x_list <- f.adstockGeometric(x=m, theta = theta)
            
          } else if (listParam$adstock == "weibull") {
            
            shape <- hypParam[paste0(listParam$set_mediaVarName[med], "_shapes")]
            scale <- hypParam[paste0(listParam$set_mediaVarName[med], "_scales")]
            x_list <- f.adstockWeibull(x=m, shape = shape, scale = scale)
            
          }
          m_adstocked <- x_list$x_decayed
          dt_transformAdstock[, (med_select):= m_adstocked]
          m_adstockedRollWind <- m_adstocked[listParam$initRollWindStartWhich:listParam$initRollWindEndWhich]
          
          ## saturation
          alpha <- hypParam[paste0(listParam$set_mediaVarName[med], "_alphas")]
          gamma <- hypParam[paste0(listParam$set_mediaVarName[med], "_gammas")]
          dt_transformSaturation[, (med_select):= f.hill(x=m_adstockedRollWind, alpha = alpha, gamma = gamma)] 

          m_decayRate[[med]] <- data.table(x_list$thetaVecCum)
          setnames(m_decayRate[[med]], "V1", paste0(listParam$set_mediaVarName[med], "_decayRate"))
          
        } 
        
        m_decayRate <- data.table(cbind(sapply(m_decayRate, function(x) sapply(x, function(y)y))))
        setnames(m_decayRate, names(m_decayRate), listParam$set_mediaVarName)
        m_decayRateSum <- m_decayRate[, lapply(.SD, sum), .SDcols = listParam$set_mediaVarName]
        
        decayRate.melt <- suppressWarnings(melt.data.table(m_decayRateSum))
        
        #decayRate.melt[, channel:=str_extract(decayRate.melt$variable, paste0(listParam$set_mediaVarName, collapse = "|"))]
        #decayRate.melt[, variable:=str_replace(decayRate.melt$variable, paste0(paste0(listParam$set_mediaVarName,"_"), collapse = "|"), "")]
        
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
        decayRate.melt[, variable:= factor(variable, levels = sort(listParam$set_mediaVarName))]
        
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
        for (i in 1:listParam$mediaVarCount) {
          coef <- plotWaterfallLoop[rn == listParam$set_mediaVarName[i], coef]
          dt_transformSaturationDecomp[, (listParam$set_mediaVarName[i]):= .SD * coef, .SDcols = listParam$set_mediaVarName[i]]
        }
        
        #mediaAdstockFactorPlot <- dt_transformPlot[, lapply(.SD, sum), .SDcols = listParam$set_mediaVarName]  / dt_transformAdstock[, lapply(.SD, sum), .SDcols = listParam$set_mediaVarName]
        #dt_transformSaturationAdstockReverse <- data.table(mapply(function(x, y) {x*y},x= dt_transformAdstock[, listParam$set_mediaVarName, with=F], y= mediaAdstockFactorPlot))
        dt_transformSaturationSpendReverse <- copy(dt_transformAdstock)
        
        for (i in 1:listParam$mediaVarCount) {
          chn <- listParam$set_mediaVarName[i]
          if (chn %in% listParam$set_mediaVarName[listParam$costSelector]) {
            
            # get Michaelis Menten nls fitting param
            get_chn <- dt_transformSaturationSpendReverse[, chn, with =F]
            Vmax <- listParam$modNLSCollect[channel == chn, Vmax]
            Km <- listParam$modNLSCollect[channel == chn, Km]
            
            # reverse exposure to spend
            dt_transformSaturationSpendReverse[, (chn):= f.micMen(x=.SD, Vmax = Vmax, Km = Km, reverse = T), .SDcols = chn] # .SD * Km / (Vmax - .SD) exposure to spend, reverse Michaelis Menthen: x = y*Km/(Vmax-y)
            
          } else if (chn %in% listParam$exposureVarName) {
            coef_lm <- listParam$modNLSCollect[channel == chn, coef_lm]
            dt_transformSaturationSpendReverse[, (chn):= .SD/coef_lm, .SDcols = chn] 
          } 
          # spendRatioFitted <- xDecompAgg[rn == chn, mean(total_spend)] / dt_transformSaturationSpendReverse[, sum(.SD), .SDcols = chn]
          # dt_transformSaturationSpendReverse[, (chn):= .SD * spendRatioFitted, .SDcols = chn]
        }
        
        dt_transformSaturationSpendReverse <- dt_transformSaturationSpendReverse[listParam$initRollWindStartWhich:listParam$initRollWindEndWhich]
        
        dt_scurvePlot <- cbind(melt.data.table(dt_transformSaturationDecomp, id.vars = "ds", variable.name = "channel",value.name = "response"),
                               melt.data.table(dt_transformSaturationSpendReverse, id.vars = "ds", value.name = "spend")[, .(spend)])
        dt_scurvePlot <- dt_scurvePlot[spend>=0] # remove outlier introduced by MM nls fitting
        
        
        dt_scurvePlotMean <- dt_transformSpend[listParam$initRollWindStartWhich:listParam$initRollWindEndWhich, !"ds"][, lapply(.SD, function(x) mean(x[x>0])), .SDcols = listParam$set_mediaVarName]
        dt_scurvePlotMean <- melt.data.table(dt_scurvePlotMean, measure.vars = listParam$set_mediaVarName, value.name = "mean_spend", variable.name = "channel")
        dt_scurvePlotMean[, ':='(mean_spend_scaled=0, mean_response=0, next_unit_response=0)]
        
        for (med in 1:listParam$mediaVarCount) {
          
          get_med <- listParam$set_mediaVarName[med]
          get_spend <- dt_scurvePlotMean[channel == get_med, mean_spend]
          
          if (get_med %in% listParam$set_mediaVarName[listParam$costSelector]) {
            Vmax <- listParam$modNLSCollect[channel == get_med, Vmax]
            Km <- listParam$modNLSCollect[channel == get_med, Km]
            get_spend_mm <- f.micMen(x = get_spend, Vmax = Vmax, Km = Km) # Vmax * get_spend/(Km + get_spend)
          } else if (get_med %in% listParam$exposureVarName) {
            coef_lm <- listParam$modNLSCollect[channel == get_med, coef_lm]
            get_spend_mm <- get_spend*coef_lm
          } else {
            get_spend_mm <- get_spend
          }
          
          m <- dt_transformAdstock[listParam$initRollWindStartWhich:listParam$initRollWindEndWhich, get(get_med)]
          # m <- m[m>0] # remove outlier introduced by MM nls fitting
          alpha <- hypParam[which(paste0(get_med, "_alphas")==names(hypParam))]
          gamma <- hypParam[which(paste0(get_med, "_gammas")==names(hypParam))]
          #gammaTrans <- round(quantile(seq(range(m)[1], range(m)[2], length.out = 100), gamma),4)
          #get_response <-  get_spend_mm**alpha / (get_spend_mm**alpha + gammaTrans**alpha)
          #get_response_marginal <- (get_spend_mm+1)**alpha / ((get_spend_mm+1)**alpha + gammaTrans**alpha)
          get_response <- f.hill(x=m, alpha = alpha, gamma = gamma, x_marginal = get_spend_mm)
          get_response_marginal <- f.hill(x=m, alpha = alpha, gamma = gamma, x_marginal = get_spend_mm+1)
          
          coef <- plotWaterfallLoop[rn == get_med, coef]
          dt_scurvePlotMean[channel == get_med, mean_spend_scaled := get_spend_mm]
          dt_scurvePlotMean[channel == get_med, mean_response := get_response * coef]
          dt_scurvePlotMean[channel == get_med, next_unit_response := get_response_marginal * coef - mean_response]
          
        }
        dt_scurvePlotMean[, solID:= uniqueSol[j]]

        p4 <- ggplot(data= dt_scurvePlot, aes(x=spend, y=response, color = channel)) +
          geom_line() +
          geom_point(data = dt_scurvePlotMean, aes(x=mean_spend, y=mean_response, color = channel)) +
          geom_text(data = dt_scurvePlotMean, aes(x=mean_spend, y=mean_response,  label = round(mean_spend,0)), show.legend = F, hjust = -0.2)+
          theme(legend.position = c(0.9, 0.2)) +
          labs(title="Response curve and mean spend by channel"
               ,subtitle = paste0("rsq_train: ", rsq_train_plot, 
                                  ", nrmse = ", nrmse_plot, 
                                  ", decomp.rssd = ", decomp_rssd_plot,
                                  ", mape.lift = ", mape_lift_plot)
               ,x="Spend" ,y="response")
        
        ## plot fitted vs actual
        
        if(!is.null(listParam$set_prophet)) {
          dt_transformDecomp <- cbind(dt_modRollWind[, c("ds", "depVar", listParam$set_prophet, listParam$set_baseVarName), with=F], dt_transformSaturation[, listParam$set_mediaVarName, with=F])
          col_order <- c("ds", "depVar", listParam$set_prophet, listParam$set_baseVarName, listParam$set_mediaVarName)
        } else {
          dt_transformDecomp <- cbind(dt_modRollWind[, c("ds", "depVar", listParam$set_baseVarName), with=F], dt_transformSaturation[, listParam$set_mediaVarName, with=F])
          col_order <- c("ds", "depVar", listParam$set_baseVarName, listParam$set_mediaVarName)
        }
        setcolorder(dt_transformDecomp, neworder = col_order)
        
        xDecompVec <- dcast.data.table(xDecompAgg[solID==uniqueSol[j], .(rn, coef, solID)],  solID ~ rn, value.var = "coef")
        if (!("(Intercept)" %in% names(xDecompVec))) {xDecompVec[, "(Intercept)":= 0]}
        setcolorder(xDecompVec, neworder = c("solID", "(Intercept)",col_order[!(col_order %in% c("ds", "depVar"))]))
        intercept <- xDecompVec$`(Intercept)`
        
        xDecompVec <- data.table(mapply(function(scurved,coefs) { scurved * coefs}, 
                                        scurved=dt_transformDecomp[, !c("ds", "depVar"), with=F] , 
                                        coefs = xDecompVec[, !c("solID", "(Intercept)")]))
        xDecompVec[, ':='(depVarHat=rowSums(xDecompVec) + intercept, solID = uniqueSol[j])]
        xDecompVec <- cbind(dt_transformDecomp[, .(ds, depVar)], xDecompVec)
        
        xDecompVecPlot <- xDecompVec[, .(ds, depVar, depVarHat)]
        setnames(xDecompVecPlot, old = c("ds", "depVar", "depVarHat"), new = c("ds", "actual", "predicted"))
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
          geom_smooth(se = T, method = 'loess', formula = 'y ~ x') + 
          xlab("fitted") + ylab("resid") + ggtitle("fitted vs. residual")
        
        
        ## save and aggregate one-pager plots
        
        modID <- paste0("Model one-pager, on pareto front ", pf,", ID: ", uniqueSol[j])

        pg <- arrangeGrob(p2,p5,p1, p4, p3, p6, ncol=2, top = text_grob(modID, size = 15, face = "bold"))
        # grid.draw(pg)
        ggsave(filename=paste0(plot_folder, "/", plot_folder_sub,"/", uniqueSol[j],".png")
               , plot = pg
               , dpi = 600, width = 18, height = 18)
        
        setTxtProgressBar(pbplot, cnt)
        
        ## prepare output
        
        
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
    

  cat("\nTotal time: ",difftime(Sys.time(),t0, units = "mins"), "mins\n")
  
  #####################################
  #### Collect results for output
  
  allSolutions <- xDecompVecCollect[, unique(solID)]
  
  fwrite(resultHypParam[solID %in% allSolutions], paste0(plot_folder, "/", plot_folder_sub,"/", "pareto_hyperparameters.csv"))
  fwrite(xDecompAgg[solID %in% allSolutions], paste0(plot_folder, "/", plot_folder_sub,"/", "pareto_aggregated.csv"))
  fwrite(mediaVecCollect, paste0(plot_folder, "/", plot_folder_sub,"/", "pareto_media_transform_matrix.csv"))
  fwrite(xDecompVecCollect, paste0(plot_folder, "/", plot_folder_sub,"/", "pareto_alldecomp_matrix.csv"))
  
  listOutput <- list(resultHypParam=resultHypParam[solID %in% allSolutions],
                     xDecompAgg=xDecompAgg[solID %in% allSolutions],
                     mediaVecCollect=mediaVecCollect,
                     xDecompVecCollect=xDecompVecCollect,
                     model_output_collect=model_output_collect,
                     allSolutions = allSolutions,
                     folder_path= paste0(plot_folder, "/", plot_folder_sub,"/"))
  
  assign("listOutput", listOutput, envir = .GlobalEnv)
  #return(listOutput)
  
  
}

#####################################
#### Define f.robyn.fixed, the function to load old model hyperparameters from csv

f.robyn.fixed <- function(plot_folder = getwd()
                          ,dt_hyppar_fixed = NULL) {
  
  ## check condition
  if (!dir.exists(plot_folder)) {
    plot_folder <- getwd()
    message("provided plot_folder doesn't exist. Using default plot_folder = getwd(): ", getwd())
  }
  if (is.null(dt_hyppar_fixed)) {stop("Please provide the table listOutput$resultHypParam from previous runs or pareto_hyperparameters.csv with desired model IDs")}
  if (nrow(dt_hyppar_fixed)==0) {stop("solID not included in fixed_hyppar_dt")}
  
  ## run f.robyn
  f.robyn(listParam = parent.frame()$listParam
          ,plot_folder = plot_folder
          ,fixed.out = T
          ,fixed.hyppar.dt = dt_hyppar_fixed)
}




f.response <- function(mediaVarName = NULL
                        , modID = NULL
                        , Spend = NULL
                        , listDT = parent.frame()$listDT
                        , listParam = parent.frame()$listParam
                        , listOutput = parent.frame()$listOutput
) {
  
  ## get input
  dt_input = listDT$dt_input
  set_mediaVarName = listParam$set_mediaVarName
  set_mediaSpendName = listParam$set_mediaSpendName
  startRW = listParam$initRollWindStartWhich
  endRW = listParam$initRollWindEndWhich
  adstock = listParam$adstock
  resultHypParam = listOutput$resultHypParam
  xDecompAgg = listOutput$xDecompAgg
  allSolutions = listOutput$allSolutions
  spendExpoMod=listParam$modNLSCollect
  
  ## check inputs
  if (is.null(mediaVarName)) {
    stop(paste0("mediaVarName must be one of these values: ", paste(set_mediaVarName, collapse = ", ")))
  } else if ( !(mediaVarName %in% set_mediaVarName) | length(mediaVarName)!=1) {
    stop(paste0("mediaVarName must be one of these values: ", paste(set_mediaVarName, collapse = ", ")))
  }
  
  mediaVar <- dt_input[, get(mediaVarName)]
  
  if (!is.null(Spend)) {
    if (length(Spend) !=1 | Spend <= 0 | !is.numeric(Spend)) {stop("Spend must be a positive number")} 
  }
  
  
  
  ## transform spend to exposure if necessary
  if (mediaVarName %in% listParam$exposureVarName) {
    
    # use non-0 mean spend as marginal level if Spend not provided
    if (is.null(Spend)) {
      mediaSpend <- dt_input[startRW:endRW, get(set_mediaSpendName[which(set_mediaVarName == mediaVarName)])]
      Spend <- mean(mediaSpend[mediaSpend>0])
      message("Spend not provided. using mean of ", mediaVarName," as marginal levl instead")
    }
    
    # fit spend to exposure
    nls_select <- spendExpoMod[channel == mediaVarName, rsq_nls > rsq_lm]
    if (nls_select) {
      Vmax <- spendExpoMod[channel == mediaVarName, Vmax]
      Km <- spendExpoMod[channel == mediaVarName, Km]
      Spend <- f.micMen(x=Spend, Vmax = Vmax, Km = Km, reverse = F) 
    } else {
      coef_lm <- spendExpoMod[channel == mediaVarName, coef_lm]
      Spend <- Spend * coef_lm
    }
  } else {
    
    # use non-0 mean spend as marginal level if Spend not provided
    if (is.null(Spend)) {
      mediaSpend <- dt_input[startRW:endRW, get(mediaVarName)]
      Spend <- mean(mediaSpend[mediaSpend>0])
      message("Spend not provided. using mean of ", mediaVarName," as marginal levl instead")
    }
  }
  
  
  ## adstocking
  if (adstock == "geometric") {
    theta <- resultHypParam[solID == modID, get(paste0(mediaVarName,"_thetas"))]
    x_list <- f.adstockGeometric(x=mediaVar, theta = theta)
  } else if (adstock == "weibull") {
    shape <- resultHypParam[solID == modID, get(paste0(mediaVarName,"_shapes"))]
    scale <- resultHypParam[solID == modID, get(paste0(mediaVarName,"_scales"))]
    x_list <- f.adstockWeibull(x=mediaVar, shape = shape, scale = scale)
  }
  m_adstocked <- x_list$x_decayed
  
  ## saturation
  m_adstockedRW <- m_adstocked[startRW:endRW]
  alpha <- resultHypParam[solID == modID, get(paste0(mediaVarName,"_alphas"))]
  gamma <- resultHypParam[solID == modID, get(paste0(mediaVarName,"_gammas"))]
  Saturated <- f.hill(x=m_adstockedRW, alpha = alpha, gamma = gamma, x_marginal = Spend)
  
  ## decomp
  coeff <- xDecompAgg[solID == modID & rn == mediaVarName, coef]
  Response <- Saturated * coeff
  
  return(Response)
}
