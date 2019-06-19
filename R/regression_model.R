#' Forecast with Regression Model
#' @export
#' @param input A tsibble or ts object
#' @param y A character, the column name of the depended variable of the input object, required (and applicable) only when the input is tsibble object
#' @param x A character, the column names of the independed variable of the input object, applicable when using tsibble object with regressors
#' @param seasonal A character, optional, create categorical variable/s to model the single or multiple seasonal components. Supporting the
#' following frequencies structure c("quarter", "month", "week", "yday", "wday", "hour") for quarterly,
#' monthly, weekly, day of the year, day of the week, and hourly respectively
#' @param trend A list, define the trend structure. Possible arguments -
#' "power", an numeric value, defines the polynomial degree of the series index (for example a power = 1 define a linear trend and power = 2 defines a squared index).
#' "exponential" - a boolean variable, if set to TRUE defines an exponential trend.
#' "log" - a boolean variable, if set to TRUE defines a log transformation for the trend.
#' By default, the trend argument is set to a linear trend (i.e., power = 1)
#' @param lags A positive integer, defines the series lags to be used as input to the model (equivalent to AR process)
#' @param method A character, defines the regression method to be used, currently only "lm" method is available
#' @param method_arg A list, defines the argument of the selected method
#' @param scale A character, scaling options of the series, methods available -
#' c("log", "normal", "standard") for log transformation, normalization, or standardization of the series, respectively
#' @description Forecasting regular time series data with regression models
#' @examples



ts_reg <- function(input,
                   y = NULL,
                   x = NULL,
                   seasonal = NULL,
                   trend = list(power = c(1), exponential = FALSE, log = FALSE),
                   lags = NULL,
                   method =  "lm",
                   method_arg = list(step = FALSE, direction = "both"),
                   scale = NULL){

  `%>%` <- magrittr::`%>%`

  freq <- md <- NULL
  freq <- base::names(base::which(purrr::map(tsibble::interval(df), ~.x) == 1))

  # Error handling

  # Check the trend argument
  if(!base::is.list(trend) || !all(base::names(trend) %in% c("power", "exponential", "log"))){
    stop("The 'trend' argument is not valid")
  } else if(!base::is.null(trend$power)){
    if(!base::is.numeric(trend$power)){
      stop("The value of the 'power' parameter of the 'trend' argument is not valid, can be either a numeric ",
           "(e.g., 1 for linear, 2 for square, and 0.5 for square root), or NULL for disable")
    }
  }

  # Checking the lags argument
  if(!base::is.null(lags)){
    if(!base::is.numeric(lags) || any(lags %% 1 != 0 ) || any(lags <= 0)){
      stop("The value of the 'ar' argument is not valid. Must be a positive integer")
    }
  }

  # Setting the input table
  if(any(class(input) == "tbl_ts")){
    df <- input
  } else if(any(class(input) == "ts")){
    df <- as_tsibble(input) %>% setNames(c("index", "y"))
    y <- "y"
    if(!base::is.null(x)){
      warning("The 'x' argument cannot be used when input is a 'ts' class")
    }
    x <- NULL
  }

  # Setting the frequency component
  if(!base::is.null(seasonal)){

    # Case series frequency is quarterly

    if(freq == "quarter"){
      if(base::length(seasonal) == 1 & seasonal == "quarter"){
        df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
        x <- c(x, "quarter")
      } else if(base::length(seasonal) > 1 & "quarter" %in% seasonal){
        warning("Only quarter seasonal component can be used with quarterly frequency")
        df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
        x <- c(x, "quarter")
      } else {
        stop("The seasonal component is not valid")
      }

      # Case series frequency is monthly

    } else if(freq == "month"){
      if(base::length(seasonal) == 1 && seasonal == "month"){
        df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
        x <- c(x, "month")
      } else if(all(seasonal %in% c("month", "quarter"))){
        df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
        df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
        x <- c(x, "month", "quarter")
      } else if(any(seasonal %in% c("month", "quarter"))){
        if("month" %in% seasonal){
          df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
          x <- c(x, "month")
        }

        if("quarter" %in% seasonal){
          df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
          x <- c(x, "quarter")
        }

        warning("For monthly frequency only 'month' or 'quarter' seasonal component could be used with the 'seasonal' argument")
      } else {stop("The seasonal component is not valid")}

      # Case series frequency is weekly

    } else if(freq == "week"){
      if(base::length(seasonal) == 1 && seasonal == "week"){
        df$week <- lubridate::week(df$index) %>% base::factor(ordered = FALSE)
        x <- c(x, "week")
      } else if(all(seasonal %in% c("week", "month", "quarter"))){
        df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
        df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
        df$week <- lubridate::week(df$index) %>% base::factor(ordered = FALSE)
        x <- c(x, "week","month", "quarter")
      } else if(any(seasonal %in% c("week", "month", "quarter"))){
        if("week" %in% seasonal){
          df$week <- lubridate::week(df$index) %>% base::factor(ordered = FALSE)
          x <- c(x, "week")
        }

        if("month" %in% seasonal){
          df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
          x <- c(x, "month")
        }

        if("quarter" %in% seasonal){
          df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
          x <- c(x, "quarter")
        }

        warning("For weekly frequency only 'week', 'month', or 'quarter' seasonal component could be used with the 'seasonal' argument")
      } else {stop("The seasonal component is not valid")}

      # Case series frequency is daily

    } else if(freq == "day"){
      if(base::length(seasonal) == 1 && seasonal == "wday"){
        df$wday <- lubridate::wday(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
        x <- c(x, "wday")
      } else if(base::length(seasonal) == 1 && seasonal == "yday"){
        df$yday <- lubridate::yday(df$index) %>% base::factor(ordered = FALSE)
        x <- c(x, "yday")
      } else if(all(seasonal %in% c("wday", "yday","week", "month", "quarter"))){
        df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
        df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
        df$week <- lubridate::week(df$index) %>% base::factor(ordered = FALSE)
        df$wday <- lubridate::wday(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
        df$yday <- lubridate::yday(df$index) %>% base::factor(ordered = FALSE)
        x <- c(x, "wday", "yday", "week","month", "quarter")
      } else if(any(seasonal %in% c("wday", "yday","week", "month", "quarter"))){
        if("wday" %in% seasonal){
          df$wday <- lubridate::wday(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
          x <- c(x, "wday")
        }

        if("yday" %in% seasonal){
          df$yday <- lubridate::yday(df$index) %>% base::factor(ordered = FALSE)
          x <- c(x, "yday")
        }

        if("week" %in% seasonal){
          df$week <- lubridate::week(df$index) %>% base::factor(ordered = FALSE)
          x <- c(x, "week")
        }


        if("month" %in% seasonal){
          df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
          x <- c(x, "month")
        }

        if("quarter" %in% seasonal){
          df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
          x <- c(x, "quarter")
        }

        warning("For daily frequency only 'wday', 'yday', 'week', 'month', or 'quarter' seasonal component could be used with the 'seasonal' argument")
      } else {stop("The seasonal component is not valid")}

      # Case series frequency is hourly

    } else if(freq == "hour"){
      if(base::length(seasonal) == 1 && seasonal == "hour"){
        df$hour <- lubridate::hour(df$index) %>% base::factor(ordered = FALSE)
        x <- c(x, "hour")
      } else if(all(seasonal %in% c("hour", "wday", "yday","week", "month", "quarter"))){
        df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
        df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
        df$week <- lubridate::week(df$index) %>% base::factor(ordered = FALSE)
        df$wday <- lubridate::wday(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
        df$yday <- lubridate::yday(df$index) %>% base::factor(ordered = FALSE)
        df$hour <- lubridate::hour(df$index) %>% base::factor(ordered = FALSE)
        x <- c(x, "hour","wday", "yday", "week","month", "quarter")
      } else if(any(seasonal %in% c("hour","wday", "yday","week", "month", "quarter"))){
        if("hour" %in% seasonal){
          df$hour <- lubridate::hour(df$index) %>% base::factor(ordered = FALSE)
          x <- c(x, "hour")
        }

        if("wday" %in% seasonal){
          df$wday <- lubridate::wday(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
          x <- c(x, "wday")
        }

        if("yday" %in% seasonal){
          df$yday <- lubridate::yday(df$index) %>% base::factor(ordered = FALSE)
          x <- c(x, "yday")
        }

        if("week" %in% seasonal){
          df$week <- lubridate::week(df$index) %>% base::factor(ordered = FALSE)
          x <- c(x, "week")
        }

        if("month" %in% seasonal){
          df$month <- lubridate::month(df$index, label = TRUE) %>% base::factor(ordered = FALSE)
          x <- c(x, "month")
        }

        if("quarter" %in% seasonal){
          df$quarter <- lubridate::quarter(df$index) %>% base::factor(ordered = FALSE)
          x <- c(x, "quarter")
        }

        warning("For daily frequency only 'hour', 'wday', 'yday', 'week', 'month', or 'quarter' seasonal component could be used with the 'seasonal' argument")
      } else {stop("The seasonal component is not valid")}
    }
  }

  # Setting the trend
  if(!base::is.null(trend$power)){
    for(i in trend$power){
      df[base::paste("trend_power_", i, sep = "")] <- c(1:base::nrow(df)) ^ i
      x <- c(x, base::paste("trend_power_", i, sep = ""))
    }
  }

  if(trend$exponential){
    df$exp_trend <- base::exp(1:base::nrow(df))
    x <- c(x, "exp_trend")
  }

  if(trend$log){
    df$log_trend <- base::log(1:base::nrow(df))
    x <- c(x, "log_trend")
  }



  # Setting the lags variables

  if(!base::is.null(lags)){
    for(i in lags){
      df[base::paste("lag_", i, sep = "")] <- df[[y]] %>% dplyr::lag( i)
      x <- c(x, base::paste("lag_", i, sep = ""))
    }
    df1 <- df[(max(lags)+ 1):base::nrow(df),]
  } else {
    df1 <- df
  }


  if(method == "lm"){
    f <- stats::as.formula(paste("y ~ ", paste0(x, collapse = " + ")))

    if(method_arg$step){
      md_init <- NULL
      md_init <- stats::lm(f, data = df)
      md <- step(md_init, direction = method_arg$direction)
    } else(
      md <- stats::lm(f, data = df)
    )

  }


  output <- list(model = md,
                 parameters = list(y = y,
                                   x = x,
                                   seasonal = seasonal,
                                   trend = trend,
                                   lags = lags,
                                   method = method,
                                   method_arg = method_arg),
                 series = df)
  return(output)

}
