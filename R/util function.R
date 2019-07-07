#' Autocorrelation Function
#' @export
#' @param input A tsibble object
#' @param var A character, optional, defines the variables names to calculate the ACF when having a multuple time series object
#' @param max.lag An integer, defines the maximum number of lags to be used

tsACF <- function(input, var = NULL, max.lag = NULL, ci = 0.95, na.rm = FALSE){
  `%>%` <- magrittr::`%>%`
  # Error handling
  if(!base::is.logical(na.rm)){
    stop("The the 'na.rm' argument must be boolean")
  }

  if(!tsibble::is.tsibble(input)){
    stop("The input object is not a 'tbl_ts' class")
  }

  if (ci > 1 | ci <= 0) {
    warning("The 'ci' value is out of bound (0-1], the default option of 0.95 will be used")
    ci <- 0.95
  }

  if(base::is.null(max.lag)){
    max.lag <- base::round(stats::frequency(input) * 2)
    if(max.lag > base::nrow(input)){
      max.lag <- base::nrow(input)
    }

  } else if(!base::is.numeric(max.lag) || max.lag %% 1 != 0){
    stop("The value of the 'max.lag' argument must be integer")
  } else if(max.lag < 1){
    stop("The value of the 'max.lag' argument must be greater than 1")
  }

  if(!base::is.null(var)){
    if(!base::all(var %in% base::names(input))){
      stop("The variables names on the 'var' argument don't match the variables names of the input object")
    } else if(!sapply(input[,var], base::is.numeric) %>% base::all){
      stop("At least one of the selected variables are not numeric")
    }
  } else {
    if(!base::any(sapply(input, base::is.numeric))){
      stop("The input object doesn't have any numeric variables")
    } else {
      var <- base::names(input)[base::which(sapply(input, base::is.numeric))]
    }
  }

  output <- base::list()
  for(i in base::seq_along(var)){
    y_mean <- s <-  ci_value <- acf <- p <- NULL
    y_mean <- base::mean(input[[var[i]]], na.rm = na.rm)

    for(k in 0:max.lag){
    s <- c(s,
           base::sum((input[[var[i]]][1:(base::nrow(input) - k)] - y_mean) *
                   (input[[var[i]]][(1 + k):(base::nrow(input))] - y_mean), na.rm = na.rm) /
                     base::sum((input[[var[i]]] - y_mean)^2, na.rm = na.rm))


    }

    ci_value <- stats::qnorm((1 + ci)/2)/sqrt(base::nrow(input))
    acf <-  base::data.frame(lag = 0:max.lag ,
                             acf = s,
                             ci_lower = - ci_value,
                             ci_upper = ci_value)

    p <- plotly::plot_ly(data = acf) %>%
      plotly::add_trace(x = ~ lag, y = ~ acf, type = "bar", width = 0.01, showlegend = FALSE) %>%
      plotly::add_lines(x = ~ lag, y = ~ ci_upper, line = list(dash = "dash", color = "red"), showlegend = FALSE) %>%
      plotly::add_lines(x = ~ lag, y = ~ ci_lower, line = list(dash = "dash", color = "red"), showlegend = FALSE) %>%
      plotly::layout(yaxis = list(title = "ACF"),
                     xaxis = list(title = "Lag"),
                     title = base::paste("Autocorrelation - ", var[i], sep = ""))




    output[[var[i]]]$acf <- acf
    output[[var[i]]]$plot <- p



  }


  return(output)
  }



