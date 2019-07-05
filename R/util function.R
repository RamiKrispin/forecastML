tsACF <- function(input, var = NULL, max.lag = NULL){

  # Error handling
  if(!tsibble::is.tsibble(input)){
    stop("The input object is not a 'tbl_ts' class")
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
    y_mean <- s <- NULL
    y_mean <- mean(input[[var[i]]])

    for(k in 0:max.lag){
    s <- c(s,
           base::sum((input[[var[i]]][1:(base::nrow(input) - k)] - y_mean) *
                   (input[[var[i]]][(1 + k):(base::nrow(input))] - y_mean)) /
                     base::sum((input[[var[i]]] - y_mean)^2))


    }
    output[[var[i]]] <- s
  }


  return(output)
  }



