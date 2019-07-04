tsACF <- function(input, x = NULL, max.lag){

  # Error handling
  if(!tsibble::is.tsibble(input)){
    stop("The input object is not a 'tbl_ts' class")
  }

  if(!base::is.null(x)){
    if(!base::all(x %in% base::names(input))){
      stop("The variables names on the 'x' argument don't match the variables names of the input object")
    } else if(!sapply(input[,x], base::is.numeric) %>% base::all){
      stop("At least one of the selected variables are not numeric")
    }
  } else {
    if(!base::any(input, base::is.numeric)){
      stop("The input object doesn't have any numeric variables")
    }
  }


  y_mean <- base::mean(input[[x]])

}
