#' Plotting the residuals
#' @export
#' @param model A trainML object

plot_res <- function(model, na.rm = FALSE){

  if(base::class(model) != "trainML"){
    stop("The input model is not a 'trainML' object")
  }


  p1 <- plotly::plot_ly(x = model$series[[model$parameters$index]],
                        y = model$series[[model$parameters$y]],
                        type = "scatter", mode = "lines") %>%
    plotly::add_lines(x = model$fitted$index,
                      y = model$fitted$fitted, line = list(dash = "dash", color = "red"))

  p2 <- plotly::plot_ly(data = model$residuals, x = ~ index, y = ~ residuals, type = "scatter", mode = "lines") %>%
    plotly::layout(yaxis = list(title = "Residuals"),
                   xaxis = list(title = "Index"))

  if(base::any(base::is.na(model$residuals)) && na.rm == FALSE){
    stop("The model residuals has missing values, please either check the residuals or set na.rm = TRUE")
  }
  p3 <- forecastML::tsACF(model$residuals, na.rm = na.rm)

  p4 <- plotly::plot_ly(x = model$residuals$residuals, type = "histogram")

  plotly::subplot(p1,p2, plotly::subplot(p3$residuals$plot, p4, nrows = 1 ), nrows = 3, titleY = TRUE, titleX = TRUE)
}


