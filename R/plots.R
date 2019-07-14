#' Plotting the residuals
#' @export
#' @param model A trainML object
#' @param na.rm A boolean, if TRUE will ignore missing values, by default set to FALSE
#' @param margin A numeric, define the margin space between the subplot components
#'
plot_res <- function(model, na.rm = FALSE, margin = 0.04){

  if(base::class(model) != "trainML"){
    stop("The input model is not a 'trainML' object")
  }


  df <- model$series %>% dplyr::select(index = model$parameters$index, actual = model$parameters$y) %>%
    dplyr::left_join(model$fitted, by = "index") %>%
    dplyr::mutate(residuals = actual - fitted) %>% as.data.frame()


  p1 <- plotly::plot_ly(data = df, x = ~ index, y = ~ actual, type = "scatter", mode = "lines", name = "Actual") %>%
    plotly::add_lines(x = ~ index, y = ~ fitted,  line = list(dash = "dash", color = "red"), name = "Fitted")

  p2 <- plotly::plot_ly(data = df, x = ~ index, y = ~ residuals,
                        type = "scatter", mode = "lines",
                        line = list(color = "green"), name = "Residuals") %>%
    plotly::layout(xaxis = list(title = "Index", range = c(min(df$index), max(df$index))))


  if(base::any(base::is.na(model$residuals)) && na.rm == FALSE){
    stop("The model residuals has missing values, please either check the residuals or set na.rm = TRUE")
  }
  p3 <- forecastML::tsACF(model$residuals, na.rm = na.rm, plot = FALSE)

  p4 <- plotly::plot_ly(x = model$residuals$residuals, type = "histogram",
                        marker = list(color = 'rgb(227, 119, 194)'),
                        name = "Residauls Dist.") %>%
    plotly::layout(xaxis = list(title = "Residuals Distribution"),
                   yaxis = list(title = "Count"))

  p_output <- plotly::subplot(plotly::subplot(p1, p2, nrows = 2, shareX = T),
                  plotly::subplot(p3$residuals$plot, p4, nrows = 1, titleY = T, titleX = T ), nrows = 2, titleY = T, titleX = T, margin = margin,
                  heights = c(0.6, 0.4)) %>%
    plotly::hide_legend() %>%
    plotly::layout(title = "Residuals Analysis")
  return(p_output)
}



