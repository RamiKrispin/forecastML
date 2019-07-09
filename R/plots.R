plot_res <- function(model){
  p1 <- plotly::plot_ly(x = model$series[[model$parameters$index]],
                        y = model$series[[model$parameters$y]],
                        type = "scatter", mode = "lines") %>%
    plotly::add_lines(x = model$fitted$index,
                      y = model$fitted$fitted, line = list(dash = "dash", color = "red"))

  p2 <- plotly::plot_ly(data = model$residuals, x = ~ index, y = ~ residuals, type = "scatter", mode = "lines") %>%
    plotly::layout(yaxis = list(title = "Residuals"),
                   xaxis = list(title = "Index"))
  p3 <- forecastML::tsACF(model$residuals)


  plotly::subplot(p1,p2, p3$residuals$plot, nrows = 3)
}
