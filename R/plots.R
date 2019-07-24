#' Plotting the residuals
#' @export
#' @param model A trainML object
#' @param na.rm A boolean, if TRUE will ignore missing values, by default set to FALSE
#' @param margin A numeric, define the margin space between the subplot components
#'
plot_res <- function(model, na.rm = FALSE, margin = 0.04){
  `%>%` <- magrittr::`%>%`
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

#' Plotting the forecast output
#' @export
#' @param forecast A forecastML object
#' @param line_color A character, defines both the actual and forecast color, by default will use dark blue ("#00526d")
#' @param pi_color A character, defines the color palette of the prediction intervals


plot_fc <- function(forecast, line_color = "#00526d", pi_color = "Greys"){

  palette_df <- palette <- maxcolors <- pi <- color_setting <- NULL
  palette_df <- RColorBrewer::brewer.pal.info
  palette <- palette_df %>% base::row.names()

  # Error handling
  if(base::is.null(pi_color)){
    pi_color <- "Greys"
  } else if(pi_color %in% palette){
    warning("The 'pi_color' argument is not valid, using the default value ('Greys'")
    pi_color <- "Greys"
  }

  if(base::is.null(line_color)){
    line_color <- "#00526d"
  } else if(base::is.character(line_color)){
    warning("The 'line_color' argument is not valid, using the default value ('#00526d'")
    line_color <- "#00526d"
  }


  maxcolors <-  palette_df$maxcolors[base::which(base::row.names(palette_df) == pi_color)]


  color_setting <- list(line = line_color)
  pi <- base::sort(forecast$parameters$pi, decreasing = TRUE)
  for(i in base::seq_along(forecast$parameters$pi)){
    color_setting[[base::paste("pi", pi[i] * 100, sep = "")]] <- RColorBrewer::brewer.pal(maxcolors, pi_color)[1 + i]
  }


  p <- plotly::plot_ly() %>%
    plotly::add_lines(x = ~ forecast$actual[[forecast$parameters$index]],
                      y = ~ forecast$actual[[forecast$parameters$y]],
                      line = list(color = color_setting$line),
                      name = "Actual")

  for(i in base::seq_along(pi)){
    p <- p %>%
      plotly::add_ribbons(x = forecast$forecast[[forecast$parameters$index]],
                          ymin = forecast$forecast[[paste("lower", pi[i] * 100, sep = "")]],
                          ymax = forecast$forecast[[paste("upper", pi[i] * 100, sep = "")]],
                          line = list(color = color_setting[[base::paste("pi", pi[i] * 100, sep = "")]]),
                          fillcolor = color_setting[[base::paste("pi", pi[i] * 100, sep = "")]],
                          name = base::paste(pi[i] * 100, "% Prediction Interval", sep = ""))
  }



  p <- p %>% plotly::add_lines(x = forecast$forecast[[forecast$parameters$index]],
                               y = forecast$forecast$yhat,
                               name = "Forecast",
                               line = list(color = color_setting$line, dash = "dash")) %>%
    plotly::layout(title = base::paste("Forecast of", forecast$parameters$y,
                                       "Series <br> Method - ",
                                       forecast$parameters$method,
                                       "; Horizon - ",
                                       forecast$parameters$h,
                                       sep = " "),
                   yaxis = list(title = forecast$parameters$y),
                   xaxis = list(title = forecast$parameters$index)
    )
  return(p)

}

