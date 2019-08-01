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
#' @param theme A character, defines the color theme to be used in the plot output. Available themes - "normal" (default), "darkBlue", "darkPink"


plot_fc <- function(forecast, theme = "normal"){

  palette_df <- palette <- maxcolors <- pi <- color_setting <- NULL

  pi <- base::sort(forecast$parameters$pi, decreasing = TRUE)

  if(base::is.null(theme) || !base::is.character(theme)){
    stop("The value of the 'theme' argument is not valid")
  } else if(theme == "normal"){
    col_setting <- base::list(
      line_color = "#00526d",
      ribbon_color = c(150, 150, 150),
      gridcolor = NULL,
      zerolinecolor = NULL,
      linecolor = NULL,
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      font = list(
        color = 'black'
      )
    )

    n_pi <- base::length(forecast$parameters$pi)
    a_pi <- seq(from = 0.6, to = 0.8, length.out = n_pi) %>% base::sort(decreasing = FALSE)
    for(i in base::seq_along(forecast$parameters$pi)){
      color_setting[[base::paste("pi", pi[i] * 100, sep = "")]] <- base::paste("rgba(",base::paste(col_setting$ribbon_color, collapse = ","), a_pi[i] , ")", collapse = " ")
    }
  } else if(theme == "darkBlue"){
      col_setting <- base::list(
        line_color = "white",
        ribbon_color = c(66, 134, 244),
        gridcolor = "#444444",
        zerolinecolor = "#6b6b6b",
        linecolor = "#6b6b6b",
        paper_bgcolor = "black",
        plot_bgcolor = "black",
        font = list(
          color = 'white'
        )
      )

      n_pi <- base::length(forecast$parameters$pi)
      a_pi <- seq(from = 0.6, to = 0.8, length.out = n_pi) %>% base::sort(decreasing = FALSE)
      for(i in base::seq_along(forecast$parameters$pi)){
        color_setting[[base::paste("pi", pi[i] * 100, sep = "")]] <- base::paste("rgba(",base::paste(col_setting$ribbon_color, collapse = ","), a_pi[i] , ")", collapse = " ")
      }
    } else if(theme == "darkPink"){
      col_setting <- base::list(
        line_color = "white",
        ribbon_color = c(227, 119, 194),
        gridcolor = "#444444",
        zerolinecolor = "#6b6b6b",
        linecolor = "#6b6b6b",
        paper_bgcolor = "black",
        plot_bgcolor = "black",
        font = list(
          color = 'white'
        )
      )

      n_pi <- base::length(forecast$parameters$pi)
      a_pi <- seq(from = 0.6, to = 0.8, length.out = n_pi) %>% base::sort(decreasing = FALSE)
      for(i in base::seq_along(forecast$parameters$pi)){
        color_setting[[base::paste("pi", pi[i] * 100, sep = "")]] <- base::paste("rgba(",base::paste(col_setting$ribbon_color, collapse = ","), a_pi[i] , ")", collapse = " ")
      }
    } else if(theme == "test"){
      col_setting <- base::list(
        line_color = "rgb(40, 99, 148)",
        ribbon_color = c(193, 136, 192),
        gridcolor = NULL,
        zerolinecolor = "rgb(197, 208, 232)",
        linecolor = NULL,
        paper_bgcolor = "rgb(255, 239, 220)",
        plot_bgcolor = "rgb(255, 239, 220)",
        font = list(
          color = 'black'
        )
      )

      n_pi <- base::length(forecast$parameters$pi)
      a_pi <- seq(from = 0.6, to = 0.8, length.out = n_pi) %>% base::sort(decreasing = FALSE)
      for(i in base::seq_along(forecast$parameters$pi)){
        color_setting[[base::paste("pi", pi[i] * 100, sep = "")]] <- base::paste("rgba(",base::paste(col_setting$ribbon_color, collapse = ","), a_pi[i] , ")", collapse = " ")
      }
    }

  p <- plotly::plot_ly() %>%
    plotly::add_lines(x = ~ forecast$actual[[forecast$parameters$index]],
                      y = ~ forecast$actual[[forecast$parameters$y]],
                      line = list(color = col_setting$line_color),
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
                               line = list(color = col_setting$line_color, dash = "dash")) %>%
    plotly::layout(title = base::paste("Forecast of", forecast$parameters$y,
                                       "Series <br> Method - ",
                                       forecast$parameters$method,
                                       "; Horizon - ",
                                       forecast$parameters$h,
                                       sep = " "),
                   paper_bgcolor = col_setting$paper_bgcolor,
                   plot_bgcolor = col_setting$plot_bgcolor,
                   font = col_setting$font,
                   yaxis = list(title = forecast$parameters$y,
                                linecolor = col_setting$linecolor,
                                zerolinecolor = col_setting$zerolinecolor,
                                gridcolor= col_setting$gridcolor),
                   xaxis = list(title = forecast$parameters$index,
                                linecolor = col_setting$linecolor,
                                zerolinecolor = col_setting$zerolinecolor,
                                gridcolor= col_setting$gridcolor)
    )

  return(p)

}

