###########################################################################
###########################################################################
#' Portfolio Composition Across Return Spectrum Function
#'
#' This function generates stacked bar chart displaying portfolio composition 
#' given a target return rate.
#'
#' @param in_data (dataframe): Input dataframe for which optimal portfolio is selected from.
#' @param target_return (numeric/vector): Optional parameter that constraints optimal portfolio to have specific return. 
#'         Default is NULL.
#' @param up_weight (numeric): Optional parameter that sets upside weight in asymmetric risk. Default is NULL, 
#'         which equates to a symmetric portfolio.
#' @param down_weight (numeric): Optional parameter that sets downside weight in asymmetric risk. Default is NULL, 
#'         which equates to a symmetric portfolio.
#' @param verbose (boolean): Print informative messages about removed columns.
#' 
#' @return (list) composition_plot_data (dataframe): Corresponding data for all efficient frontiers., 
#'                composition_plot (ggplot figure): ggplot figure of efficient frontiers.
#' @export
portfolio_composition <- function(in_data, up_weight = NULL, down_weight = NULL, verbose=TRUE){
  # Get the overall optimal portfolio
  optimal_portfolio_total <- asymmetricrisk::optimal_portfolio(in_data, up_weight=up_weight, down_weight=down_weight)
  optimal_portfolio_return <- optimal_portfolio_total$expected_return
  
  # Sequence of expected returns (x-axis of figure)
  expected_returns <- seq(min(colMeans(na.omit(in_data))), max(colMeans(na.omit(in_data))), length.out = 100)
  expected_returns <- expected_returns[expected_returns>= optimal_portfolio_return]
  
  # Compute the efficient frontier portfolios using apply()
  # If no solution can be found, pass on as NA
  portfolio_composition_data <- lapply(expected_returns, function(return_i) {
    tryCatch({
      optimal_portfolio <- asymmetricrisk::optimal_portfolio(in_data, return_i, up_weight = up_weight, down_weight = down_weight, verbose=verbose)
      list(Return = return_i, Weights = optimal_portfolio$weights)
    }, error = function(e) {
      message("Error encountered for return_i = ", return_i, ": ", e$message)
      data.frame(Return = NA, Weights = NA)
    })
  })
  
  # Create dataframe
  plot_data <- dplyr::bind_rows(lapply(portfolio_composition_data, function(entry) {
    weights <- entry$Weights
    return_i <- entry$Return
    
    # If weights is NA or NULL, create NA-filled row
    if (is.null(weights) || all(is.na(weights))) {
      return(data.frame(
        return_i = return_i,
        value = NA,
        column_name = NA
      ))
    }
    data.frame(
      return_i = return_i,
      value = as.vector(weights),
      column_name = names(weights)
    )
  })) %>% 
    filter(!is.na(value))
  
  
  plot <- ggplot2::ggplot(plot_data, aes(x = as.numeric(return_i), y = value, fill = column_name)) +
    geom_bar(stat = "identity", color = NA) +
    labs(
      x = "Return",
      y = "Cumulative Weight",
      fill = NULL,
      title = "Portfolio Composition by Return"
    ) +
    # geom_vline(xintercept = true_optimal_portfolio$expected_return, color = "black", linetype = "dashed", linewidth = 0.4) + 
    # scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand=c(0.005,0),
    #                    labels = label_percent(accuracy = 1)) +
    scale_x_continuous(expand=c(0.005,0)) +
    scale_y_continuous(expand=c(0.005,0)) +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20)
    ) +
    theme_classic()
  
  
  return(list(composition_plot_data = plot_data, composition_plot = plot))
  
}
