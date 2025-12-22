###########################################################################
###########################################################################
#' Efficient Frontier Function
#'
#' $efficient_frontier()$ creates an efficient frontier, with corresponding data and plot, for
#' given data. In the event that an optimal portfolio cannot be
#' solved for, an NA value is passed on and that (risk, return) xy-point is not plotted.
#'
#' @param in_data Input data frame, list or environment (or object coercible by $as.data.frame$ to a data frame) of percent change dataframe from which the optimal portfolio is selected.
#' @param target_return Optional numeric parameter that constrains the optimal portfolio to have a specific return. 
#'         Default is NULL.
#' @param up_weight Optional numeric parameter that sets the upside weight in asymmetric risk. Default is NULL, 
#'         which equates to a symmetric portfolio.
#' @param down_weight Optional numeric parameter that sets downside weight in asymmetric risk. Default is NULL, 
#'         which equates to a symmetric portfolio.
#' @param verbose Optional logical. If TRUE, prints informative messages about removed columns. Default value is TRUE.
#' 
#' @return (list) plot_data (dataframe): Corresponding data for efficient frontier.,
#'                optimal_portfolio (dataframe): (Risk, Return) coordinates corresponding to optimal portfolio 
#'                     as denoted in the plot., 
#'                efficient_frontier_plot (ggplot figure): ggplot figure of efficient frontier. 
#' @export
efficient_frontier <- function(in_data, up_weight=NULL, down_weight=NULL, verbose=TRUE){
  # Get the overall optimal portfolio
  optimal_portfolio_total <- asymmetricrisk::optimal_portfolio(in_data, up_weight=up_weight, down_weight=down_weight)
  optimal_portfolio_total$minimum_risk <- sqrt(optimal_portfolio_total$minimum_risk)
  optimal_portfolio_df <- data.frame(Risk = optimal_portfolio_total$minimum_risk, 
                                     Return = optimal_portfolio_total$expected_return, 
                                     Label = "Optimal Portfolio")
  
  # Sequence of expected returns for the efficient frontier
  expected_returns <- seq(min(colMeans(na.omit(in_data))), max(colMeans(na.omit(in_data))), length.out = 100)
  expected_returns <- expected_returns[expected_returns>= optimal_portfolio_df$Return]
  
  # Compute the efficient frontier portfolios using apply()
  # If no solution can be found, pass on as NA
  efficient_frontier_data <- lapply(expected_returns, function(return_i) {
    tryCatch({
      optimal_portfolio_i <- asymmetricrisk::optimal_portfolio(in_data, return_i, up_weight=up_weight, down_weight=down_weight, verbose=verbose)
      data.frame(Risk = optimal_portfolio_i$minimum_risk, Return = optimal_portfolio_i$expected_return)
    }, error = function(e) {
      message("Error encountered for return_i = ", return_i, ": ", e$message)
      data.frame(Risk = NA, Return = NA)
    })
  })
  
  # Combine results into a single dataframe
  efficient_frontier <- dplyr::bind_rows(efficient_frontier_data)
  efficient_frontier$Risk <- sqrt(efficient_frontier$Risk)
  
  print(max(efficient_frontier$Return))
  
  # Plot the efficient frontier with the optimal portfolio point
  efficient_frontier_plot <- ggplot2::ggplot(efficient_frontier, aes(x = Risk, y = Return)) +
    geom_vline(xintercept = 0, color = "black", linetype = "solid", linewidth = 0.75) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.75) +
    geom_point(aes(x = Risk, y = Return), color = "gray", size = 1) +
    geom_point(data = optimal_portfolio_df, aes(x = Risk, y = Return, color = Label), 
               size = 4, shape = 17) +  # Optimal portfolio
    scale_color_manual(values = c("Optimal Portfolio" = "red")) +
    labs(title = "Efficient Frontier with Optimal Portfolio",
         x = "Risk (Standard Deviation)",
         y = "Expected Return",
         color = "Legend") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.justification = "left", legend.title = element_blank()) +
    scale_x_continuous(limits = c(0, max(efficient_frontier$Risk)))
  
  return(list(plot_data = efficient_frontier, optimal_portfolio = optimal_portfolio_df[,c("Risk", "Return")], efficient_frontier_plot = efficient_frontier_plot))
}
