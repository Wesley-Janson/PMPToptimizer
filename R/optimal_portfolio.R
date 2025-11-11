###########################################################################
###########################################################################
#' Generate Optimal Portfolio
#'
#' This function selects the optimal municipal revenue portfolio based on parameter 
#' values given (Optionally with semivariance weighting).
#'
#' @param returns_data (dataframe): Input dataframe for which optimal portfolio is selected from.
#' @param target_return (numeric/vector): Optional parameter that constraints optimal portfolio to have specific return. 
#'         Default is NULL.
#' @param up_weight (numeric): Optional parameter that sets upside weight in asymmetric risk. Default is NULL, 
#'         which equates to a symmetric portfolio.
#' @param down_weight (numeric): Optional parameter that sets downside weight in asymmetric risk. Default is NULL, 
#'         which equates to a symmetric portfolio.
#' @param split (numeric/character): Optional parameter that is threshold that determines where to split data between upside 
#'         and downside risk. Can be a numeric value or 'mean'/'median'. Default is 0.
#' @param verbose (boolean): Print informative messages about removed columns.
#' 
#' @return (list) weights (vector): Optimal weights selected to minimize portfolio standard deviations.,
#'                minimum_risk (numeric): Variance of the optimal portfolio.,
#'                expected_return (numeric): Expected return of optimal portfolio, given optimal weights 
#'                    and mean returns of each revenue category. 
#' @export
optimal_portfolio <- function(returns_data, target_return=NULL, up_weight=NULL, down_weight=NULL, split=0, verbose=TRUE){
  ### Remove any columns with all zeros, print message if columns are removed
  zero_cols <- names(na.omit(returns_data))[sapply(na.omit(returns_data), function(col) all(col == 0, na.rm = TRUE))]
  returns_data <- returns_data[, !names(returns_data) %in% zero_cols]
  if (length(zero_cols) > 0 & verbose==TRUE) {
    message("Columns removed due to only having zero values: ", paste(zero_cols, collapse = ", "))
  }
  
  
  # Compute mean returns for each asset - used in expected return of optimal portfolio
  mean_returns <- colMeans(na.omit(returns_data))
  
  ### Split data into upside and downside data, 0 out observations (category-wise)
  if (!is.null(down_weight) && is.numeric(down_weight) && down_weight<=1 && down_weight>=0){
    if (is.numeric(split)) {
      split_values <- rep(split, length(returns_data))
    } else if (split == "mean") {
      split_values <- colMeans(returns_data, na.rm = TRUE)
    } else if (split == "median") {
      split_values <- apply(returns_data, 2, median, na.rm = TRUE)
    } else {
      print("Split must be numeric value, 'mean', or 'median'.")
    }
    
    # Upside Data
    chg_upside <- na.omit(returns_data)
    chg_upside[] <- as.matrix(Map(function(column, split_val) {
      ifelse(column <= split_val, 0, column)
    }, chg_upside, split_values))
    
    chg_downside <- na.omit(returns_data)
    chg_downside[] <- as.matrix(Map(function(column, split_val) {
      ifelse(column > split_val, 0, column)
    }, chg_downside, split_values))
    
    ### Remove any 0 value columns from upside/downside data
    zero_cols_split <- union(names(na.omit(chg_downside))[sapply(na.omit(chg_downside), function(col) all(col == 0, na.rm = TRUE))], names(na.omit(chg_upside))[sapply(na.omit(chg_upside), function(col) all(col == 0, na.rm = TRUE))])
    
    chg_upside <- chg_upside[, !names(chg_upside) %in% zero_cols_split]
    if (length(zero_cols_split) > 0 & verbose==TRUE) {
      message("Columns removed from chg_upside due to only having zero values: ", paste(zero_cols_split, collapse = ", "))
    }
    
    chg_downside <- chg_downside[, !names(chg_downside) %in% zero_cols_split]
    if (length(zero_cols_split) > 0 & verbose==TRUE) {
      message("Columns removed from chg_downside due to only having zero values: ", paste(zero_cols_split, collapse = ", "))
    }
    
    # Remove 0 value column from mean returns data and split_values
    split_values <- if (length(zero_cols_split)) split_values[-which(names(mean_returns) %in% zero_cols_split)] else split_values
    mean_returns <- mean_returns[!names(mean_returns) %in% zero_cols_split]
    
    
    # Create upside/downside deviations from defined target (category-wise)
    dev_upside <- as.data.frame(Map(function(col, split) {
      ifelse(col == 0, 0, col - split)
    }, chg_upside, split_values))
    dev_downside <- as.data.frame(Map(function(col, split) {
      ifelse(col == 0, 0, col - split)
    }, chg_downside, split_values))
    
    # Create weighted squared deviations upside (mean is the upside semivariance, total is sum of squares(semi-sum?))
    weightedSqrDev_upside <- up_weight * (dev_upside^2)
    weightedSqrDev_downside <- down_weight * (dev_downside^2)
    
    # Generate combined, weighted squared deviations
    weighted_SqrDev <- weightedSqrDev_upside + weightedSqrDev_downside
    
    ### Weighted deviations by column
    weighteddev_downside <- down_weight * dev_downside
    weighteddev_upside <- up_weight * dev_upside
    
    weighted_deviations <- weighteddev_downside + weighteddev_upside
    
    ##### Then mean of weighted deviations is our new semivariance (or maybe weighted variance)
    semivariance <- c(colMeans(weighted_SqrDev)) # Diagonal in output matrix
    
    ##### Generate Co-Semivariance matrix
    cosemivariance_matrix <- crossprod(as.matrix(weighted_deviations)) / nrow(weighted_deviations) 
    diag(cosemivariance_matrix) <- semivariance # Add semivariance vector in as diagonal
    
    cov_matrix <- cosemivariance_matrix
    
  } else if ((!is.numeric(up_weight) & !is.null(up_weight)) | (!is.numeric(down_weight) & !is.null(down_weight))) {
    print("Invalid up_weight/down_weight parameters entered. Please make sure it is a numeric value in the range [0,1].")
    stop()
  } else {
    # Compute the covariance matrix of asset returns - IF up_weight/down_weight IS NULL
    cov_matrix <- stats::cov(as.matrix(na.omit(returns_data)))*(nrow(as.matrix(na.omit(returns_data))) - 1) / nrow(as.matrix(na.omit(returns_data)))
    
  }
  
  # Define the number of assets
  n_assets <- ncol(cov_matrix)
  
  # Define inputs for quadprog: minimize w^T cov w subject to sum(w)=1 and w>=0 (and optional mean_returns @ w = target_return)
  Dmat <- 2 * cov_matrix  # Quadratic term
  dvec <- rep(0, n_assets)  # Linear term (0 because we're minimizing variance)
  
  # If no target_return
  if (is.null(target_return)){
    # Constraint matrix (Amat): We have two constraints:
    # 1. Sum of weights = 1 (No leverage)
    # 2. No short-selling (weights >= 0)
    Amat <- cbind(rep(1, n_assets), diag(n_assets))
    
    # Right-hand side of constraints
    bvec <- c(1, rep(0, n_assets))
    
    # Set equality constraints: just the first constraint (sum to 1)
    meq <- 1
    
  } else if (!is.numeric(target_return) & !is.null(target_return)){
    print("Invalid target_return parameter value supplied. It must be numeric.")
    stop()
  } else if(length(target_return)>0){ 
    
    ### Equality constraints
    # 1) sum of weights = 1
    # 2) mu_i * w_i = target_i
    A_eq <- rbind(
      rep(1, n_assets),
      t(mean_returns)
    )
    
    b_eq <- c(1, target_return)
    meq <- nrow(A_eq)
    
    ### Inequality constraints: 
    # 1) weight_i >= 0
    A_ineq <- diag(n_assets)
    b_ineq <- rep(0, n_assets)
    
    ### Combine equality and inequality constraints
    Amat <- t(rbind(A_eq, A_ineq))
    bvec <- c(b_eq, b_ineq)
    
  } else {
    # Constraint matrix (Amat): We have three constraints:
    # 1. Sum of weights = 1 (No leverage)
    # 2. Expected portfolio return = target_return
    # 3. No short-selling (weights >= 0)
    Amat <- cbind(rep(1, n_assets), mean_returns, diag(n_assets))  # (n x (2 + n)) constraint matrix
    
    # Right-hand side of constraints
    bvec <- c(1,                     # sum(weights) = 1
              target_return,         # portfolio target_return
              rep(0, n_assets)       # 
    )  # (2 + n) vector
    
    # Set equality constraints: first two constraints (sum to 1 & target return) are equality constraints
    meq <- 2
  }
  
  # Solve the quadratic program
  optimization_result <- tryCatch(
    {
      quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq)
    },
    error = function(e) {
      message("Error in optimization function, please check if inputs are feasible: ", e$message)
      stop()
    }
  )
  
  # Extract optimal weights, corresponding minimum variance
  optimal_weights <- optimization_result$solution
  if (!is.null(down_weight) && is.numeric(down_weight) && down_weight<=1 && down_weight>=0){
    names(optimal_weights) <- names(returns_data)[!(names(returns_data) %in% zero_cols_split)]
  } else {
    names(optimal_weights) <- names(returns_data)
  }
  
  min_variance <- optimization_result$value
  
  # Compute expected return of the optimal portfolio
  expected_portfolio_return <- sum(optimal_weights * mean_returns)
  
  # Add back in zero value column(s)
  optimal_weights[names(optimal_weights)] <- optimal_weights
  
  if (!is.null(down_weight) && is.numeric(down_weight) && down_weight<=1 && down_weight>=0){
    optimal_weights[setdiff(zero_cols_split, names(optimal_weights))] <- 0
  } 
  
  # Return list of results
  return(list(weights=round(optimal_weights, 7), minimum_risk=min_variance, expected_return=expected_portfolio_return))
  
}

