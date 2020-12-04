#' Bayesian Regression Function
#'
#' @param y the column number in the data frame for the dependent variable
#' @param a the start column number in the data frame for the independent variables
#' @param b the end column number in the data frame for the independent variables. The independent variables MUST be consecutive
#' @param df data frame for the regression
#' @param name.dep name of the dependent variable. Important for doing regression.
#' @param b.0 prior mean of beta. Set to mcmcRegress default, but can be updated by the user
#' @param d.0 prior scale parameter. Set to mcmcRegress default, but can be updated by the user
#' @param c.0 prior shape parameter. Set to mcmcRegress default, but can be updated by the user
#' @param burn.in value for the burn.in. Set to mcmcRegress defualt, but can be updated by the user
#'
#' @return Summary statistics for classical regression
#' @return Confidence intervals for classical regression
#' @return Summary statistics for Bayesian regression
#' @return Posterior plots of marginal distributions
#' @return Posterior MCMC Trace and History Plots
#' @return Posterior Diagnostics
#' @export
#'
#' @examples
#' data <- diamonds
#' bayes.1(y = 1, a = 2, b = 3, df = data, name.dep = "price")
#'
bayes.1 <- function(y, a, b, df, name.dep, b.0  = 0, d.0 = 0.001, c.0 = 0.001, burn.in = 1000){

  # 1. Point and Interval Estimates for Classical Statistics

  # first, create a new data frame to use for the regression

  y <- df[, y] # the dependent variable
  df.x <- df[, a:b] # the independent variables
  # doing it this way allows the user to enter as many independent variables
  # as they want
  df.new <- as.data.frame(cbind(y, df.x)) # create a new data frame to use for the regression

  # then do the linear model, summary, and intervals
  names(df.new)[names(df.new) == name.dep] <- "y" # this makes it generic so we can appropriately set up
  # the regression by calling the dependent variable, then calling all other variables independent
  ylm <- lm(y ~ . , data = df.new) # the linear model object
  sum.c <- summary(ylm) # summary of the model
  int.c <- s20x::ciReg(ylm) # confidence intervals for the betas

  # RETURNS: (1) summary stats for classical regression
  # (2) confidence intervals for classical regression

  # works up to here

  # 2. Bayesian Summary Statistics and Plots

  bayesian <- MCMCpack::MCMCregress(y ~ ., burnin=burn.in, mcmc = 10000,
                                    data = df.new, b0 = b.0, d0 = d.0, c0 = c.0)

  sum.b <- summary(bayesian)

  # RETURNS: (1) summary statistics for Bayesian regression
  # "Command line statistics for posterior and mean"

  # works up to here

  # 3. Posterior plots of marginal distributions for the parameters

  p2 <- ggmcmc::ggs(bayesian)
  hist.b <- ggmcmc::ggs_histogram(p2) # plots the histograms


  # RETURNS: (1) Posterior plots of marginal distributions

  # works up to here

  # 4. Posterior MCMC trace and history plots showing convergence

  p <- plot(bayesian, auto.layout = FALSE) # uses the results from part 1

  # works up to here!

  # 5. Posterior Diagnostics and Plot

  pd1 <- coda::geweke.diag(bayesian)


  # RETURNS: (1) Posterior Diagnostics
  # (2) Posterior Diagnostic Plots

  sir.list <- list("Point Estimates for Classical Regression" = sum.c,
                   "Interval Estimates for Classical Regression" = int.c, # end of stats from part 2
                   "Estimates from Bayesian Regression" = sum.b, # end of stats from part 3
                   "Posterior Plots of Marginal Distributions" = hist.b, # end of results from part 4
                   "Posterior MCMC Trace and History Plots" = p, # end of results from part 5
                   "Posterior Diagnostics" = pd1)

  return(sir.list)

}
