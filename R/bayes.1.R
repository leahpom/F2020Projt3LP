#' Bayesian Regression Function
#'
#' @param iter number of iterations for the simulations
#' @param init prior
#' @param y the column number in the data frame for the dependent variable
#' @param a the start column number in the data frame for the independent variables
#' @param b the end column number in the data frame for the independent variables. The independent variables MUST be consecutive
#' @param df data frame for the regression
#' @param b.0 prior mean of beta. Set to function default, but can be updated by the user
#' @param d.0 prior scale parameter. Set to function default, but can be updated by the user
#'
#' @return Command-line stats for posterior and mean
#' @return Mean for posteriors
#' @return Plot of sampler
#' @return Summary statistics for classical regression
#' @return Confidence intervals for classical regression
#' @return Summary statistics for bayesian regression
#' @return Posterior plots of marginal distributions
#' @return Posterior MCMC Trace and History Plots
#' @export
#'
#' @examples
#' \dontrun{data <- diamonds, bayes.1(y = 1, a = 2, b = 3, df = data)}
#'
bayes.1 <- function(iter = 10000, init = 0.5, y, a, b, df, b.0  = 0, d.0 = 0.001){
  # init is default set to a low-impact prior

  # 1. Command-line statistics for posterior and mean

  accept <- vector("integer", length = iter)
  accept[1]<-1
  post<-vector("integer",length=iter)
  post[1] <- init # initial value

  h<-function(x) x^4*(1-x)^6 # h function proportional to posterior

  for(i in 2:iter){
    pr <- runif(1,0,1) # prob between 0,1

    ifelse(h(pr)<h(post[i-1]), alpha<-h(pr)/h(post[i-1]), alpha<-1) # flat prior

    ifelse(runif(1,0,1)< alpha,
           {post[i] <- pr
           accept[i]<-1}, #if true
           {post[i] <- post[i-1]
           accept[i]<-0}# if false
    )

  }
  p <- plot(post, type="l") # this is a plot of our sampler
  m <- mean(accept)
  mc <- coda::as.mcmc(post) # this will be used later in bayes.5
  sum.1 <- summary(mc) # summary stats for the posterior and mean

  # RETURN: (1) command-line stats for posterior and mean
  # (2) mean for posteriors
  # (3) plot of sampler

  # works up to here

  # 2. Point and Interval Estimates for Classical Statistics

  # first, create a new data frame to use for the regression

  y <- df[, y] # the dependent variable
  df.x <- df[, a:b] # the independent variables
  # doing it this way allows the user to enter as many independent variables
  # as they want
  df.new <- as.data.frame(cbind(y, df.x)) # create a new data frame to use for the regression

  # then do the linear model, summary, and intervals
  ylm <- lm(df.new[, 1] ~ . , data = df.new) # the linear model object
  sum.c <- summary(ylm) # summary of the model
  int.c <- s20x::ciReg(ylm) # confidence intervals for the betas

  # RETURNS: (1) summary stats for classical regression
  # (2) confidence intervals for classical regression

  # works up to here

  # 3. Bayesian Summary Statistics

  bayesian <- MCMCpack::MCMCregress(df.new[,1] ~ ., burnin=1000, mcmc = 10000, data = df.new, b0 = b.0, d0 = d.0)

  sum.b <- summary(bayesian)

  # RETURNS: (1) summary statistics for bayesian regression

  # works up to here

  # 4. Posterior plots of marginal distributions for the parameters

  p2 <- ggmcmc::ggs(bayesian)
  hist.b <- ggmcmc::ggs_histogram(p2) # plots the histograms


  # RETURNS: (1) Posterior plots of marginal distributions

  # works up to here

  # 5. Posterior MCMC trace and history plots showing convergence

  p3 <- plot(mc) # uses the results from part 1

  sir.list <- list("Command-Line Statistics for Posterior and Mean" = sum.1,
                   "Mean for Posteriors" = m, "Plot of Sampler" = p, # end of stats from part 1
                   "Point Estimates for Classical Regression" = sum.c,
                   "Interval Estimates for Classical Regression" = int.c, # end of stats from part 2
                   "Estimates from Bayesian Regression" = sum.b, # end of stats from part 3
                   "Posterior Plots of Marginal Distributions" = hist.b, # end of results from part 4
                   "Posterior MCMC Trace and History Plots" = p3)

  return(sir.list)

  # RETURNS: (1) Posterior MCMC Trace and History Plots

  # works up to here!!

}
