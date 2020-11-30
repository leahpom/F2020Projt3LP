#' Title
#'
#' @param lm a linear model
#' @param k the number of independent variables in the model
#'
#' @return Shapiro-Wilk Normality Test
#' @return Breusch-Pagan Test
#' @return Durbin-Watson Test
#' @return Variance Inflation Factor
#' @return plots of the residuals against each independent variable
#' @return a plot of the fitted values vs. residuals
#' @return a QQ plot
#' @export
#'
#' @examples
#' y <- rnorm(100, 1, 0)
#' x1 <- rnorm(100, 2, 1)
#' x2 <- rnorm(100, 3, 2)
#' ylm <- lm(y ~ x1 + x2)
#' fun1(ylm, 2)
fun1 <- function(lm, k){

  # LIBRARY STATEMENTS
  # requireNamespace(lmtest)
  # requireNamespace(car)

  # COMMAND LINE STATISTICS

  # Shapiro-Wilk Statistics
  # this will allow us to check the normality assumption
  # null hypohtesis is Normal distribution

  res <- residuals(lm) # these are the residuals
  shap.test <- shapiro.test(res) # object to release later

  # Breusch-Pagan Test
  # this will allow us to check the equality of variance assumption
  # null hypothesis is equal variance (homoskedasticity)

  bp <- lmtest::bptest(lm)

  # Durbin-Watson Test
  # this will allow us to check the independence assumption
  # we expect the calculated statistic to be "small"/close to 2

  dw <- lmtest::dwtest(lm)

  # Variance Inflation Factor (VIF)
  # this allows us to check for multicollinearity
  # we want the vif to be less than 10 or less than 5 (user can pick cutoff)

  varif <- car::vif(lm)

  list1 <- list(shap.test, bp, dw, varif) # list of the command line statistics

  # PLOTS

  # Residual Plots for each independent variable
  # this will allow us to check the zero mean value of epsilon assumption

  mod <- lm$model # this will give us the data frame that the model uses
  k1 <- k + 1 # this is to help with indexing
  indep <- mod[, 2:k1] # this gives the independent variables
  indep <- data.frame(indep) # this should make it a data frame for the loop
  name <- names(mod) # a vector of the names
  name <- name[2:k1] # this should only have the names of the independent variables
  l <- list() # empty list to fill
  # res is the object for the residuals

  # now, we want a for-loop to help us look at the graphs

  for (i in 1:k) {
    # store the independent variable in an object
    x <- indep[, i]
    # create the name vector
    xname <- name[i]
    main.name <- c("Residuals vs.", xname) # this will create the main label
    # plot
    p <- plot(x=x, y=res, xlab = xname, ylab = "Residuals", main = main.name, col = "darkslategrey")
    # print the plot
    # print(p)
    l <- list(l, p)
  }

  list2 <- list(list1, l)

  # RETURNS: plots of the residuals against each independent variable

  # Residuals vs. fitted values
  # this will allow us to check the constant variance assumption

  # we already stored the residuals in an object
  # res

  # put the fitted values in an object
  fit <- fitted(lm)

  ts <- plot(res~fit, bg="Blue", pch=21, cex=1.2, ylim=c(min(res),1.1*max(res)),
             xlim=c(min(fit),1.1*max(fit)),xlab="Fitted Values", ylab="Residuals",
             main="Residuals vs. Fitted Values")

  # RETURNS: a plot of the fitted values vs. residuals

  # QQ Plot
  # this will allow us to look at the Normality Assumption

  nc <- car::qqPlot(res)

  list3 <- list(list1, ts, nc)

  return(list3) # return statement
  # this seems to return everything I want, even though I'm not entirely sure why

}
