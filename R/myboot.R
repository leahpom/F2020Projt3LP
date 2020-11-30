#' Title
#'
#' @param iter # number of iterations for the bootstrap
#' @param df # data grame with first column response, other columns are relevant independent variables
#' @param alpha # significance level
#'
#' @return Summary statistics for the bootstrapped betas
#' @return Histograms of the bootstrapped betas
#' @return Confidence intervals for the boostrapped betas
#' @export
#'
#' @examples
#' y <- rnorm(100, 0, 1)
#' x1 <- rnorm(100, 1, 2)
#' x2 <- rnorm(100, 2, 3)
#' data <- cbind(y, x1, x2)
#' data <- data.frame(data)
#' myboot(iter=1000, df=data)
myboot <- function(iter, df, alpha=0.05){ # first column is response,
  #other columns are X's (ONLY relevant data!!)
  # the last column must be the interaction term (for this problem)
  # 1. Create the empty matrices
  mat1 <- data.frame(matrix(NA, nrow = iter, ncol = ncol(df)))
  names(mat1)<-names(df)
  names(mat1)[1]<-"Intercept"

  n <- nrow(df)
  num.err<-0
  dfn <- data.frame(matrix(NA, nrow=nrow(df), ncol=ncol(df)))

  # 2. The for-loop that will calculate the beta estimates and put them in mat1
  for (i in 1:iter) {
    int <- sample(1:n, size = n, replace = TRUE)
    for(k in 1:nrow(df)){ # trying to fix sampling error i originally got
      dfn[k,] <- df[int[k], ]
    }
    Y <- dfn[, 1]
    Y <- as.matrix(Y)
    X <- df[, -1] # generic for any size
    X <- as.matrix(cbind(1, X))

    XTX.inv <- try(solve(t(X)%*%X), silent = TRUE)

    if(inherits(XTX.inv, "try-error")){
      num.err <- num.err + 1
    }
    else{
      #XTY <- t(X)%*%Y
      B.hat <- XTX.inv%*%t(X)%*%Y
      mat1[i, ] <- B.hat
    }

  }

  # return(mat1) # this works

  # 3. Summary Statistics

  mat.sum <- matrix(NA, nrow=ncol(mat1), ncol=6)
  colnames(mat.sum) <- c("Min", "1st Qu.",  "Median", "Mean", "3rd Qu.", "Max")
  rownames(mat.sum) <- paste("Beta", colnames(mat1))

  for(j in 1:ncol(df)){
    est <- mat1[, j]
    q <- summary(est)
    mat.sum[j, ] <- q
  }

  # Works up to here!!

  # THE CONFIDENCE INTERVALS

  # 4. Confidence Intervals for Betas
  dfreedom <- nrow(df) - ncol(df)
  t <- qt(1-alpha/2, dfreedom)
  z <- qnorm(0.80)

  conf.int <- matrix(NA, nrow=ncol(mat1), ncol = 2)
  colnames(conf.int) <- c("2.5%", "97.5%")
  rownames(conf.int) <- paste("Beta", colnames(mat1))

  for(j in 1:ncol(mat1)){
    est <- mat1[, j]
    beta.1 <- quantile(est, probs = alpha/2, na.rm = TRUE)
    beta.2 <- quantile(est, probs = 1-alpha/2, na.rm = TRUE)
    LB <- min(beta.1, beta.2)
    UB <- max(beta.1, beta.2)
    ci <- c(LB, UB)
    conf.int[j, ] <- ci
  }

  # 5. Histograms for the beta estimates

  hist <- mapply(mat1, 0:(ncol(mat1)-1),
                 FUN = function(vector, index){
                   hist(vector, main = paste("Histogram of Beta", index),
                        xlab = paste("Beta", index, "Values from Bootstrap"),
                        col = rainbow(iter))}, USE.NAMES = TRUE)
  # just to be safe in the number for rainbow, I'm using a ridiculously large number

  dalist <- list("Summary of Betas" = mat.sum, "Confidence Intervals" = conf.int,
                 "Occurences of Singularity" = num.err, "Histograms" = hist)
  return(dalist) # Works up to here!!
}
