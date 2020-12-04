#' Linear Model
#'
#' Calculates coefficient estimates, standard errors, t-statistics, and p-values,
#' as well as R squared, adjusted R squared, F-Statistic and p-value for model.
#'
#' @param Y input outcome
#' @param X input predictor(s)
#'
#' @return estimated effects of X on Y through OLS
#'
#' @examples linear_model(Y=mtcars$mpg,X=mtcars$hp)
#'
#' @importFrom stats pf pt
#'
#' @export
#'
linear_model <- function(Y,X){
  #add intercept to design matrix
  X <- cbind(1, X)
  X <- as.matrix(X)
  #number of rows
  n <- dim(X)[[1]]
  #number of columns (observations)
  q <- dim(X)[[2]]
  #check dimensions for matrix multiplication
  if(n != length(Y)){stop(print("Dimensions do not match"))}
  #create hat matrix
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  #create identity matrix
  I <- diag(1, n, n)
  #create one matrix
  oneMatrix <- matrix(1, n, n)
  #get beta estimates with OLS
  Betas <- solve(t(X) %*% X) %*% t(X) %*% Y
  SSE <- as.numeric(t(Y) %*% Y - 2 * t(Betas) %*% t(X) %*% Y +
                      t(Betas) %*% t(X) %*% X %*% Betas)
  #mean square error
  sigma2 <- as.numeric(SSE / (n - q))
  SSY <- as.numeric(t(Y) %*% (I - oneMatrix / n) %*% Y)
  SSR <- as.numeric(t(Y) %*% (H - oneMatrix / n) %*% Y)
  #variance covariance matrix
  VB <- sigma2 * solve(t(X) %*% X)
  #extract beta estimate variance
  SE <- as.matrix(sqrt(diag(VB)))
  #calculate t-statistics for beta estimates
  t <- Betas / SE
  #calculate p-values for beta estimates
  x1 <- 2 * pt(abs(t), n - q - 1, lower.tail = F)
  #calculate r squared for model
  R2 <- SSR / SSY
  #calculate adjusted r squared for model
  a <- SSE / (n - q)
  b <- SSY / (n - 1)
  R2A <- 1 - a / b
  #calculate f stat to test if full model is better than intercept only model
  d <- SSR / (q-1)
  e <- SSE / (n-q)
  fStat <- d/e
  #calculate p-value for f statistic
  fPvalue <- pf(fStat,q-1,n-q,lower.tail = F)
  #create list containing all relevant calculations
  valuesList1 <- as.data.frame(cbind(Betas,SE,t,x1))
  valuesList2 <- as.data.frame(cbind(R2,R2A,fStat,fPvalue))
  colnames(valuesList1) <- c("Estimates","STD.ERR","T-Stat","P(>|t|)")
  colnames(valuesList2) <- c("R-Squared","Adjusted R_Sqaured","F-statistic","F p-value")
  valuesList <- list(valuesList1,valuesList2)
  return(valuesList)
}
