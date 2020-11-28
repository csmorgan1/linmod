linear_model <- function(Y,X){
  X <- cbind(1, X)
  n <- dim(X)[[1]]
  q <- dim(X)[[2]]
  if(n != length(Y)){stop(print("Dimensions do not match"))}
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  I <- diag(1, n, n)
  oneMatrix <- matrix(1, n, n)
  Betas <- solve(t(X) %*% X) %*% t(X) %*% Y
  SSE <- as.numeric(t(Y) %*% Y - 2 * t(Betas) %*% t(X) %*% Y + 
                      t(Betas) %*% t(X) %*% X %*% Betas)
  sigma2 <- as.numeric(SSE / (n - q))
  SSY <- as.numeric(t(Y) %*% (I - oneMatrix / n) %*% Y)
  SSR <- as.numeric(t(Y) %*% (H - oneMatrix / n) %*% Y)
  VB <- sigma2 * solve(t(X) %*% X)
  SE <- as.matrix(sqrt(diag(VB)))
  t <- Betas / SE
  x1 <- 2 * pt(abs(t), n - q - 1, lower.tail = F)
  R2 <- SSR / SSY
  a <- SSE / (n - q)
  b <- SSY / (n - 1)
  R2A <- 1 - a / b
  d <- SSR / (q-1)
  e <- SSE / (n-q)
  fStat <- d/e
  fPvalue <- pf(fStat,q-1,n-q,lower.tail = F)
  valuesList <- list(Betas,SE,t,x1,R2,R2A,fStat,fPvalue)
  names(valuesList) <- c("Estimates","STD.ERR","T-Stat","P(>|t|)","R-Squared","Adjusted R_Sqaured",
                         "F-statistic","F p-value")
  return(valuesList)
}
