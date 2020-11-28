test_that("Simple Linear Regression Works", {
  data("mtcars")
  Y <- mtcars$mpg
  X <- as.matrix(mtcars[, 2])
  X <- cbind(1, X)
  Betas <- solve(t(X) %*% X) %*% t(X) %*% Y
  expect_equal(round(Betas[1,],5), 37.88458)
  expect_equal(round(Betas[2,],5),-2.87579)
})
