test_that("Simple Linear Regression Works", {
  data("mtcars")
  Y <- mtcars$mpg
  X <- as.matrix(mtcars[, 2])
  X <- cbind(1, X)
  Betas <- solve(t(X) %*% X) %*% t(X) %*% Y
  expect_equal(round(Betas[1,],5), 37.88458)
  expect_equal(round(Betas[2,],5), -2.87579)
})

test_that("Multiple Linear Regression Works", {
  data("mtcars")
  Y <- mtcars$mpg
  Z <- as.matrix(mtcars[, 2:11])
  Z <- cbind(1, Z)
  Betas2 <- solve(t(Z) %*% Z) %*% t(Z) %*% Y
  expect_equal(round(as.numeric(Betas2[1,]),5), 12.30337)
  expect_equal(round(as.numeric(Betas2[2,]),5), -0.11144)
  expect_equal(round(as.numeric(Betas2[3,]),5), 0.01334)
  expect_equal(round(as.numeric(Betas2[4,]),5), -0.02148)
  expect_equal(round(as.numeric(Betas2[5,]),5), 0.78711)
  expect_equal(round(as.numeric(Betas2[6,]),5), -3.71530)
  expect_equal(round(as.numeric(Betas2[7,]),5), 0.82104)
  expect_equal(round(as.numeric(Betas2[8,]),5), 0.31776)
  expect_equal(round(as.numeric(Betas2[9,]),5), 2.52023)
  expect_equal(round(as.numeric(Betas2[10,]),5), 0.65541)
  expect_equal(round(as.numeric(Betas2[11,]),5), -0.19942)
})
