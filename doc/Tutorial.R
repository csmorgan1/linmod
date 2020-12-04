## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(linmod)
library(bench)
library(data.table)

## -----------------------------------------------------------------------------
linear_model(Y = mtcars[ , "mpg"], X = mtcars[ , "hp"])

## -----------------------------------------------------------------------------
all.equal(as.numeric(unlist(lm(mtcars[, "mpg"] ~ mtcars[, "hp"])[["coefficients"]])),
          linear_model(Y = mtcars[, "mpg"], X = mtcars[, "hp"])[[1]][["Estimates"]])

## -----------------------------------------------------------------------------
result <- as.data.table(bench::mark(as.numeric(unlist(lm(mtcars[, "mpg"] ~ mtcars[,"hp"])[["coefficients"]])),
                      as.numeric(linear_model(Y = mtcars[, "mpg"], mtcars[, "hp"])[[1]][["Estimates"]])))

## -----------------------------------------------------------------------------
print(result[,1:10])

## -----------------------------------------------------------------------------
summary(lm(mtcars[,"mpg"]~.,data=mtcars[,2:11]))

## -----------------------------------------------------------------------------
linear_model(Y = mtcars[ , "mpg"], X=mtcars[,2:11])

