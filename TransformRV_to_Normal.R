## how to transform a non-normal RV to Normal

## Method 1: use inverse cdf
library(ggplot2)
rv <- sort(rexp(1000))
# rv <- sort(rbeta(1000, shape1 = 1, shape2 = 7)) ### another example
qplot(rv)
cdfy <- pexp(rv)
# cdfy <- pbeta(rv, shape1 = 1, shape2 = 7)
cdfy
invcdf <- qnorm(cdfy)
invcdf
qplot(invcdf)

shapiro.test(invcdf)

## how to transform it back to the original distribution
cdfy2 <- pnorm(invcdf)
rv2 <- qexp(cdfy2)
# rv2 <- qbeta(cdfy2, shape1 = 1, shape2 = 7)
qplot(rv2)

## check rv and rv 2 are the same
head(rv - rv2)

#######################################

## Method 2: use box-cox transformation
library(caret)
rv <- rexp(1000, rate = 1)
# rv <- rbeta(1000, shape1 = 1, shape2 = 7) ### another example
qplot(rv)
rv.bc <- BoxCoxTrans(rv)
rv.bc
bc.lambda <- rv.bc$lambda
## rv.transformed <- log(rv) ## if lambda = 0, do a log transform
rv.transformed <- (rv^bc.lambda - 1) / bc.lambda
qplot(rv.transformed)

shapiro.test(rv.transformed)


BoxCoxTransformation = function(original_data) {
  lambda = BoxCoxTrans(original_data)$lambda
  if (lambda == 0) {
    return(log(original_data))
  } else {
    return((original_data ** lambda - 1)/ lambda)
  }
}
