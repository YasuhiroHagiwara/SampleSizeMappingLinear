
# Calculating sample sizes for Criteria I, II and III
library(pmsampsize)
pmsampsize(type = "c", rsquared = 0.5, parameters = 15, shrinkage = 0.90, intercept = 0.7, sd = 0.3, mmoe = 1.1)


# Function of exact calculation for criterion IV
calsize4ex <-function(rsquared, sd, p, tau) {
  n <- p + 2
  amoe <- qt(df=n-p-1, 0.975) * (sd^2 * (1 - rsquared) / n)^0.5
  if (amoe <= tau) {
    n <- n
  } else {
    while (amoe > tau) {
      n <- n + 1
      amoe <- qt(df=n-p-1, 0.975) * (sd^2 * (1 - rsquared) / n)^0.5
      if (amoe <= tau) {
        n <- n
      }
    }
  }
  return(n)
}


# Function of approximation calculation for criterion IV
calsize4app <- function(rsquared, sd, tau) {
  n <- ceiling(2.00^2 * sd^2 * (1 - rsquared) / tau^2)
  return(n)
}


# Calculating sample size for Criterion IV
calsize4ex(rsquared = 0.5, sd = 0.3, p = 15, tau = 0.025)
calsize4app(rsquared = 0.5, sd = 0.3, tau = 0.025)