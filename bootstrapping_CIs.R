#bootstrapping confidence intervals with R
#WITH REPLACEMENT

#can use sample() or boot()
library(boot)
# function to obtain R-Squared from the data
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

#original data
mtcars

# Bootstrap 95% CI for R-Squared with 1000 replications
results <- boot(data=mtcars, statistic=rsq,
                R=1000, formula=mpg~wt+disp)
plot(results)
boot.ci(results, type="bca")
boot.ci(results, type="norm")
