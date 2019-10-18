#a permutation test to compare two group means
set.seed(1)
n <- 100

#Original Data
tr <- rbinom(n, 1, 0.5) #assigns treatment. assignment ~ bernouilli p
#data created with treatment assignment, random noise
y <- 1 + tr + rnorm(n, 0, 3) #data created with treatment assignment, random noise
#Take means of two groups for original data
by(y, tr, mean)
#Find difference in means
diff(by(y, tr, mean))

#Take a single permutation (NO REPLACEMENT)
s <- sample(tr, length(tr), FALSE) #sample from treatment group
diff(by(y, s, mean))

#use replicate to do this many times
numreps = 2000
dist <- replicate(numreps, diff(by(y, sample(tr, length(tr), FALSE), mean)))
hist(dist)

hist(dist, xlim = c(-3, 3), col = "black", breaks = 100)
#add line for original difference
abline(v = diff(by(y, tr, mean)), col = "blue", lwd = 2)
# one-tailed test
sum(dist > diff(by(y, tr, mean)))/numreps  
# two-tailed test (using absolute value of the difference)
sum(abs(dist) > abs(diff(by(y, tr, mean))))/2000
