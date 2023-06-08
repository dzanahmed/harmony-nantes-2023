# Session 2

twoXtwo <- matrix(c(48, 12, 4, 36), ncol=2, nrow=2)
twoXtwo
##      [,1] [,2]
## [1,]   48    4
## [2,]   12   36



library('runjags')

Tally <- as.numeric(twoXtwo)
N <- sum(Tally)

prev <- list(chain1=0.05, chain2=0.95)
se <- list(chain1=c(0.01,0.99), chain2=c(0.99,0.01))
sp <- list(chain1=c(0.01,0.99), chain2=c(0.99,0.01))

results <- run.jags('Day1_Wednesday_7_June/multinom_model.txt', n.chains=2)

results

plot(results)


prev <- list(chain1=0.05, chain2=0.95)
se <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))
sp <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))

results_tr_1000 <- run.jags('Day1_Wednesday_7_June/multinom_model.txt', n.chains=2, sample=100000)

results_tr_1000

plot(results)


# Exercise 2 --------------------------------------------------------------

library(PriorGen)

# Finding sensitivity

findbeta(themean=0.9, percentile.value = 0.85)

TeachingDemos::hpd(qbeta, shape1=101.43, shape2=11.27)
# 0. 8439 - 0.951 

curve(dbeta(x, 101.43, 11.27))

# Finding specificity

findbeta(themean=0.95, percentile.value = 0.92)

TeachingDemos::hpd(qbeta, shape1=162.533, shape2=8.55)
# 0. 8439 - 0.951 

curve(dbeta(x, 162.533, 8.55))

results_si_1000 <- run.jags('Day1_Wednesday_7_June/hw_stronginf.txt', n.chains=2)

# Note: this is only commented out to save space in the exercise file!
# plot(results_si_1000)
# check convergence and effective sample size, and then interpret results:
results_si_1000

plot(results_si_1000)
