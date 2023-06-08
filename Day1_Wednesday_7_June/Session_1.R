# Session 1 - 07 / 06 / 23

# Diagnostic test evaluation with GS

library("tidyverse")
se <- c(1, 0.6)
sp <- c(1, 0.9)
N <- 1000
prevalence <- 0.25

data <- tibble(Status = rbinom(N, 1, prevalence)) %>%
    mutate(Test1 = rbinom(N, 1, se[1]*Status + (1-sp[1])*(1-Status))) %>%
    # Which is the same as:
    mutate(Test1 = Status) %>%
    mutate(Test2 = rbinom(N, 1, se[2]*Status + (1-sp[2])*(1-Status)))

(twoXtwo <- with(data, table(Status, Test2)))
##       Test2
## Status   0   1
##      0 656  68
##      1 127 149
(sensitivity <- twoXtwo[2,2] / sum(twoXtwo[2,1:2]))
## [1] 0.5398551
(specificity <- twoXtwo[1,1] / sum(twoXtwo[1,1:2]))
## [1] 0.9060773

# Diagnostic test evaluation without GS

se <- c(0.9, 0.6)
sp <- c(0.95, 0.9)
N <- 1000
prevalence <- 0.25

data <- tibble(Status = rbinom(N, 1, prevalence)) %>%
    mutate(Test1 = rbinom(N, 1, se[1]*Status + (1-sp[1])*(1-Status))) %>%
    mutate(Test2 = rbinom(N, 1, se[2]*Status + (1-sp[2])*(1-Status)))

with(data, table(Status, Test1))
##       Test1
## Status   0   1
##      0 691  35
##      1  32 242
with(data, table(Status, Test2))
##       Test2
## Status   0   1
##      0 667  59
##      1 104 170


(twoXtwo <- with(data, table(Test1, Test2)))
##      Test2
## Test1   0   1
##     0 646  77
##     1 125 152
(sensitivity_1 <- twoXtwo[2,2] / sum(twoXtwo[1:2,2]))
## [1] 0.6637555
(sensitivity_2 <- twoXtwo[2,2] / sum(twoXtwo[2,1:2]))
## [1] 0.5487365
(specificity_1 <- twoXtwo[1,1] / sum(twoXtwo[1:2,1]))
## [1] 0.8378729
(specificity_2 <- twoXtwo[1,1] / sum(twoXtwo[1,1:2]))
## [1] 0.8934993

# Simple JAGS model

library(runjags)


# Exercise 1 --------------------------------------------------------------

# data to be retrieved by runjags
Positives <- 70
N <- 100

# initial values to be retrieved by runjags:
prevalence <- list(chain1=0.05, chain2=0.95)
prevalence <- list(chain1=0.5, chain2=0.5)

results <-
    run.jags(
        'Day1_Wednesday_7_June/basicjags.txt',
        n.chains = 2,
        burnin = 5000,
        sample = 10000
    )

plot(results)

results


# Exercise 2 --------------------------------------------------------------

Positives <- 70
N <- 100

system.time({
    freq_model <- glm(cbind(Positives, N-Positives)~1, family=binomial)
})

plogis(coef(freq_model))
plogis(confint(freq_model))

# The results are more or less the same - the sample size is large.

# When we cahgne data do 7/10 positives, we get the following:

Positives <- 7
N <- 10

system.time({
    freq_model <- glm(cbind(Positives, N-Positives)~1, family=binomial)
})

plogis(coef(freq_model))
plogis(confint(freq_model))

# Frequentist 
0.7 (0.393-0.915)

# Bayesian 
0.677 (0.407-0.933)


# Exercise 3 --------------------------------------------------------------

Positives <- 70
N <- 100

prevalence <- list(chain1=0.05, chain2=0.95)
sensitivity <- list(chain1=0.95, chain2=0.05)
specificity <- list(chain1=0.05, chain2=0.95)

results <- run.jags('Day1_Wednesday_7_June/obsprevmodel.txt',
                    n.chains = 2,
                    burnin=5000,
                    sample=10000)

results

plot(results)


# Exercise 3 - fixing Se and Sp -------------------------------------------

Positives <- 70
N <- 100
sensitivity <- 0.8
specificity <- 0.99


results <- run.jags('Day1_Wednesday_7_June/obsprevmodel_fixsesp.txt',
                    n.chains = 2,
                    burnin=5000,
                    sample=10000)

results

plot(results)