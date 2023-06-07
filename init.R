# THIS IS THE INITIALIZATION STAGE
# 
# packages <- c(
#     "tidyverse",
#     "PriorGen",
#     "rjags",
#     "runjags",
#     "coda",
#     "TeachingDemos",
#     "knitr",
#     "ggdag"
# )
# 
# if (length(find.package(packages)) != length(packages)) {
#     install.packages(packages)
# }
# 
# 
# 
# install.packages(
#     "runjags",
#     repos = c(CRAN = "https://cran.rstudio.com/",
#               `ku-awdc` = "https://ku-awdc.github.io/drat/")
# )
# 
# install.packages("rjags")
# 
# library(rjags)
# 
# # Check if everything installed correctly
# 
# stopifnot(getRversion() >= "4.1.0")
# stopifnot(require('tidyverse'))
# stopifnot(require('PriorGen'))
# stopifnot(require('rjags'))
# stopifnot(require('runjags'))
# stopifnot(require('coda'))
# stopifnot(require('TeachingDemos'))
# stopifnot(packageVersion("runjags") >= numeric_version("2.2.1-7"))
# stopifnot(testjags()$JAGS.available)
# stopifnot(numeric_version(testjags()$JAGS.version) >= "4.3.0")
# stopifnot(testjags()$rjags.found)
# stopifnot(numeric_version(testjags()$rjags.version) >= "4-13")
# 

library(tidyverse)
library(rjags)

# This is first exercise

tosses <- 10
probability <- 0.5
heads <- 5 
likelihood_1 <- choose(tosses, heads) * probability^heads * (1-probability)^(tosses-heads)

# other approach
likelihood_2 <- dbinom(heads, tosses, probability)


# Graphing the binomial dist with ggplot ----------------------------------------------

# 
# 
# xProb<-dbinom(0:8,8,0.5)
# xRolls <- 0:8
# 
# df <- data.frame(xRolls, xProb)
# 
# df |> ggplot() + 
#     geom_col(aes(xRolls, xProb))+
#     theme_bw()

likelihood_fun <- function(prevalence) dbinom(7,10,prevalence)

likelihood_fun(0.8)
# 0.201
# The likelihood of observing 7 out of 10, when true prevalence is 80%, is 0.201

likelihood_fun(0.5)
# 0.117
# The likelihood of observing 7 out of 10, when true prevalence is 50% is 0.117

optimise(likelihood_fun, interval=c(0,1), maximum = T)
# maximum is 0.6999843
# objective is 0.2668279

model <- glm(cbind(7,10-7) ~ 1, family = binomial)
plogis(coef(model))

# Profiling a likelihood

parameters <- seq(0,1,0.01)

likelihoods <- numeric(length(parameters))

for (i in 1:length(parameters)){
    likelihoods[i] <- likelihood_fun(parameters[i])
}

plot(parameters, likelihoods, type='l')
abline(h=0.267, lty="dashed", col="red")
abline(v=0.7, lty="dashed", col="red")


# Lets plot it in ggplot 
ggplot()+
    geom_col(aes(parameters, likelihoods))+
    geom_hline(yintercept = max(likelihoods), color="red")+
    geom_vline(xintercept = (which.max(likelihoods)-1)/100, color="red")



# Lets build a prior

prior_fun <- function(prevalence) dbeta(prevalence, 2, 2)

results <- data.frame(parameter=parameters, likelihoods=NA, prior=NA, posterior=NA)
view(results)


for (i in 1:nrow(results)) {
    results$likelihoods[i] <- likelihood_fun(results$parameter[i])
     results$prior[i] <- prior_fun(results$parameter[i])
     results$posterior[i] <- results$likelihoods[i] * results$prior[i]
}

view(results)

# Profiling the posterior 

par(mfrow=c(3,1))
with(results, plot(parameter, likelihoods, type='l'))
with(results, plot(parameter, prior, type='l'))
with(results, plot(parameter, posterior, type='l'))

# Summarizing the posterior 


# MC MC 
# The coda package has many utilities to work with MCMC objects:
library('coda')

metropolis <- function(burnin = 0, sample = 10000, sigma = 0.05, 
                       initial_value = 0.05, plot=TRUE){
    stopifnot(initial_value > 0, initial_value < 1)
    stopifnot(sigma > 0)
    burnin <- as.integer(burnin)
    sample <- as.integer(sample)
    stopifnot(burnin >= 0)
    stopifnot(sample > 0)
    
    # Redefine these to work on the log scale:
    llikelihood_fun <- function(prevalence)
        dbinom(7, 10, prevalence, log=TRUE)
    lprior_fun <- function(prevalence) 
        dbeta(prevalence, 2, 2, log=TRUE)
    
    parameters <- numeric(burnin+sample)
    parameters[1] <- initial_value
    current <- initial_value
    post <- llikelihood_fun(current) + lprior_fun(current)
    for(i in 2:(burnin+sample)){
        proposal <- rnorm(1, current, sigma)
        if(proposal > 0 && proposal < 1){
            newpost <- llikelihood_fun(proposal) + lprior_fun(proposal)
            accept <- newpost > post || rbinom(1, 1, exp(newpost-post))
            if(accept){
                current <- proposal
                post <- newpost
            }
        }
        parameters[i] <- current
    }
    
    if(plot && burnin > 0){
        plot(1:burnin, parameters[1:burnin], type='l', col='red', 
             xlim=c(1,burnin+sample), ylim=c(0,1), 
             main='Parameter values (red:burnin, blue:sample)', 
             ylab='prevalence', xlab='Iteration')
        lines((burnin+1):(burnin+sample), parameters[-(1:burnin)], col='blue')
    }else if(plot){
        plot(1:sample, parameters, type='l', col='blue', 
             xlim=c(1,burnin+sample), ylim=c(0,1), 
             main='Parameter values (red:burnin, blue:sample)', 
             ylab='prevalence', xlab='Iteration')
    }
    
    parameters <- window(coda::as.mcmc(parameters), start=burnin+1)
    varnames(parameters) <- 'prevalence'
    
    return(parameters)
}

samples <- metropolis(burnin = 0, sample=1000, initial_value = 0.05)

par(mfrow=c(2,1))

with(results, plot(parameter, posterior, type='l', 
                   xlim=c(0,1), main='True posterior', ylab=NA, xlab=NA))

plot(density(samples), xlim=c(0,1), main='Sampled posterior', 
     ylab=NA, xlab=NA)

mean(samples)

median(samples)

HPDinterval(samples)

# Burn in

samples <- metropolis(burnin = 100, sample = 1000, initial_value=0.05)

# Effective sample size:
effectiveSize(samples)

samples <- metropolis(burnin = 1000, sample = 20000, initial_value=0.05)

effectiveSize(samples)

# Set seed 

set.seed(2023-05-18)
samples <- metropolis(burnin = 1000, sample = 20000,
                      initial_value=0.99, plot=FALSE)
# Mean of samples:
mean(samples)


# Exercise 1  -------------------------------------------------------------

samples <- metropolis(burnin = 1000, sample = 10000)
# Effective sample size:
effectiveSize(samples)
# Mean of samples:
mean(samples)
# Median of samples:
median(samples)
# 95% CI of samples:
HPDinterval(samples)


# Exercise 2 --------------------------------------------------------------

# Large sigma
samples <- metropolis(sigma=10)

# Autocorrelation
autocorr(samples, lags=1)

# Effective samople size 
es_s_10 <- effectiveSize(samples)


# __---

# Moderately large sigma
samples <- metropolis(sigma=1)

#Autocorrelation
autocorr(samples, lags = 1)

#Effective samople size
es_s_1 <- effectiveSize(samples)

# Sigma inversely proportional to the 'prevalence'

# ___---

# Moderately small sigma
samples <- metropolis(sigma = 0.1)

#Autocorrelation
autocorr(samples, lags=1)

#effective sample size:
es_s_01 <- effectiveSize(samples)

# ___---

#Small sigma
samples <- metropolis(sigma=0.01)

#Autocorrelation
autocorr(samples, lags=1)

#Effective samople size
es_s_001 <- effectiveSize(samples)



set.seed(1)

# sigma for sample size > 2000
samples <- metropolis(sigma=0.451)

#Autocorrelation
autocorr(samples, lags=1)

#Effective samople size
es_sigma <- effectiveSize(samples)

es_sigma

