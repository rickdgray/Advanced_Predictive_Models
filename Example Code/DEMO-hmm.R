###################################################
## COMPUTING DEMO:  Hidden Markov Models
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
## Last updated: September 1, 2021
###################################################

# load libraries
library(tidyverse)
library(gridExtra)
library(xts)
library(depmixS4)

###################################################
# Fit a 2-state Poisson HMM to the earthquake data from
# "Hidden Markov Models for Time Series" by 
# Zucchini, MacDonald, and Langrock (2016)

# get the data
quakes <- read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")$V2 

year <- 1900:2006

eq_dat <- data.frame("count" = quakes,
                     "year" = year)

# create the HMM (non-stationary)
eq_mod <- depmix(count ~ 1,
                  data = eq_dat,
                  family = poisson(),
                  nstates = 2,
                  ntimes = length(year))

# fit the model using an EM algorithm
eq_fit <- fit(eq_mod)

summary(eq_fit)

# get estimate of lambda
getpars(eq_fit)

exp(getpars(eq_fit)[7:8])

# get the posterior distribution of the states
eq_dat$states <- posterior(eq_fit, type = "viterbi")$state 
eq_dat$states[eq_dat$states == 1] <- 5
eq_dat$states[eq_dat$states == 2] <- 40

# plot the earthquake data with the most likely state

ggplot(eq_dat,
       aes(x = year, y = count)) +
  geom_point(size = .7, color = "darkblue") +
  geom_line(color = "darkblue") +
  geom_point(data = eq_dat, 
            aes(x = year, y = states),
            shape = 15,
            color = "darkred") +
  ggtitle("Yearly Number of Earthquakes") 

###################################################

###################################################
# Illustration of extensions of the HMM
# using the speed test data in the 
# depmixS4 package

data(speed)

speed$time <- 1:dim(speed)[1]
speed$corr_num <- as.numeric(speed$corr) 

speed_long <- pivot_longer(speed[1:200, c("rt", "corr_num", "Pacc", "time")], 
                           cols = c("rt", "corr_num", "Pacc"),
                           names_to = "var_name",
                           values_to = "values")


speed_long$var_name[speed_long$var_name == "corr_num"] <- "corr"
speed_long$var_name <- factor(speed_long$var_name,
                              c("rt", "corr", "Pacc"))

p <- ggplot(speed_long,
            aes(y = values, x = time)) +
  geom_line() +
  ylab("") +
  ggtitle("Speed-Accuracy Tradeoff")

p + facet_grid(var_name ~ .,
               scales = "free_y")

########
# Fit and compare HMMs

# 2-state HMM for a univariate response (response time)
mod1 <- depmix(response = rt ~ 1, 
              data = speed, 
              family = gaussian(),
              nstates = 2)

fit1 <- fit(mod1)
summary(fit1)

# parameterized transition matrix and baseline prob. (delta)
mod2 <- depmix(response = rt ~ 1, 
               data = speed, 
               family = gaussian(),
               transition = ~ scale(Pacc),
               nstates = 2)

fit2 <- fit(mod2)

summary(fit2)

# compare models 1 and 2
fit1
fit2

# multivariate response extension
mod3 <- depmix(response = list(rt ~ 1, corr ~ 1), 
               data = speed, 
               family = list(gaussian(), multinomial("identity")),
               transition = ~ scale(Pacc),
               nstates = 2)

fit3 <- fit(mod3) # note that logLik is not comparable 

summary(fit3)

