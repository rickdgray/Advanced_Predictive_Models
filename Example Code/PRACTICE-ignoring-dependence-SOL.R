###################################################
## PRACTICE:  Consequences of Ignoring Dependence
#             comparing operating characteristics
##            of statistical procedures when assumptions of 
##            independence are violated
## SOLUTION CODE
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
## Last updated: July 1, 2021
###################################################

# load library
library(mvtnorm)
library(ggplot2)
library(gridExtra)
library(tidyverse)

###################################################
# Functions for generating independent
# and dependent samples

# generate n_samples of a time series of
# length n with each x[t] ~ iid N(mu, sigma2)
sim_indep <- function(n_samples, n, mu, sigma2) 
                { 
                  return(sapply(1:n_samples, 
                                function(x) rnorm(n, mu, sqrt(sigma2))))
                }

# test the function
sim_indep(3, 30, 0, 1)

# generate n_samples of a time series of
# length n with each x[1:n] ~ MVN(mu, Sigma)
# where Sigma_ij = sigma2 * rho^|i-j|
sim_dep <- function(n_samples, n, mu, sigma2, rho)
            {
              cov_mat <- sigma2 * rho^as.matrix(dist(1:n))
              return(sapply(1:n_samples,
                            function(x) rmvnorm(1, rep(mu, n), cov_mat)))
}

# test the function
sim_dep(3, 30, 0, 1, .8)

###################################################

###################################################
## QUESTION 1

# set values for the parameters
mu_tr <- 0
sigma2 <- 1
rho <- .7

# set length of the time series
n <- 30

# set number of simulated time series
num_sim <- 1

# simulate the independent time series
sim_data_indep <- data.frame("Time" = 1:n,
                           "x" = sim_indep(num_sim, 
                                           n, 
                                           mu_tr, 
                                           sigma2))

# construct a plot of the independent time series
gg_indep <- ggplot(sim_data_indep,
                    aes(x = Time, y = x)) + 
                geom_line(size = 1, color = "blue") +
                geom_point(shape = 19, size = 2, fill = "white", color = "blue") +
                geom_hline(yintercept = mu_tr,
                           color = "darkgrey") +
                geom_hline(yintercept = mean(sim_data_indep$x),
                           linetype="dashed", 
                           color = "blue") +
                coord_cartesian(ylim = c(mu_tr - 3 * sqrt(sigma2), 
                                         mu_tr + 3 * sqrt(sigma2))) +
                ggtitle("Independent Time Series")

# simulate the dependent time series
sim_data_dep <- data.frame("Time" = 1:n,
                             "x" = sim_dep(num_sim, 
                                           n, 
                                           mu_tr, 
                                           sigma2, 
                                           rho))

# construct a plot of the independent time series
gg_dep <- ggplot(sim_data_dep,
                 aes(x = Time, y = x)) + 
                geom_line(size = 1, color = "blue") +
                geom_point(shape = 19, size = 2, fill = "white", color = "blue") +
                geom_hline(yintercept = mu_tr,
                            color = "darkgrey") +
                geom_hline(yintercept = mean(sim_data_dep$x),
                          linetype="dashed", 
                          color = "blue") +
                coord_cartesian(ylim = c(mu_tr - 3 * sqrt(sigma2), 
                                mu_tr + 3 * sqrt(sigma2))) +
                ggtitle("Dependent Time Series")

# display the plots
grid.arrange(gg_indep, gg_dep)

###################################################
## QUESTION 2

# set values for the parameters
mu_tr <- 0
sigma2 <- 1
rho <- .7

# set length of the time series
n <- 30

# set number of simulated time series
num_sim <- 50

# simulate num_sim time series
sim_data_indep_big <- data.frame("Time" = 1:n,
                          sim_indep(num_sim, 
                                    n, 
                                    mu_tr, 
                                    sigma2))

# convert to long format
sim_data_indep_long <- sim_data_indep_big %>%
                          pivot_longer(cols = -Time,
                                        names_to = "rep",
                                        values_to = "x")

# construct boxplots of the simulated 
# time series collapsed over time
gg_indep_boxplots <- ggplot(sim_data_indep_long,
                            aes(x = as.factor(rep), 
                                y = x)) + 
                        geom_boxplot(color = "blue") +
                        geom_hline(yintercept = mu_tr,
                          color = "darkgrey") +
                      coord_cartesian(ylim = c(mu_tr - 3 * sqrt(sigma2), 
                           mu_tr + 3 * sqrt(sigma2))) +
                      ggtitle("Independent Time Series") + 
                      xlab("rep")

# simulate num_sim time series
sim_data_dep_big <- data.frame("Time" = 1:n,
                                 sim_dep(num_sim, 
                                         n, 
                                         mu_tr, 
                                         sigma2, 
                                         rho))

# convert to long format
sim_data_dep_long <- sim_data_dep_big %>%
                        pivot_longer(cols = -Time,
                                      names_to = "rep",
                                      values_to = "x")

# construct boxplots of the simulated 
# time series collapsed over time
gg_dep_boxplots <- ggplot(sim_data_dep_long,
                            aes(x = as.factor(rep), 
                                y = x)) + 
                      geom_boxplot(color = "blue") +
                      geom_hline(yintercept = mu_tr,
                                  color = "darkgrey") +
                      coord_cartesian(ylim = c(mu_tr - 3 * sqrt(sigma2), 
                                      mu_tr + 3 * sqrt(sigma2))) +
                      ggtitle("Dependent Time Series") + 
                      xlab("rep")

# display the plots
grid.arrange(gg_indep_boxplots, gg_dep_boxplots)

########################
## QUESTION 3

# set values for the parameters
mu_tr <- 0
sigma2 <- 1
rho <- .7

# set length of the time series
n <- 30

# set number of simulated time series
num_sim <- 5000

# set signifigance level
alpha <- 0.05

# simulate num_sim independent time series
sim_data_indep_big <- data.frame("Time" = 1:n,
                                 sim_indep(num_sim, 
                                           n, 
                                           mu_tr, 
                                           sigma2))

# simulate num_sim dependent time series
sim_data_dep_big <- data.frame("Time" = 1:n,
                               sim_dep(num_sim, 
                                       n, 
                                       mu_tr, 
                                       sigma2, 
                                       rho))

# coverage function - checks whether mu_tr
# is in the (1-alpha)*100% confidence interval
# constructed assuming independence

cover <- function(x, mu_tr, alpha)
            {
              x_bar <- mean(x)
              se <- sqrt(var(x)/length(x))
              return(mu_tr > x_bar - se * qt(1-alpha/2, length(x)-1) &
                       mu_tr < x_bar + se * qt(1-alpha/2, df=length(x)-1))
              }

# calculate the empirical coverage rate for the 
# independent time series
mean(apply(sim_data_indep_big[2:num_sim], 
           2, 
           function(x) cover(x, mu_tr, alpha)))

# calculate the empirical coverage rate for the 
# dependent time series
mean(apply(sim_data_dep_big[2:num_sim], 
           2, 
           function(x) cover(x, mu_tr, alpha)))
