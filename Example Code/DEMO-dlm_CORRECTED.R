###################################################
## COMPUTING DEMO:  Dynamic Linear Models (CORRECTED)
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
## Last updated: December 16, 2021 
###################################################

# load libraries
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(xts)
library(dlm)

###################################################
# SIMULATE DATA: univariate DLM w/ F_t = 1
# and G_t = 1 for t = 1, 2, ...

# set true values of parameters
sigma2v_tr <- .3
sigma2w_tr <- .1
m0_tr <- 0
C0_tr <- 1

# generate y = y_1,...y_n
n <- 500
theta <- rep(NA, n)
y <- rep(NA, n)

theta0 <- rnorm(1, m0_tr, sqrt(C0_tr))
theta[1] <- theta0 + rnorm(1, 0, sqrt(sigma2w_tr))
for(i in 2:n) theta[i] <- theta[i-1] + rnorm(1, 0, sqrt(sigma2w_tr))
for(j in 1:n) y[j] <- theta[j] + rnorm(1, 0, sqrt(sigma2v_tr))

# plot the simulated data
sim_data <- data.frame(y = y,
                       time = 1:n)

gg_sim <- ggplot(sim_data,
                 aes(y = y,
                     x = time)) +
  geom_line(linetype = "dashed",
            color = "black") 
gg_sim

###################################################
# ANALYZE SIMULATED DATA USING A DLM

# construct a dlm model object 
#with parameters fixed at their true values
dlm_mod <- dlm(FF = 1,
               GG = 1,
               V = sigma2v_tr,
               W = sigma2w_tr,
               m0 = m0_tr,
               C0 = C0_tr)   

# filter the simulated data y using the dlm
sim_data_filtered <- dlmFilter(y = sim_data$y,
                               mod = dlm_mod)

# store and plot the one-step-ahead predictions 
# of theta and standard errors
sim_data$pred <- sim_data_filtered$a
sim_data$pSE <- sqrt(unlist(
  dlmSvd2var(sim_data_filtered$U.R, 
             sim_data_filtered$D.R)))

gg_sim +
  geom_line(data = sim_data, 
            aes(y = pred,
                x = time),
            color = "red",
            size = 1.2) +
  geom_ribbon(data = sim_data,
              aes(x = time, 
                  ymin = pred - 2 * pSE,
                  ymax = pred + 2 * pSE),
                  fill = "red",
              alpha = 0.2) +
  labs(title = expression(
    paste("One-Step-Ahead Predictions of ",
          theta[t],
          " w/ Standard Errors"))) +
  ylab("")
         
# CORRECTED:  store and plot the filtering distribution  
# of theta
sim_data$filtered <- dropFirst(sim_data_filtered$m)
sim_data$fSE <- dropFirst(sqrt(unlist(
  dlmSvd2var(sim_data_filtered$U.C, 
             sim_data_filtered$D.C))))
gg_sim +
  geom_line(data = sim_data, 
            aes(y = pred,
                x = time),
            color = "red",
            size = .5) +
  geom_line(data = sim_data, 
            aes(y = filtered,
                x = time),
            color = "blue",
            size = 1.2) +
  geom_ribbon(data = sim_data,
              aes(x = time, 
                  ymin = filtered - 2 * fSE,
                  ymax = filtered + 2 * fSE),
                  fill = "blue",
              alpha = 0.2) +
  #CORRECTED: new title
  labs(title = expression(
    paste("Mean of the filtering distribution of ",
           theta[t],
           " w/ Standard Errors"))) +
  ylab("")

# store and plot the smoothed 
# predictions of y and standard errors

sim_data_smoothed <- dlmSmooth(sim_data_filtered)
sim_data$smoothed <- dropFirst(sim_data_smoothed$s)
sim_data$sSE <- dropFirst(sqrt(unlist(
  dlmSvd2var(sim_data_smoothed$U.S, 
             sim_data_smoothed$D.S))))

gg_sim +
  geom_line(data = sim_data, 
            aes(y = pred,
                x = time),
            color = "red",
            size = .5) +
  geom_line(data = sim_data, 
            aes(y = filtered,
                x = time),
            color = "blue",
            size = .5) +
  geom_line(data = sim_data, 
            aes(y = smoothed,
                x = time),
            color = "darkgreen",
            size = 1.2) +
  geom_ribbon(data = sim_data,
              aes(x = time, 
                  ymin = smoothed - 2 * sSE,
                  ymax = smoothed + 2 * sSE),
              fill = "green",
              alpha = 0.2) +
  labs(title = expression(
    paste("Smoothed Values of ",
          theta[t],
          " w/ Standard Errors"))) +
  ylab("")

# find the MLE of sigma2v and sigma2w
build_mod <- function(x) return(dlm(1, 
                                    FF = 1, 
                                    GG = 1,
                                    V = exp(x[1]),  
                                    W = exp(x[2]), 
                                    m0 = m0_tr,
                                    C0 = C0_tr))   

modMLE <- dlmMLE(y, c(0, 0), build_mod, hessian = T)
exp(modMLE$par)
