###################################################
## COMPUTING DEMO:  Fitting ARMA Models and Forecasting
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
## Last updated: July 29, 2021
###################################################

# load library
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(astsa)
library(xts)
library(lubridate)

# Set constants
n <- 150 #length of time series
n_train <- 100 #number of obs. for model fitting
p_sim <- 2 #AR order
d_sim <- 0 #Differencing order
q_sim <- 0 #MA order

ar_tr <- c(1.5, -.75)
ma_tr <- NULL

# Generate from an ARMA(p_sim, q_sim) model
x <- arima.sim(list(order = c(p = p_sim, 
                              d = d_sim, 
                              q = q_sim),
                    ar = ar_tr,
                    ma = ma_tr),
               n = n)

# Fit model  
fit <- sarima(x,
              p = p_sim, 
              d = d_sim, 
              q = q_sim, 
              no.constant = TRUE,
              details = FALSE)

# Examine estimated model parameters
fit

# Set the training window
x_train <- window(x, 
                  start = 1, 
                  end = n_train)

# Fit the ARMA(p_sim, q_sim) to the training data
# and forecast
fit_train <- sarima(x_train,
              p = p_sim, 
              d = d_sim, 
              q = q_sim, 
              no.constant = TRUE,
              details = FALSE)

fit_for <- sarima.for(x_train, 
                      n.ahead = n - n_train, 
                      p = p_sim, 
                      d = d_sim, 
                      q = q_sim,
                      plot = F)

# Collect time series for plotting
fit_data <- bind_rows(
  data.frame(Time = 1:n,
             Type = factor(rep("Simulated Data", n),
                              levels = c("Simulated Data",
                                         "Pred")),
             x = as.numeric(x)),
  data.frame(Time = 1:n,
             Type = factor(rep("Pred", n),
                              levels = c("Simulated Data",
                                         "Pred")),
             x = c(as.numeric(x_train) - 
                     as.numeric(resid(fit_train$fit)), 
                   as.numeric(fit_for$pred))))
fit_pred_data <- data.frame(Time = 1:n,
                            x = c(as.numeric(x_train) - 
                                    as.numeric(resid(fit_train$fit)), 
                                  as.numeric(fit_for$pred)),
                            SE = c(rep(sqrt(fit_train$fit$sigma2), n_train), 
                                   as.numeric(fit_for$se)))

# Plot data and forecasts
gg_fit <- ggplot(fit_data,
                 aes(x = Time)) +
  geom_line(aes(y = x, col = Type)) +
  geom_ribbon(data = fit_pred_data,
              aes(x = Time, 
                  ymin = x - 2*SE,
                  ymax = x + 2*SE),
              alpha = .2) +
  geom_vline(xintercept = n_train)

gg_fit
