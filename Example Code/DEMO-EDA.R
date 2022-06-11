###################################################
## COMPUTING DEMO:  Assessing stationarity and the
##                  sample autocorrelation function
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
## Last updated: July 26, 2021
###################################################

# load library
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(astsa)
library(xts)
library(lubridate)

###################################################
# SIMULATED DATA

###
# Case 1:  moving average
###

# generate the time series
n <- 500
w <- rnorm(n + 2)
v <- stats::filter(w, sides = 2, filter = rep(1/3, 3))
v <- v[2:(n+1)]
index <- 1:n

# plot the series
ma_data <- data.frame(index = index, v = v)
gg_ma <- ggplot(ma_data, 
                      aes(x = index, y = v)) + 
  geom_line(size = .5, color = "blue") +
  geom_point(shape = 21, size = 1, fill = "white", color = "blue") +
  labs(title = 'Moving Average')+
  xlab("time") +
  ylab("v")
gg_ma

# Assessing Stationarity

# 1.  Does the mean depend on time?
gg_ma + geom_hline(yintercept = mean(ma_data$v),
                   color = "darkgrey",
                   size = 1.5)

ncuts <- 5
breaks = seq(0, n, by = n/ncuts)
ma_binned <- ma_data %>%
  mutate(interval = cut(index, 
                        breaks = breaks, 
                        include.lowest = T)) %>%
  group_by(interval) %>%
  summarize(mean.v = mean(v))

gg_ma + 
  geom_hline(yintercept = mean(ma_data$v)) +
  annotate("segment",
           x = breaks[1:(ncuts)],
           xend = breaks[2:(ncuts+1)],
           y = ma_binned$mean.v,
           yend = ma_binned$mean.v,
           color = "red",
           size = 1,
           linetype = "dashed"
  )

# 2. Is the autocorrelation a function of the lag?

# Easier to see:  is the series homoskedastic (constant variance)?
ma_data %>%
  mutate(interval = cut(index, 
                        breaks = breaks, 
                        include.lowest = T)) %>%
  group_by(interval) %>%
  summarize(var.v = var(v))

# Assuming the process is stationary, we 
# can estimate the autocorrelation function

# autocorrelation function
par(mfrow = c(1,1))
acf(ma_data$v, lag.max = 4, plot = FALSE)
acf(ma_data$v, plot = TRUE)

# 95% confidence intervals = +/- 2/sqrt(n)
2/sqrt(n)

###
# Case 2:  random walk
###

# generate the time series
n <- 500
w <- rnorm(n + 50)
x <- stats::filter(w, 
                   filter = 1, 
                   method = "recursive")[-(1:50)]
index <- 1:n

# plot the series
rw_data <- data.frame(index = index, x = x)
gg_rw <- ggplot(rw_data, 
                aes(x = index, y = x)) + 
  geom_line(size = .5, color = "blue") +
  geom_point(shape = 21, size = 1, fill = "white", color = "blue") +
  labs(title = 'Random Walk')+
  xlab("time") +
  ylab("x")
gg_rw

# Assessing Stationarity

# 1.  Does the mean depend on time?
gg_rw + geom_hline(yintercept = mean(rw_data$x),
                   color = "darkgrey",
                   size = 1.5)

ncuts <- 5
breaks = seq(0, n, by = n/ncuts)
rw_binned <- rw_data %>%
  mutate(interval = cut(index, 
                        breaks = breaks, 
                        include.lowest = T)) %>%
  group_by(interval) %>%
  summarize(mean.x = mean(x))

gg_rw + 
  geom_hline(yintercept = mean(ma_data$v)) +
  annotate("segment",
  x = breaks[1:(ncuts)],
  xend = breaks[2:(ncuts+1)],
  y = rw_binned$mean.x,
  yend = rw_binned$mean.x,
  color = "red",
  size = 1,
  linetype = "dashed")

# is this expected?
             
# 2. Is the autocorrelation a function of the lag?

# Does it make sense to estimate 
# the autocorrelation function?
 
###################################################
# REAL DATA - monthly export price of Norweigian
# salmon per kg (in astsa library)

# salmon data
salmon_data <- data.frame(price = as.numeric(salmon),
                          time = as.Date(time(salmon)))

# plot the data
gg_salmon <- ggplot(salmon_data, 
                    aes(x = time, y = price)) + 
  geom_line(size = .5, color = "blue") +
  geom_point(shape = 21, size = 1, fill = "white", color = "blue") +
  labs(title = 'Salmon Export Price')
gg_salmon

# fit a linear regression on time
fit <- lm(price ~ time, data = salmon_data)
summary(fit)
  
# add fitted line to the plot
gg_salmon + stat_smooth(method = lm)

# plot the detrended (residual) series
salmon_data$resids <- fit$residuals

gg_resids <- ggplot(salmon_data, 
                    aes(x = time, y = resids)) + 
  geom_line(size = .5, color = "blue") +
  geom_point(shape = 21, size = 1, fill = "white", color = "blue") +
  labs(title = 'Detrended Salmon Export Price') +
  geom_hline(yintercept = 0, color = "darkgrey")
gg_resids

n <- length(salmon_data$time)
ncuts <- 3
breaks <- seq(1, n, by = floor(n/ncuts))
breaks[ncuts+1] <- n
break_dates <- salmon_data$time[breaks]

salmon_binned <- salmon_data %>%
  mutate(interval = cut(time, 
                        breaks = break_dates, 
                        include.lowest = T)) %>%
  group_by(interval) %>%
  summarize(mean.resids = mean(resids), 
            var.resids = var(resids))

salmon_binned

gg_resids +
  annotate("segment",
           x = salmon_data$time[breaks[1:(ncuts)]],
           xend = salmon_data$time[breaks[2:(ncuts+1)]],
           y = salmon_binned$mean.resids,
           yend = salmon_binned$mean.resids,
           color = "red",
           size = 1,
           linetype = "dashed")

# autocorrelation function (acf vs. acf1)
acf1(salmon_data$resids, plot = TRUE)

# Question: what happens as ncuts changes?

# Differencing vs detrending
salmon_data$diff <- c(NA, diff(salmon_data$price))

gg_diff <- ggplot(salmon_data[!is.na(salmon_data$diff),], 
                    aes(x = time, y = diff)) + 
  geom_line(size = .5, color = "blue") +
  geom_point(shape = 21, size = 1, fill = "white", color = "blue") +
  labs(title = 'Detrended Salmon Export Price') +
  geom_hline(yintercept = 0, color = "darkgrey")
gg_diff

salmon_binned_diff <- salmon_data[!is.na(salmon_data$diff),] %>%
  mutate(interval = cut(time, 
                        breaks = break_dates, 
                        include.lowest = T)) %>%
  group_by(interval) %>%
  summarize(mean.diff = mean(diff), 
            var.diff = var(diff))

gg_diff +
  annotate("segment",
           x = salmon_data$time[breaks[1:(ncuts)]],
           xend = salmon_data$time[breaks[2:(ncuts+1)]],
           y = salmon_binned_diff$mean.diff,
           yend = salmon_binned_diff$mean.diff,
           color = "red",
           size = 1,
           linetype = "dashed")

salmon_binned_diff

# compare sample ACFs
par(mfrow = c(2, 1))
acf1(salmon_data$resids, 48, plot = TRUE)
acf1(salmon_data$diff, 48, plot = TRUE)
