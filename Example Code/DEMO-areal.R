###################################################
## COMPUTING DEMO:  Areal Spatial Data Analysis
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
## Last updated: Oct. 2, 2021

# COVID Data Source:  https://dhsgis.wi.gov (accessed 1/20/2021)
#                     Date Range:  8/1/2020 - 10/15/2020
# Poverty Data:  https://data.census.gov (accessed 1/26/2021)
#                 ACS 2019 (5 year), percent of individuals whose
#                 income is below the poverty level for family size
###################################################

## load libraries
library(ggplot2)
library(gridExtra)
library(sf)
library(spdep)
library(spatialreg)

# load the covid sf object
load("covid.RData")

##########
# PART 1:  Exploratory Analysis

# map of the census tracts in Milwaukee County
ggplot(data = covid) +
  geom_sf(fill = "white")

# Population size (census tract)
# -> histogram
ggplot(data = covid) +
  geom_histogram(aes(x = Pop_Est), bins = 20)

covid$logPop <- log(covid$Pop_Est)
ggplot(data = covid) +
  geom_histogram(aes(x = logPop), bins = 20)

# remove census tract where no one lives
covid <- covid[!is.na(covid$Pop_Est), ]

# -> map
ggplot(data = covid) +  
  geom_sf(aes(fill = logPop))

# Total number of tests
covid$NUM_TESTS <- covid$NUM_NEG + covid$NUM_POS

# -> map
ggplot(data = covid) +
  geom_sf(aes(fill = NUM_TESTS))

# Positivity rate
ggplot(data = covid) +
  geom_histogram(aes(x = POSITIVITY), bins = 20)

# -> map
ggplot(data = covid) +
  geom_sf(aes(fill = POSITIVITY))

# Positivity rate (log scale)
covid$logPOS <- log(covid$POSITIVITY)

# -> histogram
ggplot(data = covid) +
  geom_histogram(aes(x = logPOS), bins = 20)

# -> map
p_logPOS <- 
  ggplot(data = covid) +
  geom_sf(aes(fill = logPOS)) +
  labs(title = "log Posivity")

p_logPOS

# Construct neighborhood list
nb <- poly2nb(covid) 
nb

nb[1]
nb[2]

# -> plot
nb_lines <- nb %>%
  nb2lines(coords = coordinates(as(covid, "Spatial"))) %>%
  as("sf") %>%
  st_set_crs(st_crs(covid))

ggplot(data = covid) +
  geom_sf(fill = "white", color = "lightgrey") + 
  geom_sf(data = nb_lines, col = "red") +
  labs(title = "Adjacent Census Tracts")

# Calculate avg. log positivity for
# first order neighbors
covid$avg_logPOS <- 
  sapply(nb,
        function(i) mean(covid$logPOS[i],
                         na.rm = TRUE))

# -> map
p_avg_logPOS <- 
  ggplot(data = covid) +
  geom_sf(aes(fill = avg_logPOS)) +
  labs(title = "NB Avg. log Posivity")

grid.arrange(p_logPOS, 
             p_avg_logPOS,
             nrow = 1,
             respect = TRUE)

# scatterplot of avg_logPOS vs. logPOS
ggplot(covid,
     aes(x = logPOS, y = avg_logPOS)) +
  geom_point() +
  stat_smooth(method = "lm") +
  xlab("log Positivity") +
  ylab("NB Avg. log Posivity")

##########
# Formal Test for Spatial Dependence

# construct binary adjacency matrix
W_mat <- nb2listw(nb, 
                  style = 'B', 
                  zero.policy = T) 

moran.mc(covid$POSITIVITY, 
         W_mat, 
         1000, 
         zero.policy = T)



##########
# PART 2:  Model-Based Smoothing

# SAR Model
fit_SAR <- spautolm(
  logPOS ~ 1,
  family = "SAR",
  data = covid,
  listw = W_mat
)

pred_logPOS_SAR <- fit_SAR$fit$fitted.values

p_pred_logPOS_SAR <- 
  ggplot(data = covid) +
  geom_sf(aes(fill = pred_logPOS_SAR)) +
  labs(title = "SAR Predicted log Posivity")
         
grid.arrange(p_logPOS, 
             p_pred_logPOS_SAR,
             nrow = 1,
             respect = TRUE)

# CAR Model
fit_CAR <- spautolm(
  logPOS ~ 1,
  family = "CAR",
  data = covid,
  listw = W_mat
)

covid$pred_logPOS_CAR <- fit_CAR$fit$fitted.values

p_pred_logPOS_CAR <- 
  ggplot(data = covid) +
  geom_sf(aes(fill = pred_logPOS_CAR)) +
  labs(title = "CAR Predicted log Posivity")

grid.arrange(p_pred_logPOS_SAR,
             p_pred_logPOS_CAR,
             nrow = 1,
             respect = TRUE)

# compare fits
summary(fit_SAR)
summary(fit_CAR)

##########
# PART 3: Spatial regression models

# Calculate the standardized poverty rate
covid$std_POV <- (covid$POVERTY - mean(covid$POVERTY))/
  sd(covid$POVERTY)

# -> map
ggplot(data = covid) +
  geom_sf(aes(fill = std_POV)) +
  labs(title = "Poverty Rate (standardized)")

# non-spatial linear model
fit_ns <- lm(logPOS ~ std_POV,
             data = covid)
summary(fit_ns)

# -> map residuals
covid$resids_ns <- fit_ns$residuals
ggplot(data = covid) +
  geom_sf(aes(fill = resids_ns)) +
  labs(title = "NS Residuals")

# -> examine Moran's I
moran.mc(covid$resids_ns, 
        W_mat, 
        1000, 
        zero.policy = T)

# Spatial Moving Average Model
fit_SMA <- spautolm(logPOS ~ std_POV,
                    listw = W_mat,
                    family = "SMA",
                    data = covid)
summary(fit_SMA)

# -> map residuals
covid$resids_SMA <- residuals(fit_SMA)
ggplot(data = covid) +
  geom_sf(aes(fill = resids_SMA)) +
  labs(title = "SMA Residuals")

# -> examine Moran's I
moran.mc(covid$resids_SMA, 
         W_mat, 
         1000, 
         zero.policy = T)
