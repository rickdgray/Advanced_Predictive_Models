###################################################
## COMPUTING DEMO:  Spatial Point Patterns I
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
# (adopted from analyses in "Spatial Point Patterns: Methodology and 
#  Applications with R" by Baddeley, Rupak, and Turner, 2015)
## Last updated: September 1, 2021
###################################################

# load libraries
library(spatstat)

###################################################
# EXPLORATORY ANALYSIS OF SPP 

data(bei) # true locations in a tropical rain forest

class(bei)
bei

# exploratory analysis -- scatterplot
plot(bei, 
     main="Tree Locations")

# exploratory analysis -- quadrant count
Q <- quadratcount(bei, 
                  nx = 6, 
                  ny = 3) 
Q

plot(bei, 
     cex = 0.5, 
     pch = "+", 
     main="Tree Locations")
plot(Q, 
     add = TRUE, 
     cex = 2)

# exploratory analysis -- adding covariate information to the plot
elev <- bei.extra$elev
slope <- bei.extra$grad
class(elev)
class(slope)

plot(elev, 
     col = grey(seq(1,0,length=128)),
     main="Tree Locations with Elevation")
plot(bei, 
     add=T, 
     pch=16, 
     cex=.5,
     col = "red")

plot(slope, 
     col = grey(seq(1,0,length=128)),
     main="Tree Locations with Slope")
plot(bei, 
     add=T, 
     pch=16, 
     cex=.5,
     col = "red")

# perspective plot
plot_persp <- persp(bei.extra$elev, 
                    theta=-45, 
                    phi=18, 
                    expand=7,  
                    border=NA, 
                    apron=TRUE, 
                    shade=0.3,  
                    box=FALSE, 
                    visible=TRUE,
                    main = "True Locations with Elevation")  
perspPoints(bei, 
            Z = bei.extra$elev, 
            M = plot_persp, 
            pch=16)


###################################################
# TESTING FOR CSR (chi-squire test)

bei.quadtest <- quadrat.test(bei, 
                             nx = 6, 
                             ny = 3)  # how does number of quadrants affect results?
bei.quadtest

###################################################
# KERNEL DENSITY SMOOTHING 

# fixed sigma = 50
bei_dens_50 <- density(bei, 
                       kernel="gaussian", 
                       sigma = 50) 
plot(bei_dens_50, 
     col = grey(seq(1,0,length=128)),
     main=expression(paste("KS Estimator of the Intensity Func. sigma = 50")))
plot(bei, 
     add = TRUE, 
     cex = 0.5,
     col = "red") 

###################################################
# MAXIMUM LIKELIHOOD ESTIMATION OF THE INSTENSITY FCN. 

# constant trend
fit1 <- ppm(bei, ~ 1)
summary(fit1)

# linear trend in x and y
fit2 <- ppm(bei, ~ x + y)
summary(fit2)
plot(fit2, 
     how = "image", 
     se = FALSE, 
     col = grey(seq(1,0,length=128)))

# quadratic trend in x and y
fit3 <- ppm(bei, ~ polynom(x, y, 2))
summary(fit3)
plot(fit3, 
     how = "image", 
     se = FALSE, 
     col = grey(seq(1,0,length=128)))

# linear trend in slope
fit4 <- ppm(bei, ~ slope, 
             covariates = list(slope = slope))
summary(fit4)
plot(fit4, 
     how = "image", 
     se = FALSE, 
     col = grey(seq(1,0,length=128)))

# Likelihood ratio test of 
#     H0:  homogeneous PP (CSR) vs. 
#     H1:  inhomogeneous PP with intensity that is a loglinear 
#          function of the slope covariate
anova(fit1, fit4, test = "Chi")

