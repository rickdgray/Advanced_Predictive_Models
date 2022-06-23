###################################################
## COMPUTING DEMO:  Spatial Point Patterns II
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
# (adopted from analyses in "Spatial Point Patterns: Methodology and 
#  Applications with R" by Baddeley, Rupak, and Turner, 2015)
## Last updated: September 1, 2021
###################################################

# load libraries
library(spatstat)

# load the cell data - locations of the centers
# of 42 biological cells observed under
# optical microscopy in a histological section

data(cells)

plot(cells, 
     main="Cell Locations")

###################################################
# K FUNCTION AND DIAGNOSTICS 

#  Compute the estimated K function
K_cells <- Kest(cells,
                correction = "border")
plot(K_cells, 
     main="Estimated K function")

# Monte Carlo assessments of CSR
lambda_hat <- cells$n # section is 1x1 

N_samp <- 15
X_samp <- list(N_samp)
K_X_samp <- list(N_samp)

for(i in 1:N_samp)
{
  X_samp[[i]] <- rpoispp(lambda_hat, 
                         win=owin(c(0,1),c(0,1)))
  K_X_samp[[i]] <- Kest(X_samp[[i]],
                        correction = "border")
}

par(ask=T)
for(i in 1:N_samp)
{
  plot(X_samp[[i]], 
       main=paste("Sampled SPP ", i))
}

par(ask=F)
plot(K_cells$r, 
     K_cells$border, 
     type = 'l', 
     lty = 1, 
     col = 'red', 
     xlab = "r", 
     ylab = "Kest(r)")
for(i in 1:N_samp) 
{
  lines(K_X_samp[[i]]$r, 
        K_X_samp[[i]]$border, 
        lty = 2, 
        col = "darkgray")
}

# using the envelope function
Kfunc_E <- envelope(cells, 
                    Kest, 
                    correction = "border",
                    nsim = 39, 
                    rank = 1)
plot(Kfunc_E,  main="Estimated K function")	



