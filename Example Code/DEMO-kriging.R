###################################################
## COMPUTING DEMO:  Kriging
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
## Last updated: September 26, 2021
###################################################

## load libraries
library(ggplot2)
library(mapproj)
library(geoR)
library(leaflet)
library(gridExtra)

# load scallops data
scallops <- read.table("scallops.txt",header=T)

# take the log transformation of the 
# total catch counts
ggplot(data = scallops,
       aes(x = tcatch)) +
  geom_histogram()

scallops$lgcatch <- log(scallops$tcatch + 1)

ggplot(data = scallops,
       aes(x = lgcatch)) +
  geom_histogram()

# plot the transformed data on a map
pal <- colorBin("viridis", bins = seq(0, round(max(scallops$lgcatch)), length = round(max(scallops$lgcatch))+1))#c(0, 0.25, 0.5, 0.75, 1))
leaflet(scallops) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~long, lat = ~lat, color = ~ pal(lgcatch)) %>%
  addLegend("bottomright",
            pal = pal, values = ~ lgcatch,
            title = "log(tcatch + 1)"
  ) %>%
  addScaleBar(position = c("bottomleft"))

# project the coordinates of the data
locs <- mapproject(scallops$long, 
                   scallops$lat, 
                   "mercator")

# create geodata (geoR) object
scallops_geo <- as.geodata(
  cbind(scallops$lgcatch, 
        locs$x, 
        locs$y), 
			  data.col = 1, 
				coords.col = 2:3)

# estimate the parameters of an
# exponential covariance function
# using maximum likelihood estimation
fit <- likfit(
  scallops_geo,
  cov.model = "exponential",
  ini.cov.pars = c(5, .01),
  fix.nugget=FALSE, 
  nugget=0.1)

# create a grid of prediction locations
locs_range <- apply(scallops_geo$coords, 2, range)  

pred_locs <- expand.grid(
  seq(locs_range[1,1], 
      locs_range[2,1],
      length = 30), 
	seq(locs_range[1,2], 
			locs_range[2,2], 
			length = 30))
names(pred_locs) <- c("x", "y")

border_locs <- chull(scallops_geo$coords)
pred_locs_inside <- locations.inside(
  pred_locs, 
  border=scallops_geo$coords[border_locs,])

ggplot(pred_locs_inside,
       aes(x = x, y = y)) +
  geom_point(col = "darkgrey", shape = 3) +
  geom_point(data = as.data.frame(scallops_geo$coords),
             aes(x = Coord1, y = Coord2))

# find kriging predictions of the log(tcatch+1)
# at the prediction locations 

pred <- krige.conv(
  scallops_geo, 
  locations = pred_locs_inside, 
  krige=krige.control(type.krige="ok", 
  obj.model = fit))

# plot the predicted values and standard errors
pred_dat <- data.frame(
  estimate = pred$predict,
  standard_error = sqrt(pred$krige.var),
  x = pred_locs_inside$x,
  y = pred_locs_inside$y)
                       
p1 <- ggplot() +
  geom_raster(data = pred_dat,
              aes(x = x, y = y, fill = estimate)) +
  geom_point(data = as.data.frame(scallops_geo$coords),
             aes(x = Coord1, y = Coord2)) +
  scale_fill_gradient(low = "#ffffb2", high = "#b10026") +
  guides(fill = guide_legend(title = NULL)) +
  ggtitle("Predicted log(tcatch + 1)")

p2 <- ggplot() +
  geom_raster(data = pred_dat,
              aes(x = x, y = y, fill = standard_error)) +
  geom_point(data = as.data.frame(scallops_geo$coords),
             aes(x = Coord1, y = Coord2)) +
  scale_fill_gradient(low = "#ffffb2", high = "#b10026") +
  guides(fill = guide_legend(title = NULL))+
  ggtitle("Standard Errors")

grid.arrange(p1, p2, nrow = 2)


