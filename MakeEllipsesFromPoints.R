# Code to make ellipses given four points measured in the field

library(maptools)

# Day 1 Field data

Plot5=readShapeSpatial("20140217_Day1/20140217_Plant.shp")

plot(Plot5)

str(Plot5@data)

Shrub=Plot5@data[Plot5@data$Shrub_ID=="559",c(1:7, 19, 20)]

plot(Shrub$Easting, Shrub$Northing, asp=1)  #This plots the 4 shrub points, but not in the right order
#But points are in the wrong order
text(Shrub$Easting, Shrub$Northing, row.names(Shrub), pos=2)

#chull returns An integer vector giving the indices of the unique points lying on the convex hull, in clockwise order. (The first will be returned for duplicate points.)
#So we need to subset the Shrub data frame using these indices, since it will give us clockwise order
chuld=Shrub[chull(Shrub[,c("Easting","Northing")]),]
text(chuld[,c("Easting","Northing")], labels=1:4, pos=4, col="red")

polygon(spline.poly(Shrub[,c("Easting", "Northing")], vertices=100)) #Wrong
polygon(spline.poly(chuld[,c("Easting", "Northing")], vertices=100)) #Right

#
# Splining a polygon.
# http://gis.stackexchange.com/questions/24827/how-to-smooth-the-polygons-in-a-contour-map/24929#24929
#   The rows of 'xy' give coordinates of the boundary vertices, in order.
#   'vertices' is the number of spline vertices to create.
#              (Not all are used: some are clipped from the ends.)
#   'k' is the number of points to wrap around the ends to obtain
#       a smooth periodic spline.
#
#   Returns an array of points. 
# 
spline.poly <- function(xy, vertices, k=3, ...) {
    # Assert: xy is an n by 2 matrix with n >= k.
    
    # Wrap k vertices around each end.
    n <- dim(xy)[1]
    if (k >= 1) {
        data <- rbind(xy[(n-k+1):n,], xy, xy[1:k, ])
    } else {
        data <- xy
    }
    
    # Spline the x and y coordinates.
    data.spline <- spline(1:(n+2*k), data[,1], n=vertices, ...)
    x <- data.spline$x
    x1 <- data.spline$y
    x2 <- spline(1:(n+2*k), data[,2], n=vertices, ...)$y
    
    # Retain only the middle part.
    cbind(x1, x2)[k < x & x <= n+k, ]
}