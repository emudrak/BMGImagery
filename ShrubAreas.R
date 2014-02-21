library(maptools)
library(shapefiles)
source("spline.poly.R")

# Day 1 Field data

Plot5=readShapeSpatial("20140217_Day1/20140217_Plant.shp")
#Plot5@data$Shrub_ID=factor(Plot5@data$Shrub_ID)
plot(Plot5)

str(Plot5@data)

#try renumbering sequentially
newnumbs=cbind(Shrub_ID=unique(Plot5@data$Shrub_ID), ShpID=1:length(unique(Plot5@data$Shrub_ID)))

Plot5@data=merge(Plot5@data, newnumbs, by="Shrub_ID")

VERTS=100   #Number of vertices in the spline
ell=data.frame(NULL)


for (i in 1:max(Plot5@data$ShpID)) {
    print(i)
     Shrub=Plot5@data[Plot5@data$ShpID==i,c(1:7, 19:21)] 
    plot(Shrub$Easting, Shrub$Northing, asp=1)  #This plots the 4 shrub points, but not in the right order
     chuld=Shrub[chull(Shrub[,c("Easting","Northing")]),]
    edgecoords=spline.poly(chuld[,c("Easting", "Northing")], vertices=VERTS)
     ell=rbind(ell,Id=cbind(rep(i, nrow(edgecoords)),edgecoords)) #Right

}
names(ell)[1]="Id" #convert.to.shapefile requires key to be named "Id"
#ell$Id=as.numeric(as.character(ell$Id))

#seems as if all info is on Width-D' point
ellTable <- subset(Plot5@data, Dimension=="Width-D", 
        select=c("ShpID", "Shrub_ID" , "Plot_ID" , "Species"  ,"Height_cm" , "Photo_ID" , "Comment"))
names(ellTable)[1]="Id"

#ellTable<- Plot5@data[Plot5@data$Dimension=="Width-D", c( "Plot_ID" ,"Shrub_ID" , "Species"  ,"Height_cm" , "Photo_ID" , "Comment")]
#ellTable$Shrub_ID=factor(ellTable$Shrub_ID)
ellShapefile <- convert.to.shapefile(ell, ellTable, "Id", type=5)
write.shapefile(ellShapefile, out.name="Spliny.shp", arcgis=T)
str(ell)

str(ellTable)

Plot5[1:100,]