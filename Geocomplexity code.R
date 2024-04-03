# Prepare your R packages, the computation of Geocomplexity requires
# spatial adjacency matrix from 'spdep' and 'spatialreg'.

library(rgdal)
library(spdep)
library(spatialreg)
library(dplyr)
setwd('your file location')
SA3shp <- readOGR('SA3RawData.shp')
wr <- poly2nb(SA3shp, row.names=SA3shp$SA3_CODE16, queen=TRUE)
wm <- nb2mat(wr, style='B', zero.policy = TRUE)

# Next, export the spatial adjacency matrix. As the row names is the
# id of SA3 areas, you can then join other attributes (industrial scale, industrial employee and so on) by SA3-id with
# the spatial adjacency matrix using GIS or database management systems.
write.csv(wm,'conmatrix.csv')

# Now, we can compute the geocomplexity with sex-ratio as an example
# Note: Make sure that your attribute is standardized Z-score
# Note: This is an example of geocomplexity computation for sex-ratio
# Note: The number 346 is the column number of standardized sex-ratio
# Note: 333 is the observation number

setwd('file location')
SA3data <- read.csv('SA3matrix.csv')
geocomplexSex <- numeric(333)
for (i in 1:333) {
  GeoC <- 0;
  Countm <- 0;
  for (j in 2:334){
    if ((SA3data[i,j]==1)){
      Countm <- Countm+1;
      GeoC <- GeoC+(SA3data[i,346]*SA3data[j-1,346]);
    }
    Countp <- 0;
    GeoC2 <- 0;
    for (k in 2:334){
      if ((SA3data[i,k]==1)&&(SA3data[j-1,k]==1)&&(SA3data[i,j]==1)){
        Countp <- Countp+1;
        GeoC2 <- GeoC2+(SA3data[k-1,346]*SA3data[j-1,346]);
      }
    }
    if (Countp!=0){
      GeoC2F <- GeoC2/Countp;
      GeoC <- GeoC+GeoC2F;
    }else{
      GeoC2K <- 0;
      GeoC <- GeoC + GeoC2K;
    }
  }
  geocomplexSex[i] <- GeoC/Countm;
}
