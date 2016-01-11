#Team: TEMP
# Date: 8/1/2016
#Exercise: Design a pre-processing chain to assess change in NDVI over time
setwd("~/git/geoScripting/excerciseLesson5")
getwd()
library(raster)
library(rgdal)

# download the data of the two images

download.file(url = 'http://www.dropbox.com/s/i1ylsft80ox6a32/LC81970242014109-SC20141230042441.tar.gz?dl=0', destfile = './Data/L8Wag2014/LandSat8.tar', method = "wget")
untar("./Data/L8Wag2014/LandSat8.tar",exdir ="./Data/L8Wag2014")

download.file(url='https://www.dropbox.com/s/akb9oyye3ee92h3/LT51980241990098-SC20150107121947.tar.gz?dl=0', destfile='./Data/L5Wag1990/LandSat5.tar', method='wget')
untar('./Data/L5Wag1990/LandSat5.tar', exdir ="./Data/L5Wag1990")






#processin Land sat images 1990

list1990 <- list.files('Data/L5Wag1990/', pattern = glob2rx('*.tif'), full.names = TRUE)
raster(list1990[1])

ListStack90 <- stack(list1990)
ListStack90


# Extract cloud Mask rasterLayer
fmask <- ListStack90[[1]]


# Perform value replacement
ListStack90[fmask != 0] <- NA
# First define a value replacement function
cloud2NA <- function(x, y){
	x[y != 0] <- NA
	return(x)
}
ListStack90_1 <- dropLayer(ListStack90, 1)
ListStack90_1

# Apply the function on the two raster objects using overlay
ListStackCloudFree90 <- overlay(x = ListStack90-1, y = fmask, fun = cloud2NA)
plotRGB(ListStackCloudFree90, 5,4,3,stretch='hist')





# Calculate NDVI for image 1990
NDVI <- function(x, y) {
	ndvi <- (y - x) / (x + y)
	return(ndvi)
}
NDVI90 <- overlay(x=ListStackCloudFree90[[3]], y=ListStackCloudFree90[[4]], fun=NDVI)
plot(NDVI90)
NDVI90LL <- projectRaster(NDVI90, crs='+proj=longlat')
KML(x=NDVI90LL, filename='NDVI1990.kml',overwrite= TRUE)

plot(NDVI90LL)


#processin Land sat images 2014


list2014 <- list.files('Data/L8Wag2014/', pattern = glob2rx('*.tif'), full.names = TRUE)
raster(list2014[1])

ListStack14 <- stack(list2014)
ListStack14


# Extract cloud Mask rasterLayer
fmask <- ListStack14[[1]]


# Perform value replacement
ListStack14[fmask != 0] <- NA
# First define a value replacement function
cloud2NA <- function(x, y){
	x[y != 0] <- NA
	return(x)
}
ListStack14_1 <- dropLayer(ListStack14, 1)
ListStack14_1

# Apply the function on the two raster objects using overlay
ListStackCloudFree14 <- overlay(x = ListStack14-1, y = fmask, fun = cloud2NA)
plotRGB(ListStackCloudFree14, 5,4,3,stretch='hist')





# Calculate NDVI for image 1990
NDVI <- function(x, y) {
	ndvi <- (y - x) / (x + y)
	return(ndvi)
}
NDVI14 <- overlay(x=ListStackCloudFree14[[4]], y=ListStackCloudFree14[[5]], fun=NDVI)
plot(NDVI14)
NDVI14LL <- projectRaster(NDVI14, crs='+proj=longlat')
KML(x=NDVI14LL, filename='NDVI2014.kml',overwrite= TRUE)
#give the same extent for the two images
ext <- intersect(NDVI14LL,NDVI90LL)
NDVI14Cr <-crop(NDVI14LL,ext)
NDVI90Cr<- crop(NDVI90LL,ext)
compareRaster(NDVI14Cr,NDVI90Cr)
