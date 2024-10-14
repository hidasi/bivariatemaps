"bivariatemaps"

R Package with functions developed over 10+ years at: https://rfunctions.blogspot.com


Simple example of usage:

# Load these packages (if not installed, use “install.packages()”):
libary(classInt)
library(classInt)
library(raster)
library(dismo)
libary(XML)
library(maps)
library(sp)
library(geodata)
library(bivariatemaps)

# Use ‘geodata’ to download temperature and precipitation rasters:
bclim <- worldclim_global(var="tavg", res=10,  path=tempdir())
bclim2 <- worldclim_global(var="prec", res=10,  path=tempdir())

# Use ‘bivariatemaps’ package to generate color matrix and bivariate map:
col.matrix<-colmat(nquantiles=10, upperleft="blue", upperright="green", bottomleft="grey", bottomright="red")
bivmap<-bivariate.map(bclim,bclim2, colormatrix=col.matrix, nquantiles=10)
 
![image](https://github.com/user-attachments/assets/eeae69d3-bfc2-40c4-a230-a7c75d47dd58)
