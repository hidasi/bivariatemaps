<h1>"bivariatemaps"

<h2>R Package with functions developed over 10+ years at: https://rfunctions.blogspot.com</h2>


<h2>Simple example of usage:</h2>

<h3># Load these packages (if not installed, use “install.packages()”):</h3>
library(classInt)<br>
library(classInt)<br>
library(raster)<br>
library(dismo)<br>
library(XML)<br>
library(maps)<br>
library(sp)<br>
library(geodata)<br>
library(bivariatemaps)<br>

<h3># Use ‘geodata’ to download temperature and precipitation rasters:</h3>
bclim <- worldclim_global(var="tavg", res=10,  path=tempdir())<br>
bclim2 <- worldclim_global(var="prec", res=10,  path=tempdir())<br>

<h3># Use ‘bivariatemaps’ package to generate color matrix and bivariate map:</h3>
col.matrix<-colmat(nquantiles=10, upperleft="blue", upperright="green", bottomleft="grey", bottomright="red")<br>
bivmap<-bivariate.map(bclim,bclim2, colormatrix=col.matrix, nquantiles=10)<br>
 
![image](https://github.com/user-attachments/assets/eeae69d3-bfc2-40c4-a230-a7c75d47dd58)
