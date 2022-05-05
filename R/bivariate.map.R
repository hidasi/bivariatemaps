#' bivariate.map: Create a Bivariate Map
#' @export
#' @examples
#' # https://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html
#' @description Creates a Bivariate Map using two rasters and a color matrix created with colmat() function.
#' @return A plot with the bivariate map.
#' @param rasterx raster
#' @param rastery raster
#' @param colormatrix color matrix from colmat() function
#' @param nquantiles number of quantiles in color matrix (same as used when using colmat() function)
bivariate.map<-function(rasterx, rastery, colormatrix, nquantiles=10){
quanmean<-raster::getValues(rasterx)
temp<-data.frame(quanmean, quantile=rep(NA, length(quanmean)))
brks<-with(temp, stats::quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
r1<-within(temp, quantile <- base::cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
quantr<-data.frame(r1[,2])
quanvar<-raster::getValues(rastery)
temp<-data.frame(quanvar, quantile=rep(NA, length(quanvar)))
brks<-with(temp, stats::quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
r2<-within(temp, quantile <- base::cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
quantr2<-data.frame(r2[,2])
as.numeric.factor<-function(x) {as.numeric(base::levels(x))[x]}
col.matrix2<-colormatrix
cn<-unique(colormatrix)
for(i in 1:length(col.matrix2)){
  ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
cols<-numeric(length(quantr[,1]))
for(i in 1:length(quantr[,1])){
  a<-as.numeric.factor(quantr[i,1])
  b<-as.numeric.factor(quantr2[i,1])
  cols[i]<-as.numeric(col.matrix2[b,a])}
r<-rasterx
r[1:length(r)]<-cols
return(r)}
