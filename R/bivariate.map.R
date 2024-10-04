#' bivariate.map: Create a Bivariate Map
#' @export
#' @examples
#' # https://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html
#' @description Creates a Bivariate Map using two rasters and a color matrix created with colmat() function.
#' @return A plot with the bivariate map.
#' @param rasterx raster
#' @param rastery raster
#' @param rasterz raster (only if trivariate=T)
#' @param colormatrix color matrix from colmat() function
#' @param nquantiles number of quantiles in color matrix (same as used when using colmat() function)
#' @param trivariate boolean indicating if should use a third raster for a "trivariate" map where the third variable corresponds to the transparency (alpha) of colors.
bivariate.map<-function(rasterx, rastery, colormatrix, nquantiles=10, trivariate=F, rasterz){
quanmean<-terra::values(rasterx)
temp<-data.frame(ifelse(quanmean==0,NA,quanmean), quantile=rep(NA, length(quanmean)))
brks<-with(temp, stats::quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
r1<-within(temp, quantile <- base::cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
quantr<-data.frame(r1[,2])
quanvar<-terra::values(rastery)
temp<-data.frame(ifelse(quanvar==0,NA,quanvar), quantile=rep(NA, length(quanvar)))
brks<-with(temp, stats::quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
r2<-within(temp, quantile <- base::cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
quantr2<-data.frame(r2[,2])
as.numeric.factor<-function(x) {as.numeric(base::levels(x))[x]}
col.matrix2<-colormatrix
cn<-unique(colormatrix)
for(i in 1:length(col.matrix2)){
  ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
cols<-numeric(length(quantr[,1]))
if(trivariate==T){
  quansat<-terra::values(rasterz)
  r3<-quansat[,1]
  r3<-quansat/max(r3,na.rm=TRUE)
  for(i in 1:length(quantr[,1])){
    ifelse(is.na(r3[i]),r3[i]<-1,r3[i]<-r3[i])
    ifelse(is.nan(r3[i]),r3[i]<-1,r3[i]<-r3[i])
  }
  terra::values(rasterz)=r3
}
for(i in 1:length(quantr[,1])){
  a<-as.numeric.factor(quantr[i,1])
  b<-as.numeric.factor(quantr2[i,1])
  cols[i]<-as.numeric(col.matrix2[b,a])}
r<-rasterx
terra::values(r)<-cols
if(trivariate==T){
  terra::plot(r,frame.plot=F,axes=F,box=F,add=F,legend=F,col=as.vector(colormatrix),alpha=rasterz)
}else{
  terra::plot(r,frame.plot=F,axes=F,box=F,add=F,legend=F,col=as.vector(colormatrix))
}
return(r)}
