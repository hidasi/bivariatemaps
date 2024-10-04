#' betagrid: Calculates Beta Diversity for all the Grid Cells in Comparison with Their Nearest Neighbor Cells
#' @export
#' @examples
#' # https://rfunctions.blogspot.com/2015/08/calculating-beta-diversity-on-grid.html
#' @description Uses a 'gridded' shapefile and its corresponding community data matrix to calculate beta diversity for each focal grid cell in relation to its neighbor cells (for example, 8 nearest neighbors).
#' @return A data frame with values of mean turnover, mean nestedness, and mean beta diversity for each grid cell.
#' @param gridshp Gridded Shapefile ("SpatialPolygonsDataFrame").
#' @param comp community data matrix (species occurrence on each grid cell).
#' @param xfeature number of the feature within the gridded shapefile corresponding to the longitude.
#' @param yfeature number of the feature within the grid shapefile corresponding to the latitude.
#' @param radius the radius (in degrees) that define the maximum distance to select neighbor cells (for example, use the resolution of the gridded shapefile).
#' @param phylotree optional phylogenetic tree ("phylo" class). It can also be a "phylo" class functional dendrogram.
#' @param phylobeta Optional boolean whether or not to calculate or not phylogenetic beta diversity (see "phylo.beta.pair" function in "betapart" package) instead of the usual beta diversity (see "beta.pair" function in "betapart" package). Default is F.
#' @param index to calculate "sorensen" or "jaccard". Default is "sorensen".
betagrid<-function(gridshp, comp, xfeature, yfeature, radius, phylotree, phylobeta=F, index="sorensen"){
  data<-base::data.frame(gridshp[xfeature],gridshp[yfeature],comp)
  mean_turnover<-numeric(length(comp[,1]))
  mean_nestedness<-numeric(length(comp[,1]))
  mean_beta<-numeric(length(comp[,1]))
  for(i in 1:length(gridshp[[2]])){
    adj<-CommEcol::select.window(xf=data[i,1], yf=data[i,2], radius, xydata=data)[,-c(1,2)]
    if(phylobeta==F){
      ifelse(sum(nrow(adj))==0 || ncol(adj)==0, res<-0 , res<-betapart::beta.pair(adj, index.family=index))
    }else if(phylobeta==T){
      ifelse(sum(nrow(adj))==0 || ncol(adj)==0, res<-0 , res<-betapart::phylo.beta.pair(adj, phylotree, index.family=index))
    }
    ifelse(sum(nrow(adj))==0 || ncol(adj)==0, mean_turnover[i]<-0 , mean_turnover[i]<-mean(as.matrix(res[[1]])[2:length(as.matrix(res[[1]])[,1]),1],na.rm=TRUE) )
    ifelse(sum(nrow(adj))==0 || ncol(adj)==0, mean_nestedness[i]<-0 , mean_nestedness[i]<-mean(as.matrix(res[[2]])[2:length(as.matrix(res[[2]])[,1]),1],na.rm=TRUE) )
    ifelse(sum(nrow(adj))==0 || ncol(adj)==0, mean_beta[i]<-0 , mean_beta[i]<-mean(as.matrix(res[[3]])[2:length(as.matrix(res[[3]])[,1]),1],na.rm=TRUE) )
  }
  return(data.frame(cell=row.names(comp), mean_turnover, mean_nestedness, mean_beta))
}
