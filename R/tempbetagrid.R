#' tempbetagrid: Calculates Temporal Beta Grid Using Species' Occurrences
#' @export
#' @examples
#' # https://rfunctions.blogspot.com/2016/10/calculating-temporal-beta-diversity-on.html
#' @description Uses two occurrence matrices (from different time periods) to calculate temporal beta diversity for each site or grid cell.
#' @return A data frame with values of turnover, nestedness, beta diversity, and turnover/beta for each site or grid cell.
#' @param oc1 Community data matrix (species occurrence on each grid cell).
#' @param oc2 Second community data matrix (species occurrence on each grid cell) for another time period.
#' @param phylotree Optional phylogenetic tree ("phylo" class). It can also be a functional dendrogram.
#' @param phylobeta Optional boolean indicating whether or not to calculate phylogenetic beta diversity (see "phylo.beta.pair" function in "betapart" package) instead of the usual beta diversity (see "beta.pair" function in "betapart" package). Default is F.
#' @param index Calculate "sorensen" or "jaccard". Default is "sorensen".
tempbetagrid<-function(oc1, oc2, index="sorensen", phylotree, phylobeta=F){
  tempturn<-numeric(nrow(oc1))
  tempnest<-numeric (nrow(oc1))
  tempbeta<-numeric(nrow(oc1))
  tempturnbeta<-numeric (nrow(oc1))
  for(i in 1:nrow(oc1) ){
    namesoc1<-names(oc1)[oc1[i,]==1]
    namesoc2<-names(oc2)[oc2[i,]==1]
    both<-namesoc1[namesoc1%in%namesoc2]
    bothmat<-rbind(rep(1,length(both)),rep(1,length(both)))
    colnames(bothmat)<-both
    namoc1<-namesoc1[namesoc1%in%namesoc2==FALSE]
    nam1mat<-rbind(rep(1,length(namoc1)),rep(0,length(namoc1)))
    colnames(nam1mat)<-namoc1
    namoc2<-namesoc2[namesoc2%in%namesoc1==FALSE]
    nam2mat<-rbind(rep(0,length(namoc2)),rep(1,length(namoc2)))
    colnames(nam2mat)<-namoc2
    matcomp<-cbind(bothmat,nam1mat,nam2mat)
    forprune<-t(data.frame(names(data.frame(matcomp))))
    colnames(forprune)<-forprune
    ifelse(phylobeta==T, betas<-betapart::phylo.beta.pair(matcomp, picante::prune.sample(forprune, phylotree), index.family=index), betas<-betapart::beta.pair(matcomp, index.family=index) )
    tempturn[i]<-betas[[1]]
    tempnest[i]<-betas[[2]]
    tempbeta[i]<-betas[[3]]
    tempturnbeta[i]<-betas[[1]]/betas[[3]]}
  return(data.frame(turnover=tempturn,nestedness=tempnest,beta=tempbeta,turn.beta=tempturnbeta))}
