#' EcoEDGE: Ecologically and Evolutionarily Distinct and Globally Endangered
#' @export
#' @examples
#' # https://rfunctions.blogspot.com/2021/03/ecoedge-ecologically-and-evolutionarily.html
#' @description Calculates ED, EcoD, EDGE, EcoDGE, and EcoEDGE, from Red List Categories, a phylogenetic tree, and a functional dendrogram.
#' @return A table with calculated values for each variable.
#' @param category data frame where the first column contains the red list categories of species.
#' @param phylo phylogenetic tree of species.
#' @param func functional dendrogram of species.
#' @param funcweight weight of ecological distinctiveness on the calculation of EcoEDGE scores.
#' @param phyloweight weight of evolutionary distinctiveness on the calculation of EcoEDGE scores. Sum of funcweight and phyloweight must be equal to 1.
#' @param polytomy how to handle polytomies in the phylogeny and the functional dendrogram. See "ed.calc" function for details.
EcoEDGE<-function(category,phylo,func,funcweight=0.5,phyloweight=0.5,polytomy="isaac"){
  maxphy<-max(phylo$edge.length)
  maxfun<-max(func$edge.length)
  ratio<-maxphy/maxfun
  funccor<-func
  funccor$edge.length<-func$edge.length*ratio
  cat<-data.frame(category[,1])
  rownames(cat)<-rownames(data.frame(category))
  catpre<-cat
  cat<-ifelse(cat=="LC",0,ifelse(cat=="NT",1,ifelse(cat=="VU",2,ifelse(cat=="EN",3,ifelse(cat=="CR",4,NA)))))
  mammcm<-caper::clade.matrix(phylo)
  mamm.ed<-caper::ed.calc(mammcm, polytomy.cf=c(polytomy))
  phyloED<-mamm.ed[[1]]
  rownames(phyloED)<-phyloED[,1]
  mammcmfd<-caper::clade.matrix(funccor)
  mamm.ecod<-caper::ed.calc(mammcmfd, polytomy.cf=c(polytomy))
  funcED<-mamm.ecod[[1]]
  rownames(funcED)<-funcED[,1]
  EEDGE<-data.frame(matrix(0, length(cat[,1]),6))

  for(i in 1:length(category[,1])){
    rname<-rownames(cat)[i]
    rownames(EEDGE)[i]<-rname
    categorycat<-as.character(catpre[rname,1])
    categoryval<-cat[rname,1]
    fun<-funcED[rname,2]
    phy<-phyloED[rname,2]
    EcoD<-log(1+fun)+((categoryval)*log(2))
    EDGE<-log(1+phy)+((categoryval)*log(2))
    EEDGEsp<-log(1+((fun*funcweight)+(phy*phyloweight)))+((categoryval)*log(2))
    EEDGE[i,1]<-categorycat
    EEDGE[i,2]<-fun
    EEDGE[i,3]<-phy
    EEDGE[i,4]<-EcoD
    EEDGE[i,5]<-EDGE
    EEDGE[i,6]<-EEDGEsp
  }
  colnames(EEDGE)<-c("Threatened Category","EcoD","ED","EcoDGE","EDGE","EcoEDGE")
  return(EEDGE)
}
