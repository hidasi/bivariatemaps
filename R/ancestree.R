#' ancestree: Get a Phylogeny that Starts at the Common Ancestor of a Group of Species
#' @export
#' @examples
#' # https://rfunctions.blogspot.com/2013/04/ancestree-function-get-phylogeny-that.html
#' @description Uses a sample of species to get the phylogeny that starts at the common ancestor of the sample.
#' @return A phylogeny that starts at the common ancestor of the provided sample.
#' @param sample A data frame with species in columns' names (check out example link).
#' @param tree .tre phylogenetic tree of species.
ancestree<-function(sample,tree){
  cnames<-colnames(sample)
  tree<-ape::as.phylo(tree)
  ca<-ape::mrca(tree)
  ca<-data.frame(ca)
  nameca<-ca[cnames,cnames]
  allanc<-unique(unlist(c(nameca[,1:length(nameca)])))
  unicnodes<-base::sort(allanc[allanc>length(tree$tip.label)])
  nnodefor<-numeric(length(unicnodes))
  nprunsizefor<-numeric(length(unicnodes))
  for(i in 1:length(unicnodes)){
    phy<-ape::extract.clade(tree,unicnodes[i])
    nnodefor[i]<-length(phy$tip.label)
    prunt<-picante::prune.sample(sample,phy)
    nprunsizefor[i]<-length(prunt$tip.label)
  }
  resul<-rbind(unicnodes,nprunsizefor,nnodefor)
  resul<-resul[,base::order(-resul[2,],resul[3,])]
  bestnode<-resul[1,1]
  bestphylo<-ape::extract.clade(tree,bestnode)
  return(bestphylo)
}
