library(plyr)
library(lattice)
library(latticeExtra)
library(grid)

trees <- read.table("/.mounts/labs/boutroslab/private/Collaborators/AnthonyNichols/pdx_Models/wgs/Subclonality/Phylogeny/PhyloWGS-TITAN/all_trees.txt",header=FALSE,stringsAsFactors =FALSE)
trees$V1 <- gsub('-','.',trees$V1)

prep.pga <- function(trees){
  pga.new <- trees[,c(1,3,9,6)]
  colnames(pga.new) <- c('Sample','Node','PGA','CP')
  pga.new <- pga.new[which(pga.new$Node !='child'),]
  return(pga.new)
}

setwd("/.mounts/labs/boutroslab/private/Collaborators/AnthonyNichols/pdx_Models/wgs/Subclonality/Phylogeny/PhyloWGS-TITAN/Plots")
pga <- prep.pga(trees)

colours <- c(NA, '#999793', "#5884BB", "#FCF9BF")
for (type in c("both","none","SNV","PGA")){
  for (samp  in unique(pga$Sample)){
    inputs <- prep.tree(samp=samp,trees=trees,pga=pga,axis.type=type,normal.included=FALSE,pga.percent=TRUE)
    
    out <- draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/1800,wid=60,extra.len=20,len=10,
                 scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.38,y.wid=15,maxwid=60,axis.type=inputs$axis.type,rad=3.2,gene.cex=1.22,
                 add.genes=inputs$add.genes,samp_name=samp,seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=2, axis.space.right=2, axis.cex=1.2,
                 snv.interval=1000,pga.cap=TRUE)
  }
}