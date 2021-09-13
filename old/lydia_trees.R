library(plyr)
library(lattice)
library(latticeExtra)
library(grid)
library(gridExtra)
library(BoutrosLab.plotting.general)

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/set.position.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/adjust_tree.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.calc.clone.coords.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/plot.ellipses.R')

tree_in <- read.table("~/isilon/private/users/asalcedo/Lydia_trees/SRC-BRCA/SomaticSniper-Battenberg/CPCG0103-B1F1_consensus_tree_filtered_pga.txt", header=TRUE, as.is=TRUE)

prep.inputs <- function(tree_in,sample){
  tree <- tree_in[which(tree_in[,1] == sample),c(2,3,9,4)]
  tree$parent[tree$parent==0] <- -1
  colnames(tree) <- c('parent','tip','length1','length2')
  in.tree.df <- data.frame(lab = c(tree$tip), parent = c( tree$parent))
  in.tree.df$ccf <- NULL
  in.tree.df$col <- default.colours(number.of.colours=length(unique(tree_in$child)), palette="spiral.noon" )
  branching <- ifelse(any(duplicated(tree$parent)== TRUE),TRUE,FALSE)
  return(list(in.tree.df = in.tree.df, tree=tree, branching=branching, samp=sample))
}

inputs <- prep.inputs(tree_in, "CPCG0103-B1F1")

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.calc.clone.coords.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/adjust_tree.R')

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/plot.ellipses.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')

out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= NULL,filename='tree_103.pdf', scale2=0.5/362,wid=60,extra.len=10,len=40,
                    scale.x.real = 1/30, sig.shape=2.5, w.padding =1, h.padding=1, add.bells=FALSE, add.labels=TRUE, add.circle=TRUE, offset=1.38,axis.type="both",rad=3.2,gene.cex=1.22,
                    add.genes=FALSE,samp_name=inputs$samp,seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=0.25,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=1, axis.space.right=1, axis.cex=0.75,
                    snv.interval=100,pga.cap=TRUE)

test3 <- out[[4]], vp=viewport(width=unit()))
