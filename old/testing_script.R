library(grid)
data(iris)
testxy <- xyplot(Sepal.Length~Sepal.Width, data=iris)
test_frame <- frameGrob()
packGrob(test_frame,testxy)
test_tree <-gTree(testxy,children=gList(textGrob("test")))
test_tree <-grid.grabExpr(print(testxy))
test_tree <- addGrob(test_tree,textGrob("test"))
test_tree <- removeGrob(test_tree,trellis.grobname(name="ticks.right","panel",column=1,row=1))
grid.draw(test_tree)



library(plyr)
library(lattice)
library(latticeExtra)
library(grid)
library(gridExtra)


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


trees <- read.table("/.mounts/labs/boutroslab/private/Collaborators/AnthonyNichols/pdx_Models/wgs/Subclonality/Phylogeny/PhyloWGS-TITAN/Plots/all_trees.txt",header=FALSE,stringsAsFactors =FALSE)
trees$V1 <- gsub('-','.',trees$V1)

prep.pga <- function(trees){
  pga.new <- trees[,c(1,3,9,6)]
  colnames(pga.new) <- c('Sample','Node','PGA','CP')
  pga.new <- pga.new[which(pga.new$Node !='child'),]
  return(pga.new)
}
pga <- prep.pga(trees)

colours <- c(NA, '#999793', "#5884BB", "#FCF9BF")

type = "PGA"
type="none"

samp = "PDX.47T"

inputs <- prep.tree(samp=samp,trees=trees,pga=pga,axis.type=type,normal.included=FALSE,pga.percent=TRUE)
inputs$genes.df <- data.frame(gene=c('TP53','MYC'), cn=c('loss','gain'), node=c(1,2),col=c('orchid','cornflowerblue'),stringsAsFactors = FALSE)
inputs$add.genes = TRUE

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')

out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename='tree_orig5.pdf',scale2=0.5/1800,wid=4,extra.len=20,len=10,
                    scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, add.bells=FALSE, add.labels=TRUE, add.circle=TRUE, offset=1.38,axis.type="PGA",rad=3.2,gene.cex=1.22,
                    add.genes=FALSE,samp_name=samp,seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=0.25,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=15, axis.space.right=15, axis.cex=0.5,
                    snv.interval=1000,pga.cap=TRUE)



setwd("~")

samp = "CPCG0089"
# inputs <- prep.tree(samp=samp,trees=trees,pga=pga,axis.type=type,normal.included=FALSE,pga.percent=TRUE)
inputs <- prep.tree(samp=samp,trees=trees,pga=pga.new,axis.type="PGA",normal.included=FALSE,pga.percent=TRUE)
out <- draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename='tree_orig6.pdf',scale2=0.5/362,wid=20,extra.len=20,len=10,
                    scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, add.labels=FALSE, add.circle=TRUE, offset=1.38,axis.type="both",rad=3.2,gene.cex=1.22,
                    add.genes=FALSE,samp_name=samp,seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=25, axis.space.right=25, axis.cex=0.5,
                    snv.interval=400,pga.cap=TRUE,add.normal=FALSE)

samp = "CPCG0256"
# inputs <- prep.tree(samp=samp,trees=trees,pga=pga,axis.type=type,normal.included=FALSE,pga.percent=TRUE)
inputs <- prep.tree(samp=samp,trees=trees,pga=pga.new,axis.type="PGA",normal.included=FALSE,pga.percent=TRUE)
out <- draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename='tree_6.pdf',scale2=0.5/362,wid=20,extra.len=20,len=10,
                    scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, add.labels=FALSE, add.circle=TRUE, offset=1.38,axis.type="PGA",rad=3.2,gene.cex=1.22,
                    add.genes=FALSE,samp_name=samp,seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=12.5, axis.space.right=12.5, axis.cex=0.5,
                    snv.interval=400,pga.cap=TRUE,add.normal=FALSE)

out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename='tree_7.pdf',scale2=0.5/362,wid=20,extra.len=20,len=10,
                    scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, add.labels=FALSE, add.circle=FALSE, offset=1.38,axis.type="PGA",rad=3.2,gene.cex=1.22,
                    add.genes=TRUE,samp_name=samp,seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=12.5, axis.space.right=12.5, axis.cex=0.5,
                    snv.interval=400,pga.cap=TRUE)

pdf("tree_test.pdf")
grid.newpage()
pushViewport(viewport(height=unit(2,"in"),width=unit(2,"in")))
print(out[[3]])
dev.off()

samp = "CPCG0196"
# inputs <- prep.tree(samp=samp,trees=trees,pga=pga,axis.type=type,normal.included=FALSE,pga.percent=TRUE)
inputs <- prep.tree(samp=samp,trees=trees,pga=pga.new,axis.type="PGA",normal.included=FALSE,pga.percent=TRUE)
out <- draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename='tree_8.pdf',scale2=0.5/362,wid=60,extra.len=20,len=10,
                    scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, add.labels=FALSE, add.circle=FALSE, offset=1.38,y.wid=15,maxwid=60,axis.type="both",rad=3.2,gene.cex=1.22,
                    add.genes=TRUE,samp_name=samp,seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=25, axis.space.right=25, axis.cex=0.5,
                    snv.interval=400,pga.cap=TRUE,add.normal=FALSE)




out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename='tree_new.pdf',scale2=0.5/1800,wid=60,extra.len=20,len=10,
                    scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.38,y.wid=15,maxwid=60,axis.type=inputs$axis.type,rad=3.2,gene.cex=1.22,
                    add.genes=inputs$add.genes,samp_name=samp,seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=2, axis.space.right=2, axis.cex=0.8,
                    snv.interval=1000,pga.cap=TRUE)


pdf("tree_orig.pdf")
xyplot(x~y,data=data.frame(x=1,y=1), 
       ylim=c(59.8562799807669,-2),
       xlim=c(-9.5,9.5),type="n", 
       xlab='', ylab.right=list(label='',cex=0),
       ylab=list(label='PGA',cex=1.55),
       scales=list(cex=1.2,col=1,x=list(at=c(-ymax/2,ymax/2),
       labels=c(0,paste0(round(max(v$ccf)*100,0),'%'))),
       y=list(alternating= 1,labels=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
      at=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))),
      par.settings = list(axis.line = list(col = 0),
      par.main.text=list(font=1,just="center",x=grid::unit(1.73666666666667,"inches"),y=grid::unit(0.3,"in"),cex=1.7)),
      main = samp_name, cex.lab=3 ,panel=panel.func)
  dev.off()


source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/adjust_tree.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.calc.clone.coords.R')


# inputs <- prep.smchet(out_3A="3A.txt", samp.name="test",branch.length=7)
inputs <- prep.smchet(out_3A="/u/asalcedo/DREAM/HET/DATA/Challenge/scoring_trees/3clust_lin.txt", samp.name="test",branch.length=7)
samp <- ""
out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename='tree_orig5.pdf',scale2=0.5/1800,wid=20,extra.len=10,len=5,
                    scale.x.real = 1/30, sig.shape=2.5, w.padding = 0.1, h.padding=0.2, add.bells=FALSE, add.labels=TRUE, add.circle=TRUE, offset=1.38,axis.type="none",rad=3.2,gene.cex=1.22,
                    add.genes=FALSE,samp_name=samp,seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=0.25,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=15, axis.space.right=15, axis.cex=0.5,
                    snv.interval=1000,pga.cap=TRUE)

inputs <- prep.smchet(out_3A="/u/asalcedo/DREAM/HET/DATA/Challenge/scoring_trees/6clust_orig_truth.txt", samp.name="test",branch.length=7)

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/plot.ellipses.R')
out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename='6_clust_truth.pdf',scale2=0.5/1800,wid=20,extra.len=10,len=5,
                    scale.x.real = 1/30, sig.shape=2.5, w.padding = 1, h.padding=0.2, add.bells=FALSE, add.labels=TRUE, add.circle=TRUE, offset=1.38,axis.type="none",rad=3.2,gene.cex=1.22,
                    add.genes=FALSE,samp_name="truth",seg1.col='grey65',seg2.col='gold',circle.col="grey20",lin.width=0.25,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=15, axis.space.right=15, axis.cex=0.5,
                    snv.interval=1000,pga.cap=TRUE)

inputs <- prep.smchet(out_3A="/u/asalcedo/DREAM/HET/DATA/Challenge/scoring_trees/6clust_mergedClustMidBotOneChild.txt", samp.name="test",branch.length=7)
out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename='6_clust_merged.pdf',scale2=0.5/1800,wid=30,extra.len=10,len=5,
                    scale.x.real = 1/30, sig.shape=2.5, w.padding = 0.1, h.padding=0.2, add.bells=FALSE, add.labels=TRUE, add.circle=TRUE, offset=1.38,axis.type="none",rad=3.2,gene.cex=1.22,
                    add.genes=FALSE,samp_name="truth",seg1.col='grey65',seg2.col='gold',circle.col="grey20",lin.width=0.25,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=15, axis.space.right=15, axis.cex=0.5,
                    snv.interval=1000,pga.cap=TRUE)
