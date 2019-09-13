library(plyr)
library(lattice)
library(latticeExtra)
library(grid)
library(gridExtra)
library(BoutrosLab.plotting.general)
library(tidyr)

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_segs.R')
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
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/tree_tiers.R')
colours <- c(NA, '#999793', "#5884BB", "#FCF9BF","#0a610d", "#c946bd")

trees_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/max5/trees_to_plot.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)
snvs_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/max5/genes_to_plot.txt"),header=TRUE,stringsAsFactors =FALSE)

names(cnas_df)[1] <- 'Sample'
names(cnas_df)[2] <- 'gene'


col <- c("#969492",
  "#5bab95",
  "#72b348",
  "#ce4fb7",
  "#545e2c",
  "#7083be",
  "#c44739",
  "#6e3155",
  "#c49449",
  "#cd7b91")


sample = unique(trees_df[,1])[3]

for (sample in unique(trees_df[trees_df[,1] != "Sample",1])){
	filename <- paste0(sample, "_sampled.pdf")
	samp_tree <- trees_df[trees_df$V1 == sample,]
	samp_snvs <- snvs_df[snvs_df$Sample ==sample,]

	colnames(samp_tree) <- c("Sample", "parent","child","cellular_prevalence","cp_related","CCFN","CNP","num_ssms")
	colnames(samp_snvs)[c(2,3)] <- c("gene","node")

	title_dist <- 0.905
	scale.x.real <- 1/30
	scale2 <- 0.5/1500
	scale.x.real <- scale2
	snv.interval<- 1000
	pga.interval <-10
	extra.len <- 1700     


	 axis.type="SNV"

	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
	inputs <- prep.tree(samp=sample,trees=samp_tree, axis.type='SNV',snvs=samp_snvs,colours=colours, normal.included = FALSE)

	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')

	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
	out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=filename,scale2=scale2,wid=1.2,extra.len=extra.len,len=10,
	        scale.x.real = scale.x.real,  w.padding = inputs$w.padding, h.padding=1, offset=0.09,axis.type="SNV single",rad=0.1,gene.cex=0.85,
	        add.genes=inputs$add.genes,add.circle=TRUE, add.labels=TRUE,seg1.col='navy',seg2.col='gold',samp_name=inputs$samp.name,circle.col="grey20",lin.width=1,curve=3,
	        branching=inputs$branching,title.y=0.5, add.normal=TRUE, line.lwd=4,axis.space.left=0.27, axis.space.right=0.27,alternate.genes=FALSE,pga.cap=FALSE,snv.interval=snv.interval, 
	        pga.interval=pga.interval, spread=1.1,xlab="CP")

}
