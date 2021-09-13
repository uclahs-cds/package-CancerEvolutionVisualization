library(plyr)
library(lattice)
library(latticeExtra)
library(grid)
library(gridExtra)
library(BoutrosLab.plotting.general)
library(tidyr)

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
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/tree_tiers.R')

colours <- c(NA, '#999793', "#5884BB", "#FCF9BF","#0a610d", "#c946bd")

trees_df <- read.table(paste0("~/isilon/private/users/asalcedo/Lydia_trees_TNM/TMHN_WGS_TSP_all_trees.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)
snvs_df <- read.table(paste0("~/isilon/private/users/asalcedo/Lydia_trees_TNM/2019-01-09_Driver_SNV_stacked.txt"),header=TRUE,stringsAsFactors =FALSE)
cnas_df <- read.table(paste0("~/isilon/private/users/asalcedo/Lydia_trees_TNM/2019-01-10_Driver_CNA_stacked.txt"),header=TRUE,stringsAsFactors =FALSE)
names(cnas_df)[2] <- 'gene'
names(cnas_df)[1] <- 'Sample'

sample_names <- read.table("wgs_tumour_names.txt", header=TRUE, as.is=TRUE)

colours <- c("#969492",
  "#5bab95",
  "#72b348",
  "#ce4fb7",
  "#545e2c",
  "#7083be",
  "#c44739",
  "#6e3155",
  "#c49449",
  "#cd7b91")


trees_df <- read.table(paste0("/.mounts/labs/boutroslab/private/Collaborators/AnthonyNichols/AnaplasticThyroid/analysis_sdp/WholeGenome/SRC/Phylogeny/PhyloWGS-TITAN/2019-10-18_combined_tree_data.txt"),header=TRUE,stringsAsFactors =FALSE, fill=TRUE)

for (sample in unique(trees_df[,1])){
	samp_tree <- trees_df[trees_df$Sample == sample,]
	samp_tree$cellular_prevalence <- as.character(samp_tree$cellular_prevalence)
	# samp_cnas <- cnas_df[cnas_df$Sample == sample,]
	# samp_snvs <- snvs_df[snvs_df$Sample ==sample,]

	samp_pga <- samp_tree[,c(1,3,11,6)]
	colnames(samp_pga) <- c('Sample','Node','PGA','CP')
	# samp_pga <- samp_pga[which(samp_pga$Node !='child'),]

	title_dist <- 0.905
	  scale.x.real <- 3/500
	  snv.interval<- 50
	  pga.interval <-10
	  extra.len <- 17   
	  spread=1
	  min_width=1.5
	  wid=2   
 # extra.len <- 1700     

    if((sum(as.numeric(samp_tree$num_ssms)) < 500)){
      scale2 <- 3/500
      snv.interval <- 50
      # title_dist <- 0.885
    }else if((sum(as.numeric(samp_tree$num_ssms)) < 1200)){
      scale2 <- 0.75/500
      snv.interval <- 250
      # title_dist <- 0.885
    }else if((sum(as.numeric(samp_tree$num_ssms)) < 5000)){
      scale2 <- 0.75/1000
      snv.interval <- 500
      # extra.len <- 900
      # title_dist <- 0.885
    }else{
      scale2 <- 0.5/3500
      snv.interval<- 2000
      # extra.len <- 0.6*(1/scale2)
    }

    scale.x.real <- 1/50
    extra.len <- 0.35*(1/scale.x.real)


  	# scale2 <- scale.x.real
	axis.type="both"

	inputs <- prep.tree(samp=sample,trees=samp_tree, cnas=NULL,axis.type=axis.type,snvs=NULL,colours=colours,pga=samp_pga,pga.percent=TRUE, normal.included = FALSE)
	# inputs <- prep.tree(samp=sample,trees=samp_tree, cnas=samp_cnas,axis.type=axis.type,snvs=samp_snvs,colours=colours,pga=samp_pga,pga.percent=TRUE, normal.included = FALSE)

	inputs$in.tree.df$c.col <- inputs$in.tree.df$color
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/plot.ellipses.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_segs.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')

	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_nodes_radial.R')

    tree <-   draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= NULL,filename="~/scrap.pdf", scale2=scale2,wid=wid, min_width=min_width, extra.len=extra.len,len=40,
                        scale.x.real = scale.x.real, sig.shape=2.5, w.padding =1, h.padding=1, add.bells=FALSE, add.labels=TRUE, add.circle=TRUE, offset=0.0467,axis.type=axis.type,rad=0.1,gene.cex=1.22,
                        add.genes=FALSE,samp_name='',seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=1,curve=3.2,branching=inputs$branching,line.lwd=4,
                        axis.space.left=0.3, axis.space.right=0.3, axis.cex=0.75, add.normal=TRUE, fixed_angle=NULL,
                        snv.interval=snv.interval, pga.interval=pga.interval,pga.cap=TRUE, spread=0.5)
        
	 tree_gTree <- editGrob(tree[[4]],vp=viewport(x=unit(0.47,"npc"), y= unit(0.5,"npc"), just=c("center","center"), height=unit(3.5,"inches"), width=unit(3.1,"inches")))
	 
	 tree_df <- samp_tree
	 # tree_df <- samp_tree[2:nrow(samp_tree),]
	 tree_df$fill <- inputs$in.tree.df$c.col[2:nrow(inputs$in.tree.df)]
	 tree_df$ccf <- as.character(tree_df$ccf)
	 if(sample %in% c("ATCWGS.33","ATCWGS.34","ATCWGS.36","ATCWGS.39")){
	 	for (i in 1:nrow(tree_df)){
	 		ccfs <- strsplit(tree_df$ccf[i],split=',')[[1]]
	 		tree_df$ccf[i] <- paste(rev(ccfs), collapse=",")
	 	}
	 }

	 source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/ccf_circles.R')
	 widths=c(4,2)
	 height=5.5
	 tumour_trees <- multiregion_ccfs(tree_in=tree_df,ccf_names=c("ATC", "DTC"), tumour_name=as.character(tree_df$Sample)[1], tree_gTree=tree_gTree,width=1.5, nrow=2, ncol=1, widths=widths, height=height)
	 
	 pdf(paste0("~/multisample_",sample,".pdf"),height=6, width=sum(widths))
	 grid.draw(tumour_trees)
	 dev.off()
}

