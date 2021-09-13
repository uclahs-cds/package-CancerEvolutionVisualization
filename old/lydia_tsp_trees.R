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


sample=unique(trees_df[,1])[3]

for (sample in unique(trees_df[,1])){
	filename <- paste0(sample, "_single.pdf")
	samp_tree <- trees_df[trees_df$V1 == sample,]
	samp_cnas <- cnas_df[cnas_df$Sample == sample,]
	samp_snvs <- snvs_df[snvs_df$Sample ==sample,]

	samp_pga <- samp_tree[,c(1,3,9,6,10)]
	colnames(samp_pga) <- c('Sample','Node','PGA','CP','pipeline')
	samp_pga <- samp_pga[which(samp_pga$Node !='child'),]

	title_dist <- 0.905
	  scale.x.real <- 1/30
	  scale2 <- 0.5/1500
	  snv.interval<- 1000
	  pga.interval <-10
	  extra.len <- 17      


	 axis.type="both"

	inputs <- prep.tree(samp=sample,trees=samp_tree, cnas=samp_cnas,axis.type='both',snvs=samp_snvs,colours=colours,pga=samp_pga,pga.percent=TRUE, normal.included = FALSE)

	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')

	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')

	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')


	 out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=filename,scale2=scale2,wid=1.2,extra.len=extra.len,len=10,
	        scale.x.real = scale.x.real,  w.padding = inputs$w.padding, h.padding=1, offset=0.09,axis.type=axis.type,rad=0.1,gene.cex=1.22,
	        add.genes=inputs$add.genes,add.circle=TRUE, add.labels=TRUE,seg1.col='navy',seg2.col='gold',samp_name=inputs$samp.name,circle.col="grey20",lin.width=1,curve=3,
	        branching=inputs$branching,title.y=0.5, add.normal=TRUE, line.lwd=4,axis.space.left=0.27, axis.space.right=0.27,alternate.genes=FALSE,pga.cap=FALSE,snv.interval=snv.interval, 
	        pga.interval=pga.interval, spread=1.1)
}

trees_df <- read.table(paste0("~/isilon/private/users/asalcedo/Lydia_trees_TNM/TMHN_WGS_TSP_multiregion_all_trees.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)

for (sample in unique(trees_df[,1])){

	# sample <- trees_df[1,1]
	# filename <- paste0(sample, "_single.pdf")
	samp_tree <- trees_df[trees_df$V1 == sample,]
	samp_cnas <- cnas_df[cnas_df$Sample == sample,]
	samp_snvs <- snvs_df[snvs_df$Sample ==sample,]

	samp_pga <- samp_tree[,c(1,3,9,6,10)]
	colnames(samp_pga) <- c('Sample','Node','PGA','CP','pipeline')
	samp_pga <- samp_pga[which(samp_pga$Node !='child'),]

	title_dist <- 0.905
	  scale.x.real <- 1/30
	  scale2 <- 0.6/1500
	  snv.interval<- 1000
	  pga.interval <-10
	  extra.len <- 17   
	  spread=1.1
	  min_width=1.5
	  wid=2   

	if(sample=="WHO007"){
		scale2 <- 1/200
		  snv.interval<- 100
	}

	axis.type="both"

	inputs <- prep.tree(samp=sample,trees=samp_tree, cnas=samp_cnas,axis.type='both',snvs=samp_snvs,colours=colours,pga=samp_pga,pga.percent=TRUE, normal.included = FALSE)

	inputs$in.tree.df$c.col <- c(NA,col[1:nrow(samp_pga)])
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/plot.ellipses.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_nodes_radial.R')

	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')

	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')

      tree <-   draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= NULL,filename="scrap.pdf", scale2=scale2,wid=wid, min_width=min_width, extra.len=15,len=40,
                        scale.x.real = scale.x.real, sig.shape=2.5, w.padding =1, h.padding=1, add.bells=FALSE, add.labels=TRUE, add.circle=TRUE, offset=0.0467,axis.type="both",rad=0.1,gene.cex=1.22,
                        add.genes=FALSE,samp_name='',seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=1,curve=3.2,branching=inputs$branching,line.lwd=4,
                        axis.space.left=0.3, axis.space.right=0.3, axis.cex=0.75, add.normal=TRUE, fixed_angle=NULL,
                        snv.interval=snv.interval, pga.interval=pga.interval,pga.cap=TRUE, spread=spread)
        
	 tree_gTree <- editGrob(tree[[4]],vp=viewport(x=unit(0.47,"npc"), y= unit(0.5,"npc"), just=c("center","center"), height=unit(3.5,"inches"), width=unit(3.1,"inches")))
	 
	 tree_df <- samp_tree[2:nrow(samp_tree),]
	 colnames(tree_df) <- samp_tree[1,]
	 tree_df$fill <- inputs$in.tree.df$c.col[2:nrow(inputs$in.tree.df)]
	 source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/ccf_circles.R')

	 tumour_trees <- multiregion_ccfs(tree_in=tree_df,ccf_names=sample_names$Site[sample_names$Represent == sample], tumour_name=sample_names$Patient[sample_names$Represent ==sample][1], tree_gTree=tree_gTree,width=1.2, nrow=(nrow(tree_df)/2 + nrow(tree_df)%%2), ncol=2)
	 
	 pdf(paste0("multisample_",sample,".pdf"),height=6, width=8)
	 grid.draw(tumour_trees)
	 dev.off()

}


samp_tree <- read.table(paste0("~/Lydia_trees_tsp/consensus_tree_filtered_pga.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)
samp_cnas <- read.table(paste0("~/Lydia_trees_tsp/2019-01-29_Driver_CNA.txt"),header=TRUE,stringsAsFactors =FALSE)

samp_pga <- samp_tree[,c(1,3,9,6,10)]
colnames(samp_pga) <- c('Sample','Node','PGA','CP','pipeline')
samp_pga <- samp_pga[which(samp_pga$Node !='child'),]

title_dist <- 0.905
scale.x.real <- 1/30
scale2 <- 0.6/1500
snv.interval<- 1000
pga.interval <-10
extra.len <- 17   
spread <- 1.1
min_width=1.5
wid <- 2   
scale2 <- 0.5/200
snv.interval<- 100

axis.type <-"both"
sample <- "MBT06"

inputs <- prep.tree(samp=sample,trees=samp_tree, axis.type='both', colours=colours,pga=samp_pga,pga.percent=TRUE, normal.included = FALSE)

inputs$in.tree.df$c.col <- c(NA,col[1:nrow(samp_pga)])

# inputs$tree$length1[2] <-5
# inputs$tree$length2[2] <-15
# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_segs.R')

tree <-   draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= NULL,filename="scrap.pdf", scale2=scale2,wid=wid, min_width=min_width, extra.len=15,len=40,
                scale.x.real = scale.x.real, sig.shape=2.5, w.padding =1, h.padding=1, add.bells=FALSE, add.labels=TRUE, add.circle=TRUE, offset=0.0404,axis.type="both",rad=0.1,gene.cex=1.22,
                add.genes=FALSE,samp_name='',seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=1,curve=3.2,branching=inputs$branching,line.lwd=4,
                axis.space.left=0.3, axis.space.right=0.3, axis.cex=0.75, add.normal=TRUE, fixed_angle=NULL,
                snv.interval=snv.interval, pga.interval=pga.interval,pga.cap=TRUE, spread=spread)

tree_gTree <- editGrob(tree[[4]],vp=viewport(x=unit(0.47,"npc"), y= unit(0.5,"npc"), just=c("center","center"), height=unit(3.5,"inches"), width=unit(3.1,"inches")))

tree_df <- samp_tree[2:nrow(samp_tree),]
colnames(tree_df) <- samp_tree[1,]
tree_df$fill <- inputs$in.tree.df$c.col[2:nrow(inputs$in.tree.df)]
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/ccf_circles.R')

tumour_trees <- multiregion_ccfs(tree_in=tree_df,ccf_names=c("Primary","Recurrent"), tumour_name=sample, tree_gTree=tree_gTree,width=1.2, nrow=1, ncol=2)
# tumour_trees <- multiregion_ccfs(tree_in=tree_df,ccf_names=sample_names$Site[sample_names$Represent == sample], tumour_name=sample_names$Patient[sample_names$Represent ==sample][1], tree_gTree=tree_gTree,width=1.2, nrow=(nrow(tree_df)/2 + nrow(tree_df)%%2), ncol=2)

pdf(paste0("multisample_",sample,".pdf"),height=6, width=8)
grid.draw(tumour_trees)
dev.off()
