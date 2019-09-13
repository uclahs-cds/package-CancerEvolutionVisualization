
trees_df <- read.table(paste0("~/isilon/private/users/asalcedo/Lydia_trees_TNM/TMHN_WGS_TSP_all_trees.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)
snvs_df <- read.table(paste0("~/isilon/private/users/asalcedo/Lydia_trees_TNM/2019-01-09_Driver_SNV_stacked.txt"),header=TRUE,stringsAsFactors =FALSE)
cnas_df <- read.table(paste0("~/isilon/private/users/asalcedo/Lydia_trees_TNM/2019-01-10_Driver_CNA_stacked.txt"),header=TRUE,stringsAsFactors =FALSE)
colours <- c(NA, '#999793', "#5884BB", "#FCF9BF","#0a610d", "#c946bd")

names(cnas_df)[2] <- 'gene'
names(cnas_df)[1] <- 'Sample'


sample=unique(trees_df[,1])[3]

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

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_segs.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/tree_tiers.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/adjust_tree.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')


source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/set_up_plot_area.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/calculate_clone_polygons.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/main.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_segs.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_nodes.R')

pdf("test.pdf")
grid.newpage()
 out <- SRCGrob(inputs$in.tree.df, inputs$tree, genes_df= inputs$genes.df,filename=filename,scale2=scale2,wid=1.2,extra_len=extra.len,
	        scale1 = scale.x.real,  w_padding = inputs$w.padding, h_padding=1, rad=0.1, gene.cex=0.85,
	        genes=inputs$add.genes, label_nodes=TRUE,seg1.col='navy',seg2.col='gold',title=inputs$samp.name,node_col="grey20",sig_curve=3,
	        title.y=0.5, add_normal=TRUE, line.lwd=4, xaxis_space_left=0.27, xaxis_space_right=0.27,yaxis1_interval=snv.interval, 
	         spread=1.1, xlabel="CP")

grid.draw(out)
dev.off()
