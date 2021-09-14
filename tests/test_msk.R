trees_df <- read.table("data/TMHN_WGS_TSP_all_trees.txt", header=FALSE, stringsAsFactors=FALSE, fill=TRUE);
snvs_df <- read.table("data/2019-01-09_Driver_SNV_stacked.txt", header=TRUE, stringsAsFactors=FALSE);
cnas_df <- read.table("data/2019-01-10_Driver_CNA_stacked.txt", header=TRUE, stringsAsFactors=FALSE);
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

scale.x.real <- 1/20
out <- SRCGrob(inputs$in.tree.df, inputs$tree, genes_df= inputs$genes.df,filename=filename,scale2=scale2,wid=2, extra_len=.1,
	        scale1 = scale.x.real,  w_padding = inputs$w.padding, h_padding=1, rad=0.1, gene.cex=0.85, fixed_angle=pi/6,
	        add_genes=TRUE,  label_nodes=TRUE, seg1.col='navy', seg2.col='gold',  node_col="grey40", sig_curve=3,
	         add_polygons=TRUE, add_normal=TRUE, line.lwd=4, xaxis_space_left=0.1, xaxis_space_right=0.1, yaxis1_interval=10, min_width=1,
	        yaxis_position="both", yaxis1_label="PGA",yaxis2_label="SNV", yaxis2_interval=1000, 
	         spread=1.1, xaxis_label="CP" ,title=inputs$samp.name, title.cex=1.55, title.y=0.3, title.y.units="inches")


pdf("test.pdf", height=15)
grid.newpage()
grid.draw(out[[1]])
dev.off()