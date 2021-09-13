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

trees_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/half_2/half_trees_to_plot.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)
snvs_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/half_2/half_genes_to_plot.txt"),header=TRUE,stringsAsFactors =FALSE)

names(snvs_df)[1] <- 'Sample'
names(snvs_df)[2] <- 'gene'


for (sample in unique(trees_df[trees_df[,1] != "Sample",1])){
	filename <- paste0(sample, "_sampled.pdf")
	samp_tree <- trees_df[trees_df$V1 == sample,]
	samp_snvs <- snvs_df[snvs_df$Sample ==sample,]

	colnames(samp_tree) <- c("Sample", "parent","child","cellular_prevalence","cp_related","CCFN","CNP","num_ssms")
	colnames(samp_snvs)[c(2,3)] <- c("gene","node")

	title_dist <- 0.905
	extra.len <- 1700     

	if((sum(as.numeric(samp_tree$num_ssms)) < 100)){
	  scale2 <- 0.5/200
	  snv.interval <- 5
	  # title_dist <- 0.885
	}else if((sum(as.numeric(samp_tree$num_ssms)) < 1200)){
	  scale2 <- 0.5/500
	  snv.interval <- 250
	  # title_dist <- 0.885
	}else if((sum(as.numeric(samp_tree$num_ssms)) < 2500)){
	  scale2 <- 0.5/750
	  snv.interval <- 500
	  # extra.len <- 900
	  # title_dist <- 0.885
	}else{
		scale2 <- 0.5/1500
		scale.x.real <- scale2
		snv.interval<- 1000
		# extra.len <- 0.6*(1/scale2)
	}
	extra.len <- 0.6*(1/scale2)
	scale.x.real <- scale2

	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
	inputs <- prep.tree(samp=sample,trees=samp_tree,CF_col="CCFN", axis.type='SNV',snvs=samp_snvs,colours=colours, normal.included = FALSE)

	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')

	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
	out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=filename,scale2=scale2,wid=1.2,extra.len=extra.len,len=10,
	        scale.x.real = scale.x.real,  w.padding = inputs$w.padding, h.padding=1, offset=0.09,axis.type="SNV single",rad=0.1,gene.cex=0.85,
	        add.genes=inputs$add.genes,add.circle=TRUE, add.labels=TRUE,seg1.col='navy',seg2.col='gold',samp_name=inputs$samp.name,circle.col="grey20",lin.width=1,curve=3,
	        branching=inputs$branching,title.y=0.5, add.normal=TRUE, line.lwd=4,axis.space.left=0.27, axis.space.right=0.27,alternate.genes=FALSE,pga.cap=FALSE,snv.interval=snv.interval, 
	        pga.interval=pga.interval, spread=1.1,xlab="CP")

	}



trees_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/max5_2/max5_trees_to_plot.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)
snvs_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/max5_2/max5_genes_to_plot.txt"),header=TRUE,stringsAsFactors =FALSE)
coding_snvs <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/max5_2/max5_coding_genes_to_plot.txt"),header=TRUE,stringsAsFactors =FALSE)

coding_snvs$func <- sapply(coding_snvs$Function, function(x) {
																if(grepl("missense_variant",x)){
																	return("missense_variant")
																}
																if(grepl("stop_gained",x)){
																	return("stop_gained")
																}
																if(grepl("splice_region",x)){
																	return("splice_region")
																}
																if(grepl("3_prime_UTR_variant",x)){
																	return("3_prime_UTR_variant")
																}if(grepl("upstream_gene_variant",x)){
																	return("upstream_gene_variant")
																}if(grepl("downstream_gene_variant",x)){
																	return("downstream_gene_variant")
																}
																if(grepl("\\|",x)){
																	return(gsub("\\|.*","",x))
																}else{
																	return(x)
																}
																})


names(snvs_df)[1] <- 'Sample'
names(snvs_df)[2] <- 'gene'


for (sample in c('Mayo_01', 'Mayo_08', 'Mayo_13', 'Mayo_16', 'Mayo_19', 'MSK49')){
	filename <- paste0(sample, "_full.pdf")
	samp_tree <- trees_df[trees_df$V1 == sample,]
	samp_snvs <- snvs_df[snvs_df$Sample == sample,]
	samp_csnvs <- coding_snvs[coding_snvs$Sample == sample,]

	colnames(samp_tree) <- c("Sample", "parent","child","cellular_prevalence","cp_related","CCFN","CNP","num_ssms")
	colnames(samp_snvs)[c(2,3)] <- c("gene","node")

	title_dist <- 0.905
	extra.len <- 1700     

	if((sum(as.numeric(samp_tree$num_ssms)) < 100)){
	  scale2 <- 0.5/200
	  snv.interval <- 5
	  # title_dist <- 0.885
	}else if((sum(as.numeric(samp_tree$num_ssms)) < 1200)){
	  scale2 <- 0.5/500
	  snv.interval <- 250
	  # title_dist <- 0.885
	}else if((sum(as.numeric(samp_tree$num_ssms)) < 2500)){
	  scale2 <- 0.5/750
	  snv.interval <- 500
	  # extra.len <- 900
	  # title_dist <- 0.885
	}else{
		scale2 <- 0.5/2000
		scale.x.real <- scale2
		snv.interval<- 1500
		# extra.len <- 0.6*(1/scale2)
	}
	extra.len <- 0.6*(1/scale2)
	scale.x.real <- scale2

	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
	inputs <- prep.tree(samp=sample,trees=samp_tree,CF_col="CCFN", axis.type='SNV',snvs=samp_snvs,colours=colours, normal.included = FALSE)
	inputs$genes.df <- adply(inputs$genes.df, 1,function(x) {if(x$gene %in% samp_csnvs$Gene){
																			y <- data.frame(gene=samp_csnvs$Gene[samp_csnvs$Gene==x$gene])
																			y$col <- recode.vector(samp_csnvs$func[samp_csnvs$Gene == y$gene], list("forestgreen"="missense_variant", "grey"="synonymous_variant", "darkorchid"='stop_gained', "slateblue"='3_prime_UTR_variant', "black"="intragenic_variant", "turqoise"=c("upstream_gene_variant", "downstream_gene_variant")))
																		}else{
																			y <- data.frame(gene=x$gene)
																			y$col <- "black"};
																			return(merge(x,y))
																			})
	# inputs$genes.df[grepl(",", genes.df$col),]
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.clone.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/adjust_tree.R')
# 
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_segs.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
	out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=filename,scale2=scale2,wid=1.2,extra.len=extra.len,len=10,
	        scale.x.real = scale.x.real,  w.padding = inputs$w.padding, h.padding=1, offset=0.09,axis.type="SNV single",rad=0.1,gene.cex=0.85,
	        add.genes=inputs$add.genes,add.circle=TRUE, add.labels=TRUE,seg1.col='navy',seg2.col='gold',samp_name=inputs$samp.name,circle.col="grey20",lin.width=1,curve=3,
	        branching=inputs$branching,title.y=0.5, add.normal=TRUE, line.lwd=4,axis.space.left=0.27, axis.space.right=0.27,alternate.genes=TRUE,pga.cap=FALSE,snv.interval=snv.interval, 
	        pga.interval=pga.interval, spread=1.1,xlab="CP")

	}


trees_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/half_2_7/half7_trees_to_plot.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)
snvs_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/half_2_7/half7_genes_to_plot.txt"),header=TRUE,stringsAsFactors =FALSE)

names(snvs_df)[1] <- 'Sample'
names(snvs_df)[2] <- 'gene'

for (sample in unique(trees_df[trees_df[,1] != "Sample",1])){
	filename <- paste0(sample, "_max7.pdf")
	samp_tree <- trees_df[trees_df$V1 == sample,]
	samp_snvs <- snvs_df[snvs_df$Sample ==sample,]

	colnames(samp_tree) <- c("Sample", "parent","child","cellular_prevalence","cp_related","CCFN","CNP","num_ssms")
	colnames(samp_snvs)[c(2,3)] <- c("gene","node")

	title_dist <- 0.905
	extra.len <- 1700     

	if((sum(as.numeric(samp_tree$num_ssms)) < 100)){
	  scale2 <- 0.5/200
	  snv.interval <- 5
	  # title_dist <- 0.885
	}else if((sum(as.numeric(samp_tree$num_ssms)) < 1200)){
	  scale2 <- 0.5/500
	  snv.interval <- 250
	  # title_dist <- 0.885
	}else if((sum(as.numeric(samp_tree$num_ssms)) < 2500)){
	  scale2 <- 0.5/750
	  snv.interval <- 500
	  # extra.len <- 900
	  # title_dist <- 0.885
	}else{
		scale2 <- 0.5/1500
		scale.x.real <- scale2
		snv.interval<- 1000
		# extra.len <- 0.6*(1/scale2)
	}
	extra.len <- 0.6*(1/scale2)
	scale.x.real <- scale2

	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
	inputs <- prep.tree(samp=sample,trees=samp_tree,CF_col="CCFN", axis.type='SNV',snvs=samp_snvs,colours=colours, normal.included = FALSE)

	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')

	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
	out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=filename,scale2=scale2,wid=1.2,extra.len=extra.len,len=10,
	        scale.x.real = scale.x.real,  w.padding = inputs$w.padding, h.padding=1, offset=0.09,axis.type="SNV single",rad=0.1,gene.cex=0.85,
	        add.genes=inputs$add.genes,add.circle=TRUE, add.labels=TRUE,seg1.col='navy',seg2.col='gold',samp_name=inputs$samp.name,circle.col="grey20",lin.width=1,curve=3,
	        branching=inputs$branching,title.y=0.5, add.normal=TRUE, line.lwd=4,axis.space.left=0.27, axis.space.right=0.27,alternate.genes=TRUE,pga.cap=FALSE,snv.interval=snv.interval, 
	        pga.interval=pga.interval, spread=1.1,xlab="CP")

	}



source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')



trees_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/max5_2/max5_trees_to_plot.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)
snvs_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/max5_2/max5_genes_to_plot.txt"),header=TRUE,stringsAsFactors =FALSE)
coding_snvs_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/max5_2/max5_genes_to_plot.txt"),header=TRUE,stringsAsFactors =FALSE)

names(snvs_df)[1] <- 'Sample'
names(snvs_df)[2] <- 'gene'

trees_df <- trees_df[trees_df$V1 != "Sample",]
colnames(trees_df)[1:8] <- c("Sample", "parent","child","cellular_prevalence","cp_related","CCFN","CNP","num_ssms")
colnames(snvs_df)[c(2,3)] <- c("gene","node")

trees_df_msk <- trees_df[grep("MSK",trees_df$Sample),]
trees_df_mayo <- trees_df[grep("Mayo",trees_df$Sample),]

msk_order <- ddply(trees_df_msk,.(Sample), summarise, sum_ssms=sum(as.numeric(num_ssms)))
msk_order <- msk_order[order(-msk_order$sum_ssms),]

mayo_order <- ddply(trees_df_mayo,.(Sample), summarise, sum_ssms=sum(as.numeric(num_ssms)))
mayo_order <- mayo_order[order(mayo_order$sum_ssms),]

for (trees_df in list(trees_df_msk, trees_df_mayo)){
i = 0
	msk_trees <- list()
	for (sample in msk_order$Sample[1:10]){
		filename <- paste0(sample, "_full2.pdf")
		samp_tree <- trees_df[trees_df$Sample == sample,]
		samp_snvs <- snvs_df[snvs_df$Sample ==sample,]

		title_dist <- 0.905
		extra.len <- 1700     

		if((sum(as.numeric(samp_tree$num_ssms)) < 100)){
		  scale2 <- 0.5/200
		  snv.interval <- 5
		  # title_dist <- 0.885
		}else if((sum(as.numeric(samp_tree$num_ssms)) < 1200)){
		  scale2 <- 0.5/500
		  snv.interval <- 250
		  # title_dist <- 0.885
		}else if((sum(as.numeric(samp_tree$num_ssms)) < 2000)){
		  scale2 <- 0.5/750
		  snv.interval <- 500
		  # extra.len <- 900
		  # title_dist <- 0.885
		}else{
			scale2 <- 0.5/4000
			scale.x.real <- scale2
			snv.interval<- 2000
			# extra.len <- 0.6*(1/scale2)
		}
		extra.len <- 0.4*(1/scale2)
		scale.x.real <- scale2

		# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
		inputs <- prep.tree(samp=sample,trees=samp_tree,CF_col="CCFN", axis.type='SNV',snvs=samp_snvs,colours=colours, normal.included = FALSE)

		# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
		# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.clone.R')
		# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
		# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')

		# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
		# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
		axis.type <- ifelse(i %% 5 == 0, "SNV single","none")
		# axis.type = "SNV single"
		# axis.type = "none"
		out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=filename,scale2=scale2,wid=1.2,extra.len=extra.len,len=10,
		        scale.x.real = scale.x.real,  w.padding = inputs$w.padding, h.padding=1, offset=0.09,axis.type=axis.type,rad=0.1,gene.cex=0.5,
		        add.genes=inputs$add.genes,add.circle=TRUE, add.labels=TRUE,seg1.col='navy',seg2.col='gold',samp_name=inputs$samp.name,circle.col="grey20",lin.width=1,curve=3,
		        branching=inputs$branching,title.y=0.5, add.normal=TRUE, line.lwd=4,axis.space.left=0.27, axis.space.right=0.27,alternate.genes=TRUE,pga.cap=FALSE,snv.interval=snv.interval, 
		         spread=1.1,xlab="")

		msk_trees[[sample]] <- out[[4]]
		 i <- i+1
		}
		# title <- "MSK Cohort"
		# titleGrob <- textGrob(title,gp=gpar(cex=1.7,fontface=2)) 
		
		test_ag <- arrangeGrob(grobs=msk_trees,
			ncol=5, nrow=2,
			# heights=unit(c(0.5,0.5),"npc"),
			# heights=unit(c(0.15,0.15,0.25,0.25),"npc"),
			# weights=unit(c(0.15,0.15,0.2,0.20,0.2),"npc"),
			layout_matrix= matrix(seq_len(20),nrow=4,ncol=5,byrow=TRUE)
			)
		
		  # pdf(paste0("~/test.pdf"),onefile=FALSE,height=10, width=8)
		  pdf(paste0("~/MSK_test.pdf"),onefile=FALSE,height=8, width=10)
		  grid.newpage()
		  pushViewport(viewport())
		  grid.rect(gp=gpar(fill="white"),draw=TRUE)
		  # upViewport()
		  grid.draw(test_ag)
		  dev.off()
}

