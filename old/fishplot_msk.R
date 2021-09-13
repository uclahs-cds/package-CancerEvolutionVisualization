library(grid)
library(gridExtra)
library(BoutrosLab.plotting.general)
library(tidyr)
library(fishplot)




trees_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/max5_2/max5_trees_to_plot.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)
snvs_df <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/max5_2/max5_genes_to_plot.txt"),header=TRUE,stringsAsFactors =FALSE)
coding_snvs <- read.table(paste0("/.mounts/labs/cpcgene/scratch/tyamaguchi/SRC_FACETS/Pyclone/max5_2/max5_coding_genes_to_plot.txt"),header=TRUE,stringsAsFactors =FALSE)


trees_df <- trees_df[trees_df$V1 != "Sample",]
colnames(trees_df)[1:8] <- c("Sample", "parent","child","cellular_prevalence","cp_related","CCFN","CNP","num_ssms")
colnames(snvs_df)[c(2,3)] <- c("gene","node")

sample <- "MSK20"

samp_tree <- trees_df[trees_df$Sample== sample,]
samp_snvs <- snvs_df[snvs_df$Sample == sample,]
samp_csnvs <- coding_snvs[coding_snvs$Sample == sample,]

cum_snvs <- cumsum(samp_tree$num_ssms)    
timepoints=c(0,round(cum_snvs/max(cum_snvs)*100), 110)

frac.table <- NULL
len_t <- length(timepoints)
# frac.table <- matrix(nrow=1,ncol=len_t)
j <- 2
parents <- c()
for (i in seq_along(samp_tree$Sample)){
	clonevec <- numeric(length=len_t)
	clonevec[j:len_t] <- round(as.numeric(samp_tree$cellular_prevalence[i]),2)*100
	frac.table <- rbind(frac.table,clonevec)
	parents <- c(parents,as.numeric(samp_tree$parent[i]) )
	j <- j +1
	i <- i +1
}


fish = createFishObject(frac.table,parents,timepoints=timepoints)

fish = layoutClones(fish)

pdf(paste0(sample, ".pdf"), height=5)
fishPlot(fish,shape="spline",title.btm=sample,
             cex.title=1, vlines=timepoints[len_t-1], 
             vlab=c("","",paste(max(cum_snvs), "SNVs")))
dev.off()


frac.table = matrix(
      rbind(c( 0, 100, 100),
      c(0,50,90) ),     
      ncol=length(timepoints))

parents = c(0,1)

fish = createFishObject(frac.table,parents,timepoints=timepoints)

fish = layoutClones(fish)

pdf("Rplots.pdf")
fishPlot(fish,shape="spline",title.btm="Sample1",
             cex.title=0.5, vlines=c(0,50), 
             vlab=c("day 0","day 50", "day 100"))
dev.off()

