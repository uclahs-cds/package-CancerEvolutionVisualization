library(plyr)
library(lattice)
library(latticeExtra)
library(grid)
library(gridExtra)
library(stringr)
library(gridBase)


get_casename <- function(x){
	if(grepl("split_bottom", x)){
		return("Split Bottom Node")
	} else if(grepl("merged_bottom", x)){
		return("Merge Bottom Nodes")
	} else if(grepl("merged_top", x)){
		return("Merge Top Nodes")
	} else if(grepl("extra_intermediate", x) | grepl("extra_bottom", x)){
		return("Spurious Intermediate Node")
	} else if(grepl("extra_merged", x)){
		return(expression("Merge Top Nodes \n + Spurious Node"))
	} else if(grepl("pig", x)){
		return("Parent is Grandparent")
	} else if(grepl("sip", x)){
		return("Sibling is Parent")
	}else if(grepl("linear", x)){
		return("Linear")
	} else{
		return("Truth: 1200 SSMs")
	} 
}


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
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_nodes_radial.R')


# args=commandArgs(trailingOnly = TRUE)
# path = args[1]
# pattern = args[2]


pattern = "unique_6clust_.*_split_bottom.csv"
pattern = "unique_6clust_.*_merged_bottom.txt"
pattern = "unique_2clust_.*_ottom.txt"

path = "~/Shad_scoring/smc_het_eval/tree_permutations/"
# pattern = "unique_.*clust_[0-9].*.txt"
pattern = "unique_4clust_[0].*.txt"

files = list.files(path, full.names=TRUE,pattern=pattern)
files_2A <- files[grep("_2A", files)]
files_3A <- files[grep("_3A", files)]


# print(args)
# print(files)

names = sapply(files_2A, function(x) gsub("_2A.txt", ".pdf", x))
names = sapply(names, function(x) gsub(".*/", "", x))

col <- c("#969492",
"#6f5883",
"#769f79",
"#b362a2",
"#005300",
"#549eb3",
"#805a34",
"#275067",
"#217676",
"#db794a")

for (i in seq_along(files_2A)){
	i = 5
	file_2A = files_2A[i]
	file_3A = files_3A[i]
	truth_2A = paste0( str_extract(file_2A, ".*_\\d+_"), "truth_2A.txt")
	outname = names[i]

	print(file_2A)
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
	inputs <- prep.smchet(out_3A=file_3A,  out_2A=c(truth_2A,file_2A), samp.name="test", col=col)
	samp <- get_casename(outname)


	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/adjust_tree.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')

	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/plot.ellipses.R')
	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_segs.R')

	source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
	
	out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree, cluster_list=inputs$cluster_list, genes.df= inputs$genes.df,filename=outname,scale2=0.5/1800,wid=1.2,extra.len=400,len=5,
	                    scale.x.real = 1/2000, sig.shape=2.5, w.padding = 0.1, h.padding=0.5, add.bells=FALSE, add.labels=FALSE, add.circle=TRUE, offset=1.38,axis.type="none",rad=0.15,gene.cex=0.9,
	                    add.genes=TRUE,samp_name=samp,seg1.col='black',seg2.col='gold',circle.col="grey20",lin.width=1.2,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=0.55, 
	                    axis.space.right=0.55, axis.cex=0.5, snv.interval=1000,pga.cap=TRUE,title.cex=1, title.y = 0.1, fixed_angle=pi/6, min_width=3,spread=FALSE,line.dist=0.05)

}


files <- list.files("~/Shad_scoring/smc_het_eval/", full.names=TRUE,pattern="unique_.*clust_ssm")

files_2A <- files[grep("_2A", files)]
files_1C <- files[grep("_1C", files)]
files_3A <- files[grep("_3A", files)]

# names = sapply(files_3A, function(x) gsub(".txt", ".png", x))
names = sapply(files_3A, function(x) gsub("_3A.txt", ".png", x))
names = sapply(names, function(x) gsub(".*/", "", x))


for (i in seq_along(files_3A)){

		file_3A <- files_3A[i]
		file_2A <- files_2A[i]
		truth_2A = paste0( str_extract(file_2A, ".*_\\d+_"), "truth_2A.txt")

		outname  <- names[i]
		samp <- get_casename(outname)
		# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
		inputs <- prep.smchet(out_3A = file_3A, out_2A = c(truth_2A,file_2A), samp.name = "test", col=col)


	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
		# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
		out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree, cluster_list=inputs$cluster_list, genes.df= inputs$genes.df,filename=outname,scale2=0.5/1800,wid=1.2,extra.len=400,len=5,
	                    scale.x.real = 1/2000, sig.shape=2.5, w.padding = 0.1, h.padding=0.5, add.bells=FALSE, add.labels=FALSE, add.circle=TRUE, offset=1.38,axis.type="none",rad=0.15,gene.cex=0.9,
	                    add.genes=TRUE,samp_name=samp,seg1.col='black',seg2.col='gold',circle.col="grey20",lin.width=1.2,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=0.55, 
	                    axis.space.right=0.55, axis.cex=0.5, snv.interval=1000,pga.cap=TRUE,title.cex=1, title.y = 0.1, fixed_angle=pi/6, min_width=3,spread=FALSE,line.dist=0.05)
	}


files <- list.files("~/Shad_scoring/smc_het_eval/", full.names=TRUE,pattern="monoclonal")

files_2A <- files[grep("_2A", files)]
files_1C <- files[grep("_1C", files)]
files_3A <- files[grep("_3A", files)]

names = sapply(files_3A, function(x) gsub(".txt", ".png", x))
names = sapply(names, function(x) gsub(".*/", "", x))


for (i in seq_along(files_3A)){

		file_3A <- files_3A[i]
		file_2A <- files_2A[i]
		truth_2A = paste0( str_extract(file_2A, ".*monoclonal_"), "truth_2A.txt")
	
		outname  <- names[i]
		samp <- get_casename(outname)
		# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
		inputs <- prep.smchet(out_3A = file_3A, out_2A = c(truth_2A,file_2A), samp.name = "test", col=col)
		# inputs <- prep.smchet(out_3A = file_3A, out_1C= file_1C, samp.name = "test")


	# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
		# source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
		out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree, cluster_list=inputs$cluster_list, genes.df= inputs$genes.df,filename=outname,scale2=0.5/1800,wid=1.2,extra.len=400,len=5,
		                    scale.x.real = 1/2000, sig.shape=2.5, w.padding = 0.1, h.padding=0.5, add.bells=FALSE, add.labels=FALSE, add.circle=TRUE, offset=1.38,axis.type="none",rad=0.15,gene.cex=0.9,
		                    add.genes=TRUE,samp_name=samp,seg1.col='black',seg2.col='gold',circle.col="grey20",lin.width=1.2,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=0.55, 
		                    axis.space.right=0.55, axis.cex=0.5, snv.interval=1000,pga.cap=TRUE,title.cex=1, title.y = 0.1, fixed_angle=pi/6, min_width=3,spread=FALSE,line.dist=0.05)

	}
