library(grid)
library(gridExtra)
library(gtable)
library(plyr)
library(stringr)


source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_segs.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/tree_tiers.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/tree_tiers.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/adjust_tree.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_nodes_radial.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/set_up_plot_area.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_segs.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/main.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_nodes.R')



path = "~/Shad_scoring/smc_het_eval/tree_permutations/"
# pattern = "unique_.*clust_[0-9].*.txt"
pattern = "unique_4clust_[0].*.txt"

files = list.files(path, full.names=TRUE,pattern=pattern)
files_2A <- files[grep("_2A", files)]
files_3A <- files[grep("_3A", files)]


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


i = 5
file_2A = files_2A[i]
file_3A = files_3A[i]
truth_2A = paste0( str_extract(file_2A, ".*_\\d+_"), "truth_2A.txt")
outname = names[i]

print(file_2A)
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
inputs <- prep.smchet(out_3A=file_3A,  out_2A=c(truth_2A,file_2A), samp.name="test", col=col)

extra.len <- 17
scale.x.real <- 1/2000

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_nodes.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_segs.R')

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/calculate_clone_polygons.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/set_up_plot_area.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/main.R')
out <- SRCGrob(inputs$in.tree.df, inputs$tree, genes_df= inputs$genes.df,filename=filename,scale2=scale2*2,wid=1.2,extra_len=extra.len,
	        scale1 = scale.x.real*2,  w_padding = inputs$w.padding, h_padding=1, rad=0.1, gene.cex=0.85, fixed_angle=NULL,
	        add_genes=inputs$add.genes, genes_on_nodes=TRUE, seg1.col='navy', seg2.col='gold',  node_col="grey40", sig_curve=3,
	         add_polygons=FALSE, add_normal=TRUE, line.lwd=4, xaxis_space_left=0.47, xaxis_space_right=0.47, yaxis1_interval=200, min_width=2,
	        yaxis_position="none", yaxis1_label="SNV",yaxis2_label="", yaxis2_interval=1, cluster_list=inputs$cluster_list,
	         spread=1.1, xaxis_label="CP", title=inputs$samp.name, title.cex=1, title.y=0.3, title.y.units="inches")


pdf("Rplots.pdf", height=15)
grid.newpage()
grid.draw(out[[1]])
dev.off()


