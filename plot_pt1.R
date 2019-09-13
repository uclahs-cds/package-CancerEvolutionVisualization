genes <- read.table("~/isilon/private/Collaborators/BrendaGallie/Retinoblastoma/Rb1++/WGS/phyloWGS_bb_recSNV2/Pt1_fig_cancer_genes.txt",header=TRUE,stringsAsFactors =FALSE,sep="\t")
trees <- read.table("~/isilon/private/Collaborators/BrendaGallie/Retinoblastoma/Rb1++/WGS/phyloWGS_bb_recSNV2/Pt1/consensus_tree.txt",header=TRUE,stringsAsFactors =FALSE)
pga <- read.table("~/isilon/private/Collaborators/BrendaGallie/Retinoblastoma/Rb1++/WGS/phyloWGS_bb_recSNV2/node_pgas_mets.txt",header=TRUE,stringsAsFactors =FALSE)
colours <- c(NA, '#999793', '#8d4891', '#f8e356' ,"#fe9536","#0E68A0","#0ad379")

names(genes)[c(1:3)] <- c("gene","cn","node")
genes <- genes[,c(1:3)]


tree.2271.df <- trees
tree.2271.df <- tree.2271.df[,c(2:ncol(tree.2271.df))]
tree.2271.df$parent[tree.2271.df$parent ==0] <- -1

tree.prim.df <- tree.2271.df
tree.met.df <- tree.2271.df
tree.prim.df$cellular_prevalence <- as.numeric(tree.prim.df$cp1)
tree.met.df$cellular_prevalence <- as.numeric(tree.prim.df$cp2)


pga.2271.df <- pga[pga$Sample=='Pt1',]
pga.2271.df$Node[pga.2271.df$Node ==0] <- -1

bells = TRUE
in.prim.df <- data.frame(lab=c(-1,tree.prim.df$child),  
                         ccf=as.numeric(c(1,tree.prim.df$cellular_prevalence)),
                         color= colours[1:nrow(pga.2271.df)],
                         parent = as.numeric(c(NA,tree.prim.df$parent)),
                         excluded = c(TRUE,rep(FALSE,nrow(tree.prim.df))), 
                         bell = c(FALSE,rep(bells,nrow(tree.prim.df))), 
                         alpha =  rep(0.5,nrow(pga.2271.df)),stringsAsFactors = FALSE)



in.met.df <- data.frame(lab=c(-1,tree.met.df$child),  
                         ccf=as.numeric(c(1,tree.met.df$cellular_prevalence)),
                         color= colours[1:nrow(pga.2271.df)],
                         parent = as.numeric(c(NA,tree.met.df$parent)),
                         excluded = c(TRUE,rep(FALSE,nrow(tree.met.df))), 
                         bell = c(FALSE,rep(bells,nrow(tree.met.df))), 
                         alpha =  rep(0.5,nrow(pga.2271.df)),stringsAsFactors = FALSE)


my.prim.tree <- data.frame(parent=as.numeric(tree.prim.df$parent),tip = as.numeric(tree.prim.df$child), length1=(pga.2271.df$PGA[-1]*100), length2=as.numeric(tree.prim.df$num_ssms),stringsAsFactors = FALSE)
my.met.tree <- my.prim.tree

in.prim.df <- in.prim.df[in.prim.df$ccf>0.05,]
in.met.df <- in.met.df[in.met.df$ccf>0.05,]

my.prim.tree <- my.prim.tree[which(my.prim.tree$parent %in% in.prim.df$lab & my.prim.tree$tip %in% in.prim.df$lab),]
my.met.tree <- my.met.tree[which(my.met.tree$parent %in% in.met.df$lab & my.met.tree$tip %in% in.met.df$lab),]

genes.prim <- genes[genes$node %in% in.prim.df$lab,]
genes.met <- genes[genes$node %in% in.met.df$lab,]

#renumber mets
# in.met.df$lab[in.met.df$lab==4] <- 3
# in.met.df$parent[in.met.df$parent==4] <- 3
# 
# in.met.df$lab[in.met.df$lab==6] <- 4
# in.met.df$parent[in.met.df$parent==6] <- 4
# 
# my.met.tree$parent[my.met.tree$parent == 4] <- 3
# my.met.tree$tip[my.met.tree$tip == 4] <- 3
# my.met.tree$tip[my.met.tree$tip == 6] <- 4
# 
# genes.met$node[genes.met$node ==4 ] <- 3
# genes.met$node[genes.met$node ==6 ] <- 4
# 

#prim
draw.my.tree(in.tree.df=in.prim.df,tree=my.prim.tree,genes.df= genes.prim,filename="Pt1_prim.pdf",scale2=0.5/362,wid=60,extra.len=20,len=10,
             scale.x.real = 1/30, sig.shape=2.5, w.padding = 1, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type="PGA",rad=3.2,gene.cex=1.22,
             add.genes=TRUE,samp_name="Primary",circle.col="grey20",lin.width=0.5,curve=3.2,branching=TRUE,line.lwd=4,axis.space.left=20, axis.space.right=2)

draw.my.tree(in.tree.df=in.met.df,tree=my.met.tree,genes.df= genes.met,filename="Pt1_met.pdf",scale2=0.5/362,wid=60,extra.len=20,len=10,
             scale.x.real = 1/30, sig.shape=2.5, w.padding = 2, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type="SNV",rad=3.2,gene.cex=1.1,
             add.genes=TRUE,samp_name="Metastasis",circle.col="grey20",lin.width=0.5,curve=3.2,branching=FALSE,line.lwd=4,axis.space.left=10, 
             axis.space.right=17, alternate.genes= TRUE)

