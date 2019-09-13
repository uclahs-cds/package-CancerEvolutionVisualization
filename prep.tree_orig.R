library(plyr)
library(lattice)
library(latticeExtra)
library(grid)


genes <- read.table("~/isilon/private/Collaborators/BrendaGallie/Retinoblastoma/Rb1++/WGS/phyloWGS_bb_recSNV2/cnv_per_node_cnv_no_met_loh3.txt",header=TRUE,stringsAsFactors =FALSE)
trees <- read.table("~/isilon/private/Collaborators/BrendaGallie/Retinoblastoma/Rb1++/WGS/phyloWGS_bb_recSNV2/all_trees4.txt",header=FALSE,stringsAsFactors =FALSE)
pga <- read.table("~/isilon/private/Collaborators/BrendaGallie/Retinoblastoma/Rb1++/WGS/phyloWGS_bb_recSNV2/node_pgas_all4.txt",header=TRUE,stringsAsFactors =FALSE)
colours <- c(NA, '#999793', '#8d4891', '#f8e356' ,"#fe9536")
names(genes)[3] <- "cn"


tree.2271.df <- trees[trees$V1 == "ATC-2271",]
colnames(tree.2271.df) <- tree.2271.df[1,]
colnames(tree.2271.df)[1] <- "Sample"
tree.2271.df <- tree.2271.df[c(2:3),c(2:ncol(tree.2271.df))]
tree.2271.df$parent[tree.2271.df$parent ==0] <- -1

pga.2271.df <- pga[pga$Sample=="ATC-2271",]
pga.2271.df$Node[pga.2271.df$Node ==0] <- -1


in.2271.df <- data.frame(lab=pga.2271.df$Node,  
                         ccf=pga.2271.df$CP,
                         color= colours[1:nrow(pga.2271.df)],
                         parent = as.numeric(c(NA,tree.2271.df$parent)),
                         excluded = c(TRUE,rep(FALSE,nrow(tree.2271.df))), 
                         bell = c(FALSE,rep(TRUE,nrow(tree.2271.df))), 
                         alpha =  rep(0.5,nrow(pga.2271.df)),stringsAsFactors = FALSE)

my.2271.tree <- data.frame(parent=as.numeric(tree.2271.df$parent),tip = as.numeric(tree.2271.df$child), length1=(pga.2271.df$PGA[-1]*100), length2=as.numeric(tree.2271.df$num_ssms),stringsAsFactors = FALSE)

genes.2271 <- genes[genes$Sample =="ATC-2271",c(2:4)]
genes.2271 <- genes.2271[which(!is.na(genes.2271$cn)),]

clone.out <- draw.my.tree(in.tree.df=in.2271.df,tree=my.2271.tree,genes.df=genes.2271,filename="~/visevol_test_2271.pdf",scale2=0.5/362,wid=60,extra.len=20,len=55,
                          scale.x.real = 1/30, sig.shape=2.5, w.padding = 1, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type="PGA",rad=3.2,gene.cex=1.22,
                          add.genes=TRUE,samp_name="RB-2271",circle.col="grey20",lin.width=0.5,curve=2,branching=FALSE)





genes <- read.table("~/isilon/private/Collaborators/BrendaGallie/Retinoblastoma/Rb1++/WGS/phyloWGS_bb_recSNV2/cnv_per_node_cnv_no_met_loh3.txt",header=TRUE,stringsAsFactors =FALSE)
trees <- read.table("~/isilon/private/Collaborators/BrendaGallie/Retinoblastoma/Rb1++/WGS/phyloWGS_bb_recSNV2/all_trees4.txt",header=FALSE,stringsAsFactors =FALSE)
pga <- read.table("~/isilon/private/Collaborators/BrendaGallie/Retinoblastoma/Rb1++/WGS/phyloWGS_bb_recSNV2/node_pgas_all4.txt",header=TRUE,stringsAsFactors =FALSE)
colours <- c(NA, '#999793', '#8d4891', '#f8e356' ,"#fe9536","#0E68A0")
names(genes)[3] <- "cn"

setwd("Trees2")


samp= "ATC-6145"

reorder_clones <- function(in.df){
  new.df <- in.df
  new.df$new.lab <- order(-as.numeric(new.df$cellular_prevalence))
#  new.df$new.lab[new.df$new.lab == 0 ] <- -1
  new.df$new.par <- sapply(new.df$parent, function(x){ return(new.df$new.lab[new.df$child==x])})
  in.df$child <- new.df$new.lab
  in.df$parent <- new.df$new.par
  in.df$parent[in.df$child == 1] <- -1
  return(in.df)
}


for (samp in unique(genes$Sample)){
  
}

prep.tree <- function(samp=NULL,bells=TRUE){
    tree.2271.df <- trees[trees$V1 == samp,]
    colnames(tree.2271.df) <- tree.2271.df[1,]
    colnames(tree.2271.df)[1] <- "Sample"
    tree.2271.df <- tree.2271.df[c(2:nrow(tree.2271.df)),c(2:ncol(tree.2271.df))]
    tree.2271.df$parent[tree.2271.df$parent ==0] <- -1
    tree.2271.df$cellular_prevalence <- as.numeric(tree.2271.df$cellular_prevalence)
    
    pga.2271.df <- pga[pga$Sample==samp,]
    pga.2271.df$Node[pga.2271.df$Node ==0] <- -1
    
    tree.2271.df <- reorder_clones(tree.2271.df)
    
    in.2271.df <- data.frame(lab=c(-1,tree.2271.df$child),  
                             ccf=as.numeric(c(1,tree.2271.df$cellular_prevalence)),
                             color= colours[1:nrow(pga.2271.df)],
                             parent = as.numeric(c(NA,tree.2271.df$parent)),
                             excluded = c(TRUE,rep(FALSE,nrow(tree.2271.df))), 
                             bell = c(FALSE,rep(bells,nrow(tree.2271.df))), 
                             alpha =  rep(0.5,nrow(pga.2271.df)),stringsAsFactors = FALSE)
    
    
    pga.2271.df$Node <- order(-pga.2271.df$CP)-1
    pga.2271.df$Node[pga.2271.df$Node==0] <- -1
    my.2271.tree <- data.frame(parent=as.numeric(tree.2271.df$parent),tip = as.numeric(tree.2271.df$child), length1=(pga.2271.df$PGA[-1]*100), length2=as.numeric(tree.2271.df$num_ssms),stringsAsFactors = FALSE)
    
    genes.2271 <- genes[genes$Sample ==samp,c(2:4)]
    genes.2271 <- genes.2271[which(!is.na(genes.2271$cn)),]
    
    samp_num <- gsub("ATC-","", samp )
    out.name <- paste0("visevol_",samp_num,"_",axis.type, ".pdf")
    samp.name<- paste0("RB-",samp_num)
    add.genes <- ifelse(nrow(genes.2271)==0,FALSE,TRUE)
    w.padding=1
    if(axis.type =="none"){
      w.padding = 0.85
    }
    branching <- ifelse(any(duplicated(my.2271.tree$parent)== TRUE),TRUE,FALSE)
    return(list(in.tree.df = in.2271.df,tree = my.2271.tree, genes.df = genes.2271, out.name = out.name, w.padding=w.padding, samp.name = samp.name, branching=branching,add.genes=add.genes  ))
  }
  
axis.type="PGA"
for (samp in unique(genes$Sample)){
  inputs <- prep.tree(samp)
  samp_num <- gsub("ATC-","", samp )
  out.name <- paste0("visevol_",samp_num,"_",axis.type, "_mycn.pdf")
  
  
  clone.out <- draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
                            scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
                            add.genes=inputs$add.genes,samp_name=inputs$samp.name,circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)
  
  
}



  inputs <- prep.tree("ATC-6145")
  
  clone.out <- draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
                            scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
                            add.genes=add.genes,samp_name=inputs$samp.name,circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)
  
  

  draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename="6145_no_text.pdf",scale2=0.5/362,wid=60,extra.len=20,len=10,
               scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
               add.genes=FALSE,samp_name=inputs$samp.name,circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)
  
  
  inputs <- prep.tree("ATC-6145",bells=FALSE)
  
  draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename="6145_no_bells.pdf",scale2=0.5/362,wid=60,extra.len=20,len=10,
               scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type="none",rad=3.2,gene.cex=1.22,
               add.genes=FALSE,samp_name=inputs$samp.name,circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)
  
  
  inputs <- prep.tree("ATC-6145",bells=FALSE)
  
  draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename="6145_no_bells.pdf",scale2=0.5/362,wid=60,extra.len=20,len=10,
               scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type="none",rad=3.2,gene.cex=1.22,
               add.genes=FALSE,samp_name=inputs$samp.name,circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)
  
  
  
  draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename="6145_no_bells_PGA.pdf",scale2=0.5/362,wid=60,extra.len=20,len=10,
               scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type="PGA",rad=3.2,gene.cex=1.22,
               add.genes=FALSE,samp_name='',circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)
 
   draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename="6145_no_bells_PGA_genes.pdf",scale2=0.5/362,wid=60,extra.len=20,len=10,
               scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type="PGA",rad=3.2,gene.cex=1.22,
               add.genes=TRUE,samp_name='',circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)
  
  
  draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename="6145_no_bells_SNV.pdf",scale2=0.5/362,wid=60,extra.len=20,len=10,
               scale.x.real = 1/30, sig.shape=2.5, w.padding = 0.8, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type="SNV",rad=3.2,gene.cex=1.22,
               add.genes=FALSE,samp_name='',circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)
  
  
  clone.out <- draw.my.tree(in.tree.df=in.2271.df,tree=my.2271.tree,genes.df=genes.2271,filename=out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
                            scale.x.real = 1/30, sig.shape=2.5, w.padding = w.padding, h.padding=0.75, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.,rad=3.2,gene.cex=1.22,
                            add.genes=add.genes,samp_name=samp.name,circle.col="grey20",lin.width=0.5,curve=3.2,branching=branching,line.lwd=4)
  

  axis.type="SNV"
  inputs <- prep.tree("ATC-6719")
  
  draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
               scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
               add.genes=inputs$add.genes,samp_name="RB-6719",circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)
  
  
  axis.type="none"
  inputs <- prep.tree("RA1")
  
  draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
               scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
               add.genes=inputs$add.genes,samp_name="RA1",circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)
  
  
  
  
  
  
  inputs <- prep.tree("RA1")
  
draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
                            scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
                            add.genes=add.genes,samp_name="Pt B Pr.",circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)
  
  
inputs <- prep.tree("RB6658S")

draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
             scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
             add.genes=add.genes,samp_name="Pt B Met",circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)


inputs <- prep.tree("RB5662S")

draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
             scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
             add.genes=add.genes,samp_name="Pt A Pr.",circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)


inputs <- prep.tree("KP2")

draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
             scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
             add.genes=add.genes,samp_name="Pt A Met",circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)

  
  



inputs <- prep.tree(samp)
samp_num <- gsub("ATC-","", samp )
out.name <- paste0("visevol_",samp_num,"_",axis.type, "_mycn.pdf")


clone.out <- draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
                          scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding+0.2, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
                          add.genes=inputs$add.genes,samp_name=inputs$samp.name,circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4)


  

samp= "ATC-6145"

tree.2271.df <- trees[trees$V1 == samp,]
colnames(tree.2271.df) <- tree.2271.df[1,]
colnames(tree.2271.df)[1] <- "Sample"
tree.2271.df <- tree.2271.df[c(2:nrow(tree.2271.df)),c(2:ncol(tree.2271.df))]
tree.2271.df$parent[tree.2271.df$parent ==0] <- -1
tree.2271.df$cellular_prevalence <- as.numeric(tree.2271.df$cellular_prevalence)

pga.2271.df <- pga[pga$Sample==samp,]
pga.2271.df$Node[pga.2271.df$Node ==0] <- -1

tree.2271.df <- reorder_clones(tree.2271.df)

in.2271.df <- data.frame(lab=c(-1,tree.2271.df$child),  
                         ccf=as.numeric(c(1,tree.2271.df$cellular_prevalence)),
                         color= colours[1:nrow(pga.2271.df)],
                         parent = as.numeric(c(NA,tree.2271.df$parent)),
                         excluded = c(TRUE,rep(FALSE,nrow(tree.2271.df))), 
                         bell = c(FALSE,rep(TRUE,nrow(tree.2271.df))), 
                         alpha =  rep(0.5,nrow(pga.2271.df)),stringsAsFactors = FALSE)


pga.2271.df$Node <- order(-pga.2271.df$CP)-1
pga.2271.df$Node[pga.2271.df$Node==0] <- -1
my.2271.tree <- data.frame(parent=as.numeric(tree.2271.df$parent),tip = as.numeric(tree.2271.df$child), length1=(pga.2271.df$PGA[-1]*100), length2=as.numeric(tree.2271.df$num_ssms),stringsAsFactors = FALSE)

genes.2271 <- genes[genes$Sample ==samp,c(2:4)]
genes.2271 <- genes.2271[which(!is.na(genes.2271$cn)),]

samp_num <- gsub("ATC-","", samp )
out.name <- paste0("visevol_",samp_num,"_",axis.type, ".pdf")
samp.name<- paste0("RB-",samp_num)
add.genes <- ifelse(nrow(genes.2271)==0,FALSE,TRUE)
w.padding=1
if(axis.type =="none"){
  w.padding = 0.65
}
branching <- ifelse(any(duplicated(my.2271.tree$parent)== TRUE),TRUE,FALSE)
clone.out <- draw.my.tree(in.tree.df=in.2271.df,tree=my.2271.tree,genes.df=genes.2271,filename=out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
                          scale.x.real = 1/30, sig.shape=2.5, w.padding = w.padding, h.padding=0.85, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
                          add.genes=add.genes,samp_name=samp.name,circle.col="grey20",lin.width=0.5,curve=3.2,branching=branching,line.lwd=4)
