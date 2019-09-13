
cnas <- read.table("~/Downloads/2017-12-21_Driver_CNAs_stacked.txt",header=TRUE,stringsAsFactors =FALSE)
trees <- read.table("~/Downloads/2017-12-22_bare_trees_200PT.txt",header=FALSE,stringsAsFactors =FALSE)
#pga <- read.table("~/Downloads/2017-12-21_node_pgas_to_plot.txt",header=TRUE,stringsAsFactors =FALSE)
snvs <- read.table("~/Downloads/2017-12-21_Driver_SNVs_stacked.txt",header=TRUE,stringsAsFactors =FALSE)
colours <- c(NA, '#999793', "#0E68A0", '#f8e356' ,"#fe9536")
names(genes)[3] <- "cn"

pga.new <- trees[,c(1,3,9,6)]
colnames(pga.new) <- c('Sample','Node','PGA','CP')
pga.new <- pga.new[which(pga.new$Node !='child'),]
setwd("~/200PT")
names(cnas)[2] <- 'gene'
names(cnas)[1] <- 'Sample'
pga.new$Sample <- gsub('-','.',pga.new$Sample)
cnas$Sample <- gsub('-','.',cnas$Sample)
snvs$Sample <- gsub('-','.',snvs$Sample)
trees$V1 <- gsub('-','.',trees$V1)

prep.tree <- function(samp=NULL,trees=NULL,cnas = NULL, snvs = NULL, pga=NULL, pga.percent=FALSE, bells=TRUE){
  if(!(samp %in% trees$V1 )){return(NA)}
    tree.2271.df <- trees[trees$V1 == samp,]
 
  if(!(samp %in% cnas$Sample ) | !(samp %in% snvs$Sample)){return(NA) }
  colnames(tree.2271.df) <- tree.2271.df[1,]
  colnames(tree.2271.df)[1] <- "Sample"
  tree.2271.df <- tree.2271.df[c(2:nrow(tree.2271.df)),c(2:ncol(tree.2271.df))]
  tree.2271.df$parent[tree.2271.df$parent ==0] <- -1
  tree.2271.df$cellular_prevalence <- as.numeric(tree.2271.df$cellular_prevalence)
  if(!(samp %in% pga$Sample )){return(NA)}
  pga.2271.df <- pga[pga$Sample==samp,]
  names.pga <- colnames(pga)
  pga.2271.df <- rbind(c(samp,0,0,0,'Normal',1),pga.2271.df)
  pga.2271.df$Node <- as.numeric(pga.2271.df$Node)
  pga.2271.df$PGA <- as.numeric(pga.2271.df$PGA)
 # pga.2271.df$PGA_accumulated <- as.numeric(pga.2271.df$PGA_accumulated)
  if(pga.percent==TRUE){
    pga.2271.df$PGA <- pga.2271.df$PGA/100
#    pga.2271.df$PGA_accumulated <- pga.2271.df$PGA_accumulated/100
  }
  pga.2271.df$CP <- as.numeric(pga.2271.df$CP)
  pga.2271.df$Node[pga.2271.df$Node ==0] <- -1
  
  tree.2271.df <- reorder_clones(tree.2271.df)
  
  in.2271.df <- data.frame(lab=c(-1,tree.2271.df$child),  
                           ccf=as.numeric(c(1,tree.2271.df$cellular_prevalence)),
                           color= colours[1:nrow(pga.2271.df)],
                           parent = as.numeric(c(NA,tree.2271.df$parent)),
                           excluded = c(TRUE,rep(FALSE,nrow(tree.2271.df))), 
                           bell = c(FALSE,rep(bells,nrow(tree.2271.df))), 
                           alpha =  rep(0.5,nrow(pga.2271.df)),stringsAsFactors = FALSE)
  
  if(nrow(in.2271.df)==4){
    in.2271.df$alpha[4] <- 0.8
  }
  pga.2271.df$Node <- order(-pga.2271.df$CP)-1
  pga.2271.df$Node[pga.2271.df$Node==0] <- -1
  my.2271.tree <- data.frame(parent=as.numeric(tree.2271.df$parent),tip = as.numeric(tree.2271.df$child), length1=(pga.2271.df$PGA[-1]*100), length2=as.numeric(tree.2271.df$num_ssms),stringsAsFactors = FALSE)

  cnas.2271 <- cnas[cnas$Sample ==samp,c(2:4)]
  cnas.2271 <- cnas.2271[which(!is.na(cnas.2271$cn)),]
  snvs.2271 <- snvs[snvs$Sample ==samp,c(2:3)]
  snvs.2271$cn <- NA
  snvs.2271 <- snvs.2271[which(!is.na(snvs.2271$node)),]
  genes.2271 <- rbind(cnas.2271,snvs.2271)
  genes.2271 <- genes.2271[order(genes.2271$node,genes.2271$cn),]
  
  out.name <- paste0("SR_",samp,"_",axis.type, ".pdf")
  samp.name<- samp
  add.genes <- ifelse(nrow(genes.2271)==0,FALSE,TRUE)
  w.padding=1
  if(axis.type =="none"){
    w.padding = 0.85
  }
  branching <- ifelse(any(duplicated(my.2271.tree$parent)== TRUE),TRUE,FALSE)
  
  return(list(in.tree.df = in.2271.df,tree = my.2271.tree, genes.df = genes.2271, out.name = out.name, w.padding=w.padding, samp.name = samp.name, branching=branching,add.genes=add.genes  ))
}



inputs <- prep.tree(samp="CPCG0117",trees=trees,cnas=cnas,snvs=snvs,pga=pga.new,pga.percent=TRUE)
colours <- c(NA, '#999793', "#5884BB", "#FCF9BF")
inputs <- prep.tree(samp="CPCG0355",trees=trees,cnas=cnas,snvs=snvs,pga=pga.new,pga.percent=TRUE)
axis.type='both'
draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
             scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding+0.5, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
             add.genes=add.genes,seg1.col='navy',seg2.col='gold',samp_name=inputs$samp.name,circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=25, axis.space.right=25,alternate.genes=FALSE)

still_missing <- c()
for (sampid in unique(cnas$Sample)){
  
  for(sampid in missing){
inputs <- prep.tree(samp=sampid,trees=trees,cnas=cnas,snvs=snvs,pga=pga.new,pga.percent=TRUE)
if(!is.na(inputs)){
axis.type='both'
draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=60,extra.len=20,len=10,
             scale.x.real = 1/30, sig.shape=2.5,seg1.col='navy',seg2.col='gold', w.padding = inputs$w.padding+0.5, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
             add.genes=add.genes,samp_name=inputs$samp.name,circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=25, axis.space.right=25,alternate.genes=FALSE)
}else {still_missing <- c(still_missing,sampid)}
}


inputs <- prep.tree(samp="CPCG0117",trees=trees,cnas=cnas,snvs=snvs,pga=pga.new,pga.percent=TRUE)
colours <- c(NA, '#999793', "#5884BB", "#FCF9BF")

inputs$in.tree.df$ccf[4] <- 0.15
inputs$tree$length1 <- c(10,15,5)
inputs$tree$length2[1]<- 550
inputs$tree$length2[3]<- 200
inputs$genes.df$gene[2] <- 'MYC'
inputs$genes.df$cn[2] <- 'gain'
inputs$genes.df$node[2] <- 2
inputs$genes.df$cn[3] <- NA
inputs$genes.df$gene[3] <- 'SPOP'
inputs$genes.df$node[3] <- 1
inputs$genes.df$node[1] <- 2

axis.type='both'
draw.my.tree(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename="Sample_tree.pdf",scale2=0.5/362,wid=60,extra.len=20,len=10,
             scale.x.real = 1/30, sig.shape=2.5, w.padding = inputs$w.padding+0.5, h.padding=1, offset=1.4,y.wid=15,maxwid=60,axis.type=axis.type,rad=3.2,gene.cex=1.22,
             add.genes=add.genes,seg1.col='navy',seg2.col='gold',samp_name="Sample Tree",circle.col="grey20",lin.width=0.5,curve=3.2,branching=inputs$branching,line.lwd=4,axis.space.left=45, axis.space.right=45,alternate.genes=TRUE)

