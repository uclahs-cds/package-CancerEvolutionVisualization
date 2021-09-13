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


cnas <- NULL
trees <- NULL
SNVs <- NULL

for (pipeline in c('SomaticSniper-Battenberg', 'SomaticSniper-TITAN', 'MuTect-Battenberg','MuTect-TITAN')){
  cnas_df <- read.table(paste0("~/isilon/private/users/asalcedo/Lydia_trees/SingleTrees/",pipeline,"/2018-04-26_Driver_CNAs_stacked.txt"),header=TRUE,stringsAsFactors =FALSE)
  snvs_df <- read.table(paste0("~/isilon/private/users/asalcedo/Lydia_trees/SingleTrees/",pipeline,"/2018-04-26_Driver_SNVs_stacked.txt"),header=TRUE,stringsAsFactors =FALSE)
  trees_df <- read.table(paste0("~/isilon/private/users/asalcedo/Lydia_trees/SingleTrees/",pipeline,"/", pipeline, "_baretrees.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)

  trees_df <- trees_df[which(trees_df$V9 != ""),]
  trees_df$pipeline <- pipeline
  cnas_df$pipeline <- pipeline
  snvs_df$pipeline <- pipeline

  names(cnas_df)[2] <- 'gene'
  names(cnas_df)[1] <- 'Sample'
  cnas_df$Sample <- gsub('-','.',cnas_df$Sample)
  snvs_df$Sample <- gsub('-','.',snvs_df$Sample)
  trees_df$V1 <- gsub('-','.',trees_df$V1)

  if(is.null(cnas)){
    cnas <- cnas_df
    snvs <- snvs_df
    trees <- trees_df
  }else{
    cnas <- rbind(cnas,cnas_df)
    snvs <- rbind(snvs,snvs_df)
    trees <- rbind(trees,trees_df)
  }

}

trees$V1[trees$V1 == "CPCG0100.B1F1"] <- "CPCG0100"
trees$V1[trees$V1 == "CPCG0334.B1F1"] <- "CPCG0334"
trees$V1[trees$V1 == "CPCG0260.B1F1"] <- "CPCG0260"

   source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
   source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
   source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')


  for (sample in unique(trees$V1)){
  # for (sample in c("CPCG0575", "CPCG0166")){
              
    trees_df <- trees[trees$V1==sample,]
    cnas_df <- cnas[cnas$Sample==sample,]
    snvs_df <- snvs[snvs$Sample==sample,]

    trees_df <- separate(trees_df, "pipeline", into=c("V_caller","C_caller"),remove=FALSE)
    # cnas_df <- separate(cnas_df, "pipeline", into=c("V_caller","C_caller"))
    # snvs_df <- separate(snvs_df, "pipeline", into=c("V_caller","C_caller"))
   
    pipe_trees <- list()

    caller.names <- NULL
    # if((length(unique(paste(trees_df$V_caller, trees_df$C_caller))) == 2) & (length(unique(trees_df$V_caller)) == 2)){
    #   caller.names <- unique(paste0(trees_df$V_caller,"-",trees_df$C_caller))
    #   trees_df$C_caller[trees_df$V_caller=="MuTect"] <- c("Battenberg", "TITAN")[grep(trees_df$C_caller[1],c("Battenberg", "TITAN"),invert=TRUE)]
    #   trees_df$V_caller <- "SomaticSniper"
    # }

    Vcallers <- c("SomaticSniper", "MuTect")

    for (Vcaller in Vcallers){
      title_dist <- 0.905
      scale.x.real <- 1/30
      scale2 <- 0.5/1500
      snv.interval<- 1000
      pga.interval <-10
      Ccallers <- c("Battenberg", "TITAN")
      extra.len <- 17      

      print("Vcaller")
      print(Vcaller)
      Vsamp_tree <- trees_df[trees_df$V_caller==Vcaller,]

      if(nrow(Vsamp_tree)==0){
        for(Ccaller in Ccallers){
          blankGrob <- gTree(children=gList(rectGrob(gp=gpar(fill="#FFFFFF",col="transparent"))))
          pipe_trees[[paste0(Vcaller,"-",Ccaller)]] <- blankGrob
        }
        next
      }

      
      tree_heights <- ddply(Vsamp_tree[Vsamp_tree$V2 != "parent",], .(V_caller, C_caller), function(x) data.frame(PGA=sum(as.numeric(x$V9))*scale.x.real+17*scale.x.real+1, SNV=sum(as.numeric(x$V4))*scale2+17*scale.x.real+1))


      if(any(tree_heights[,3] > 3.5)){
        scale.x.real <- 1/60
        pga.interval <- 20

      }
      if(any(tree_heights[,4] > 3.5)){
        scale2 <- 0.5/2000
        snv.interval <- 2000 
      }
      if((max(as.numeric(Vsamp_tree$V4[Vsamp_tree$V2 != "parent"])) < 500) & (max(as.numeric(Vsamp_tree$V9[Vsamp_tree$V2 != "parent"])) < 5)){
        scale2 <- 0.5/750
        scale.x.real <- 1/15
        snv.interval <- 500
        pga.interval <- 5
        title_dist <- 0.885
      }

      lengths <- c()
      length_diff <- Inf
      

      while(length_diff > 0.01){
          print(Ccallers)
            for(Ccaller in Ccallers ){
              print(Ccaller)
              print("Ccaller")
              samp_tree <- Vsamp_tree[Vsamp_tree$C_caller==Ccaller,]
              samp_cnas <- cnas_df[cnas_df$pipeline==samp_tree$pipeline[1],]
              samp_snvs <- snvs_df[snvs_df$pipeline==samp_tree$pipeline[1],]
              samp_pga <- samp_tree[,c(1,3,9,6,10)]
              colnames(samp_pga) <- c('Sample','Node','PGA','CP','pipeline')
              samp_pga <- samp_pga[which(samp_pga$Node !='child'),]

              if( nrow(samp_tree)==0){ #no trees
                blankGrob <- gTree(children=gList(rectGrob(gp=gpar(fill="#FFFFFF",col="transparent"))))
                pipe_trees[[paste0(Vcaller,"-",Ccaller)]] <- blankGrob
                next
              } else if(nrow(samp_tree[which(samp_tree$V2==0),])>1){
                blankGrob <- gTree(vp=viewport(y=unit(1, "npc"), height=unit(4,"inches"), just=c("center","top")),
                 children=gList(rectGrob(gp=gpar(fill="#FFFFFF",col="transparent")), 
                  textGrob("Poly-tumour", gp=gpar(col="black",cex=1.8)), textGrob(paste0(Vcaller,"-",Ccaller), y=unit(title_dist, "npc"), gp=gpar(col="black",cex=1.7))))
                pipe_trees[[paste0(Vcaller,"-",Ccaller)]] <- blankGrob
                next
              } else{

                axis.type=NULL
                if(length(unique(paste(trees_df$V_caller, trees_df$C_caller))) == 4){
                  axis.type <- ifelse(grepl('Battenberg',Ccaller),'PGA','SNV')
                  } else if( length(unique(paste(trees_df$V_caller, trees_df$C_caller))) == 3 ){
                    if(length(unique(Vsamp_tree$C_caller))==2){
                      # axis.type <- "both"
                      axis.type <- ifelse(grepl('Battenberg',Ccaller),'PGA','SNV')
                      }else{
                        axis.type <- "both"
                      }
                  }else if(length(unique(trees_df$V_caller)) == 2){
                    # axis.type <- ifelse(grepl('SomaticSniper',Vcaller),'PGA','SNV')
                    axis.type <- "both"
                  }else if(length(unique(trees_df$C_caller)) == 2){
                    axis.type <- ifelse(grepl('Battenberg',Ccaller),'PGA','SNV')             
                  }else{
                    axis.type <- "both"
                  }

                if(length(Vsamp_tree$V2[Vsamp_tree$V2==0]) >2){
                  axis.type="both"
                }
                
                inputs <- prep.tree(samp=sample,trees=samp_tree, cnas=samp_cnas,axis.type=axis.type,snvs=samp_snvs,colours=colours,pga=samp_pga,pga.percent=TRUE, normal.included = FALSE)
                inputs$samp.name <- samp_tree$pipeline[1]

                print(sample)
                print(samp_tree$pipeline[1])
                print("scale.x.real")
                print(scale.x.real)
                print("scale2")
                print(scale2)
                print("snv.interval")
                print(snv.interval)
                print("pga.interval")
                print(pga.interval)

                #find trees that aren't binary
    #  d_ply(samp_tree, .(V1,pipeline), function(x) {non_bin <- any(sapply(unique(x$V2), function(p) length(x$V2[x$V2==p])>2) == TRUE); if(non_bin == TRUE){print(x)}})              
               # source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
               # source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
               # source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
               # source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
               source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')
   
                out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename="scrap.pdf",scale2=scale2,wid=1.2,extra.len=extra.len,len=10,
                       scale.x.real = scale.x.real,  w.padding = inputs$w.padding, h.padding=1, offset=0.0467,axis.type=axis.type,rad=0.1,gene.cex=1.22,
                       add.genes=inputs$add.genes,add.circle=TRUE, add.labels=TRUE,seg1.col='navy',seg2.col='gold',samp_name=inputs$samp.name,circle.col="grey20",lin.width=1,curve=3,
                       branching=inputs$branching,title.y=0.5, add.normal=TRUE, line.lwd=4,axis.space.left=0.27, axis.space.right=0.27,alternate.genes=FALSE,pga.cap=FALSE,snv.interval=snv.interval, 
                       pga.interval=pga.interval)


                # test_ag <- arrangeGrob(out[[4]])
                # pdf("~/test.pdf",onefile=FALSE)
                # grid.rect(gp=gpar(fill="#FFFFFF"))
                # plot(test_ag)
                #  dev.off()
                
                   pipe_trees[[paste0(Vcaller,"-",Ccaller)]] <- out[[4]]
                   lengths <- c(lengths,max(out[[1]]$v$len+out[[1]]$v$x))
              }

            }
            if(length(lengths)>1){
              length_diff <- max(lengths)-min(lengths)
              Ccallers <- Ccallers[which.min(lengths)]
              extra.len <- extra.len + length_diff
              print("length_diff")
              print(length_diff)
              print("extra.len")
              print(extra.len)
              lengths <- c()

            }else{
              length_diff <- 0
            }
        }

      }
      print("done drawing")
      title <- gsub("\\.","-", sample)
      titleGrob <- textGrob(title,gp=gpar(cex=1.7,fontface=2)) 
      test_ag <- arrangeGrob(titleGrob, pipe_trees[['SomaticSniper-Battenberg']],pipe_trees[['SomaticSniper-TITAN']], pipe_trees[['MuTect-Battenberg']],pipe_trees[['MuTect-TITAN']],
          ncol=2,nrow=3,padding=unit(0.25,"inches"),heights=unit(c(0.1,0.4,0.5),"npc"),layout_matrix=rbind(c(1,1),c(2,3),c(4,5)))
      
        # pdf(paste0("~/test.pdf"),onefile=FALSE,height=10, width=8)
        pdf(paste0("~/Lydia_trees/",sample,".pdf"),onefile=FALSE,height=10, width=8)
        grid.newpage()
        pushViewport(viewport())
        grid.rect(gp=gpar(fill="white"),draw=TRUE)
        # upViewport()
        grid.draw(test_ag)
        dev.off()

      
}



source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')


      test_ag <- arrangeGrob(out[[4]])
      pdf("~/test.pdf",onefile=FALSE)
      grid.rect(gp=gpar(fill="#FFFFFF"))
      plot(test_ag)
       dev.off()
  

      titleGrob <- textGrob(sample,gp=gpar(cex=1.7,fontface=2)) 
       test_ag <- arrangeGrob(titleGrob, pipe_trees[['SomaticSniper-Battenberg']],pipe_trees[['SomaticSniper-TITAN']],
        ncol=2,nrow=3,padding=unit(0.25,"inches"),heights=unit(c(0.1,0.45,0.45),"npc"),layout_matrix=rbind(c(1,1),c(2,3),c(4,5)))
       # test_ag <- rbind(titleGrob, test_ag)
     
      pdf("~/test2.pdf",onefile=FALSE,height=10, width=8)
      grid.newpage()
      pushViewport(viewport())
      grid.rect(gp=gpar(fill="white",col="white"),draw=TRUE)
      # upViewport()
      grid.draw(test_ag)
      dev.off()




cnas <- read.table("~/isilon/private/users/asalcedo/Lydia_trees/SomaticSniper-Battenberg/2018-04-26_Driver_CNAs_stacked.txt",header=TRUE,stringsAsFactors =FALSE)
trees <- read.table("~/isilon/private/users/asalcedo/Lydia_trees/SomaticSniper-Battenberg/2018-07-26_bare_trees.txt",header=FALSE,stringsAsFactors =FALSE)
snvs <- read.table("~/isilon/private/users/asalcedo/Lydia_trees/SomaticSniper-Battenberg/2018-04-26_Driver_SNVs_stacked.txt",header=TRUE,stringsAsFactors =FALSE)
names(genes)[3] <- "cn"

# pga.new <- trees[,c(1,3,9,6)]
# colnames(pga.new) <- c('Sample','Node','PGA','CP')
# pga.new <- pga.new[which(pga.new$Node !='child'),]
# setwd("~/200PT")
names(cnas)[2] <- 'gene'
names(cnas)[1] <- 'Sample'
# pga.new$Sample <- gsub('-','.',pga.new$Sample)
cnas$Sample <- gsub('-','.',cnas$Sample)
snvs$Sample <- gsub('-','.',snvs$Sample)
trees$V1 <- gsub('-','.',trees$V1)

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
colours <- c(NA, '#999793', "#5884BB", "#FCF9BF")
inputs <- prep.tree(samp="CPCG0073",trees=trees,cnas=cnas,snvs=snvs,colours=colours)


# axis.type='single'
axis.type='single'
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/plot.ellipses.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/adjust_tree.R')

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')

out <- draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= inputs$genes.df,filename=inputs$out.name,scale2=0.5/362,wid=30,extra.len=20,len=10,
             scale.x.real = 1/1000, sig.shape=2.5, w.padding = inputs$w.padding+0.5, h.padding=1, offset=1.4,axis.type=axis.type,rad=0.1,gene.cex=1.22,
             add.genes=inputs$add.genes,seg1.col='navy',seg2.col='gold',samp_name=inputs$samp.name,circle.col="grey20",lin.width=1,curve=3.2,
             branching=inputs$branching,add.normal=TRUE, line.lwd=4,axis.space.left=5, axis.space.right=5,alternate.genes=FALSE,pga.cap=FALSE)

