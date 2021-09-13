library(plyr)
library(lattice)
library(latticeExtra)
library(grid)
library(gridExtra)
library(BoutrosLab.plotting.general)
library(tidyr)
library(yaml)
library(gtable)

source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add.text2.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.sample.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.draw.sample.clone.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/set.position.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/prep.tree.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/adjust_tree.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_clones.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/my.calc.clone.coords.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/plot.ellipses.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/ccf_circles.R')
source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_nodes_radial.R')


prep.inputs <- function(tree_in,sample){
  tree <- tree_in[which(tree_in[,1] == sample),c(2,3,9,4)]
  tree$parent[tree$parent==0] <- -1
  colnames(tree) <- c('parent','tip','length1','length2')
  in.tree.df <- data.frame(lab = c(tree$tip), parent = c( tree$parent))
  in.tree.df$ccf <- NULL
  # col <-  c("#7a7876",
  #         "#6f5883",
  #         "#769f79",
  #         "#948bb3",
  #         "#005300",
  #         "#549eb3",
  #         "#805a34",
  #         "#40677f",
  #         "#39755f",
  #         "#6f7c55")
col <- c("#969492",
  "#5bab95",
  "#72b348",
  "#ce4fb7",
  "#545e2c",
  "#7083be",
  "#c44739",
  "#6e3155",
  "#c49449",
  "#cd7b91")


          # c("#6b8fce",
          # "#4db598",
          # "#677e39",
          # "#c55d86",
          # "#c27743",
          # "#c2a944",
          # "#7a67ca",
          # "#d04c42",
          # "#6ab64c",
          # "#c25abc")
  col <- col[seq_along(tree_in$child)]
  if(all(tree$length2==0)){
    tree$length2 <- NULL
  }
  in.tree.df$c.col <- col
  # in.tree.df$col <- default.colours(number.of.colours=length(unique(tree_in$child)), palette="spiral.noon" )
  branching <- ifelse(any(duplicated(tree$parent)== TRUE),TRUE,FALSE)
  return(list(in.tree.df = in.tree.df, tree=tree, branching=branching, samp=sample))
}

colours <- c("#969492",
  "#5bab95",
  "#72b348",
  "#ce4fb7",
  "#545e2c",
  "#7083be",
  "#c44739",
  "#6e3155",
  "#c49449",
  "#cd7b91",
  "#8600c9")

# sample_names <- read_yaml("/u/asalcedo/isilon/private/users/asalcedo/Lydia_trees/SRC-BRCA/BRCAsamples_displayname.yaml")
trees <- NULL
for (pipeline in c('SomaticSniper-Battenberg', 'SomaticSniper-TITAN','SomaticSniper-FACETS', 'MuTect-Battenberg','MuTect-TITAN', 'MuTect-FACETS')){
  trees_df <- read.table(paste0("~/isilon/private/users/asalcedo/baretrees/2020-06-15_", pipeline, "_baretrees_multi-region.txt"),header=FALSE,stringsAsFactors =FALSE, fill=TRUE)
  print(pipeline)
  trees_df <- trees_df[which(trees_df$V9 != ""),]
  trees_df$pipeline <- pipeline

  trees_df$V1 <- gsub('-','.',trees_df$V1)
  # colnames(trees_df) <- trees_df[1,]
  # colnames(trees_df)[1] <- "V1"
  # trees_df <- trees_df[which(trees_df$parent != "parent"),]


  if(is.null(trees)){
    trees <- trees_df
  }else{
    trees <- rbind(trees,trees_df)
  }

}

Vcallers <- c("SomaticSniper", "MuTect")
    Ccallers <- c("Battenberg", "TITAN", "FACETS")



for (tumour in unique(trees$V1)){
 trees_df <- trees[trees$V1==tumour,]
  trees_df <- separate(trees_df, "pipeline", into=c("V_caller","C_caller"),remove=FALSE)
  pipe_trees <- list()
  caller.names <- NULL


  for (Vcaller in Vcallers){
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


    tumour_trees <- list()
    for(Ccaller in Ccallers ){
    
    scale.x.real <- 1/30
    scale2 <- 0.5/1000
    snv.interval <- 1000
    pga.interval <- 10
    min_width <- 1.5
    wid <- 2
    spread <- 1
    poly_tumour <- FALSE
    # while(length_diff > 0.01){
      print(Ccaller)
      print("Ccaller")
      samp_tree <- Vsamp_tree[Vsamp_tree$C_caller==Ccaller,]

    if(nrow(samp_tree[which(samp_tree$V2==0),])>1){
        # samp_tree <- samp_tree[,c(2:ncol(samp_tree))]
        colnames(samp_tree) <- samp_tree[1,]
        samp_tree$parent <- as.numeric(samp_tree$parent) + 1
        # samp_tree$parent[samp_tree$parent== 0] <- 1
        samp_tree$child <- as.numeric(samp_tree$child) + 1
        samp_tree[1,c(2:ncol(samp_tree))] <- NA
        samp_tree[1,]$parent <- 0
        samp_tree[1,]$child <- 1
        samp_tree[1,]$PGA <- 0
        samp_tree[1,]$num_ssms <- 0
        samp_tree[1,]$CP <- 0
        spread <- 0.8
        poly_tumour <- TRUE
    }
      # samp_tree <- data.table::rbind(data.frame(parent=0, child=1, num_ssms=0, PGA=0), v, fill=TRUE)

      samp_pga <- samp_tree[,c(1,3,9,6,10)]
      colnames(samp_pga) <- c('Sample','Node','PGA','CP','pipeline')
      samp_pga <- samp_pga[which(samp_pga$Node !='child'),]
     # inputs <- prep.tree(samp=tumour,trees=samp_tree, cnas=NULL,axis.type=axis.type,snvs=NULL,colours=colours,pga=samp_pga,pga.percent=TRUE, normal.included = FALSE)
    

      if(nrow(samp_tree)==0){
      # for(Ccaller in Ccallers){
        blankGrob <- gTree(children=gList(rectGrob(gp=gpar(fill="#FFFFFF",col="transparent"))))
        tumour_trees[[Ccaller]] <- blankGrob
      
      next
      # }else if(nrow(samp_tree[which(samp_tree$V2==0),])>1){
      #     blankGrob <- gTree(vp=viewport(y=unit(1, "npc"), height=unit(4,"inches"), just=c("center","top")),
      #      children=gList(rectGrob(gp=gpar(fill="#FFFFFF",col="transparent")), 
      #       textGrob("Poly-tumour", gp=gpar(col="black",cex=1.8)), textGrob(paste0(Vcaller,"-",Ccaller), y=unit(.5, "npc"), gp=gpar(col="black",cex=1.7))))
      #     tumour_trees[[Ccaller]] <- blankGrob
      #     next
        }


      inputs <- prep.tree(samp=tumour,trees=samp_tree, cnas=NULL,axis.type=axis.type,snvs=NULL,colours=colours,pga=samp_pga,pga.percent=TRUE, normal.included = FALSE)
      inputs$in.tree.df$c.col <- inputs$in.tree.df$color  
      

      tree_height <- data.frame(PGA=sum(as.numeric(samp_tree[samp_tree[,2] != "parent",9]),na.rm=TRUE)*scale.x.real, SNV=sum(as.numeric(samp_tree[samp_tree[,2] != "parent",4]),na.rm=TRUE)*scale2)
    

      if(any(tree_height[,1] > 4.5)){
        scale.x.real <- 1/90
        pga.interval <- 20
        if(any(tree_height[,1] > 3.2)){
          scale2 <- 0.5/2000
          snv.interval <- 2000 
        }
      } else if(any(tree_height[,1] > 3.5)){
        scale.x.real <- 1/60
        pga.interval <- 20

      }
      if(any(tree_height[,2] > 3.2)){
        scale2 <- 0.5/2000
        snv.interval <- 2000 
      }
      # if((max(as.numeric(Vsamp_tree$V4[Vsamp_tree$V2 != "parent"])) < 500) & (max(as.numeric(Vsamp_tree$V9[Vsamp_tree$V2 != "parent"])) < 5)){
      #   scale2 <- 0.5/750
      #   scale.x.real <- 1/15
      #   snv.interval <- 500
      #   pga.interval <- 5
      #   title_dist <- 0.885
      # }

      

      if(max(tree_height[1,]) < 0.1){
        if(tree_height[1,1] < 0.01 & sum(as.numeric(samp_tree[,9]),na.rm=TRUE) > 0){
          scale.x.real <- 1/15
          pga.interval <- 5
        }
        if(tree_height[1,2] < 0.1 & sum(as.numeric(samp_tree[,4]),na.rm=TRUE) > 0){
          scale2 <- 0.5/750
          snv.interval <- 500
        }
      }

          if((max(as.numeric(samp_tree[,4]), na.rm=TRUE) < 800) | tumour =="CPCG0260-B1F1"){
            scale2 <- 0.5/750
            # scale.x.real <- 1/15
            snv.interval <- 500
            # pga.interval <- 5
            # min_width <- 1.7
            # wid=2.5
          }

          if((sum(as.numeric(samp_tree[,4]), na.rm=TRUE) < 200) ){
            scale2 <- 1/250
            snv.interval <- 50

          }
        if((sum(as.numeric(samp_tree[,9]), na.rm=TRUE) < 10) ){
            scale.x.real <- 1/15
            pga.interval <- 5
   
          }

          if((max(as.numeric(samp_tree[,4]), na.rm=TRUE) == 0) & (max(as.numeric(samp_tree[,9]), na.rm=TRUE) ==0 )){
            next
          }
          if(tumour =="CPCG0260-B1F1" & Vcaller=="MuTect" & Ccaller=="Battenberg"){
            min_width=2
            wid=4
          }
          if(length(unique(samp_tree[,3]))>8){
            spread <- 1.1
          }
                   
  # source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/add_segs.R')
  # source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
  # source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/position_nodes_radial.R')
  
  # source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/plot.ellipses.R')
  # source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/draw.my.tree2.R')
          tree <-   draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= NULL,filename="scrap.pdf", scale2=scale2,wid=wid, min_width=min_width, extra.len=5,len=40,
                          scale.x.real = scale.x.real, sig.shape=2.5, w.padding =1, h.padding=1, add.bells=FALSE, add.labels=TRUE, add.circle=TRUE, offset=0.0467,axis.type="both",rad=0.1,gene.cex=1.22,
                          add.genes=FALSE,samp_name='',seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=1,curve=3.2,branching=inputs$branching,line.lwd=4,
                          axis.space.left=0.3, axis.space.right=0.3, axis.cex=0.75, add.normal=TRUE, fixed_angle=NULL, lab.cex=1.1,
                          snv.interval=snv.interval, pga.interval=pga.interval,pga.cap=TRUE, spread=spread, node.cex=1.2, poly_tumour=poly_tumour)

          # tree_gTree <- editGrob(tree[[4]],vp=viewport(x=unit(0.45,"npc"), y= unit(0.5,"npc"), just=c("center","center"), height=tree[[4]]$vp$height, width=tree[[4]]$vp$width))
          tree_gTree <- editGrob(tree[[4]],vp=viewport(x=unit(0.47,"npc"), y= unit(0.5,"npc"), just=c("center","center"), height=unit(3.5,"inches"), width=unit(3.1,"inches")))
          
          tree_df <- samp_tree
          if(poly_tumour != TRUE){
            colnames(tree_df) <- tree_df[1,]      
            tree_df <- tree_df[-1,]
            tree_df$fill <- inputs$in.tree.df$c.col[c(-1)]
            }else{
              tree_df <- tree_df[-1,]
              tree_df$fill <- inputs$in.tree.df$c.col[c(-1, -2)]
              tree_df$parent <- tree_df$parent -1
              tree_df$child <- tree_df$child -1
            }
          # tree_df$fill <- inputs$in.tree.df$c.col[-1]
           # source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/ccf_circles.R')
          tumour_trees[[Ccaller]] <- multiregion_ccfs(tree_in=tree_df, tumour_name=paste0(tumour," ",Vcaller,"-", Ccaller), tree_gTree=tree_gTree, width=1.2, height=3.1, title.cex=.9)
          # pdf(paste0("multisample/",tumour,"_",Vcaller,"2.pdf"),height=10, width=8)
          # grid.draw(tumour_trees[[Ccaller]])
          # dev.off()
          }
        # }
        both_trees <- arrangeGrob(grobs=tumour_trees,nrow=3, heights=unit(c(3.3,3.3,3.3),"in"))
        pdf(paste0("multisample/",tumour,"_",Vcaller,"2.pdf"), height=10, width=8)
        grid.draw(both_trees)
        dev.off()          
    }
}


          source('~/cluster/svn/Collaborators/BrendaGallie/Rb1_follow_up/WGS/subclone_plotting/ccf_circles.R')




for (tumour in unique(trees$V1)){
  	# for (cCaller in c('Battenberg', 'TITAN', 'FACET')){
    tumour_trees <- list()
  # for (vCaller in c("SomaticSniper", "MuTect") ){
        pipeline <- paste0(vCaller,"-",cCaller)
    		if(file.exists(paste0("~/isilon/private/users/asalcedo/baretrees/",vCaller,"-",cCaller,"/", tumour, "_consensus_tree_filtered_pga.txt"))){
        scale.x.real <- 1/30
        scale2 <- 0.5/1000
        snv.interval <- 1000
        pga.interval <- 10
        min_width <- 1.5
        wid <- 2
        spread <- 1
        tree_df <- read.table(paste0("~/test_subclone_plotting/Lydia_trees/SRC-BRCA/",vCaller,"-",cCaller,"/", tumour, "_consensus_tree_filtered_pga.txt"),header=TRUE,stringsAsFactors =FALSE, fill=TRUE)
        inputs <- prep.inputs(tree_df, tumour)  

        if(((max(as.numeric(tree_df$num_ssms)) < 800) & (max(as.numeric(tree_df$PGA)) < 8))| tumour =="CPCG0260-B1F1"){
          scale2 <- 0.5/750
          scale.x.real <- 1/15
          snv.interval <- 500
          pga.interval <- 5
          min_width <- 1.7
          wid=2.5
        }

        if((max(as.numeric(tree_df$num_ssms)) == 0) & (max(as.numeric(tree_df$PGA)) ==0 )){
          next
        }
        if(tumour =="CPCG0260-B1F1" & vCaller=="MuTect" & cCaller=="Battenberg"){
          min_width=2
          wid=4
        }
        if(length(unique(tree_df$child))>6){
          spread <- 1.1
        }
        # source('~/test_subclone_plotting/subclone_plotting/draw.my.tree2.R')
        # source('~/test_subclone_plotting/subclone_plotting/position_clones.R')
        # source('~/test_subclone_plotting/subclone_plotting/my.draw.sample.clone.R')
        # source('~/test_subclone_plotting/subclone_plotting/draw.sample.clone.R')
        # source('~/test_subclone_plotting/subclone_plotting/position_nodes_radial.R')
        # source('~/test_subclone_plotting/subclone_plotting/position_nodes_radial.R')
        # source('~/test_subclone_plotting/subclone_plotting/tree_tiers.R')
        # source('~/test_subclone_plotting/subclone_plotting/add_segs.R')

        tree <-   draw.my.tree2(in.tree.df=inputs$in.tree.df,tree=inputs$tree,genes.df= NULL,filename="scrap.pdf", scale2=scale2,wid=wid, min_width=min_width, extra.len=15,len=40,
                        scale.x.real = scale.x.real, sig.shape=2.5, w.padding =1, h.padding=1, add.bells=FALSE, add.labels=TRUE, add.circle=TRUE, offset=0.0467,axis.type="both",rad=0.1,gene.cex=1.22,
                        add.genes=FALSE,samp_name='',seg1.col='navy',seg2.col='gold',circle.col="grey20",lin.width=1,curve=3.2,branching=inputs$branching,line.lwd=4,
                        axis.space.left=0.3, axis.space.right=0.3, axis.cex=0.75, add.normal=TRUE, fixed_angle=NULL,
                        snv.interval=snv.interval, pga.interval=pga.interval,pga.cap=TRUE, spread=spread)
        
        # tree_gTree <- editGrob(tree[[4]],vp=viewport(x=unit(0.45,"npc"), y= unit(0.5,"npc"), just=c("center","center"), height=tree[[4]]$vp$height, width=tree[[4]]$vp$width))
        tree_gTree <- editGrob(tree[[4]],vp=viewport(x=unit(0.47,"npc"), y= unit(0.5,"npc"), just=c("center","center"), height=unit(3.5,"inches"), width=unit(3.1,"inches")))
        
        tree_df$fill <- inputs$in.tree.df$c.col
        # source('~/test_subclone_plotting/subclone_plotting/ccf_circles.R')
        tumour_trees[[cCaller]] <- multiregion_ccfs(tree_in=tree_df,ccf_names=sample_names[tumour][[tumour]]$singles, tumour_name=paste0(tumour," ",vCaller,"-", cCaller), tree_gTree=tree_gTree,width=1.2)

    	  }
      }
      both_trees <- arrangeGrob(grobs=tumour_trees,nrow=2)
      pdf(paste0("multisample/",tumour,"_",vCaller,".pdf"),height=10, width=8)
      grid.draw(both_trees)
      dev.off()
  # }
}
