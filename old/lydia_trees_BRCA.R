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

sample_names <- read_yaml("/u/asalcedo/isilon/private/users/asalcedo/Lydia_trees/SRC-BRCA/BRCAsamples_displayname.yaml")

for (tumour in names(sample_names)){
  for (vCaller in c("SomaticSniper", "MuTect") ){
    tumour_trees <- list()
  	for (cCaller in c('Battenberg', 'TITAN')){
    		if(file.exists(paste0("~/isilon/private/users/asalcedo/Lydia_trees/SRC-BRCA/",vCaller,"-",cCaller,"/", tumour, "_consensus_tree_filtered_pga.txt"))){
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
  }
}
