prep.tree <- function(samp=NULL,trees=NULL,cnas = NULL, snvs = NULL, pga=NULL, CF_col = "cellular_prevalence",pga.percent=FALSE, bells=TRUE, normal.included=TRUE,axis.type='both',w.padding=NULL,colours=colours){
  if(!(samp %in% trees[,1] )){stop("Sample not found in trees file")}
  tree.df <- trees[trees[,1] == samp,]
  
  if(any(grepl("parent",tree.df[1,])==TRUE)){
    colnames(tree.df) <- tree.df[1,]
    colnames(tree.df)[1] <- "Sample"
    tree.df <- tree.df[c(2:nrow(tree.df)),]
  } else if(any(grepl("parent",colnames(trees))==TRUE)){
    colnames(tree.df) <- colnames(trees)
    colnames(tree.df)[1] <- "Sample"
  }else{stop("No column names detected")}
  
  tree.df$parent[tree.df$parent == 0] <- -1
  tree.df$cellular_prevalence <- as.numeric(tree.df[,CF_col])  
  
  if(all(!is.na(tree.df$cellular_prevalence))){
    tree.df <- reorder_clones(tree.df)
  }
  
  out.df <- data.frame(lab=c(-1,tree.df$child),  
                           ccf=as.numeric(c(1,tree.df$cellular_prevalence)),
                           color= colours[1:(nrow(tree.df)+1)],
                           parent = as.numeric(c(NA,tree.df$parent)),
                           excluded = c(TRUE,rep(FALSE,nrow(tree.df))), 
                           bell = c(FALSE,rep(bells,nrow(tree.df))), 
                           alpha =  rep(0.5,(nrow(tree.df)+1)),stringsAsFactors = FALSE)
  
  if("#FCF9BF" %in% out.df$color){
    out.df$alpha[which(out.df$color == "#FCF9BF")] <- 0.8
  }
  
  if(!(is.null(pga))){
    if(!(samp %in% pga$Sample )){stop("Sample not found in PGA file")}
    pga.df <- pga[pga$Sample==samp,]
    names.pga <- colnames(pga)
    if(!normal.included){
      pga.df <- rbind(c(samp,0,0,0,'Normal',1),pga.df)
    }
    pga.df$Node <- as.numeric(pga.df$Node)
    pga.df$PGA <- as.numeric(pga.df$PGA)
    
    if(pga.percent==TRUE){
      pga.df$PGA <- pga.df$PGA/100
    }
    pga.df$CP <- as.numeric(pga.df$CP)
    pga.df$Node[pga.df$Node == 0] <- -1 

    pga.df$Node <- order(-pga.df$CP)    
    out.tree <- data.frame(parent=as.numeric(tree.df$parent),tip = as.numeric(tree.df$child), length1=(pga.df$PGA[-1]*100), length2=as.numeric(tree.df$num_ssms),stringsAsFactors = FALSE)
  } else{
    out.tree <- data.frame(parent=as.numeric(tree.df$parent),tip = as.numeric(tree.df$child), length1=as.numeric(tree.df$num_ssms),stringsAsFactors = FALSE)
  }
  genes.df <-  NULL

  if ((!is.null(cnas) | !is.null(snvs)) & ((samp %in% cnas$Sample ) | (samp %in% snvs$Sample )) ){
    if(!is.null(cnas)  & (samp %in% cnas$Sample ) ){
      cnas.df <- cnas[cnas$Sample ==samp,c(2:4)]
      cnas.df <- cnas.df[which(!is.na(cnas.df$node)),]
    }
    if(!is.null(snvs) & (samp %in% snvs$Sample )){
      snvs.df <- snvs[snvs$Sample ==samp,c(2:3)]
      snvs.df$cn <- NA
      snvs.df <- snvs.df[which(!is.na(snvs.df$node)),]
    }
    if(!is.null(cnas) & !is.null(snvs) & (samp %in% cnas$Sample ) & (samp %in% snvs$Sample )){
        genes.df <- rbind(cnas.df,snvs.df)
      } else if(!is.null(snvs) & (samp %in% snvs$Sample )){
        genes.df <- snvs.df
      } else if (!is.null(cnas) & (samp %in% cnas$Sample )){
      genes.df <- cnas.df
    }
  
  genes.df <- genes.df[order(genes.df$node,genes.df$cn),]
  }
  out.name <- paste0("SR_",samp,"_",axis.type, ".pdf")
  samp.name<- samp
  add.genes <- ifelse((is.null(genes.df) || nrow(genes.df)==0 ),FALSE,TRUE)
  if (is.null(w.padding)){
    w.padding=1.05
    if(axis.type =="none"){
      w.padding = 0.85
    }
  }
  branching <- ifelse(any(duplicated(out.tree$parent)== TRUE),TRUE,FALSE)
  
  return(list(in.tree.df = out.df,tree = out.tree, genes.df = genes.df, out.name = out.name, w.padding=w.padding, samp.name = samp.name, branching=branching,add.genes=add.genes,axis.type=axis.type  ))
}

prep.smchet <- function(out_1C = NULL, out_2A = NULL,  out_3A = NULL, samp.name = NULL, branch.length=NULL, tot_ssm = 100, smc_labs =TRUE, col=NULL){
    #read in 3A output to get the basic tree structure
    if( !is.null(out_3A)){
      tree <- process_3A(out_3A)
    }else{
      stop("must provide a 3A output with the tree structure")
    }
    
    #if a 1C output is provided use that to populate the branch lengths
    if (!is.null(out_1C) ){
      ssms <- process_1C(out_1C)
      tree <- merge(tree,ssms, by="tip")      
    }

    cluster_list <- NULL
    pred2A <- NULL
    truth2A <- NULL

    if(!is.null(out_2A)){
      cluster_list <- process_2A(truth=out_2A[1], pred=out_2A[2])      
      pred2A <- scan(out_2A[2], what="numeric")
      truth2A <- scan(out_2A[1], what="numeric")
      if(is.null(out_1C)){
        pred_counts <- data.frame(table(pred2A))
        colnames(pred_counts) <- c("tip","length1")
        tree <- merge(tree, pred_counts, by="tip")
      }
      cluster_list[["col"]] <- data.frame(lab=sort(unique(c(truth2A))), colour=col[seq_along(unique(truth2A))], stringsAsFactors=FALSE)
    }

    add.genes <- FALSE
    genes.df <- NULL

    if(smc_labs){
      prop.ssms <- tree$length1/sum(tree$length1)
      percents <- round(prop.ssms,2)*100
      labs <- tree$plot.lab
      labs <- str_replace_all(labs, "(\\d)","N\\1")
      genes.df <- data.frame(node=tree$tip, gene=paste0(labs, ": ",percents,"%"), cn = rep(NA,length(nrow(tree))), stringsAsFactors=FALSE)
      add.genes <- TRUE
    }

    #if no ssm assignment is provided then either give each branch the same number of ssms or a proportion of the total
     if((is.null(out_1C) & is.null(out_2A)) | !is.null(branch.length) ){
      if(length(branch.length) ==1){
        tree$length1 <- branch.length
      } else{      
        if(length(branch.length) != nrow(tree)){
          stop("branch length must be a single number or the same length as the number of nodes")        
        }
      nssm <- tot_ssm*branch.length
      tree$length1 <- nssm
      }
    }

    tree <- tree[order(tree$tip),]
     
    branching <- ifelse(any(duplicated(tree$parent)== TRUE),TRUE,FALSE)
    out.name <- paste0("SMC_",samp.name,".pdf")
    w.padding = 2
    in.tree.df <- data.frame(lab = c(tree$tip), parent = c( tree$parent), stringsAsFactors=FALSE)
    if("plot.lab" %in% colnames(tree)){
      in.tree.df$plot.lab <- tree$plot.lab
    } else{
      in.tree.df$plot.lab <- tree$tip
    }
    in.tree.df$ccf <- NULL  

    if(!is.null(out_2A)){
      col_df <- cluster_list[["col"]]
    } else if (!is.null(col)){
      col_df <- data.frame(lab=unique(in.tree.df$lab), col, stringsAsFactors=FALSE) 
    }
    in.tree.df <- merge(in.tree.df, col_df, by="lab", all.x=TRUE)
    in.tree.df$colour <- as.character(in.tree.df$colour)
    in.tree.df$plot.lab <- as.character(in.tree.df$plot.lab)
    return(list(in.tree.df = in.tree.df,tree = tree, out.name = out.name, w.padding=w.padding, samp.name = samp.name, branching=branching,add.genes=add.genes, genes.df = genes.df, axis.type="none", cluster_list=cluster_list))
}

process_1C <- function(out_1C){
  in.df <- read.table(out_1C, header=FALSE)
  colnames(in.df)[1:2] <- c("tip","length1")
  if(ncol(in.df) == 3){
    colnames(in.df)[3] <- "ccf"
  }
  return(in.df)
}

process_3A <- function(out_3A){
  in.df <- read.table(out_3A, header=FALSE)
  out.df <- data.frame(parent = in.df$V2, tip = in.df$V1)
  out.df$parent[out.df$parent==0] <- -1
  if(ncol(in.df)==3){
    out.df$plot.lab <- in.df$V3
  }
  return(out.df[order(out.df$tip),])
}

process_2A <- function(truth=NULL, pred=NULL){
  pred <- scan(pred, what="numeric")
  true <- scan(truth, what="numeric")
  out2A <- data.frame(ssm=seq_along(pred), truth=true, pred=pred)
  origins <- dlply(out2A, .(pred), function(x) {props=table(x$truth)/nrow(x); props[props!=0]})
  names(origins) <- paste0("N", names(origins))
  return(origins)
}

reorder_clones <- function(in.df){
  new.df <- in.df
  new.df$new.lab <- order(-as.numeric(new.df$cellular_prevalence))
  new.df$new.par <- sapply(new.df$parent, function(x){ return(new.df$new.lab[new.df$child==x])})
  in.df$child <- new.df$new.lab
  in.df$parent <- new.df$new.par
  in.df$parent[in.df$child == 1] <- -1
  return(in.df)
}