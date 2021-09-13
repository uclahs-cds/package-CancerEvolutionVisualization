library(BoutrosLab.utilities);
library(BoutrosLab.plotting.general);
library(yaml);
library(tidyr);
library(data.table)
library(colourschemes)
library(betareg)
library(gridExtra)
library(bestNormalize)
library(caret)
library(png)
library(xtable)
source("~/project-SMCHET-analysis/results_analysis_functions.R")

get_data()

scores_min3A <- scores_filt[SC=="sc3A",]
scores_min3A <- scores_min3A[,.SD[1,], by=Entry]

source("~/project-SMCHET-analysis/results_analysis_functions.R")
tumour_dfs <- get_tree_info(x, minmax.long,  indir="/u/asalcedo/DREAM/HET/DATA/Challenge/scoring/submissions/", truthdir="~/DREAM/HET/DATA/Challenge/scoring/truthFiles/", exclude_S=TRUE, downsampled=FALSE)
tumour_df <- tumour_dfs[tumour=="P10" & true_rank > 0,]


get_ancestors <- function(node, tree){
	ancestors <- c()
	parent_nodes <- function(node, ancestors, tree){
		ancestors <- c(ancestors, node$true_parent_rank)
	    if (node$true_parent_rank != 0){
	    	node <- tree[true_rank == node$true_parent_rank,]
	        ancestors <- parent_nodes(node,ancestors, tree)
	    }	    
	    return(ancestors)
	}
	ancestors <- parent_nodes(node, ancestors, tree)
	return(ancestors)
}

node_quant <- function(.SD, true_tree){
	pred_parent_rank <- .SD$pred_parent_rank[1]
	true_parent_rank <- paste(unique(.SD[order(true_parent_rank),]$true_parent_rank), collapse=",");
	true_parent_cps <- paste(unique(.SD[order(true_parent_rank),]$true_parent_cp), collapse=",");	
	true_rank <- paste(unique(.SD[order(true_rank),]$true_rank), collapse=",");
	true_cps <- paste(unique(.SD[order(true_rank),]$true_cp), collapse=",");
	
	snvs_by_parent <- .SD[,.N, by=true_parent_rank][order(true_parent_rank),]
	snvs_by_parent <- snvs_by_parent[!is.na(true_parent_rank),]
	snvs_by_parent[,freq:=N/sum(snvs_by_parent$N)]
	# print(snvs_by_parent)
	true_parent_snvs <- paste(snvs_by_parent$N, collapse=",");
	
	snvs_by_origin <- .SD[,.N, by=true_rank][order(true_rank),]
	snvs_by_origin <- snvs_by_origin[!is.na(true_rank),]
	snvs_by_origin[,freq:=N/sum(snvs_by_origin$N)]
	true_origin_snvs <- paste(snvs_by_origin$N, collapse=",");
	d_1 <- 1-(sum(sapply(snvs_by_origin$freq, function(x) x^2)))
	d_2 <- 1/(sum(sapply(snvs_by_origin$freq, function(x) x^2)))
	evenness <- d_2/nrow(snvs_by_origin)
	dominant_origin <- snvs_by_origin[which.max(N),]$true_rank
	max_freq <- snvs_by_origin[which.max(N),]$freq

	merged_top_snvs <- 0
	if(nrow(snvs_by_origin) > 1){
		merged_top_snvs <- sum(snvs_by_origin[true_rank %in% true_tree[top_node == TRUE,]$true_rank,]$N)
		merged_bottom_snvs <- sum(snvs_by_origin[true_rank %in% true_tree[bottom_node == TRUE,]$true_rank,]$N)
		# merged_relatedness <- 
	}
	browser()

	N_snv <- nrow(.SD)
	return(list(N_snv=N_snv,
				pred_parent_rank=pred_parent_rank,
				pred_cp = .SD$pred_cp[1],
				true_rank=true_rank,
				true_cps=true_cps,
				true_snv_origin=true_origin_snvs,
				d_1=d_1,
				eveness=evenness,
				dominant_origin = dominant_origin,
				max_freq=max_freq,
				merged_top_snvs = merged_top_snvs,
				merged_bottom_snvs=merged_bottom_snvs,				
				true_parent_rank=true_parent_rank,
				true_parent_cps=true_parent_cps,
				true_parent_snvs=true_parent_snvs
				))
}
node_summary <- tumour_df[,node_quant(.SD, true_tree), by=.(pred_rank)][order(pred_rank),]

tree_mistakes <- function(tumour_df){
	true_tree <- unique(tumour_df[,.(true_rank, true_parent_rank)])
	true_tree[,top_node:=(true_parent_rank %in% c(0, true_tree[true_parent_rank == 0,]$true_rank))]
	true_tree[,leaf_node:=!(true_rank %in% true_tree$true_parent_rank),]
	true_tree[top_node == FALSE, bottom_node:=(leaf_node == TRUE | (true_rank %in% true_tree[leaf_node == TRUE,]$true_parent_rank))]
	# node_summary <- tumour_df[,node_quant(.SD, true_tree), by=.(pred_rank)][order(pred_rank),]
	true_tree
}


# get_ancestors(true_tree[true_rank == 4,], true_tree)

tree_summary <- node_summary[,tree_quant(.SD), by=.(pred_rank, true_rank)][order(true_rank, pred_rank),]
tree_summary

