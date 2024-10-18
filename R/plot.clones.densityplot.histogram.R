# Description: Plot density curves of clones from SRC tool results and histograms of SNVs per clone

### PREAMBLE #######################################################################################
library(BoutrosLab.utilities);
library(BoutrosLab.plotting.general);

# Source functions
dir.functions <- '/hot/project/disease/KidneyTumor/GHLR-000108-SubclonalArchitecture/project-disease-KidneyTumor-GHLR-000108-SubclonalArchitecture/sSRC-prep-and-analysis/plot_DPClust_clones/functions';
files.functions <- list.files(dir.functions, full.names = TRUE);
lapply(files.functions, source);

dir.dpclust.parsed <- '/hot/project/disease/KidneyTumor/GHLR-000108-SubclonalArchitecture/call-SRC/DPClust_results_parsed';
dir.plot.output <- '/hot/project/disease/KidneyTumor/GHLR-000108-SubclonalArchitecture/project-disease-KidneyTumor-GHLR-000108-SubclonalArchitecture/sSRC-prep-and-analysis/plot_DPClust_clones/plot';


### LOAD DATA ######################################################################################
# Read in clustered SNVs data
files.dpclust <- list.files(dir.dpclust.parsed, pattern = 'DPClust_data.tsv', full.names = TRUE);
names(files.dpclust) <- regmatches(
    files.dpclust,
    regexpr('STGHKGFH\\d{6}-\\S{4}-\\S{3}-\\w|TCGA-\\S{2}-\\S{4}-\\S{3}', files.dpclust)
    );
list.dpclust.dfs <- lapply(files.dpclust, read.table, sep = '\t', header = TRUE);


### PLOTTING #######################################################################################
# Plot histograms
for (i in 1:length(list.dpclust.dfs)) {
    dpclust.df <- list.dpclust.dfs[[i]];
    sampleID <- names(list.dpclust.dfs)[i];
    plot.snv.histogram(
        dpclust.df,
        sampleID,
        'DPClust',
        dir.plot.output
        );
    }

# Plot density plots
for (i in 1:length(list.dpclust.dfs)) {
    dpclust.df <- list.dpclust.dfs[[i]];
    sampleID <- names(list.dpclust.dfs)[i];
    plot.clone.densityplot(
        dpclust.df,
        sampleID,
        'DPClust',
        dir.plot.output
        );
    }
