#1) Install the packages we need for this analysis. If packages are already installed we can proceed to the next step

#2) Load all the libraries we need 
library(R.utils) # required for gzip files
library(ggplot2)
library(reshape2)
library(tidyverse)

#3) set the working directory
setwd("/Users/jgordon3/Downloads/")

#4) Download the data. The main site for CCLE data: https://depmap.org 
#https://sites.broadinstitute.org/ccle/datasets
#If you already have a table you can skip the next steps. You can either use raw counts or Read normalized data (RPKM).  
#counts
#download.file("https://depmap.org/portal/download/api/download?file_name=ccle%2Fccle_2019%2FCCLE_RNAseq_genes_counts_20180929.gct.gz&bucket=depmap-external-downloads", "CCLE_RNAseq_genes_counts_20180929.gct.gz")
#rpkm
#download.file("https://depmap.org/portal/download/api/download?file_name=ccle%2Fccle_2019%2FCCLE_RNAseq_genes_rpkm_20180929.gct.gz&bucket=depmap-external-downloads", "CCLE_RNAseq_genes_rpkm_20180929.gct.gz")
#TPM(gene)
#download.file("https://depmap.org/portal/data_page/?tab=allData&releasename=CCLE%202019&filename=CCLE_RNAseq_rsem_genes_tpm_20180929.txt.gz", "CCLE_RNAseq_rsem_genes_tpm_20180929.txt.gz")
#TPM(transcript)
#download.file("https://depmap.org/portal/data_page/?tab=allData&releasename=CCLE%202019&filename=CCLE_RNAseq_rsem_transcripts_tpm_20180929.txt.gz")
#download the cell line annotations
#download.file("https://depmap.org/portal/download/api/download?file_name=ccle%2Fccle_2019%2FCell_lines_annotations_20181226.txt&bucket=depmap-external-downloads", "CCLE_annotations.txt")

#5) unzip the file and import counts table into R using read.table
#the CCLE data sometimes has two annoying lines before the header. We need to remove these before reading the table.
#counts
#gunzip("CCLE_RNAseq_genes_counts_20180929.gct.gz", remove=FALSE)
#filename="CCLE_RNAseq_genes_counts_20180929.gct"
#rpkm
#gunzip("CCLE_RNAseq_genes_rpkm_20180929.gct.gz", remove=FALSE)
RPKM.filename="CCLE_RNAseq_genes_rpkm_20180929.gct"
RPKM.counts=read.table(RPKM.filename, sep="\t", skip=2, header = TRUE)
TPM.filename="CCLE_RNAseq_rsem_genes_tpm_20180929.txt"
TPM.counts=read.table(TPM.filename, sep="\t", header = TRUE)
#counts=read.table(filename, sep="\t", header = TRUE)
annotations=read.csv("CCLE_annotations.txt", sep="\t", header = TRUE)

#6) process CCLEE gene expression counts tables 
#remove decimals from first column (ENSBL IDS) and convert to rownames
RPKM.counts$Name <- sub("\\.\\d+$", "", RPKM.counts$Name)
rownames(RPKM.counts) <- RPKM.counts[,1]
RPKM.counts$Name <- NULL

TPM.counts$gene_id <- sub("\\.\\d+$", "", TPM.counts$gene_id)
rownames(TPM.counts) <- TPM.counts[,1]
TPM.counts$gene_id <- NULL
#select specific gene names 

gene <- c("ENSG00000231298") #LINC00704
#gene <- c("ENSG00000159216") #RUNX1
#gene<-c("ENSG00000123500") #COL10A
#gene<-c("ENSG00000283554") #LINC02341
#gene <- c("ENSG00000124813") #RUNX2
#gene <- c("ENSG00000166770") # ZNF667-AS1
#gene <- c("ENSG00000198046") # ZNF667
#gene <- c("ENSG00000113580") #NR3C1 (GCR)
#gene <- c("ENSG00000091831") #ESR1 (ERa)
#gene <- c("ENSG00000143126") #CELSR2 

RPKM.singlegene <- RPKM.counts[gene,]
RPKM.m<-melt(RPKM.singlegene)

TPM.singlegene <- TPM.counts[gene,]
TPM.m<-melt(TPM.singlegene)

# find top 20 values
library(dplyr)
library(tibble)
library(forcats)

#sort in descending order
RPKM.top <- RPKM.m %>% slice_max(value, n = 20)
RPKM.bottom <- RPKM.m %>% slice_min(value, n = 20)
#plot the reshaped data using ggplot
p1<- RPKM.top %>% mutate(variable = fct_reorder(variable, value)) %>%
ggplot( aes(x=variable, y=value, fill = variable)) + 
      geom_bar(stat="identity", show.legend=FALSE, width=0.5, fill="steelblue") +  
      labs(title = gene, x = "cells", y = "counts (RPKM)") +
      coord_flip() +
      theme_bw()
p1

#sort in descending order
TPM.top <- TPM.m %>% slice_max(value, n = 20)
TPM.bottom <- TPM.m %>% slice_min(value, n = 20)
#plot the reshaped data using ggplot
p2<- TPM.top %>% mutate(variable = fct_reorder(variable, value)) %>%
  ggplot( aes(x=variable, y=value, fill = variable)) + 
  geom_bar(stat="identity", show.legend=FALSE, width=0.5, fill="steelblue") +  
  labs(title = gene, x = "cells", y = "counts (TPM)") +
  coord_flip() +
  theme_bw()
p2

#TISSUE SPECIFIC: select all cells of a certain cell type
#OPTIONS; BREAST; HAEMATOPOIETIC_AND_LYMPHOID_TISSUE
RPKM.cellspecificcounts <-RPKM.counts %>% dplyr::select(contains('BREAST'))
RPKM.cellsinglegene <- RPKM.cellspecificcounts[gene,]
colnames(RPKM.cellsinglegene) <- sub("\\_.*","",colnames(RPKM.cellsinglegene)) 
RPKM.m<-reshape2::melt(RPKM.cellsinglegene)

TPM.cellspecificcounts <-TPM.counts %>% dplyr::select(contains('BREAST'))
TPM.cellsinglegene <- TPM.cellspecificcounts[gene,]
colnames(TPM.cellsinglegene) <- sub("\\_.*","",colnames(TPM.cellsinglegene)) 
TPM.m<-reshape2::melt(TPM.cellsinglegene)

#plot the reshaped data
p3<- RPKM.m %>% mutate(variable = fct_reorder(variable, value)) %>%
  ggplot( aes(x=variable, y=value, fill = variable)) + 
  geom_bar(stat="identity", show.legend=FALSE, width=0.5, fill="steelblue") +  
  labs(title = gene, x = "cells", y = "counts (RPKM)") +
  theme_bw()+
  theme(axis.text.y = element_text(size = 2))+
  coord_flip() 
p3

p4<- TPM.m %>% mutate(variable = fct_reorder(variable, value)) %>%
  ggplot( aes(x=variable, y=value, fill = variable)) + 
  geom_bar(stat="identity", show.legend=FALSE, width=0.5, fill="steelblue") +  
  labs(title = gene, x = "cells", y = "counts (RPKM)") +
  theme_bw()+
  theme(axis.text.y = element_text(size = 2))+
  coord_flip() 
p4


#sort in descending order
TPM.top <- TPM.m %>% slice_max(value, n = 30)
TPM.bottom <- TPM.m %>% slice_min(value, n = 30)
#plot the reshaped data using ggplot
p5<- TPM.top %>% mutate(variable = fct_reorder(variable, value)) %>%
  ggplot( aes(x=variable, y=value, fill = variable)) + 
  geom_bar(stat="identity", show.legend=FALSE, width=0.5, fill="steelblue") +  
  labs(title = "MANCR In Breast Cell Lines (CCLE)", x = "cells", y = "counts (TPM)") +
  coord_flip() +
  theme(axis.text.x= element_text(size = 4))+
  theme_bw()
p5


#start grouping data
celltypes <- unique(annotations$Site_Primary)

#ggplot Refeerence: https://www.sharpsightlabs.com/blog/histogram-r-ggplot2/
