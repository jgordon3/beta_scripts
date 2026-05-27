BiocManager::install("biomaRt", quietly = TRUE )
library(gplots)
library(RColorBrewer)
library(readr)
library(dplyr)
library(ggplot2)
library(pheatmap)
library("org.Hs.eg.db")
library(biomaRt)

# taxon ID Human 9606
# Ossification GO:0001503, 
# ossification involved in bone maturation GO:0043931

#gets gene symbol, transcript_id and go_id for all genes annotated with osteogenesis

ensembl = useMart("ensembl",dataset="hsapiens_gene_ensembl") #uses human ensembl annotations
GO0001503.genes <- as.data.frame(getBM(attributes=c('hgnc_symbol', 'ensembl_transcript_id', 'go_id'),
                   filters = 'go', values = 'GO:0001503', mart = ensembl))

GO0001503.genes.symbols <- dplyr::distinct(GO0001503.genes, hgnc_symbol, .keep_all=TRUE)
ENSEMBL <- mapIds(org.Hs.eg.db, keys = GO0001503.genes.symbols$hgnc_symbol, keytype = "SYMBOL", column = "ENSEMBL")
EntrezID <- mapIds(org.Hs.eg.db, keys = GO0001503.genes.symbols$hgnc_symbol, keytype = "SYMBOL", column = "ENTREZID")
GO0001503.genes.symbols["Ensembl"] <- ENSEMBL 
GO0001503.genes.symbols["EntrezID"] <- EntrezID 


GO0043931.genes <- as.data.frame(getBM(attributes=c('hgnc_symbol', 'ensembl_transcript_id', 'go_id'),
                                       filters = 'go', values = 'GO:0043931', mart = ensembl))
GO0043931.genes.symbols <- dplyr::distinct(GO0043931.genes, hgnc_symbol, .keep_all=TRUE)

ENSEMBL <- mapIds(org.Hs.eg.db, keys = GO0043931.genes.symbols$hgnc_symbol, keytype = "SYMBOL", column = "ENSEMBL")
EntrezID <- mapIds(org.Hs.eg.db, keys = GO0043931.genes.symbols$hgnc_symbol, keytype = "SYMBOL", column = "ENTREZID")
GO0043931.genes.symbols["Ensembl"] <- ENSEMBL 
GO0043931.genes.symbols["EntrezID"] <- EntrezID 


gene.symbols <- osteogenesis$Symbol

EntrezID <- mapIds(org.Hs.eg.db, keys = gene.symbols, keytype = "SYMBOL", column = "ENTREZID")
ENSEMBL <- mapIds(org.Hs.eg.db, keys = gene.symbols, keytype = "SYMBOL", column = "ENSEMBL")

osteogenesis["EntrezID"] <- EntrezID
osteogenesis["ENSEMBL"] <- ENSEMBL 
write.csv(osteogenesis, file ="/Users/jgordon3/Desktop/Human_osteogenesis_array_fixed.csv" )
write.csv(osteogenesis, file ="/Users/jgordon3/Desktop/Human_osteogenesis_array_fixed.csv" )
