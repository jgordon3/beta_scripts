source("http://bioconductor.org/biocLite.R")
library("GenomicFeatures")
library("ggplot2")

setwd("/Users/jgordon3/Desktop/HTSEQ/CT/")

?makeTranscriptDb 
#import chromosome sizes and put into data frame
chrom_sizes <- as.data.frame(read.table("mm10.chrom.sizes", header=TRUE))

#Make the transcript db buy importing a GTF/GFF into GenomicRanges
txdb<- makeTranscriptDbFromGFF("gencode.vM3.annotation.gff3", format = "gff3", species="mus musculus", exonRankAttributeName="exon_number", chrominfo=chrom_sizes)
tx_by_gene= transcriptsBy(txdb,'gene')
ex_by_gene= exonsBy(txdb,'gene')

exons.tx <- data.frame(tx = 1:length(ex_by_gene), exons = sapply(ex_by_gene, length))
# plot
ggplot(exons.tx) + geom_histogram(aes(exons), fill = "blue") + theme_bw()

names(seqnames(tx_by_gene))

#The annotations have chromosomes called



UTR5 <- fiveUTRsByTranscript(txdb, use.names = TRUE)
UTR3 <- threeUTRsByTranscript(txdb, use.names = TRUE)
whichNo3prime <- setdiff(names(UTR5), names(UTR3))
whichNo5prime <- setdiff(names(UTR3), names(UTR5))
whichNo3prime 