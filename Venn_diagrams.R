library(venneuler)

set.seed(1)
x = paste("cluster-",sample(c(1:100),500,replace=TRUE),sep="")
y = c(
  paste("factor-",sample(c(letters[1:3]),300, replace=TRUE),sep=""),
  paste("factor-",sample(c(letters[1]),100, replace=TRUE),sep=""),
  paste("factor-",sample(c(letters[2]),50, replace=TRUE),sep=""),
  paste("factor-",sample(c(letters[3]),50, replace=TRUE),sep="")
)
data = data.frame(cluster=x,factor=y)

## Modify the "factor" column, by renaming it and converting
## it to a character vector.
levels(data$factor) <- c("a", "b", "c")
data$factor <- as.character(data$factor)

## FUN is an anonymous function that determines which letters are present
## 2 or more times in the cluster and then pastes them together into 
## strings of a form that venneuler() expects.
##
inter <- aggregate(factor ~ cluster, data=data,
                   FUN = function(X) {
                     tab <- table(X)
                     names <- names(tab[tab>=2])
                     paste(sort(names), collapse="&")
                   })            
## Count how many clusters contain each combination of letters
counts <- table(inter$factor)
counts <- counts[names(counts)!=""]  # To remove groups with <2 of any letter
#  a   a&b a&b&c   a&c     b   b&c     c 
# 19    13    12    14    13     9    12 

## Convert to proportions for venneuler()
ps <- counts/sum(counts)

## Calculate the Venn diagram
vd <- venneuler(c(a=ps[["a"]], b = ps[["b"]], c = ps[["c"]],
                  "a&b" = ps[["a&b"]],
                  "a&c" = ps[["a&c"]],
                  "b&c" = ps[["b&c"]],
                  "a&b&c" = ps[["a&b&c"]]))
## Plot it!
plot(vd)

#simple venneuler example 
require(venneuler)
#v <- venneuler(c(A=500, B=300, C=200, "A&B"=100, "A&C"=100, "B&C"=100, "A&B&C"=50))
#v <- venneuler(c(A=16071, B=15335, "A&B"=16071,))
plot(v)

#Venndiagram example
#library(VennDiagram)
#require(VennDiagram)
#venn.diagram(list(B = 1:1800, A = 1571:2020),fill = c("red", "green"),
#             alpha = c(0.5, 0.5), cex = 2,cat.fontface = 4,lty =2, fontfamily =3, 
#             filename = "trial2.emf");