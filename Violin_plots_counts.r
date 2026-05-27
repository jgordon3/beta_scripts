#Violin plots examples
#http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
library(ggplot2)

#data
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)

setwd("/Users/jgordon3/Downloads/")
filename="Violin.txt"
counts=read.table(filename, sep="\t", header = TRUE)
counts$cluster <- as.factor(counts$cluster)

# Basic violin plot
p <- ggplot(counts, aes(x=cluster, y=counts)) + 
  geom_violin()
p
# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(counts, aes(x=Cluster, y=Count)) + 
  geom_violin(trim=FALSE)

p + scale_x_discrete(limits=c("0.5", "2"))

# violin plot with mean points
p + stat_summary(fun=mean, geom="point", shape=23, size=2)
# violin plot with median points
p + stat_summary(fun=median, geom="point", size=2, color="red")

p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_violin(trim=FALSE)
p + stat_summary(fun.data="mean_sdl", mult=1, 
                 geom="crossbar", width=0.2 )
p + stat_summary(fun.data=mean_sdl, mult=1, 
                 geom="pointrange", color="red")

# violin plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# violin plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))

# Change violin plot line colors by groups
p<-ggplot(ToothGrowth, aes(x=dose, y=len, color=dose)) +
  geom_violin(trim=FALSE)
p

# Use custom color palettes
p+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
# Use grey scale
p + scale_color_grey() + theme_classic()

# Use single color
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()
# Change violin plot colors by groups
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  geom_violin(trim=FALSE)
p

# Basic violin plot
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(title="Plot of length  by dose",x="Dose (mg)", y = "Length")+
  geom_boxplot(width=0.1)+
  theme_classic()
# Change color by groups
dp <- ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Plot of length  by dose",x="Dose (mg)", y = "Length")
dp + theme_classic()