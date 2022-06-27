packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","factoextra","cluster","ggplot2","tree","class","car", "magrittr", "ggpubr", "MASS","smacof", "gplots", "ca", "reticulate", "FactoMineR", "ellipse")
install.packages("Hmisc")
install.packages("backports")
install.packages(packages)
lapply(packages, library, character.only = TRUE)
library(c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","factoextra","cluster","ggplot2","tree","class","car", "magrittr", "ggpubr", "MASS","smacof", "gplots", "ca", "reticulate", "FactoMineR", "ellipse"))



#MULTIDIMENSIONAL SCALING
data1<- as.data.frame(as.matrix(read.csv("C:/Users/Andri/Downloads/fuel_cons.csv")))
colnames(data1) <- rownames(data1)
head(data1)

library(dplyr)
library(mds)
library(magrittr)
#Metric
# Cmpute MDS
data11 <- as.dist(data1)
mds1 <- data11 %>% cmdscale() %>% as_tibble()
colnames(mds1) <- c("Dim.1", "Dim.2")

mds11 <- mds(data11,ndim = 2, type = "ratio" )
mds11

options(repr.plot.width = 10, repr.plot.height = 7, repr.plot.res = 100)
# Plot MDS
ggscatter(mds1, x = "Dim.1", y = "Dim.2", 
          label = rownames(data1),
          size = 1,
          repel = TRUE) + geom_vline(xintercept = 0) + geom_hline(yintercept=0)

#Non Metric
data2 <- as.data.frame(as.matrix(read.csv("C:/Users/Andri/Downloads/fuel_cons.csv")))
colnames(data2) <- rownames(data2)
head(data2)

# Cmpute MDS
data22 <- as.dist(t(data2))
mds2 <- data22 %>% isoMDS() %>% .$points %>% as_tibble()
colnames(mds2) <- c("Dim.1", "Dim.2")

scree.plot = function(d, k) {
  stresses=sammon(d, k=k)$stress
  for(i in rev(seq(k-1)))  
    stresses=append(stresses,sammon(d, k=i)$stress)
  plot(seq(k),rev(stresses), type="b", xaxp=c(1,k, k-1), ylab="Stress", xlab="Number of dimensions")
}
scree.plot(data22, k=5)

options(repr.plot.width = 10, repr.plot.height = 7, repr.plot.res = 100)
# Plot MDS
library(ggpubr)
ggscatter(mds2, x = "Dim.1", y = "Dim.2", 
          label = rownames(data2),
          size = 1,
          repel = TRUE) + geom_vline(xintercept = 0) + geom_hline(yintercept=0)

?mds

