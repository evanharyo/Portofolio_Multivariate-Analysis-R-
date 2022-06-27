#aman
packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","factoextra","cluster","ggplot2","tree","class","CCA","vegan","candisc")
install.packages(packages)
lapply(packages, library, character.only = TRUE)



data2 <- read.csv("C:/Users/Andri/Downloads/AirQualityUCI.csv", header=TRUE, sep=";")
data2
databaru=data2[-1:-2]
databaru
datafinal = na.omit(databaru)
head(datafinal)

X <- datafinal[1:10]
Y <- datafinal[11:13]
head(X)
head(Y)

library("CCA")
correl <- matcor(X, Y )
correl
img.matcor(correl, type = 2)

cc1 <- cancor(X, Y)  ### function from standard R instalation
cc2 <- cc(X, Y)      ### function for the R package 'CCA'

cc1
cc2

par(mfrow = c(1,2))
barplot(cc1$cor, main = "Canonical correlations for 'cancor()'", col = "gray")
barplot(cc2$cor, main = "Canonical correlations for 'cancor()'", col = "gray")

cc1$xcoef  ### function from standard R instalation

plt.cc(cc2, var.label = TRUE)

# ANALISIS KORELASI KANONIK
ccan <- candisc::cancor(X,Y)
summary(ccan)

# Korelasi kanonik
res.cc <- cc(X,Y) 
res.cc

# Plot korelasi
plot(res.cc$cor,type="b") 

