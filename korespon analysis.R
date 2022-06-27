#aman
packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","factoextra","cluster","ggplot2","tree","class","car", "magrittr", "ggpubr", "MASS","smacof", "gplots", "ca", "reticulate", "FactoMineR", "ellipse")
install.packages(packages)
lapply(packages, library, character.only = TRUE)
library(c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","factoextra","cluster","ggplot2","tree","class","car", "magrittr", "ggpubr", "MASS","smacof", "gplots", "ca", "reticulate", "FactoMineR", "ellipse"))

#CORRESPONDENCE ANALYSIS
data3 <-  read.csv("C:/Users/Andri/Downloads/T16_17_BIRTHDEATH.csv", header=TRUE, sep=";")
data3

data33 <- data3 %>%  group_by(Birth,Death) %>%  summarise(count = sum(count))
data33$Birth = factor(data33$Birth, levels = month.name)
data33$Death = factor(data33$Death, levels = month.name)
data33<-with(data33, data33[order(Birth, Death, count),])
head(data33)

column_names <- c("D-Jan","D-Feb","D-Mar","D-Apr","D-May","D-Jun","D-Jul","D-Aug","D-Sep","D-Oct","D-Nov","D-Dec")
row_names <- c("B-Jan","B-Feb","B-Mar","B-Apr","B-May","B-Jun","B-Jul","B-Aug","B-Sep","B-Oct","B-Nov","B-Dec")
tabel3 <- matrix(data33$count,ncol=12,byrow=TRUE)
colnames(tabel3) <- column_names
rownames(tabel3) <- row_names
tabel3

#Matriks P atau Matriks Korespondensi
prop.table(tabel3)

#The value of each cell is divided by the sum of the row cells.
#Matriks R (Row)
prop.table(prop.table(tabel3), 1)

#The value of each cell is divided by the sum of the column cells.
#Matriks C (Column)
prop.table(prop.table(tabel3), 2)

library(ca)
fit <- ca(tabel3)
print(fit) # basic results

summary(fit) # extended results

plot(fit) # symmetric map
plot(fit, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map

#Uji Independensi antara kategori baris bulan kelahiran dan kolom bulan kematian
#H0 : baris bulan kelahiran dan kolom bulan kematian saling bebas
ca(tabel3)

#Yang ini ga usah dimasukin btw
#vektor r
#CA(tabel3)$call$marge.row
vek_r <- rowSums(tabel3)/sum(tabel3)

#vektor c
#CA(tabel3)$call$marge.col
vek_c <- colSums(tabel3)/sum(tabel3)

#Matriks Dr
mat_dr <- matrix(diag(vek_r),ncol=12) 

#Matriks Dc
mat_dc <- matrix(diag(vek_c),ncol=12)

#kalau vektor ri itu baris dari matriks R
#kalau vektor cj itu kolom dari matriks C

##Berganda
data4 <- read.delim("C:/Users/Andri/Downloads/T16_19_BYSSINOSIS.dat", header=TRUE)
head(data4)

library(FactoMineR)
mca <- MCA(data4[3:7], graph = FALSE)
mca

# table of eigenvalues
mca$eig

# column coordinates
head(mca$var$coord)

# row coordinates
head(mca$ind$coord)

# number of categories per variable
cats = apply(data4[3:7], 2, function(x) nlevels(as.factor(x)))
cats

# data frames for ggplot
mca_vars_df = data.frame(mca$var$coord, Variable = rep(names(cats), 
                                                       cats))
mca_obs_df = data.frame(mca$ind$coord)

# plot of variable categories
ggplot(data = mca_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, 
                                                             colour = "gray70") + geom_text(aes(colour = Variable)) + ggtitle("MCA plot of variables using R package FactoMineR")

# MCA plot of observations and categories
ggplot(data = mca_obs_df, aes(x = Dim.1, y = Dim.2)) + geom_hline(yintercept = 0, 
                                                                  colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + geom_point(colour = "gray50", 
                                                                  alpha = 0.7) + geom_density2d(colour = "gray80") + geom_text(data = mca_vars_df, 
                                                                  aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df), colour = Variable)) + 
                                                                  ggtitle("MCA plot of variables using R package FactoMineR") + scale_colour_discrete(name = "Variable")

# default biplot in FactoMineR
plot(mca)

