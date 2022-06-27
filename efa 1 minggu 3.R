library(tidyverse)
library(psych)
library(lavaan)
library(semPlot)

KMO(data)
cortest.bartlett(data)

#EFA
hs.efa <- fa(data, nfactors = 10, 
             rotate = "none")
hs.efa

plot(hs.efa$e.values)
fa.parallel(data, fa = "fa", fm = "ml")

hs.efa2 <- fa(data, nfactors = 2, rotate = "target", fm = "ml")
hs.efa2

hs.efa2$loadings
loads <- hs.efa2$loadings

fa.diagram(loads)
