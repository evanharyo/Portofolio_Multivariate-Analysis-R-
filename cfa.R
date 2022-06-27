packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","lavaan")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)

hs.mod <- '
factor1 =~ y1 + y2 + x2 + x3 + x4 + x5 + x8  
factor2 =~ x1 + y2 + x2 + x3 + x4 + x5 + x8
factor3 =~ x6 + y2 + x2 + x3 + x4 + x5 + x8
factor4 =~ x7 + y2 + x2 + x3 + x4 + x5 + x8
'
hs.fit <- cfa(hs.mod, data = data)
hs.fit
summary(hs.fit, fit.measures = TRUE, standardized = TRUE)