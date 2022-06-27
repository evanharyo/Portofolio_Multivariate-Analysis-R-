packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","lavaan")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)

hs.mod <- '
factor1 =~ x3 + x8 + x1  +x2
factor2 =~ x6 + x5   
'
hs.fit <- cfa(hs.mod, data = data)
hs.fit
summary(hs.fit, fit.measures = TRUE, standardized = TRUE)
