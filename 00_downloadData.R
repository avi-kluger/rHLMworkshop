rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots

if (!require("mlmRev")) install.packages("mlmRev")
library(mlmRev)

x <- Hsb82
save.image("HLMbook.Rdata")