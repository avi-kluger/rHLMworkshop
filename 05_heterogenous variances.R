rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots

load("HLMbook.Rdata"); 

suppressMessages(library(nlme))

fm1 <- lme(fixed = mAch ~  cses*sector,
           data = x,
           random = ~ cses | school)

fm2 <- lme(fixed = mAch ~  cses*sector,
           data = x,
           random = ~ cses | school, 
           weights = varIdent(form = ~ 1 | sector))
           
summary(fm2)
anova(fm1, fm2)
