rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots

load("HLMbook.Rdata"); # Instead of running R code to read Hsb82 dataset
                       # I read it once, saved it with save.image, and now
                       # restore it to the current session.

###-------------- Level 1 and Level 2 as predictors with lme4 ------------------

library(lme4)

fm1     <- lmer(mAch ~ cses + meanses + sector + cses:meanses + cses:sector +
                  (1 + cses | school), x)
summary(fm1)
as.data.frame(VarCorr(fm1))
suppressWarnings(confint(fm1, oldNames = FALSE))
-2*logLik(fm1)

fm2     <- lmer(mAch ~ cses + meanses + sector + cses:meanses + cses:sector +
                  (1 | school), x)
as.data.frame(VarCorr(fm2))
-2*logLik(fm2)

-2*logLik(fm2) - (-2*logLik(fm1))
anova(fm1, fm2)
