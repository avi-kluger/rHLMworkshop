rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots

load("HLMbook.Rdata"); # Instead of running R code to read Hsb82 dataset
                       # I read it once, saved it with save.image, and now
                       # restore it to the current session.

# First step before testing Level 1 predictor is group centering
# This dataset have the ses already group centered, but here, we replicate
# the process with *dplyr* package.

library(dplyr)
x <- x %>% group_by(school) %>% dplyr::mutate(cent= ses-mean(ses))
all.equal(x$cses, x$cent)

###-------------- lme4 ---------------------

library(lme4)

# Repeat the null model and name it fm0
fm0     <- lmer(mAch ~ (1 | school), x)

# Test a natural Level 2 predictor
fm2     <- lmer(mAch ~ sector + (1 | school), x)
summary(fm2)

as.data.frame(VarCorr(fm0))
as.data.frame(VarCorr(fm2))

Explained.var <- (as.data.frame(VarCorr(fm0))[1, "vcov"] - 
                  as.data.frame(VarCorr(fm2))[1, "vcov"]) /
                  as.data.frame(VarCorr(fm0))[1, "vcov"]  

cat("Explained variance in achievent by sector at Level 2 = ", 
    round(Explained.var, 3)) 

anova(fm0, fm2)

# Create a  Level 2 predictor from a Level 1 preditor + group mean centering
grandMeanSES <- mean(x$ses, na.rm = TRUE)
library(dplyr)
x <- x %>% group_by(school) %>%
    dplyr::mutate(grandMeanCenteredSES = mean(ses, na.rm=TRUE) - grandMeanSES)
x
all.equal(x$meanses, x$grandMeanCenteredSES)

fm3     <- lmer(mAch ~ meanses + (1 | school), x)
summary(fm3)

Explained.var <- (as.data.frame(VarCorr(fm0))[1, "vcov"] - 
                  as.data.frame(VarCorr(fm3))[1, "vcov"]) /
                  as.data.frame(VarCorr(fm0))[1, "vcov"]  

cat("Explained variance in achievent by SES at Level 2 = ", 
    round(Explained.var, 3)) 

# Two Level 2 predictors

fm4     <- lmer(mAch ~ meanses + sector + (1 | school), x)
summary(fm4)

Explained.var <- (as.data.frame(VarCorr(fm0))[1, "vcov"] - 
                  as.data.frame(VarCorr(fm4))[1, "vcov"]) /
                  as.data.frame(VarCorr(fm0))[1, "vcov"]  

cat("Explained variance in achievent by SES and sector at Level 2 = ", 
    round(Explained.var, 3)) 
