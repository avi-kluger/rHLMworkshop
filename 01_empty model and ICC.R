rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots

load("HLMbook.Rdata"); # Instead of running R code to read Hsb82 dataset
                       # It was saved in 00_downloadData.R with save.image, and 
                       # restored in the current session.

###--------------  lme4 ---------------------
if (!require("lme4")) install.packages("lme4"); library(lme4)

fm1     <- lmer(mAch ~ 1 + (1 | school), x)
summary(fm1)

as.data.frame(VarCorr(fm1))["vcov"]
ICC     <- as.data.frame(VarCorr(fm1))["vcov"][1, 1] /
           sum(as.data.frame(VarCorr(fm1))["vcov"]) 
cat("ICC = ", round(ICC, 2))


###--------------  nlme using lme function ---------------------
if (!require("nlme")) install.packages("nlme"); library(nlme)

mod1 <- lme(mAch ~ 1, random = ~ 1|school, x)
summary(mod1)
VarCorr(mod1)
varests <- as.numeric(VarCorr(mod1)[1:2])
ICC     <- varests[1]/sum(varests) # computes ICC
round(ICC, 2)                      # displays value

### Build ICC function. Based on http://davidakenny.net/papers/k&h/MLM_R.pdf
ICClme <- function(out) {
          varests <- as.numeric(VarCorr(out)[1:2])
          return(cat(paste("ICC =", round(varests[1]/sum(varests), 2))))
}

ICClme(mod1)

###------------  nlme using gls function ----
mod2    <- gls(mAch ~ 1,
               data = x,
               correlation = corCompSymm(form = ~ 1 | school))
summary(mod2)
methods(class=class(mod2))
intervals(mod2)
