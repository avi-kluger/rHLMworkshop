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
x <- x %>%
    group_by(school) %>%
    # Explicitly referring to dplyr prevents clash with plyr
    dplyr::mutate(groupCenteredSES = ses - mean(ses, na.rm=TRUE))
x
all.equal(x$cses, x$groupCenteredSES)


###-------------- lme4 ---------------------

library(lme4)

# Repeat the null model and name it fm0
fm0     <- lmer(mAch ~ (1 | school), x)

# Test Level 1 predictor
fm1     <- lmer(mAch ~ cses + (cses | school), x)
summary(fm1)
suppressWarnings(confint(fm1, oldNames = FALSE)) # Create warnings that are bugs

VarCorr(fm0)
as.data.frame(VarCorr(fm0))
as.data.frame(VarCorr(fm1))

Explained.var <- (as.data.frame(VarCorr(fm0))[2, 4] - 
                  as.data.frame(VarCorr(fm1))[4, 4]) /
                  as.data.frame(VarCorr(fm0))[2, 4]  

cat("Explained variance in achievent by SES at Level 1 = ", round(Explained.var, 3)) 

anova(fm0, fm1)

# Is the slope-intercept covariance needed?  Drop U1j (random slope)

fm2     <- lmer(mAch ~ cses + (1 | school), x)
anova(fm0, fm1, fm2)

###------------ nlme using lme function ---------
library(nlme)

mod0 <- lme(fixed = mAch ~ 1, random = ~ 1 |school, x)
summary(mod0)

mod1 <- lme(fixed = mAch ~ cses, random = ~ cses|school, x)
summary(mod1)
intervals(mod1)

###------------ nlme using gls function obtain partial ICC ---------
mod0    <-   gls(mAch ~ 1,
                 data = x,
                 correlation = corCompSymm(form = ~ 1 | school))
summary(mod0)

mod1    <-   gls(mAch ~ cses,
                 data = x,
                 correlation = corCompSymm(form = ~ cses | school))
summary(mod1)

###-------------- Inspect Slopes ------------------------
func <- function(x) return(data.frame(Slope = coef(lm(x$mAch ~ x$cses))[2]))
library(plyr)
Slopes <- ddply(x, .(school), func)
head (Slopes)
stem(Slopes$Slope)

###-------------- Plot Slopes ------------------------
library(mlmRev)
# prevent clashes
detach(lme4)

#### Visualization function coppied from
#### http://www.rensenieuwenhuis.nl/r-sessions-20-plotting-multilevel-models/

visualize.lme <- function (model, coefficient, group, ...) {
r <- ranef(model)
f <- fixef(model)

effects <- data.frame(r[,1]+f[1], r[,2]+f[2])

number.lines <- nrow(effects)

predictor.min <- tapply(model$data[[coefficient]], model$data[[group]], min)
predictor.max <- tapply(model$data[[coefficient]], model$data[[group]], max)

outcome.min <- min(predict(model))
outcome.max <- max(predict(model))

plot(c(min(predictor.min), max(predictor.max)), c(outcome.min, outcome.max), 
     type = "n", ...)

for (i in 1:number.lines)
{
expression <- function(x) {effects[i,1] + (effects[i,2] * x) }
curve(expression, from=predictor.min[i], to=predictor.max[i], add=TRUE)
}
}

mod1 <- lme(fixed = mAch ~ cses, random = ~ cses|school, x)
summary(mod1)
intervals(mod1)

visualize.lme(mod1, "cses", "school", 
              xlab="Student SES", 
              ylab="Math achievement", 
              main="Math achievement by student SES and school")

###-------------- Meta Analysis ------------------------

func <- function(x) return(data.frame(COR = cor(x$cses, x$mAch)))
fit  <- lm(x$mAch ~ x$ses)

MAinput <- ddply(x, .(school), func)
MAinput$n <- as.vector(table(x$school))
head (MAinput)
stem(MAinput$COR)

suppressMessages(library(meta))
metacor(COR, n, data = MAinput, comb.fixed=F, prediction=T)