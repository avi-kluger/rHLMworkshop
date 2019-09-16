rm(list = ls())                               #Clean the Global Environment
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
cat ("\014")                                  #Clean the R console

# browseURL("http://rpsychologist.com/r-guide-longitudinal-lme-lmer")
library("nlme")
library("dplyr")

# This dataset is based on real data provided by a former student
# I introduced random noise to some variables to make useful only
# for instruction.  I centered the time variable on the first meeting
x <- read.csv("semiFictious.csv", stringsAsFactors = FALSE)
data <- na.omit(x)[, -1]


data     <- data %>% group_by(subjects) %>% 
  mutate(TA = scale(T.alliance, scale = FALSE),
         PA = scale(P.alliance, scale = FALSE)) #Center x and add to data
data     <- data %>% group_by(therapist) %>% mutate(TAbar = mean(T.alliance),
                                               PAbar = mean(P.alliance)) 
data$TAbar <- data$TAbar-mean(data$TAbar) #Center W
data$PAbar <- data$TAbar-mean(data$PAbar) #Center W

m1 <- lme(y ~ 1, random = ~ 1 | subjects, data=data)
summary(m1)

### Build ICC function. Based on http://davidakenny.net/papers/k&h/MLM_R.pdf
ICClme <- function(out) {
  varests <- as.numeric(VarCorr(out)[1:2])
  return(paste("ICC =", round(varests[1]/sum(varests), 2)))
}

ICClme(m1)

m2 <- lme(y ~ time, random = ~ time | subjects, data=data)
summary(m2)
intervals(m2)

#Drop intercept-slope covariance
m3 <- lme(y ~ time, 
          random = list(subjects = pdDiag(~time)), 
          data=data)
summary(m3)
intervals(m3)

new.data.grp <- groupedData(y ~ time | subjects, data = data)

m3 <- lme(y ~ time, 
          random = list(subjects = pdDiag(~time)), 
          data=new.data.grp)
plot(augPred(m3, level = 0:1))

m4 <- lme(y ~ time*TA, 
          random = list(subjects = pdDiag(~time)), 
          data=data)
summary(m4)


m5 <- lme(y ~ time+TA, 
          random = list(subjects = pdDiag(~time)), 
          data=data)
summary(m5)


table(data$therapist, data$subjects)
level3 <- table(data$therapist, data$subjects)
rowSums(level3 != 0)

m6 <- lme(y ~ time, 
          random =  ~time | therapist/subjects, 
          data=data)
summary(m6)
VarCorr(m6)

m7 <- lme(y ~ time, 
          random =  list(therapist = pdDiag(~time),
                         subjects = pdDiag(~time)), 
          data=data)
summary(m7)
intervals(m7)

m8 <- lme(y ~ time*TAbar, 
          random = list(subjects = pdDiag(~time)), 
          data=data)
summary(m8)

m9 <- lme(y ~ time + TAbar, 
        random = ~ time | subjects, 
        data=data)
summary(m9)


# First-order Autoregressive AR(1) residuals

m10 <- lme(y ~ time + TAbar, 
        random = ~ time | subjects, 
        correlation = corAR1(),
        data=data,
        control = lmeControl(opt='optim'))
summary(m10)


# Heterogenous AR(1)
m11 <- lme(y ~ time + TAbar, 
        random = ~ time | subjects, 
        weights = varIdent(form= ~ 1 | time), 
        correlation = corAR1(),
        data=data,
        control = lmeControl(opt='optim'))
summary(m11)

anova(m9, m10, m11)

m12 <- lme(y ~ time +I(time^2), 
          random = list(subjects = pdDiag(~time + I(time^2))), 
          data=new.data.grp)
summary(m12)


m13 <- lme(y ~ time + PA + PAbar, 
        random = ~ time + PA | subjects, 
        weights = varIdent(form= ~ 1 | time), 
        correlation = corAR1(),
        data=data,
        control = lmeControl(opt='optim'))
summary(m13)
intervals(m13)

m14 <- lme(y ~ time + PA , 
        random = list(subjects = pdDiag(~time + PA)), 
        # weights = varIdent(form= ~ 1 | time), 
        # correlation = corAR1(),
        data=data,
        control = lmeControl(opt='optim'))
summary(m14)
intervals(m14)

m15 <- lme(y ~ PA * time , 
        random = list(subjects = pdDiag(~time + PA)), 
        weights = varIdent(form= ~ 1 | time), 
        correlation = corAR1(),
        data=data,
        control = lmeControl(opt='optim'))
summary(m15)


#Code creates graphs in pdf format in the same directory as the data file

gammas=fixef(m15)

# pdf('intplot.xw.pdf',width=10,height=8)

par(mar=c(3.25,3.25, 4,.5),cex=2,bty='l',las=1,family='serif',mgp=c(1.85,.5,0))

#Figure 3 Panel (a) - Full Y Scale

Wjs = c(0, 7, 15)
xlb = mean(data$PA)-sd(data$PA); xub = mean(data$PA)+sd(data$PA)
ylb=60; yub=85

curve(0+1*x,xlb,xub,xlab='Patient Working Alliance',
      ylab='Symptoms',lwd=2,type='n',
      ylim=c(ylb,yub))
for(i in 1:length(Wjs)){
  B0j=gammas[1]+gammas[3]*Wjs[i]
  B1j=gammas[2]+gammas[4]*Wjs[i]
  curve(B0j+B1j*x,xlb,xub,add=T,lwd=2,lty=i)
}                       
labs=c(expression("Week 1"),expression("Week 8"), expression("Week 16"))
legend(xlb, 77,legend=c("Session",labs[1],labs[2], labs[3]),bty='n',lty=c(0:3))
title("Symptoms\n by Patient Working Alliance and Session", line=2)



m16 <- lme(TA ~ time + PA , 
        random = list(subjects = pdDiag(~time + PA)), 
        weights = varIdent(form= ~ 1 | time), 
        correlation = corAR1(),
        data=data,
        control = lmeControl(opt='optim'))
summary(m16)


new.data.grp <- groupedData(TA ~ time | subjects, data = data)

m3 <- lme(TA ~ time, 
          random = list(subjects = pdDiag(~time)), 
          data=new.data.grp)
plot(augPred(m3, level = 0:1))

m17 <- lme(PA ~ time + I(time^2)  , 
        random = list(subjects = pdDiag(~time )), 
        weights = varIdent(form= ~ 1 | time), 
        correlation = corAR1(),
        data=data,
        control = lmeControl(opt='optim'))
summary(m17)

