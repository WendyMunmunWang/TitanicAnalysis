#data file path, comment out /add yours!
titanic <- read.delim("C:/Users/Jennifer She/Downloads/TitanicAnalysis-master(1)/TitanicAnalysis-master/cleaned_data.txt")
#titanic <- read.delim("~/Documents/STAT306/TitanicAnalysis/cleaned_data.txt") #wendy
View(titanic)

ntot=nrow(titanic)
names(train)
sum(titanic$Survived); mean(titanic$Survived)

#categorical data
table(titanic$Survived) #response variable
table(titanic$Pclass)
table(titanic$Sex)

#continuous/non-categorical data
summary(titanic$Age)
summary(titanic$SibSp)
summary(titanic$Parch)
summary(titanic$Fare)

#trasforming continuous/non-categorical data
titanic$sqrtAge=sqrt(titanic$Age)
titanic$sqrtSibSp=sqrt(titanic$SibSp)
titanic$sqrtParch=sqrt(titanic$Parch)
titanic$sqrtFare=sqrt(titanic$Fare)

titanic$logAge=log(1+titanic$Age)
titanic$logSibSp=log(1+titanic$SibSp)
titanic$logParch=log(1+titanic$Parch)
titanic$logFare=log(1+titanic$Fare)

hist(titanic$Age) 
hist(titanic$sqrtAge)
hist(titanic$logAge)
#non-transformed data is the most symmetric, and will be used

hist(titanic$SibSp)
hist(titanic$sqrtSibSp)
hist(titanic$logSibSp)
#non-transformed data is right skewed, but range is small enough and the transformations
#did not really help much, so the non-transformed data will be used

hist(titanic$Parch)
hist(titanic$sqrtParch)
hist(titanic$logParch)
#non-transformed data is right skewed, but range is small enough and the transformations
#did not really help much, so the non-transformed data will be used

hist(titanic$Fare)
hist(titanic$sqrtFare)
hist(titanic$logFare)
#non-transformed data is right skewed, and the log-transformed data seems to be the most
#symmetric, so the log-transformed data will be used

##plotting
#install.packages("ggplot2", dependencies = TRUE)
#library(ggplot2)
ggplot(titanic, aes(x=titanic$Age, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)
ggplot(titanic, aes(x=titanic$Parch, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)
ggplot(titanic, aes(x=titanic$logFare, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)
ggplot(titanic, aes(x=titanic$SibSp, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)
ggplot(titanic, aes(x=titanic$Pclass, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)
ggplot(titanic, aes(x=titanic$Sex, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)

set.seed(12345)
# 3000 for training set, 1521 for holdout
iperm=sample(ntot,ntot) # random permutation of 1...ntot
n=445
train=titanic[iperm[1:n],]
hold=titanic[iperm[(n+1):ntot],]

names(train)
sum(train$Survival); mean(train$Survival)

#correlation
attach(train)
summcor=cor(Survival,train[,c(3:7,16)])
print(summcor)
detach(train)

##fitting 1 explanatory variable
fit_Survival_Age=glm(train$Survived~train$Age, family="binomial", data=titanic)
summSurvival_Age=summary(fit_Survival_Age)
fit_Survival_Sex=glm(train$Survived~train$Sex, family="binomial", data=titanic)
summSurvival_Sex=summary(fit_Survival_Sex)
fit_Survival_SibSp=glm(train$Survived~train$SibSp, family="binomial", data=titanic)
summSurvival_SibSp=summary(fit_Survival_SibSp)
fit_Survival_Parch=glm(train$Survived~train$Parch, family="binomial", data=titanic)
summSurvival_Parch=summary(fit_Survival_Parch)
fit_Survival_logFare=glm(train$Survived~train$logFare, family="binomial", data=titanic)
summSurvival_logFare=summary(fit_Survival_logFare)
fit_Survival_Pclass=glm(train$Survived~train$Pclass, family="binomial", data=titanic)
summSurvival_Pclass=summary(fit_Survival_Pclass)
##fitting 2 explanatory variables
fit_Survival_AgeSex=glm(train$Survived~train$Age+train$Sex, family="binomial", data=titanic)
summSurvival_AgeSex=summary(fit_Survival_AgeSex)
fit_Survival_AgeSibSp=glm(train$Survived~train$Age+train$SibSp, family="binomial", data=titanic)
summSurvival_AgeSipSp=summary(fit_Survival_AgeSibSp)
fit_Survival_AgePclass=glm(train$Survived~train$Age+train$Pclass, family="binomial", data=titanic)
summSurvival_AgePclass=summary(fit_Survival_AgePclass)
fit_Survival_AgeParch=glm(train$Survived~train$Age+train$Parch, family="binomial", data=titanic)
summSurvival_AgeParch=summary(fit_Survival_AgeParch)
fit_Survival_AgelogFare=glm(train$Survived~train$Age+train$logFare, family="binomial", data=titanic)
summSurvival_AgelogFare=summary(fit_Survival_AgelogFare)

fit_Survival_SexPclass=glm(train$Survived~train$Pclass+train$Sex, family="binomial", data=titanic)
summSurvival_SexPclass=summary(fit_Survival_SexPclass)
fit_Survival_SexSibSp=glm(train$Survived~train$Sex+train$SibSp, family="binomial", data=titanic)
summSurvival_SexSipSp=summary(fit_Survival_SexSibSp)
fit_Survival_SexParch=glm(train$Survived~train$Sex+train$Parch, family="binomial", data=titanic)
summSurvival_SexParch=summary(fit_Survival_SexParch)
fit_Survival_SexlogFare=glm(train$Survived~train$Sex+train$logFare, family="binomial", data=titanic)
summSurvival_SexlogFare=summary(fit_Survival_SexParch)

fit_Survival_PclassSibSp=glm(train$Survived~train$Pclass+train$SibSp, family="binomial", data=titanic)
summSurvival_PclassSibSp=summary(fit_Survival_PclassSibSp)
fit_Survival_PclassParch=glm(train$Survived~train$Pclass+train$Parch, family="binomial", data=titanic)
summSurvival_PclassParch=summary(fit_Survival_PclassParch)
fit_Survival_PclasslogFare=glm(train$Survived~train$Pclass+train$logFare, family="binomial", data=titanic)
summSurvival_PclasslogFare=summary(fit_Survival_PclasslogFare)

fit_Survival_SibSplogFare=glm(train$Survived~train$logFare+train$SibSp, family="binomial", data=titanic)
summSurvival_SibSplogFare=summary(fit_Survival_SibSplogFare)
fit_Survival_SibSpParch=glm(train$Survived~train$SibSp+train$Parch, family="binomial", data=titanic)
summSurvival_SibSpParch=summary(fit_Survival_SibSpParch)

fit_Survival_logFareParch=glm(train$Survived~train$logFare+train$Parch, family="binomial", data=titanic)
summSurvival_logFareParch=summary(fit_Survival_logFareParch)

##fitting 3 explanatory variables
fit_Survival_AgeSexSibSp=glm(train$Survived~train$Age+train$Sex+train$SibSp, family="binomial", data=titanic)
summSurvival_AgeSexSibSp=summary(fit_Survival_AgeSexSibSp)
fit_Survival_AgeSexlogFare=glm(train$Survived~train$Age+train$Sex+train$logFare, family="binomial", data=titanic)
summSurvival_AgeSexlogFare=summary(fit_Survival_AgeSexlogFare)
fit_Survival_AgeSexParch=glm(train$Survived~train$Age+train$Sex+train$Parch, family="binomial", data=titanic)
summSurvival_AgeSexParch=summary(fit_Survival_AgeSexParch)
fit_Survival_AgeSexPclass=glm(train$Survived~train$Age+train$Sex+train$Pclass, family="binomial", data=titanic)
summSurvival_AgeSexPclass=summary(fit_Survival_AgeSexPclass)

fit_Survival_SexSibSplogFare=glm(train$Survived~train$logFare+train$Sex+train$SibSp, family="binomial", data=titanic)
summSurvival_SexSibSplogFare=summary(fit_Survival_SexSibSplogFare)
fit_Survival_SexSibSpParch=glm(train$Survived~train$SibSp+train$Sex+train$Parch, family="binomial", data=titanic)
summSurvival_SexSibSpParch=summary(fit_Survival_SexSibSpParch)
fit_Survival_SexSibSpPclass=glm(train$Survived~train$SibSp+train$Sex+train$Pclass, family="binomial", data=titanic)
summSurvival_SexSibSpPclass=summary(fit_Survival_SexSibSpPclass)

fit_Survival_ParchlogFareSibSp=glm(train$Survived~train$Parch+train$SibSp+train$logFare, family="binomial", data=titanic)
summSurvival_ParchlogFareSibSp=summary(fit_Survival_ParchlogFareSibSp)
fit_Survival_ParchlogFarePclass=glm(train$Survived~train$logFare+train$Pclass+train$Parch, family="binomial", data=titanic)
summSurvival_ParchlogFarePclass=summary(fit_Survival_ParchlogFarePclass)

fit_Survival_logFareSibSpPclass=glm(train$Survived~train$SibSp+train$Pclass+train$Pclass, family="binomial", data=titanic)
summSurvival_logFareSibSpPclass=summary(fit_Survival_logFareSibSpPclass)

##fitting 4 explanatory variables 
fit_Survival_AgeSexSibSpParch=glm(train$Survived~train$Age+train$Sex+train$SibSp+train$Parch, family="binomial", data=titanic)
summSurvival_AgeSexSibSpParch=summary(fit_Survival_AgeSexSibSpParch)
fit_Survival_AgeSexSibSplogFare=glm(train$Survived~train$Age+train$Sex+train$SibSp+train$logFare, family="binomial", data=titanic)
summSurvival_AgeSexSibSplogFare=summary(fit_Survival_AgeSexSibSplogFare)
fit_Survival_AgeSexSibSpPclass=glm(train$Survived~train$Age+train$Sex+train$SibSp+train$Pclass, family="binomial", data=titanic)
summSurvival_AgeSexSibSpPclass=summary(fit_Survival_AgeSexSibSpPclass)

fit_Survival_SexlogFarePclassSibSp=glm(train$Survived~train$SibSp+train$Sex+train$logFare+train$Pclass, family="binomial", data=titanic)
summSurvival_SexlogFarePclassSibSp=summary(fit_Survival_SexlogFarePclassSibSp)
fit_Survival_SexlogFarePclassParch=glm(train$Survived~train$Parch+train$Sex+train$logFare+train$Pclass, family="binomial", data=titanic)
summSurvival_SexlogFarePclassParch=summary(fit_Survival_SexlogFarePclassParch)

fit_Survival_logFarePclassSibSpParch=glm(train$Survived~train$Parch+train$SibSp+train$logFare+train$Pclass, family="binomial", data=titanic)
summSurvival_logFarePclassSibSpParch=summary(fit_Survival_logFarePclassSibSpParch)

##fitting 5 explanatory variables
fit_Survival_AgeSexParchSibSpPclass=glm(train$Survived~train$Age+train$Sex+train$Parch+train$SibSp+train$Pclass, family="binomial", data=titanic)
summSurvival_AgeSexParchSibSpPclass=summary(fit_Survival_AgeSexParchSibSpPclass)

fit_Survival_AgeSexParchSibSplogFare=glm(train$Survived~train$Age+train$Sex+train$Parch+train$SibSp+train$logFare, family="binomial", data=titanic)
summSurvival_AgeSexParchSibSplogFare=summary(fit_Survival_AgeSexParchSibSplogFare)

##fitting 6 explanatory variables
fit_Survival_AgeSexParchSibSpPclasslogFare=glm(train$Survived~train$Age+train$Sex+train$Parch+train$SibSp+train$logFare+train$Pclass, family="binomial", data=titanic)
summSurvival_AgeSexParchSibSpPclasslogFare=summary(fit_Survival_AgeSexParchSibSpPclasslogFare)

##model evaluation
subsetvec=c("Age", "Sex", "SibSp", "logFare", "Pclass", "Parch",
            "Age_Sex","Age_SibSp", "Age_Pclass", "Age_Parch", "Age_logFare",
            "Sex_logFare", "Sex_Parch", "Sex_Pclass", "Sex_SibSp",
            "Pclass_logFare", "Pclass_SibSp", "Pclass_Parch",
            "SibSp_logFare", "SibSp_Parch",
            "logFare_Parch",
            "Age_Sex_Pclass", "Age_Sex_logFare", "Age_Sex_Parch", "Age_Sex_SibSp",
            "Sex_SibSp_logFare", "Sex_SibSp_Parch", "Sex_SibSp_Pclass",
            "Parch_logFare_SibSp", "Parch_logFare_Pclass",
            "logFare_SibSp_Pclass",
            "Age_Sex_SibSp_Parch", "Age_Sex_SibSp_logFare", "Age_Sex_SibSp_Pclass",
            "Sex_logFare_Pclass_SibSp", "Sex_logFare_Pclass_Parch",
            "logFare_Pclass_SibSp_Parch",
            "Age_Sex_Parch_SibSp_Pclass", "Age_Sex_Parch_SibSp_logFare",
            "Age_Sex_Parch_SibSp_Pclass_logFare")

deviancevec=c(summSurvival_Age$deviance,summSurvival_Sex$deviance,summSurvival_SibSp$deviance,summSurvival_logFare$deviance, summSurvival_Pclass$deviance, summSurvival_Parch$deviance,
              summSurvival_AgeSex$deviance, summSurvival_AgeSipSp$deviance, summSurvival_AgePclass$deviance, summSurvival_AgeParch$deviance,summSurvival_AgelogFare$deviance,
              summSurvival_SexlogFare$deviance, summSurvival_SexParch$deviance, summSurvival_SexPclass$deviance, summSurvival_SexSipSp$deviance,
              summSurvival_PclasslogFare$deviance, summSurvival_PclassSibSp$deviance, summSurvival_PclassParch$deviance,
              summSurvival_SibSplogFare$deviance, summSurvival_SibSpParch$deviance, 
              summSurvival_logFareParch$deviance,
              summSurvival_AgeSexPclass$deviance, summSurvival_AgeSexlogFare$deviance, summSurvival_AgeSexParch$deviance, summSurvival_AgeSexSibSp$deviance,
              summSurvival_SexSibSplogFare$deviance, summSurvival_SexSibSpParch$deviance, summSurvival_SexSibSpPclass$deviance,
              summSurvival_ParchlogFareSibSp$deviance, summSurvival_ParchlogFarePclass$deviance,
              summSurvival_logFareSibSpPclass$deviance,
              summSurvival_AgeSexSibSpParch$deviance, summSurvival_AgeSexSibSplogFare$deviance, summSurvival_AgeSexSibSpPclass$deviance,
              summSurvival_SexlogFarePclassSibSp$deviance, summSurvival_SexlogFarePclassParch$deviance,
              summSurvival_logFarePclassSibSpParch$deviance,
              summSurvival_AgeSexParchSibSpPclass$deviance, summSurvival_AgeSexParchSibsplogFare$deviance, 
              summSurvival_AgeSexParchSibspPclasslogFare$deviance)

aicvec=c(summSurvival_Age$aic,summSurvival_Sex$aic,summSurvival_SibSp$aic,summSurvival_logFare$aic, summSurvival_Pclass$aic, summSurvival_Parch$aic,
         summSurvival_AgeSex$aic, summSurvival_AgeSipSp$aic, summSurvival_AgePclass$aic, summSurvival_AgeParch$aic,summSurvival_AgelogFare$aic,
         summSurvival_SexlogFare$aic, summSurvival_SexParch$aic, summSurvival_SexPclass$aic, summSurvival_SexSipSp$aic,
         summSurvival_PclasslogFare$aic, summSurvival_PclassSibSp$aic, summSurvival_PclassParch$aic,
         summSurvival_SibSplogFare$aic, summSurvival_SibSpParch$aic, 
         summSurvival_logFareParch$aic,
         summSurvival_AgeSexPclass$aic, summSurvival_AgeSexlogFare$aic, summSurvival_AgeSexParch$aic, summSurvival_AgeSexSibSp$aic,
         summSurvival_SexSibSplogFare$aic, summSurvival_SexSibSpParch$aic, summSurvival_SexSibSpPclass$aic,
         summSurvival_ParchlogFareSibSp$aic, summSurvival_ParchlogFarePclass$aic,
         summSurvival_logFareSibSpPclass$aic,
         summSurvival_AgeSexSibSpParch$aic, summSurvival_AgeSexSibSplogFare$aic, summSurvival_AgeSexSibSpPclass$aic,
         summSurvival_SexlogFarePclassSibSp$aic, summSurvival_SexlogFarePclassParch$aic,
         summSurvival_logFarePclassSibSpParch$aic,
         summSurvival_AgeSexParchSibSpPclass$aic, summSurvival_AgeSexParchSibSplogFare$aic, 
         summSurvival_AgeSexParchSibSpPclasslogFare$aic)

print(cbind(subsetvec, deviancevec, aicvec))

##best model summary based on deviance
summSurvival_AgeSexParchSibSpPclasslogFare

#best model summary based on AIC
summSurvival_AgeSexSibSpPclass

#compare fit_Survival_AgeSexSibSpPclass, fit_Survival_AgeSexParchSibSpPclasslogFare for misclassification rates
#in-sample misclassification
pred4=predict(fit_Survival_AgeSexSibSpPclass,type="response")
pred6=predict(fit_Survival_AgeSexParchSibSpPclasslogFare,type="response")

#compare mean of predictions vs mean of actual training data
print(summary(pred4));
print(summary(pred6))

#boundary of 0.5 0.3 0.1 for misclassification
tab4a=table(train$Survival,pred4>0.5)
tab4b=table(train$Survival,pred4>0.3)
tab4c=table(train$Survival,pred4>0.1)
tab6a=table(train$Survival,pred6>0.5)
tab6b=table(train$Survival,pred6>0.3)
tab6c=table(train$Survival,pred6>0.1)

#convert to rates
tab4a/apply(tab4a,1,sum);    tab6a/apply(tab6a,1,sum)       
tab4b/apply(tab4b,1,sum);    tab6b/apply(tab6b,1,sum)
tab4c/apply(tab4c,1,sum);    tab6c/apply(tab6c,1,sum)

#conclusion based on in-sample misclassifcation

# out-of-sample misclassication
pred4.hold=predict(fit_Survival_AgeSexSibSpPclass,type="response",newdata=hold)
pred6.hold=predict(fit_Survival_AgeSexParchSibSpPclasslogFare,type="response",newdata=hold)
htab4a=table(hold$Survival,pred4.hold>0.5)
htab4b=table(hold$Survival,pred4.hold>0.3)
htab4c=table(hold$Survival,pred4.hold>0.1)
htab6a=table(hold$Survival,pred6.hold>0.5)
htab6b=table(hold$Survival,pred6.hold>0.3)
htab6c=table(hold$Survival,pred6.hold>0.1)

#convert to rates
htab4a/apply(htab4a,1,sum);   htab6a/apply(htab6a,1,sum)
htab4b/apply(htab4b,1,sum);   htab6b/apply(htab6b,1,sum)
htab4c/apply(htab4c,1,sum);   htab6c/apply(htab6c,1,sum)

#conclusion based on out-of-sample misclassification

# Hosmer-Lemeshow calibration check
prcateg4=cut(pred4,breaks=c(0,.01,.02,.03,.04,.06,.08,.10,.13,.2,.5,1))
print(table(prcateg4))

HLsumm4=tapply(train$Survival,prcateg4,mean)
print(HLsumm4)

prcateg6=cut(pred6,breaks=c(0,.01,.02,.03,.04,.06,.08,.10,.13,.2,.5,1))
print(table(prcateg6))

HLsumm9=tapply(train$Survival,prcateg6,mean)
print(HLsumm6)

#conclusion based on calibration of fit

#quadratic model <- add quadratic terms to best linear model
#do summary for quadratic model (deviance, AIC)
#in-sample/out-of-sample misclassification for quadratic model
#calibration of fit for quadratic model

#conclusion of quadratic model vs best model model