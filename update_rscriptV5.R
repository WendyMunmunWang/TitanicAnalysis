#data file path, comment out /add yours!
#titanic_na <- read.delim("~/Desktop/STAT306/TitanicAnalysis/cleaned_data.txt") 
titanic_na <- read.delim("C:/Users/Jennifer She/Downloads/TitanicAnalysis-master(1)/TitanicAnalysis-master/cleaned_data.txt")
#titanic_na <- read.delim("~/Documents/STAT306/TitanicAnalysis/cleaned_data.txt") #wendy

# remove rows where Age is NA
titanic=na.omit(titanic_na)
View(titanic)

ntot=nrow(titanic)
names(train)
sum(titanic$Survived)
mean(titanic$Survived)

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

##histograms/deciding on transformations for categorical/non-categorical data
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
library(ggplot2)

#$ boxplots for continuous/non-categorical variables
plot(factor(titanic$Survived),titanic$Age)
plot(factor(titanic$Survived),titanic$Parch)
plot(factor(titanic$Survived),titanic$SibSp)
plot(factor(titanic$Survived),titanic$logFare)

## tables for categorical variables
table(factor(titanic$Survived),factor(titanic$Pclass))
table(factor(titanic$Survived),factor(titanic$Sex))

## bargraphs for categorical variables
titanic$notSurvived=1-titanic$Survived
ggplot(titanic, aes(x=titanic$Pclass, y=titanic$Survived)) + geom_bar(stat="identity")
ggplot(titanic, aes(x=titanic$Sex, y=titanic$Survived)) + geom_bar(stat="identity")
ggplot(titanic, aes(x=titanic$Pclass, y=titanic$notSurvived)) + geom_bar(stat="identity")
ggplot(titanic, aes(x=titanic$Sex, y=titanic$notSurvived)) + geom_bar(stat="identity")

set.seed(12345)
# 357 for training set, 357 for holdout
iperm=sample(ntot,ntot) # random permutation of 1...ntot
n=357
train=titanic[iperm[1:n],]
hold=titanic[iperm[(n+1):ntot],]
View(train)
View(hold)

names(train)
sum(train$Survived)
mean(train$Survived)

#correlation
attach(train)
summcor=cor(Survived,train[,c(3:7,16)])
print(summcor)
detach(train)


## We tried variable selection manually first
##fitting 1 explanatory variable
fit_Survival_Age=glm(Survived~Age, family="binomial", data=train)
summSurvival_Age=summary(fit_Survival_Age)
fit_Survival_Sex=glm(Survived~factor(Sex), family="binomial", data=train)
summSurvival_Sex=summary(fit_Survival_Sex)
fit_Survival_SibSp=glm(Survived~SibSp, family="binomial", data=train)
summSurvival_SibSp=summary(fit_Survival_SibSp)
fit_Survival_Parch=glm(Survived~Parch, family="binomial", data=train)
summSurvival_Parch=summary(fit_Survival_Parch)
fit_Survival_logFare=glm(Survived~logFare, family="binomial", data=train)
summSurvival_logFare=summary(fit_Survival_logFare)
fit_Survival_Pclass=glm(Survived~factor(Pclass), family="binomial", data=train)
summSurvival_Pclass=summary(fit_Survival_Pclass)
##fitting 2 explanatory variables
fit_Survival_AgeSex=glm(Survived~Age+factor(Sex), family="binomial", data=train)
summSurvival_AgeSex=summary(fit_Survival_AgeSex)
fit_Survival_AgeSibSp=glm(Survived~Age+SibSp, family="binomial", data=train)
summSurvival_AgeSipSp=summary(fit_Survival_AgeSibSp)
fit_Survival_AgePclass=glm(Survived~Age+factor(Pclass), family="binomial", data=train)
summSurvival_AgePclass=summary(fit_Survival_AgePclass)
fit_Survival_AgeParch=glm(Survived~Age+Parch, family="binomial", data=train)
summSurvival_AgeParch=summary(fit_Survival_AgeParch)
fit_Survival_AgelogFare=glm(Survived~Age+logFare, family="binomial", data=train)
summSurvival_AgelogFare=summary(fit_Survival_AgelogFare)

fit_Survival_SexPclass=glm(Survived~factor(Pclass)+factor(Sex), family="binomial", data=train)
summSurvival_SexPclass=summary(fit_Survival_SexPclass)
fit_Survival_SexSibSp=glm(Survived~factor(Sex)+SibSp, family="binomial", data=train)
summSurvival_SexSipSp=summary(fit_Survival_SexSibSp)
fit_Survival_SexParch=glm(Survived~factor(Sex)+Parch, family="binomial", data=train)
summSurvival_SexParch=summary(fit_Survival_SexParch)
fit_Survival_SexlogFare=glm(Survived~factor(Sex)+logFare, family="binomial", data=train)
summSurvival_SexlogFare=summary(fit_Survival_SexParch)

fit_Survival_PclassSibSp=glm(Survived~factor(Pclass)+SibSp, family="binomial", data=train)
summSurvival_PclassSibSp=summary(fit_Survival_PclassSibSp)
fit_Survival_PclassParch=glm(Survived~factor(Pclass)+Parch, family="binomial", data=train)
summSurvival_PclassParch=summary(fit_Survival_PclassParch)
fit_Survival_PclasslogFare=glm(Survived~factor(Pclass)+logFare, family="binomial", data=train)
summSurvival_PclasslogFare=summary(fit_Survival_PclasslogFare)

fit_Survival_SibSplogFare=glm(Survived~logFare+SibSp, family="binomial", data=train)
summSurvival_SibSplogFare=summary(fit_Survival_SibSplogFare)
fit_Survival_SibSpParch=glm(Survived~SibSp+Parch, family="binomial", data=train)
summSurvival_SibSpParch=summary(fit_Survival_SibSpParch)

fit_Survival_logFareParch=glm(Survived~logFare+Parch, family="binomial", data=train)
summSurvival_logFareParch=summary(fit_Survival_logFareParch)

##fitting 3 explanatory variables
fit_Survival_AgeSexSibSp=glm(Survived~Age+factor(Sex)+SibSp, family="binomial", data=train)
summSurvival_AgeSexSibSp=summary(fit_Survival_AgeSexSibSp)
fit_Survival_AgeSexlogFare=glm(Survived~Age+factor(Sex)+logFare, family="binomial", data=train)
summSurvival_AgeSexlogFare=summary(fit_Survival_AgeSexlogFare)
fit_Survival_AgeSexParch=glm(Survived~Age+factor(Sex)+Parch, family="binomial", data=train)
summSurvival_AgeSexParch=summary(fit_Survival_AgeSexParch)
fit_Survival_AgeSexPclass=glm(Survived~Age+factor(Sex)+factor(Pclass), family="binomial", data=train)
summSurvival_AgeSexPclass=summary(fit_Survival_AgeSexPclass)

fit_Survival_AgeSibSpPclass=glm(Survived~Age+SibSp+factor(Pclass), family="binomial", data=train)
summSurvival_AgeSibSpPclass=summary(fit_Survival_AgeSibSpPclass)
fit_Survival_AgeSibSplogFare=glm(Survived~Age+SibSp+logFare, family="binomial", data=train)
summSurvival_AgeSibSplogFare=summary(fit_Survival_AgeSibSplogFare)
fit_Survival_AgeSibSpParch=glm(Survived~Age+SibSp+Parch, family="binomial", data=train)
summSurvival_AgeSibSpParch=summary(fit_Survival_AgeSibSpParch)

fit_Survival_AgePclasslogFare=glm(Survived~Age+factor(Pclass)+logFare, family="binomial", data=train)
summSurvival_AgePclasslogFare=summary(fit_Survival_AgePclasslogFare)
fit_Survival_AgePclassParch=glm(Survived~Age+factor(Pclass)+Parch, family="binomial", data=train)
summSurvival_AgePclassParch=summary(fit_Survival_AgePclassParch)

fit_Survival_AgelogFareParch=glm(Survived~Age+logFare+Parch, family="binomial", data=train)
summSurvival_AgelogFareParch=summary(fit_Survival_AgelogFareParch)

fit_Survival_SexSibSplogFare=glm(Survived~logFare+factor(Sex)+SibSp, family="binomial", data=train)
summSurvival_SexSibSplogFare=summary(fit_Survival_SexSibSplogFare)
fit_Survival_SexSibSpParch=glm(Survived~SibSp+factor(Sex)+Parch, family="binomial", data=train)
summSurvival_SexSibSpParch=summary(fit_Survival_SexSibSpParch)
fit_Survival_SexSibSpPclass=glm(Survived~SibSp+factor(Sex)+factor(Pclass), family="binomial", data=train)
summSurvival_SexSibSpPclass=summary(fit_Survival_SexSibSpPclass)

fit_Survival_SexPclasslogFare=glm(Survived~factor(Sex)+factor(Pclass)+logFare, family="binomial", data=train)
summSurvival_SexPclasslogFare=summary(fit_Survival_SexPclasslogFare)
fit_Survival_SexPclassParch=glm(Survived~factor(Sex)+factor(Pclass)+Parch, family="binomial", data=train)
summSurvival_SexPclassParch=summary(fit_Survival_SexPclassParch)

fit_Survival_SexlogFareParch=glm(Survived~factor(Sex)+logFare+Parch, family="binomial", data=train)
summSurvival_SexlogFareParch=summary(fit_Survival_SexlogFareParch)

fit_Survival_ParchlogFareSibSp=glm(Survived~Parch+SibSp+logFare, family="binomial", data=train)
summSurvival_ParchlogFareSibSp=summary(fit_Survival_ParchlogFareSibSp)
fit_Survival_ParchlogFarePclass=glm(Survived~logFare+factor(Pclass)+Parch, family="binomial", data=train)
summSurvival_ParchlogFarePclass=summary(fit_Survival_ParchlogFarePclass)

fit_Survival_ParchPclassSibSp=glm(Survived~Parch+factor(Pclass)+SibSp, family="binomial", data=train)
summSurvival_ParchPclassSibSp=summary(fit_Survival_ParchPclassSibSp)

fit_Survival_logFareSibSpPclass=glm(Survived~SibSp+logFare+factor(Pclass), family="binomial", data=train)
summSurvival_logFareSibSpPclass=summary(fit_Survival_logFareSibSpPclass)

##fitting 4 explanatory variables 
fit_Survival_AgeSexSibSpParch=glm(Survived~Age+factor(Sex)+SibSp+Parch, family="binomial", data=train)
summSurvival_AgeSexSibSpParch=summary(fit_Survival_AgeSexSibSpParch)
fit_Survival_AgeSexSibSplogFare=glm(Survived~Age+factor(Sex)+SibSp+logFare, family="binomial", data=train)
summSurvival_AgeSexSibSplogFare=summary(fit_Survival_AgeSexSibSplogFare)
fit_Survival_AgeSexSibSpPclass=glm(Survived~Age+factor(Sex)+SibSp+factor(Pclass), family="binomial", data=train)
summSurvival_AgeSexSibSpPclass=summary(fit_Survival_AgeSexSibSpPclass)

fit_Survival_AgeSexPclasslogFare=glm(Survived~Age+factor(Sex)+factor(Pclass)+logFare, family="binomial", data=train)
summSurvival_AgeSexPclasslogFare=summary(fit_Survival_AgeSexPclasslogFare)
fit_Survival_AgeSexPclassParch=glm(Survived~Age+factor(Sex)+factor(Pclass)+Parch, family="binomial", data=train)
summSurvival_AgeSexPclassParch=summary(fit_Survival_AgeSexPclassParch)

fit_Survival_AgeSexlogFareParch=glm(Survived~Age+factor(Sex)+logFare+Parch, family="binomial", data=train)
summSurvival_AgeSexlogFareParch=summary(fit_Survival_AgeSexlogFareParch)

fit_Survival_AgeSibSpPclasslogFare=glm(Survived~Age+SibSp+factor(Pclass)+logFare, family="binomial", data=train)
summSurvival_AgeSibSpPclasslogFare=summary(fit_Survival_AgeSibSpPclasslogFare)
fit_Survival_AgeSibSpPclassParch=glm(Survived~Age+SibSp+factor(Pclass)+Parch, family="binomial", data=train)
summSurvival_AgeSibSpPclassParch=summary(fit_Survival_AgeSibSpPclassParch)

fit_Survival_AgeSibSplogFareParch=glm(Survived~Age+SibSp+logFare+Parch, family="binomial", data=train)
summSurvival_AgeSibSplogFareParch=summary(fit_Survival_AgeSibSplogFareParch)

fit_Survival_AgePclasslogFareParch=glm(Survived~Age+factor(Pclass)+logFare+Parch, family="binomial", data=train)
summSurvival_AgePclasslogFareParch=summary(fit_Survival_AgePclasslogFareParch)

fit_Survival_SexlogFarePclassSibSp=glm(Survived~SibSp+factor(Sex)+logFare+factor(Pclass), family="binomial", data=train)
summSurvival_SexlogFarePclassSibSp=summary(fit_Survival_SexlogFarePclassSibSp)
fit_Survival_SexlogFarePclassParch=glm(Survived~Parch+factor(Sex)+logFare+factor(Pclass), family="binomial", data=train)
summSurvival_SexlogFarePclassParch=summary(fit_Survival_SexlogFarePclassParch)

fit_Survival_SexlogFareSibspParch=glm(Survived~factor(Sex)+logFare+SibSp+Parch, family="binomial", data=train)
summSurvival_SexlogFareSibspParch=summary(fit_Survival_SexlogFareSibspParch)

fit_Survival_SexPclassSibspParch=glm(Survived~factor(Sex)+factor(Pclass)+SibSp+Parch, family="binomial", data=train)
summSurvival_SexPclassSibspParch=summary(fit_Survival_SexPclassSibspParch)

fit_Survival_logFarePclassSibSpParch=glm(Survived~Parch+SibSp+logFare+factor(Pclass), family="binomial", data=train)
summSurvival_logFarePclassSibSpParch=summary(fit_Survival_logFarePclassSibSpParch)

##fitting 5 explanatory variables
fit_Survival_AgeSexParchSibSpPclass=glm(Survived~Age+factor(Sex)+Parch+SibSp+factor(Pclass), family="binomial", data=train)
summSurvival_AgeSexParchSibSpPclass=summary(fit_Survival_AgeSexParchSibSpPclass)

fit_Survival_AgeSexParchSibSplogFare=glm(Survived~Age+factor(Sex)+Parch+SibSp+logFare, family="binomial", data=train)
summSurvival_AgeSexParchSibSplogFare=summary(fit_Survival_AgeSexParchSibSplogFare)

fit_Survival_AgeSexSibSpPclasslogFare=glm(Survived~Age+factor(Sex)+SibSp+factor(Pclass)+logFare, family="binomial", data=train)
summSurvival_AgeSexSibSpPclasslogFare=summary(fit_Survival_AgeSexSibSpPclasslogFare)

fit_Survival_AgeSexParchPclasslogFare=glm(Survived~Age+factor(Sex)+Parch+factor(Pclass)+logFare, family="binomial", data=train)
summSurvival_AgeSexParchPclasslogFare=summary(fit_Survival_AgeSexParchPclasslogFare)

fit_Survival_AgeParchSibSpPclasslogFare=glm(Survived~Age+Parch+SibSp+factor(Pclass)+logFare, family="binomial", data=train)
summSurvival_AgeParchSibSpPclasslogFare=summary(fit_Survival_AgeParchSibSpPclasslogFare)

fit_Survival_SexParchSibSpPclasslogFare=glm(Survived~factor(Sex)+Parch+SibSp+factor(Pclass)+logFare, family="binomial", data=train)
summSurvival_SexParchSibSpPclasslogFare=summary(fit_Survival_SexParchSibSpPclasslogFare)

##fitting 6 explanatory variables
fit_Survival_AgeSexParchSibSpPclasslogFare=glm(Survived~Age+factor(Sex)+Parch+SibSp+logFare+factor(Pclass), family="binomial", data=train)
summSurvival_AgeSexParchSibSpPclasslogFare=summary(fit_Survival_AgeSexParchSibSpPclasslogFare)

##model evaluation
subsetvec=c("Age", "Sex", "SibSp", "logFare", "Pclass", "Parch",
            
            "Age_Sex","Age_SibSp", "Age_Pclass", "Age_Parch", "Age_logFare",
            "Sex_logFare", "Sex_Parch", "Sex_Pclass", "Sex_SibSp",
            "Pclass_logFare", "Pclass_SibSp", "Pclass_Parch",
            "SibSp_logFare", "SibSp_Parch",
            "logFare_Parch",
            
            "Age_Sex_Pclass", "Age_Sex_logFare", "Age_Sex_Parch", "Age_Sex_SibSp",
            "Sex_SibSp_logFare", "Sex_SibSp_Parch", "Sex_SibSp_Pclass","Parch_logFare_SibSp", 
            "Parch_logFare_Pclass","logFare_SibSp_Pclass","Age_SibSp_Pclass", "Age_SibSp_logFare", 
            "Age_SibSp_Parch","Age_Pclass_logFare", "Age_Pclass_Parch", "Age_logFare_Parch", 
            "Sex_Pclass_logFare","Sex_Pclass_Parch", "Sex_logFare_Parch", "Parch_Pclass_SibSp",
            
            "Age_Sex_SibSp_Parch", "Age_Sex_SibSp_logFare", "Age_Sex_SibSp_Pclass",
            "Sex_logFare_Pclass_SibSp", "Sex_logFare_Pclass_Parch","logFare_Pclass_SibSp_Parch",
            "Age_Sex_Pclass_logFare", "Age_Sex_Pclass_Parch","Age_Sex_logFare_Parch",
            "Age_SibSp_Pclass_logFare", "Age_SibSp_Pclass_Parch","Age_SibSp_logFare_Parch",
            "Age_Pclass_logFare_Parch", "Sex_logFare_Sibsp_Parch", "Sex_Pclass_Sibsp_Parch",
            
            "Age_Sex_Parch_SibSp_Pclass", "Age_Sex_Parch_SibSp_logFare", "Age_Sex_SibSp_Pclass_logFare",
            "Age_Sex_Parch_Pclass_logFare", "Age_Parch_SibSp_Pclass_logFare", "Sex_Parch_SibSp_Pclass_logFare",
            
            "Age_Sex_Parch_SibSp_Pclass_logFare")

deviancevec=c(summSurvival_Age$deviance,summSurvival_Sex$deviance,summSurvival_SibSp$deviance,summSurvival_logFare$deviance, summSurvival_Pclass$deviance, summSurvival_Parch$deviance,
              
              summSurvival_AgeSex$deviance, summSurvival_AgeSipSp$deviance, summSurvival_AgePclass$deviance, summSurvival_AgeParch$deviance, summSurvival_AgelogFare$deviance,
              summSurvival_SexlogFare$deviance, summSurvival_SexParch$deviance, summSurvival_SexPclass$deviance, summSurvival_SexSipSp$deviance,
              summSurvival_PclasslogFare$deviance, summSurvival_PclassSibSp$deviance, summSurvival_PclassParch$deviance,
              summSurvival_SibSplogFare$deviance, summSurvival_SibSpParch$deviance, summSurvival_logFareParch$deviance,
              
              summSurvival_AgeSexPclass$deviance, summSurvival_AgeSexlogFare$deviance, summSurvival_AgeSexParch$deviance, summSurvival_AgeSexSibSp$deviance,
              summSurvival_SexSibSplogFare$deviance, summSurvival_SexSibSpParch$deviance, summSurvival_SexSibSpPclass$deviance, summSurvival_ParchlogFareSibSp$deviance, 
              summSurvival_ParchlogFarePclass$deviance, summSurvival_logFareSibSpPclass$deviance, summSurvival_AgeSibSpPclass$deviance, summSurvival_AgeSibSplogFare$deviance,
              summSurvival_AgeSibSpParch$deviance, summSurvival_AgePclasslogFare$deviance, summSurvival_AgePclassParch$deviance, summSurvival_AgelogFareParch$deviance,
              summSurvival_SexPclasslogFare$deviance, summSurvival_SexPclassParch$deviance, summSurvival_SexlogFareParch$deviance, summSurvival_ParchPclassSibSp$deviance,
              
              summSurvival_AgeSexSibSpParch$deviance, summSurvival_AgeSexSibSplogFare$deviance, summSurvival_AgeSexSibSpPclass$deviance,
              summSurvival_SexlogFarePclassSibSp$deviance, summSurvival_SexlogFarePclassParch$deviance, summSurvival_logFarePclassSibSpParch$deviance,
              summSurvival_AgeSexPclasslogFare$deviance, summSurvival_AgeSexPclassParch$deviance, summSurvival_AgeSexlogFareParch$deviance,
              summSurvival_AgeSibSpPclasslogFare$deviance, summSurvival_AgeSibSpPclassParch$deviance, summSurvival_AgeSibSplogFareParch$deviance,
              summSurvival_AgePclasslogFareParch$deviance, summSurvival_SexlogFareSibspParch$deviance, summSurvival_SexPclassSibspParch$deviance,
              
              summSurvival_AgeSexParchSibSpPclass$deviance, summSurvival_AgeSexParchSibSplogFare$deviance, summSurvival_AgeSexSibSpPclasslogFare$deviance, 
              summSurvival_AgeSexParchPclasslogFare$deviance, summSurvival_AgeParchSibSpPclasslogFare$deviance, summSurvival_SexParchSibSpPclasslogFare$deviance,
              
              summSurvival_AgeSexParchSibSpPclasslogFare$deviance)

aicvec=c(summSurvival_Age$aic,summSurvival_Sex$aic,summSurvival_SibSp$aic,summSurvival_logFare$aic, summSurvival_Pclass$aic, summSurvival_Parch$aic,
         
         summSurvival_AgeSex$aic, summSurvival_AgeSipSp$aic, summSurvival_AgePclass$aic, summSurvival_AgeParch$aic,summSurvival_AgelogFare$aic,
         summSurvival_SexlogFare$aic, summSurvival_SexParch$aic, summSurvival_SexPclass$aic, summSurvival_SexSipSp$aic,
         summSurvival_PclasslogFare$aic, summSurvival_PclassSibSp$aic, summSurvival_PclassParch$aic,
         summSurvival_SibSplogFare$aic, summSurvival_SibSpParch$aic, summSurvival_logFareParch$aic,
         
         summSurvival_AgeSexPclass$aic, summSurvival_AgeSexlogFare$aic, summSurvival_AgeSexParch$aic, summSurvival_AgeSexSibSp$aic,
         summSurvival_SexSibSplogFare$aic, summSurvival_SexSibSpParch$aic, summSurvival_SexSibSpPclass$aic, summSurvival_ParchlogFareSibSp$aic, 
         summSurvival_ParchlogFarePclass$aic, summSurvival_logFareSibSpPclass$aic, summSurvival_AgeSibSpPclass$aic, summSurvival_AgeSibSplogFare$aic,
         summSurvival_AgeSibSpParch$aic, summSurvival_AgePclasslogFare$aic, summSurvival_AgePclassParch$aic, summSurvival_AgelogFareParch$aic,
         summSurvival_SexPclasslogFare$aic, summSurvival_SexPclassParch$aic, summSurvival_SexlogFareParch$aic, summSurvival_ParchPclassSibSp$aic,
         
         summSurvival_AgeSexSibSpParch$aic, summSurvival_AgeSexSibSplogFare$aic, summSurvival_AgeSexSibSpPclass$aic,
         summSurvival_SexlogFarePclassSibSp$aic, summSurvival_SexlogFarePclassParch$aic, summSurvival_logFarePclassSibSpParch$aic,
         summSurvival_AgeSexPclasslogFare$aic, summSurvival_AgeSexPclassParch$aic, summSurvival_AgeSexlogFareParch$aic,
         summSurvival_AgeSibSpPclasslogFare$aic, summSurvival_AgeSibSpPclassParch$aic, summSurvival_AgeSibSplogFareParch$aic,
         summSurvival_AgePclasslogFareParch$aic, summSurvival_SexlogFareSibspParch$aic, summSurvival_SexPclassSibspParch$aic,
         
         summSurvival_AgeSexParchSibSpPclass$aic, summSurvival_AgeSexParchSibSplogFare$aic, summSurvival_AgeSexSibSpPclasslogFare$aic, 
         summSurvival_AgeSexParchPclasslogFare$aic, summSurvival_AgeParchSibSpPclasslogFare$aic, summSurvival_SexParchSibSpPclasslogFare$aic,
         
         summSurvival_AgeSexParchSibSpPclasslogFare$aic)

DevAicData=cbind(subsetvec, deviancevec, aicvec)

## we did Variable selection using stepAIC method too, and ended up with the same best models
library(MASS)
stepAIC(fit_Survival_AgeSexParchSibSpPclasslogFare, direction = "both",trace = 1)

# summary of best 3 models based on manual deviance (not so useful decreases with the # of variables)
summSurvival_AgeSexParchSibSpPclasslogFare #308.13
summSurvival_AgeSexSibSpPclasslogFare #309.97
summSurvival_AgeSexParchSibSpPclass #312.29

# summary of best 3 models based on manual AIC
summSurvival_AgeSexSibSpPclasslogFare #323.97
summSurvival_AgeSexParchSibSpPclasslogFare #324.13
summSurvival_AgeSexSibSpPclass #324.73

# summary of best 3 models based on stepAIC method
summSurvival_AgeSexSibSpPclasslogFare #323.97
summSurvival_AgeSexParchSibSpPclasslogFare #324.13
summSurvival_AgeSexSibSpPclass #324.73


# comparison of fit_Survival_AgeSexSibSpPclass, fit_Survival_AgeSexParchSibSpPclass, 
# fit_Survival_AgeSexSibSpPclasslogFare, fit_Survival_AgeSexParchSibSpPclasslogFare 
# for misclassification rates
# in-sample misclassification
pred4=predict(fit_Survival_AgeSexSibSpPclass, type="response")
pred5_1=predict(fit_Survival_AgeSexParchSibSpPclass, type="response")
pred5_2=predict(fit_Survival_AgeSexSibSpPclasslogFare, type="response")
pred6=predict(fit_Survival_AgeSexParchSibSpPclasslogFare,type="response")

#compare mean of predictions vs mean of actual training data
print(summary(pred4))
print(summary(pred5_1))
print(summary(pred5_2))
print(summary(pred6))

#boundary of 0.5 0.4 0.3 for misclassification
tab4a=table(train$Survived,as.numeric(pred4>0.5))
tab4b=table(train$Survived,as.numeric(pred4>0.4))
tab4c=table(train$Survived,as.numeric(pred4>0.3))

tab5_1a=table(train$Survived,as.numeric(pred5_1>0.5))
tab5_1b=table(train$Survived,as.numeric(pred5_1>0.4))
tab5_1c=table(train$Survived,as.numeric(pred5_1>0.3))

tab5_2a=table(train$Survived,as.numeric(pred5_2>0.5))
tab5_2b=table(train$Survived,as.numeric(pred5_2>0.4))
tab5_2c=table(train$Survived,as.numeric(pred5_2>0.3))

tab6a=table(train$Survived,as.numeric(pred6>0.5))
tab6b=table(train$Survived,as.numeric(pred6>0.4))
tab6c=table(train$Survived,as.numeric(pred6>0.3))

#convert to rates
tab4a/apply(tab4a,1,sum)  
tab5_1a/apply(tab5_1a,1,sum)
tab5_2a/apply(tab5_2a,1,sum)
tab6a/apply(tab6a,1,sum)

tab4b/apply(tab4b,1,sum)
tab5_1b/apply(tab5_1b,1,sum)
tab5_2b/apply(tab5_2b,1,sum)
tab6b/apply(tab6b,1,sum)

tab4c/apply(tab4c,1,sum)
tab5_1c/apply(tab5_1c,1,sum)
tab5_2c/apply(tab5_2c,1,sum)
tab6c/apply(tab6c,1,sum)

#table of # of misclassifications
numMis4 = c(tab4a[1,2]+tab4a[2,1],tab4b[1,2]+tab4b[2,1],tab4c[1,2]+tab4c[2,1])
numMis5_1 = c(tab5_1a[1,2]+tab5_1a[2,1],tab5_1b[1,2]+tab5_1b[2,1],tab5_1c[1,2]+tab5_1c[2,1])
numMis5_2 = c(tab5_2a[1,2]+tab5_2a[2,1],tab5_2b[1,2]+tab5_2b[2,1],tab5_2c[1,2]+tab5_2c[2,1])
numMis6 = c(tab6a[1,2]+tab6a[2,1],tab6b[1,2]+tab6b[2,1],tab6c[1,2]+tab6c[2,1])

#conclusion based on in-sample misclassifcation:
#fit_Survival_AgeSexParchSibSpPclasslogFare with 0.5 cutoff has the smallest # of in-sample misclassifications

# out-of-sample misclassication
pred4.hold=predict(fit_Survival_AgeSexSibSpPclass,type="response",newdata=hold)
pred5_1.hold=predict(fit_Survival_AgeSexParchSibSpPclass, type="response", newdata = hold)
pred5_2.hold=predict(fit_Survival_AgeSexSibSpPclasslogFare, type="response", newdata = hold)
pred6.hold=predict(fit_Survival_AgeSexParchSibSpPclasslogFare,type="response",newdata=hold)

htab4a=table(hold$Survived,as.numeric(pred4.hold>0.5))
htab4b=table(hold$Survived,as.numeric(pred4.hold>0.4))
htab4c=table(hold$Survived,as.numeric(pred4.hold>0.3))

htab5_1a=table(hold$Survived,as.numeric(pred5_1.hold>0.5))
htab5_1b=table(hold$Survived,as.numeric(pred5_1.hold>0.4))
htab5_1c=table(hold$Survived,as.numeric(pred5_1.hold>0.3))

htab5_2a=table(hold$Survived,as.numeric(pred5_2.hold>0.5))
htab5_2b=table(hold$Survived,as.numeric(pred5_2.hold>0.4))
htab5_2c=table(hold$Survived,as.numeric(pred5_2.hold>0.3))

htab6a=table(hold$Survived,as.numeric(pred6.hold>0.5))
htab6b=table(hold$Survived,as.numeric(pred6.hold>0.4))
htab6c=table(hold$Survived,as.numeric(pred6.hold>0.3))

#convert to rates
htab4a/apply(htab4a,1,sum)  
htab5_1a/apply(htab5_1a,1,sum)
htab5_2a/apply(htab5_2a,1,sum)
htab6a/apply(htab6a,1,sum)

htab4b/apply(htab4b,1,sum)
htab5_1b/apply(htab5_1b,1,sum)
htab5_2b/apply(htab5_2b,1,sum)
htab6b/apply(htab6b,1,sum)

htab4c/apply(htab4c,1,sum)
htab5_1c/apply(htab5_1c,1,sum)
htab5_2c/apply(htab5_2c,1,sum)
htab6c/apply(htab6c,1,sum)

#table of # of misclassifications
hNumMis4 = c(htab4a[1,2]+htab4a[2,1],htab4b[1,2]+htab4b[2,1],htab4c[1,2]+htab4c[2,1])
hNumMis5_1 = c(htab5_1a[1,2]+htab5_1a[2,1],htab5_1b[1,2]+htab5_1b[2,1],htab5_1c[1,2]+htab5_1c[2,1])
hNumMis5_2 = c(htab5_2a[1,2]+htab5_2a[2,1],htab5_2b[1,2]+htab5_2b[2,1],htab5_2c[1,2]+htab5_2c[2,1])
hNumMis6 = c(htab6a[1,2]+htab6a[2,1],htab6b[1,2]+htab6b[2,1],htab6c[1,2]+htab6c[2,1])

#conclusion based on out-of-sample misclassifcation:
#fit_Survival_AgeSexSibSpPclass with 0.4 cutoff has the smallest # of out-of-sample misclassifications

# Hosmer-Lemeshow calibration check
prcateg4=cut(pred4,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
print(table(prcateg4))

HLsumm4=tapply(train$Survived,prcateg4,mean)
print(HLsumm4)

prcateg5_1=cut(pred5_1,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
print(table(prcateg5_1))

HLsumm5_1=tapply(train$Survived,prcateg5_1,mean)
print(HLsumm5_1)

prcateg5_2=cut(pred5_2,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
print(table(prcateg5_2))

HLsumm5_2=tapply(train$Survived,prcateg5_2,mean)
print(HLsumm5_2)

prcateg6=cut(pred6,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
print(table(prcateg6))

HLsumm6=tapply(train$Survived,prcateg6,mean)
print(HLsumm6)

#conclusion based on calibration of fit:
#fit_Survival_AgeSexSibSpPclass has the best calibration (with only 1 bin out of range)
#overall fit looks okay

#quadratic model of best model based on best out-of-sample # of misclassifications
fit_Survival_AgeSexSibSpPclass_quad=glm(Survived~Age+factor(Sex)+SibSp+factor(Pclass)+I(Age^2)+I(SibSp^2)+Age:SibSp, family="binomial", data=train)
summSurvival_AgeSexSibSpPclass_quad=summary(fit_Survival_AgeSexSibSpPclass_quad)

#quadratic model of best model based on AIC
fit_Survival_AgeSexSibSpPclasslogFare_quad=glm(Survived~Age+factor(Sex)+SibSp+factor(Pclass)+logFare+I(Age^2)+I(SibSp^2)+I(logFare^2)+Age:SibSp+Age:logFare+SibSp:logFare, family="binomial", data=train)
summSurvival_AgeSexSibSpPclasslogFare_quad=summary(fit_Survival_AgeSexSibSpPclasslogFare_quad)

#do summary for quadratic models (deviance, AIC)

pred4_quad=predict(fit_Survival_AgeSexSibSpPclass_quad, type="response")
pred5_2_quad=predict(fit_Survival_AgeSexSibSpPclasslogFare_quad, type="response")

#conclusion based on AIC:
#fit_Survival_AgeSexSibSpPclass_quad has lower AIC than any of the other models (#321.66)
#fit_Survival_AgeSexSibSpPclasslogFare_quad has wrose AIC comared to the linear models (#326.56)

#compare mean of predictions vs mean of actual training data
print(summary(pred4_quad))
print(summary(pred5_2_quad))

#boundary of 0.5 0.4 0.3 for misclassification
tab4a_quad=table(train$Survived,as.numeric(pred4_quad>0.5))
tab4b_quad=table(train$Survived,as.numeric(pred4_quad>0.4))
tab4c_quad=table(train$Survived,as.numeric(pred4_quad>0.3))

tab5_2a_quad=table(train$Survived,as.numeric(pred5_2_quad>0.5))
tab5_2b_quad=table(train$Survived,as.numeric(pred5_2_quad>0.4))
tab5_2c_quad=table(train$Survived,as.numeric(pred5_2_quad>0.3))

#convert to rates
tab4a_quad/apply(tab4a_quad,1,sum)  
tab5_2a_quad/apply(tab5_2a_quad,1,sum)

tab4b_quad/apply(tab4b_quad,1,sum)
tab5_2b_quad/apply(tab5_2b_quad,1,sum)

tab4c_quad/apply(tab4c_quad,1,sum)
tab5_2c_quad/apply(tab5_2c_quad,1,sum)

#table of # of misclassifications
numMis4_quad = c(tab4a_quad[1,2]+tab4a_quad[2,1],tab4b_quad[1,2]+tab4b_quad[2,1],tab4c_quad[1,2]+tab4c_quad[2,1])
numMis5_2_quad = c(tab5_2a_quad[1,2]+tab5_2a_quad[2,1],tab5_2b_quad[1,2]+tab5_2b_quad[2,1],tab5_2c_quad[1,2]+tab5_2c_quad[2,1])

#conclusion based on in-sample misclassifcation:
#adding quadratic variables lowered the # of in-sample misclassifications for both models
#fit_Survival_AgeSexSibSpPclass_quad with 0.4 or 0.5 cutoff has the lowest number of in-sample misclassifications

# out-of-sample misclassication
pred4_quad.hold=predict(fit_Survival_AgeSexSibSpPclass_quad,type="response",newdata=hold)
pred5_2_quad.hold=predict(fit_Survival_AgeSexSibSpPclasslogFare_quad, type="response", newdata = hold)

htab4a_quad=table(hold$Survived,as.numeric(pred4_quad.hold>0.5))
htab4b_quad=table(hold$Survived,as.numeric(pred4_quad.hold>0.4))
htab4c_quad=table(hold$Survived,as.numeric(pred4_quad.hold>0.3))

htab5_2a_quad=table(hold$Survived,as.numeric(pred5_2_quad.hold>0.5))
htab5_2b_quad=table(hold$Survived,as.numeric(pred5_2_quad.hold>0.4))
htab5_2c_quad=table(hold$Survived,as.numeric(pred5_2_quad.hold>0.3))

#convert to rates
htab4a_quad/apply(htab4a_quad,1,sum)  
htab5_2a_quad/apply(htab5_2a_quad,1,sum)

htab4b_quad/apply(htab4b_quad,1,sum)
htab5_2b_quad/apply(htab5_2b_quad,1,sum)

htab4c_quad/apply(htab4c_quad,1,sum)
htab5_2c_quad/apply(htab5_2c_quad,1,sum)


#table of # of misclassifications
hNumMis4_quad = c(htab4a_quad[1,2]+htab4a_quad[2,1],htab4b_quad[1,2]+htab4b_quad[2,1],htab4c_quad[1,2]+htab4c_quad[2,1])
hNumMis5_2_quad = c(htab5_2a_quad[1,2]+htab5_2a_quad[2,1],htab5_2b_quad[1,2]+htab5_2b_quad[2,1],htab5_2c_quad[1,2]+htab5_2c_quad[2,1])

#conclusion based on in-sample misclassifcation:
#adding quadratic variables lowered the # of out-of-sample misclassifications for both models
#fit_Survival_AgeSexSibSpPclass_quad with 0.4 and 0.5 cutoff have the lowest # of out-of-sample misclassifications

# Hosmer-Lemeshow calibration check
prcateg4_quad=cut(pred4_quad,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
#print(table(prcateg4))

HLsumm4_quad=tapply(train$Survived,prcateg4_quad,mean)
print(HLsumm4_quad)

prcateg5_2_quad=cut(pred5_2_quad,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
#print(table(prcateg5_2))

HLsumm5_2_quad=tapply(train$Survived,prcateg5_2_quad,mean)
print(HLsumm5_2_quad)

#conclusion based on calibration of fit:
#the tested quadratic models have more bins that are out of range, but the magnitude of "deviation"
#from the intervals seem to be smaller
#overall fit look okay

#conclusion of quadratic models vs linear models:
#fit_Survival_AgeSexSibSpPclass_quad has the lowest AIC and least # of out-of-sample misclassifications
#out of all the models tested, so it is the best model
#cut-off of 0.40 is the best because it has one of the smallest # of in-sample misclassifications and it is closest to the sample mean
