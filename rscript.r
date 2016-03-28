titanic <- read.delim("~/Documents/STAT306/TitanicAnalysis/cleaned_data.txt")
View(titanic)
##correlation
correlation = cor(titanic)
print(correlation)
##plotting
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
ggplot(titanic, aes(x=titanic$Age, y=titanic$Survived)) + geom_point()+stat_smooth(method="glm", family="binomial", se=FALSE)
plot(titanic$Age, titanic$Survived)
curve(predict(fit_Survival_Age, data.frame(titanic$Age=x), type="response"), add=TRUE)
##fitting 1 explanatory variable
fit_Survival_Age=glm(titanic$Survived~titanic$Age, family="binomial", data=titanic)
summSurvival_Age=summary(fit_Survival_Age)
fit_Survival_Sex=glm(titanic$Survived~titanic$Sex, family="binomial", data=titanic)
summSurvival_Sex=summary(fit_Survival_Sex)
fit_Survival_Sibsp=glm(titanic$Survived~titanic$SibSp, family="binomial", data=titanic)
summSurvival_Sibsp=summary(fit_Survival_Sibsp)
fit_Survival_Parch=glm(titanic$Survived~titanic$Parch, family="binomial", data=titanic)
summSurvival_Parch=summary(fit_Survival_Parch)
fit_Survival_Fare=glm(titanic$Survived~titanic$Fare, family="binomial", data=titanic)
summSurvival_Fare=summary(fit_Survival_Fare)
fit_Survival_Pclass=glm(titanic$Survived~titanic$Pclass, family="binomial", data=titanic)
summSurvival_Pclass=summary(fit_Survival_Pclass)
##fitting 2 explanatory variables
fit_Survival_AgeSex=glm(titanic$Survived~titanic$Age+titanic$Sex, family="binomial", data=titanic)
summSurvival_AgeSex=summary(fit_Survival_AgeSex)
fit_Survival_AgeSibsp=glm(titanic$Survived~titanic$Age+titanic$SibSp, family="binomial", data=titanic)
summSurvival_AgeSipsp=summary(fit_Survival_AgeSibsp)
fit_Survival_AgePclass=glm(titanic$Survived~titanic$Age+titanic$Pclass, family="binomial", data=titanic)
summSurvival_AgePclass=summary(fit_Survival_AgePclass)
fit_Survival_AgeParch=glm(titanic$Survived~titanic$Age+titanic$Parch, family="binomial", data=titanic)
summSurvival_AgeParch=summary(fit_Survival_AgeParch)
fit_Survival_AgeFare=glm(titanic$Survived~titanic$Age+titanic$Fare, family="binomial", data=titanic)
summSurvival_AgeFare=summary(fit_Survival_AgeFare)

fit_Survival_SexPclass=glm(titanic$Survived~titanic$Pclass+titanic$Sex, family="binomial", data=titanic)
summSurvival_SexPclass=summary(fit_Survival_SexPclass)
fit_Survival_SexSibsp=glm(titanic$Survived~titanic$Sex+titanic$SibSp, family="binomial", data=titanic)
summSurvival_SexSipsp=summary(fit_Survival_SexSibsp)
fit_Survival_SexParch=glm(titanic$Survived~titanic$Sex+titanic$Parch, family="binomial", data=titanic)
summSurvival_SexParch=summary(fit_Survival_SexParch)
fit_Survival_SexFare=glm(titanic$Survived~titanic$Sex+titanic$Fare, family="binomial", data=titanic)
summSurvival_SexFare=summary(fit_Survival_SexParch)

fit_Survival_PclassSibsp=glm(titanic$Survived~titanic$Pclass+titanic$SibSp, family="binomial", data=titanic)
summSurvival_PclassSibsp=summary(fit_Survival_PclassSibsp)
fit_Survival_PclassParch=glm(titanic$Survived~titanic$Pclass+titanic$Parch, family="binomial", data=titanic)
summSurvival_PclassParch=summary(fit_Survival_PclassParch)
fit_Survival_PclassFare=glm(titanic$Survived~titanic$Pclass+titanic$Fare, family="binomial", data=titanic)
summSurvival_PclassFare=summary(fit_Survival_PclassFare)

fit_Survival_SibSpFare=glm(titanic$Survived~titanic$Fare+titanic$SibSp, family="binomial", data=titanic)
summSurvival_SibSpFare=summary(fit_Survival_SibspFare)
fit_Survival_SibSpParch=glm(titanic$Survived~titanic$SibSp+titanic$Parch, family="binomial", data=titanic)
summSurvival_SibSpParch=summary(fit_Survival_SibSpParch)

fit_Survival_FareParch=glm(titanic$Survived~titanic$Fare+titanic$Parch, family="binomial", data=titanic)
summSurvival_FareParch=summary(fit_Survival_FareParch)

##fitting 3 explanatory variables
fit_Survival_AgeSexSibsp=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$SibSp, family="binomial", data=titanic)
summSurvival_AgeSexSibsp=summary(fit_Survival_AgeSexSibsp)
fit_Survival_AgeSexFare=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$Fare, family="binomial", data=titanic)
summSurvival_AgeSexFare=summary(fit_Survival_AgeSexFare)
fit_Survival_AgeSexParch=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$Parch, family="binomial", data=titanic)
summSurvival_AgeSexParch=summary(fit_Survival_AgeSexParch)
fit_Survival_AgeSexPclass=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$Pclass, family="binomial", data=titanic)
summSurvival_AgeSexPclass=summary(fit_Survival_AgeSexPclass)

fit_Survival_SexSibspFare=glm(titanic$Survived~titanic$Fare+titanic$Sex+titanic$SibSp, family="binomial", data=titanic)
summSurvival_SexSibspFare=summary(fit_Survival_SexSibspFare)
fit_Survival_SexFareParch=glm(titanic$Survived~titanic$Parch+titanic$Sex+titanic$Fare, family="binomial", data=titanic)
summSurvival_SexFareParch=summary(fit_Survival_SexFareParch)
fit_Survival_SexParchSibSp=glm(titanic$Survived~titanic$SibSp+titanic$Sex+titanic$Parch, family="binomial", data=titanic)
summSurvival_SexParchSibSp=summary(fit_Survival_SexParchSibSp)

fit_Survival_ParchFareSibSp=glm(titanic$Survived~titanic$Parch+titanic$SibSp+titanic$Fare, family="binomial", data=titanic)
summSurvival_ParchFareSibSp=summary(fit_Survival_ParchFareSibSp)
fit_Survival_ParchSibSpPclass=glm(titanic$Survived~titanic$SibSp+titanic$Pclass+titanic$Parch, family="binomial", data=titanic)
summSurvival_ParchSibSpPclass=summary(fit_Survival_ParchSibSpPclass)
fit_Survival_ParchFarePclass=glm(titanic$Survived~titanic$Fare+titanic$Pclass+titanic$Parch, family="binomial", data=titanic)
summSurvival_ParchFarePclass=summary(fit_Survival_ParchFarePclass)

fit_Survival_FareSibSpPclass=glm(titanic$Survived~titanic$SibSp+titanic$Pclass+titanic$Pclass, family="binomial", data=titanic)
summSurvival_FareSibSpPclass=summary(fit_Survival_FareSibSpPclass)


##fitting 4 explanatory variables 
fit_Survival_AgeSexSibSpParch=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$SibSp+titanic$Parch, family="binomial", data=titanic)
summSurvival_AgeSexSibSpParch=summary(fit_Survival_AgeSexSibSpParch)
fit_Survival_AgeSexSibSpFare=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$SibSp+titanic$Fare, family="binomial", data=titanic)
summSurvival_AgeSexSibSpFare=summary(fit_Survival_AgeSexSibSpFare)

fit_Survival_SexFarePclassSibSp=glm(titanic$Survived~titanic$SibSp+titanic$Sex+titanic$Fare+titanic$Pclass, family="binomial", data=titanic)
summSurvival_SexFarePclassSibSp=summary(fit_Survival_SexFarePclassSibSp)


##fitting 5 explanatory variables
fit_Survival_AgeSexParchSibspPclassFare=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$Parch+titanic$SibSp+titanic$Fare+titanic$Pclass, family="binomial", data=titanic)
summSurvival_AgeSexParchSibspPclassFare=summary(fit_Survival_AgeSexParchSibspPclassFare)

##model evaluation
aicvec=c(summSurvival_Age$aic,summSuvival_Sex$aic,summSurvival_Sibsp$aic,summSurvival_Fare$aic, summSurvival_Pclass$aic, 
         summSurvival_AgeSex$aic, summSurvival_AgeSipsp$aic, summSurvival_AgePclass$aic, summSurvival_AgeParch$aic,summSurvival_AgeFare$aic,
         summSurvival_SexFare$aic, summSurvival_SexParch$aic, summSurvival_SexPclass$aic, summSurvival_SexSipSp$aic,
         summSurvival_PclassFare$aic, summSurvival_PclassSibsp$aic, summSurvival_PclassParch$aic,
         summSurvival_SibSpFare$aic, summSurvival_SibSpParch$aic, 
         summSurvival_FareParch$aic,
         summSurvival_AgeSexPclass$aic, summSurvival_AgeSexFare$aic, summSurvival_AgeSexParch$aic, summSurvival_AgeSexSibSpFare$aic,
         summSurvival_SexFareParch$aic, summSurvival_SexParchSibSp$aic,summSurvival_SexFareParch$aic,
         summSurvival_ParchFareSibSp$aic, summSurvival_ParchSibSpPclass$aic, summSurvival_ParchFarePclass$aic,
         summSurvival_AgeSexSibSpFare$aic, summSurvival_AgeSexSibSpParch$aic, summSurvival_AgeSexPclass
         )
