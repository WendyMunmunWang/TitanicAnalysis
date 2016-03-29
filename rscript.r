#data file path, comment out /add yours!
titanic <- read.delim("~/Documents/STAT306/TitanicAnalysis/cleaned_data.txt") #wendy
View(titanic)
##correlation
correlation = cor(titanic)
print(correlation)


##plotting
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
ggplot(titanic, aes(x=titanic$Age, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)
ggplot(titanic, aes(x=titanic$Parch, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)
ggplot(titanic, aes(x=titanic$Fare, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)
ggplot(titanic, aes(x=titanic$Sibsp, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)
ggplot(titanic, aes(x=titanic$Pclass, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)
ggplot(titanic, aes(x=titanic$Sex, y=titanic$Survived)) + geom_point() + geom_smooth(method="glm", method.args = list(family="binomial"), se=FALSE)


##fitting 1 explanatory variable
fit_Survival_Age=glm(titanic$Survived~titanic$Age, family="binomial", data=titanic)
summSurvival_Age=summary(fit_Survival_Age)
fit_Survival_Sex=glm(titanic$Survived~titanic$Sex, family="binomial", data=titanic)
summSurvival_Sex=summary(fit_Survival_Sex)
fit_Survival_SibSp=glm(titanic$Survived~titanic$SibSp, family="binomial", data=titanic)
summSurvival_SibSp=summary(fit_Survival_SibSp)
fit_Survival_Parch=glm(titanic$Survived~titanic$Parch, family="binomial", data=titanic)
summSurvival_Parch=summary(fit_Survival_Parch)
fit_Survival_Fare=glm(titanic$Survived~titanic$Fare, family="binomial", data=titanic)
summSurvival_Fare=summary(fit_Survival_Fare)
fit_Survival_Pclass=glm(titanic$Survived~titanic$Pclass, family="binomial", data=titanic)
summSurvival_Pclass=summary(fit_Survival_Pclass)
##fitting 2 explanatory variables
fit_Survival_AgeSex=glm(titanic$Survived~titanic$Age+titanic$Sex, family="binomial", data=titanic)
summSurvival_AgeSex=summary(fit_Survival_AgeSex)
fit_Survival_AgeSibSp=glm(titanic$Survived~titanic$Age+titanic$SibSp, family="binomial", data=titanic)
summSurvival_AgeSipSp=summary(fit_Survival_AgeSibSp)
fit_Survival_AgePclass=glm(titanic$Survived~titanic$Age+titanic$Pclass, family="binomial", data=titanic)
summSurvival_AgePclass=summary(fit_Survival_AgePclass)
fit_Survival_AgeParch=glm(titanic$Survived~titanic$Age+titanic$Parch, family="binomial", data=titanic)
summSurvival_AgeParch=summary(fit_Survival_AgeParch)
fit_Survival_AgeFare=glm(titanic$Survived~titanic$Age+titanic$Fare, family="binomial", data=titanic)
summSurvival_AgeFare=summary(fit_Survival_AgeFare)

fit_Survival_SexPclass=glm(titanic$Survived~titanic$Pclass+titanic$Sex, family="binomial", data=titanic)
summSurvival_SexPclass=summary(fit_Survival_SexPclass)
fit_Survival_SexSibSp=glm(titanic$Survived~titanic$Sex+titanic$SibSp, family="binomial", data=titanic)
summSurvival_SexSipSp=summary(fit_Survival_SexSibSp)
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
fit_Survival_SexSibSpParch=glm(titanic$Survived~titanic$SibSp+titanic$Sex+titanic$Parch, family="binomial", data=titanic)
summSurvival_SexSibSpParch=summary(fit_Survival_SexSibSpParch)
fit_Survival_SexSibSpPclass=glm(titanic$Survived~titanic$SibSp+titanic$Sex+titanic$Pclass, family="binomial", data=titanic)
summSurvival_SexSibSpPclass=summary(fit_Survival_SexSibSpPclass)


fit_Survival_ParchFareSibSp=glm(titanic$Survived~titanic$Parch+titanic$SibSp+titanic$Fare, family="binomial", data=titanic)
summSurvival_ParchFareSibSp=summary(fit_Survival_ParchFareSibSp)
fit_Survival_ParchFarePclass=glm(titanic$Survived~titanic$Fare+titanic$Pclass+titanic$Parch, family="binomial", data=titanic)
summSurvival_ParchFarePclass=summary(fit_Survival_ParchFarePclass)

fit_Survival_FareSibSpPclass=glm(titanic$Survived~titanic$SibSp+titanic$Pclass+titanic$Pclass, family="binomial", data=titanic)
summSurvival_FareSibSpPclass=summary(fit_Survival_FareSibSpPclass)

##fitting 4 explanatory variables 
fit_Survival_AgeSexSibSpParch=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$SibSp+titanic$Parch, family="binomial", data=titanic)
summSurvival_AgeSexSibSpParch=summary(fit_Survival_AgeSexSibSpParch)
fit_Survival_AgeSexSibSpFare=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$SibSp+titanic$Fare, family="binomial", data=titanic)
summSurvival_AgeSexSibSpFare=summary(fit_Survival_AgeSexSibSpFare)
fit_Survival_AgeSexSibSpPclass=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$SibSp+titanic$Pclass, family="binomial", data=titanic)
summSurvival_AgeSexSibSpPclass=summary(fit_Survival_AgeSexSibSpPclass)

fit_Survival_SexFarePclassSibSp=glm(titanic$Survived~titanic$SibSp+titanic$Sex+titanic$Fare+titanic$Pclass, family="binomial", data=titanic)
summSurvival_SexFarePclassSibSp=summary(fit_Survival_SexFarePclassSibSp)
fit_Survival_SexFarePclassParch=glm(titanic$Survived~titanic$Parch+titanic$Sex+titanic$Fare+titanic$Pclass, family="binomial", data=titanic)
summSurvival_SexFarePclassParch=summary(fit_Survival_SexFarePclassParch)

fit_Survival_FarePclassSibSpParch=glm(titanic$Survived~titanic$Parch+titanic$SibSp+titanic$Fare+titanic$Pclass, family="binomial", data=titanic)
summSurvival_FarePclassSibSpParch=summary(fit_Survival_FarePclassSibSpParch)

##fitting 5 explanatory variables
fit_Survival_AgeSexParchSibspPclass=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$Parch+titanic$SibSp+titanic$Pclass, family="binomial", data=titanic)
summSurvival_AgeSexParchSibspPclass=summary(fit_Survival_AgeSexParchSibspPclass)

fit_Survival_AgeSexParchSibspFare=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$Parch+titanic$SibSp+titanic$Fare, family="binomial", data=titanic)
summSurvival_AgeSexParchSibspFare=summary(fit_Survival_AgeSexParchSibspFare)

##fitting 6 explanatory variables
fit_Survival_AgeSexParchSibspPclassFare=glm(titanic$Survived~titanic$Age+titanic$Sex+titanic$Parch+titanic$SibSp+titanic$Fare+titanic$Pclass, family="binomial", data=titanic)
summSurvival_AgeSexParchSibspPclassFare=summary(fit_Survival_AgeSexParchSibspPclassFare)

##model evaluation
aicvec=c(summSurvival_Age$aic,summSuvival_Sex$aic,summSurvival_SibSp$aic,summSurvival_Fare$aic, summSurvival_Pclass$aic, summSurvival_Parch$aic,
              summSurvival_AgeSex$aic, summSurvival_AgeSipSp$aic, summSurvival_AgePclass$aic, summSurvival_AgeParch$aic,summSurvival_AgeFare$aic,
              summSurvival_SexFare$aic, summSurvival_SexParch$aic, summSurvival_SexPclass$aic, summSurvival_SexSipSp$aic,
              summSurvival_PclassFare$aic, summSurvival_PclassSibSp$aic, summSurvival_PclassParch$aic,
              summSurvival_SibSpFare$aic, summSurvival_SibSpParch$aic, 
              summSurvival_FareParch$aic,
              summSurvival_AgeSexPclass$aic, summSurvival_AgeSexFare$aic, summSurvival_AgeSexParch$aic, summSurvival_AgeSexSibSp$aic,
              summSurvival_SexSibspFare$aic, summSurvival_SexSibSpParch$aic, summSurvival_SexSibSpPclass$aic,
              summSurvival_ParchFareSibSp$aic, summSurvival_ParchFarePclass$aic,
              summSurvival_FareSibSpPclass$aic,
              summSurvival_AgeSexSibSpParch$aic, summSurvival_AgeSexSibSpFare$aic, summSurvival_AgeSexSibSpPclass$aic,
              summSurvival_SexFarePclassSibSp$aic, summSurvival_SexFarePclassParch$aic,
              summSurvival_FarePclassSibSpParch$aic,
              summSurvival_AgeSexParchSibspPclass$aic, summSurvival_AgeSexParchSibspFare$aic, 
              summSurvival_AgeSexParchSibspPclassFare$aic)

subsetvec=c("Age", "Sex", "SibSp", "Fare", "Pclass", "Parch",
            "Age_Sex","Age_SibSp", "Age_Pclass", "Age_Parch", "Age_Fare",
            "Sex_Fare", "Sex_Parch", "Sex_Pclass", "Sex_SibSp",
            "Pclass_Fare", "Pclass_SibSp", "Pclass_Parch",
            "SibSp_Fare", "SibSp_Parch",
            "Fare_Parch",
            "Age_Sex_Pclass", "Age_Sex_Fare", "Age_Sex_Parch", "Age_Sex_SibSp",
            "Sex_SibSp_Fare", "Sex_SibSp_Parch", "Sex_SibSp_Pclass",
            "Parch_Fare_SibSp", "Parch_Fare_Pclass",
            "Fare_SibSp_Pclass",
            "Age_Sex_SibSp_Parch", "Age_Sex_SibSp_Fare", "Age_Sex_SibSp_Pclass",
            "Sex_Fare_Pclass_SibSp", "Sex_Fare_Pclass_Parch",
            "Fare_Pclass_SibSp_Parch",
            "Age_Sex_Parch_Sibsp_Pclass", "Age_Sex_Parch_Sibsp_Fare",
            "Age_Sex_Parch_Sibsp_Pclass_Fare")
deviancevec=c(summSurvival_Age$deviance,summSuvival_Sex$deviance,summSurvival_SibSp$deviance,summSurvival_Fare$deviance, summSurvival_Pclass$deviance, summSurvival_Parch$deviance,
              summSurvival_AgeSex$deviance, summSurvival_AgeSipSp$deviance, summSurvival_AgePclass$deviance, summSurvival_AgeParch$deviance,summSurvival_AgeFare$deviance,
              summSurvival_SexFare$deviance, summSurvival_SexParch$deviance, summSurvival_SexPclass$deviance, summSurvival_SexSipSp$deviance,
              summSurvival_PclassFare$deviance, summSurvival_PclassSibsp$deviance, summSurvival_PclassParch$deviance,
              summSurvival_SibSpFare$deviance, summSurvival_SibSpParch$deviance, 
              summSurvival_FareParch$deviance,
              summSurvival_AgeSexPclass$deviance, summSurvival_AgeSexFare$deviance, summSurvival_AgeSexParch$deviance, summSurvival_AgeSexSibsp$deviance,
              summSurvival_SexSibspFare$deviance, summSurvival_SexSibSpParch$deviance, summSurvival_SexSibSpPclass$deviance,
              summSurvival_ParchFareSibSp$deviance, summSurvival_ParchFarePclass$deviance,
              summSurvival_FareSibSpPclass$deviance,
              summSurvival_AgeSexSibSpParch$deviance, summSurvival_AgeSexSibSpFare$deviance, summSurvival_AgeSexSibSpPclass$deviance,
              summSurvival_SexFarePclassSibSp$deviance, summSurvival_SexFarePclassParch$deviance,
              summSurvival_FarePclassSibSpParch$deviance,
              summSurvival_AgeSexParchSibspPclass$deviance, summSurvival_AgeSexParchSibspFare$deviance, 
              summSurvival_AgeSexParchSibspPclassFare$deviance)

print(cbind(subsetvec, deviancevec, aicvec))

##best model summary
summSurvival_AgeSexSibSpPclass
