titanic <- read.delim("~/Documents/STAT306/TitanicAnalysis/cleaned_data.txt")
View(titanic)
##correlation
correlation = cor(titanic)
print(correlation)
##plotting
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
ggplot(titanic, aes(x=titanic$Age, y=titanic$Survived)) 

plotBinResp(titanic$Age, factor(titanic$Survived))
plot(titanic$Fare, factor(titanic$Survived), main = "FARE")
survival_age <- plot(titanic$Age, titanic$Survived)
