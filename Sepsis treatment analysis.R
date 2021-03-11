library(lme4)
library(ggplot2)
df.one <- read.csv("SepsisData.csv")
summary(df.one)
by(df.one, df.one$treat, summary)
timePlot <- ggplot(data = df.one, aes(x=hour, y=temp)) + geom_point(aes(colour = factor(hour))) + geom_smooth(method=lm) + theme_bw()
timePlot

set.seed(01134)
randIntercept <- lmer(temp ~ hour + treat + age + (1|studyid), data = df.one, REML = 0)
summary(randIntercept)

set.seed(01134)
randEffects <- lmer(temp ~ hour + treat + age + (hour|studyid), data = df.one, REML = 0)
summary(randEffects)
anova(randIntercept, randEffects, test = "LRT")
finalModel <- lmer(temp ~ hour + treat + age + (hour|studyid), data = df.one, REML = TRUE)
summary(finalModel)
modelPlot <- ggplot(df.one, aes(x=hour, y=temp, colour=as.factor(treat))) +
  geom_point(size=2) +
  geom_line(aes(y=predict(finalModel), group=studyid)) +
  scale_color_manual(values=c("#FC7777", "#77BFFC")) +
  theme_bw(base_size=15) 
modelPlot
