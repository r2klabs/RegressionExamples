#Linear Regression with R.
library(tidyverse)

#The Penguin Dataset
df<-read.csv("processed_penguins.csv")
plot(mass, flipper)

penguin.lm<-lm(body_mass_g ~ flipper_length_mm, data=df)
penguin.resid<-resid(penguin.lm)
summary(penguin.lm)

ggplot(df, aes(x=flipper_length_mm, y=body_mass_g)) +
  geom_point(color='#2980B9', size=2) +
  geom_smooth(method='lm', color='#2C3E50')

plot(df$flipper_length_mm, penguin.resid,
     ylab="Residuals",
     xlab="Body Mass",
     main="Body Mass Residuals")
abline(0,0)

qqnorm(penguin.resid)
qqline(penguin.resid)
plot(density(penguin.resid))

#plot(penguin.lm)


df<-read.csv('age_bloodpressure.csv')
plot(df$AGE, df$BP)

ggplot(df, aes(x=AGE, y=BP)) +
  geom_point(color='#2980B9', size=2) +
  geom_smooth(method='lm', color='#2C3E50')

bp.lm<-lm(BP ~ AGE, data=df)
summary(bp.lm)
bp.resid<-resid(bp.lm)

plot(df$AGE, bp.resid,
     ylab="Residuals",
     xlab="AGE",
     main="AGE Residuals")
abline(0,0)
