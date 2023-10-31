library(readr)
library(ggplot2)
library(tidyverse)
library(stargazer)
library(reshape)
library(plyr)
library(plm)
library(dplyr)

#Excercise1----
##a----
twitch_data <- read.csv("twitch.csv", header = TRUE, sep=";")
stargazer(twitch_data, type = "text", out = "twitch_data.txt")
str(twitch_data)
##b----
###1
scatter_length.nstreams <- ggplot(twitch_data, aes(nstreams, length)) +
  geom_point() + geom_smooth(method = "lm", colour = "Red") + labs(x = "Number of streams", y = "Streamed minutes")
scatter_length.nstreams
###2
scatter_length.viewgain <- ggplot(twitch_data, aes(viewgain, length)) +
  geom_point() + geom_smooth(method = "lm", colour = "Red") + labs(x = "Number of viewers gained", y = "Streamed minutes")
scatter_length.viewgain
###3
scatter_length.numgames <- ggplot(twitch_data, aes(numgames, length)) +
  geom_point() + geom_smooth(method = "lm", colour = "Red") + labs(x = "Number of games played", y = "Streamed minutes")
scatter_length.numgames
##c----
###1
lm1 <- lm(length ~ numgames, data = twitch_data)
summary(lm1)

lm2 <- lm(length ~ numgames + played_topgame , data = twitch_data)
summary(lm2)

lm3 <- lm(length ~ numgames + played_topgame + played_star_game , data = twitch_data)
summary(lm3)

lm4 <- lm(length ~ numgames + played_topgame + played_star_game + followergain  , data = twitch_data)
summary(lm4)

stargazer(lm1, lm2, lm3, lm4,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text",
          column.labels = c("1st model", "2nd model", "3rd model", "4th model"),
          out = "lm.txt")



#Exercise 2----
##a----
whr_data <- read.csv("whr.csv", header = TRUE, sep=";")

whr_data <- whr_data[complete.cases(whr_data),]
stargazer(whr_data, type = "text", out = "whr_data_na.txt")
n_distinct(whr_data$Country.name)

pdim(whr_data) #not a balanced data
##b----
whr_data.sub <- subset(whr_data, !is.na(Life.Ladder),
                      select = c(Country.name, year, Life.Ladder))
whr_data.avg <- 
  ddply(whr_data.sub, .(Country.name), summarise,
        avgLife.Ladder = mean(Life.Ladder, na.rm=TRUE),
        numValid = length(Country.name))
#top5 countries:
top_5 <- whr_data.avg %>%
  arrange(desc(avgLife.Ladder)) %>%
  top_n(5, avgLife.Ladder)
top_5
#Normally distributed
hist_whr_data.avg <- ggplot(whr_data.avg, aes(avgLife.Ladder)) +
                     geom_histogram(binwidth = 0.1, fill = "skyblue", colour = "black") + labs(x = "avgLife.Ladder", y = "frequency", title = "Histogram of Happiness")
hist_whr_data.avg
shapiro.test (whr_data.avg$avgLife.Ladder)
#merge back
whr_data.sub <- merge(whr_data.sub, whr_data.avg, by="Country.name")


##c---- whr_data
###1----
mdl <- Life.Ladder ~ Healthy.life.expectancy.at.birth + Social.support + Freedom.to.make.life.choices + Generosity + Positive.affect + Negative.affect
#mdlvars <- all.vars(mdlA)
#Pooled model
#pool <- lm( Life.Ladder ~ Healthy.life.expectancy.at.birth + Social.support + Freedom.to.make.life.choices + Generosity + Positive.affect + Negative.affect, whr_data)
#summary(pool)
#Fixed effect regression model
fe <- 
  plm(mdl, data = whr_data, 
      index=c("Country.name", "year"), model = "within")
summary(fe)
fe.twoways <- 
  plm(mdl, data = whr_data, 
      index=c("Country.name", "year"), model = "within", effect="twoways")
summary(fe.twoways)
re <- 
  plm(mdl, data = whr_data, 
      index=c("Country.name", "year"), model = "random")
summary(re)
re.twoways <- 
  plm(mdl, data = whr_data, 
      index=c("Country.name", "year"), model = "random", effect="twoways")
summary(re.twoways)


stargazer(fe, re,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text",
          column.labels = c("Fixed Effects", "Random Effects"),
          out = "fe_re.txt")
#Whether we need fe or re. h0: choose RE
stargazer(phtest(fe,re),type="text")
#=> choose FE










