install.packages("ggplot2")
install.packages("tidyverse")
install.packages("ggridges")
install.packages("pheatmap")
install.packages("RColorBrewer")
install.packages("ggalt")
install.packages("ggrepel")
install.packages("factoextra")
install.packages("outliers")
install.packages("epiDisplay") 
install.packages("neuralnet")

library(epiDisplay)
library(outliers)
library(ggplot2)
library(tidyverse) 
library(ggridges)
library(RColorBrewer)
library(ggalt)
library(ggrepel)
library(factoextra) 
library(neuralnet)
attach(vgamer)
vgame <- vgamer
vgame <- vgame %>% filter(Year >= "1989-01-01")
str(vgame)


#1.Number ~ Genre 
vgame %>%
  ggplot(aes(Genre)) + geom_bar(aes(fill = Genre)) + geom_label(stat = 'count', aes(label = ..count..)) +
  theme(legend.position = 'none')+
  theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

#2.Genre ~ Sales
vgame %>%
  ggplot(aes(x=Genre,y=Global_Sales,fill=Genre))+
  geom_histogram(stat="identity")+theme_classic()+coord_flip()+
  labs(title="Sales in the globe")+ 
  theme(plot.title = element_text(hjust = 0.5))

#3.relationship between user_scores and critic_scores
ggplot(vgame,aes(User_Score,Critic_Score))+geom_point()+stat_smooth(method="lm")

#4.facet grid about Scores
p <- ggplot(vgame,aes(User_Score,Critic_Score))+geom_point()+stat_smooth(method="lm")
p+facet_wrap(~Genre,scale="free")

#5.sales ~ Year
ggplot(vgame,aes(x=Year,y=Global_Sales))+ geom_line()+labs(title="Global_Sales changed by years")+   
  scale_x_continuous(breaks = Year)+ theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#6.boxplot - outliers
#6.1 original
vgame %>%
  arrange(Global_Sales)%>%
  ggplot(aes(Genre, Global_Sales,fill=Genre))+geom_boxplot(outlier.shape = 17,outlier.colour = "red")+theme_classic()

#6.2 with notch
vgame %>%
  arrange(Global_Sales)%>%
  ggplot(aes(Genre, Global_Sales,fill=Genre))+geom_boxplot(outlier.shape = 17,outlier.colour = "red",notch=TRUE)+theme_classic()+coord_cartesian(ylim = c(0,4))

#6.3 with notch but no outliers
vgame %>%
  arrange(Global_Sales)%>%
  ggplot(aes(Genre, Global_Sales,fill=Genre))+geom_boxplot(notch=TRUE,outlier.shape = NA)+theme_classic()+coord_cartesian(ylim = c(0,4))

#6.4 outliers (highest/lowest)
test1 <- grubbs.test(vgame$Global_Sales)
test1

test2 <- grubbs.test(vgame$Global_Sales,opposite=TRUE)
test2

#7. outliers dataset
out <- boxplot.stats(vgame$Global_Sales)$out
out_ind <- which(vgame$Global_Sales %in% c(out))
out_ind

vgame2 <- vgame[out_ind,] #outliers
vgame3 <- vgame[-out_ind,] #without outliers

#7.1.relationship between user_scores and critic_scores (with outliers)
vgame <- vgame[,-11]
vgame2 <- na.omit(vgame)
ggplot(vgame2,aes(User_Score,Critic_Score))+geom_point()+stat_smooth(method="lm")

#7.2. regression without outliers
ggplot(vgamer3,aes(User_Score,Critic_Score))+geom_boxplot()
vgame3 %>%
  filter(Critic_Score >= 5)

tab1(vgamer$Genre)
tab1(vgamer$Genre,
     sort.group="increasing")

#ANOVA ANALYSIS
anova(lm(Global_Sales~ESRB_Rating))
plot(Global_Sales ~ ESRB_Rating)

#REGRESSION MODEL
mod1 <- lm(Global_Sales ~ ESRB_Rating  + Critic_Score)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)
