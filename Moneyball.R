setwd("C:/Users/User/Desktop/Äéáöïñá Óåìéíáñéá,E-learnings/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects")

batting <- read.csv("Capstone Project/Batting.csv")

library(dplyr)

batting %>% distinct(teamID)

#checking the structure of the df
str(batting)

head(batting$AB,5)
head(batting$X2B)

#calculating the batting average and add it on our df

batting<- batting %>% mutate(BA= H/AB)

tail(batting$BA,5) #check if everything's fine

#calculate OBP (on base percentage)

batting <- batting %>% mutate(OBP=(H+BB+HBP)/(AB+BB+HBP+SF))

#calculating slugging average (SLG)
batting<- batting %>% mutate(X1B=H-X2B-X3B-HR)
batting<- batting %>% mutate(SLG=(X1B + (2*X2B) + (3*X3B) + (4*HR))/AB)

#importing our salaries df
salaries<-read.csv("Capstone Project/Salaries.csv")

summary(batting) #minimum yearID 1971
summary(salaries) #minimum yearID 1985

batting<-batting %>% filter(yearID>=1985) #both batting and salaries start from the same year
summary(batting) #checking if everything is correct

combo<-merge(batting,salaries,by=c('yearID','playerID')) #combining the 2 df in one

#subsetting the 3 players that left 
lost_players <- combo %>% filter(playerID=='giambja01' | playerID=='saenzol01' | playerID=='damonjo01')
#OR
v<- c('giambja01','saenzol01','damonjo01')
lostp <- combo %>% filter(playerID %in% v)

#Since all these players were lost in after 2001 in the offseason,
#let's only concern ourselves with the data from 2001.

lost_2001<-lost_players %>% 
  filter(yearID==2001) %>%
  select(playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB)

#Find Replacement Players for the key three players we lost!

replace_players<-combo %>% 
  filter(yearID==2001) %>% 
  select(playerID,yearID,AB,OBP,salary)

sum(lost_2001$AB) 
mean(lost_2001$OBP)

library(ggplot2)
ggplot(replace_players,aes(OBP,salary)) + geom_point(size=2) +
  scale_y_continuous(labels = scales::comma)

replace_players<-replace_players %>% filter(salary<8000000 & OBP>0)

1469/3 #Finding the AB we need for each player

replace_players<- replace_players %>% filter(AB>=450)
replace_players<- replace_players %>% arrange(desc(OBP)) %>% 
  filter(playerID != 'damonjo01' & playerID !='giambja01' & playerID !='saenzol01' )
