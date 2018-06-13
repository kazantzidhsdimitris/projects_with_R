setwd("C:/Users/User/Desktop/Διαφορα Σεμιναρια,E-learnings/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects/Data Visualization Project")

df<- read.csv("Economist_Assignment_Data.csv",stringsAsFactors = FALSE)

head(df)
summary(df)
library(ggthemes)
library(ggplot2)

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

df$Region[df$Region=='SSA']<-"Sub-Saharan Africa"
df$Region[df$Region=='MENA']<-"Middle-East & North Africa"


ggplot(df,aes(CPI,HDI)) +
  geom_point(aes(color=Region),shape=1,size=3)+ #adding color per region
  geom_smooth(method='lm',se=FALSE,formula = y~log(x),color='red') + #inserting geom_smooth
  geom_text(aes(label=Country),data = subset(df,Country %in% pointsToLabel),check_overlap = TRUE)+ #subsetting the text in the graph
  theme_economist_white() +
  scale_x_continuous(name = 'Corruption Perceptions Index,2011( 10=least corrupt)',limits=c(1,10),breaks =seq(1,10,1))+ #limiting x-axis
  scale_y_continuous(name='Human Development Index,2011(1=best)',limits = c(0,1),breaks = seq(0,1,0.2)) + #limiting y-axis
  ggtitle('Corruption and Human Development')+ #adding title to the graph
  theme(legend.position = 'top',legend.background = element_rect( fill='grey',size=0.5)) #changing position of the graph legenf
