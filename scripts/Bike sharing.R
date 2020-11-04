rm(list=ls())
##Installing and loading libraries + loading datafile####

# Install the packages if not already installed
if (!requireNamespace("tidyverse"))
  install.packages("tidyverse")
if (!requireNamespace("dunn.test"))
  install.packages("dunn.test")
if (!requireNamespace("plyr"))
  install.packages("plyr")

# loading library
library("tidyverse")
library("dunn.test")
library("plyr")

#Loading datafile
Data <- read_csv("data/train.csv")

##Check if there is a relationship between count and season####

#Making season as a factor and renaming values in season
Data$season <- as.character(Data$season)
Data$season <- revalue(Data$season, c("1"="1:January-March", "2"="2:April-June", "3"="3:July-September", "4"="4:October-December"))

#Boxplot with counts per season
CSplot <- ggplot(data = Data, mapping = aes(x = season, y = count, group=season)) +
  geom_boxplot(alpha = 0) + theme_bw() +
  xlab("")
CSplot

#Test to see if there is a difference in counts between seasons
kruskal.test(count ~ season, data = Data)
#Since the p-value is lower than 0.001 we assume that there is a difference
#Dunn test as a post-hoc test (to see where the difference is)
dunn.test(Data$count, Data$season, method = "sidak")

#Adding the differences between seasons in the boxplot
CSplot <- CSplot + 
  annotate("text", x=1, y=425, label="a", size=7) +
  annotate("text", x=2, y=775, label="b", size=7) +
  annotate("text", x=3, y=800, label="c", size=7) +
  annotate("text", x=4, y=700, label="b", size=7)
CSplot

##Check if there is a relationship between count and holiday####

#Making holiday as a factor and renaming values in holiday
Data$holiday <- as.character(Data$holiday)
Data$holiday <- revalue(Data$holiday, c("1"="Yes", "0"="No"))

#Boxplot with counts per whether or not there is a holiday
CHplot <- ggplot(data = Data, mapping = aes(x = holiday, y = count, group=holiday)) +
  geom_boxplot(alpha = 0) + theme_bw()
CHplot

#Test to see if there is a difference in counts between whether or not there is a holiday
wilcox.test(count ~ holiday, data = Data)
#Since the p-value is 0.8646 we can conclude that there is no difference between whether or not there is a holiday

##Check if there is a relationship between count and workingday####

#Making workingday as a factor and renaming values in workingday
Data$workingday <- as.character(Data$workingday)
Data$workingday <- revalue(Data$workingday, c("1"="Yes", "0"="No"))

#Boxplot with counts per workingday
CWplot <- ggplot(data = Data, mapping = aes(x = workingday, y = count, group=workingday)) +
  geom_boxplot(alpha = 0) + theme_bw() 
CWplot

#Test to see if there is a difference in counts between whether or not there is a workingday
wilcox.test(count ~ workingday, data = Data)
#Since the p-value is 0.9679 we can conclude that there is no difference between whether or not there is a workingday

##Check if there is a relationship between count and weather####

#Making weather as a factor and renaming values in weather
Data$weather <- as.character(Data$weather)

#Boxplot with counts per weather
CWeplot <- ggplot(data = Data, mapping = aes(x = weather, y = count, group=weather)) +
  geom_boxplot(alpha = 0) + theme_bw() +
  xlab("weathertype")
CWeplot

#Test to see if there is a difference in counts between weather
kruskal.test(count ~ weather, data = Data)
#Since the p-value is lower than 0.001 we assume that there is a difference
#Dunn test as a post-hoc test (to see where the difference is)
dunn.test(Data$count, Data$weather, method = "sidak")

#Adding the differences between weather in the boxplot
CWeplot <- CWeplot + 
  annotate("text", x=1, y=750, label="a", size=7) +
  annotate("text", x=2, y=655, label="b", size=7) +
  annotate("text", x=3, y=420, label="c", size=7) +
  annotate("text", x=4, y=220, label="a,b,c", size=7)
CWeplot


##Check if there is a relationship between count and temp####

#Scatterplot with counts per temp
CTplot <- ggplot(Data, aes(x=temp, y=count)) +
  geom_point() + theme_bw()
CTplot

#Test to see if there is a association between counts and temp
m1 <- lm(count ~ temp, data=Data)
summary(m1)
#Since the p-value is lower than 0.001 we asume that there is an association between counts and temp

#Adding regression line to the scatterplot
CTplot <- CTplot + geom_smooth(method="lm")
CTplot

##Check if there is a relationship between count and atemp####

#Scatterplot with counts per atemp
CAplot <- ggplot(Data, aes(x=atemp, y=count)) +
  geom_point() + theme_bw()
CAplot

#Test to see if there is a association between counts and atemp
m2 <- lm(count ~ atemp, data=Data)
summary(m2)
#Since the p-value is lower than 0.001 we asume that there is an association between counts and atemp

#Adding regression line to the scatterplot
CAplot <- CAplot + geom_smooth(method="lm")
CAplot

##Check if there is a relationship between count and humidity####

#Scatterplot with counts per humidity
CHuplot <- ggplot(Data, aes(x=humidity, y=count)) +
  geom_point() + theme_bw()
CHuplot

#Test to see if there is a association between counts and humidity
m3 <- lm(count ~ humidity, data=Data)
summary(m3)
#Since the p-value is lower than 0.001 we asume that there is an association between counts and humidity

#Adding regression line to the scatterplot
CHuplot <- CHuplot + geom_smooth(method="lm")
CHuplot

##Check if there is a relationship between count and windspeed####

#Scatterplot with counts per windspeed
CWiplot <- ggplot(Data, aes(x=windspeed, y=count)) +
  geom_point() + theme_bw()
CWiplot

#Test to see if there is a association between counts and windspeed
m4 <- lm(count ~ windspeed, data=Data)
summary(m4)
#Since the p-value is lower than 0.001 we asume that there is an association between counts and windspeed

#Adding regression line to the scatterplot
CWiplot <- CWiplot + geom_smooth(method="lm")
CWiplot
