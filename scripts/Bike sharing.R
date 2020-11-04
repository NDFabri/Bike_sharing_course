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

#Adding the (non)differences between whether or not there is a holiday in the boxplot
CHplot <- CHplot + 
  annotate("text", x=1, y=700, label="a", size=7) +
  annotate("text", x=2, y=760, label="a", size=7)
CHplot

##Check if there is a relationship between count and workingday####

#Making season as a factor and renaming values in workingday
Data$workingday <- as.character(Data$workingday)
Data$workingday <- revalue(Data$workingday, c("1"="Yes", "0"="No"))

#Boxplot with counts per workingday
CWplot <- ggplot(data = Data, mapping = aes(x = workingday, y = count, group=workingday)) +
  geom_boxplot(alpha = 0) + theme_bw() 
CWplot

#Test to see if there is a difference in counts between whether or not there is a workingday
wilcox.test(count ~ workingday, data = Data)
#Since the p-value is 0.9679 we can conclude that there is no difference between whether or not there is a workingday

#Adding the (non)differences between whether or not there is a workingday in the boxplot
CWplot <- CWplot + 
  annotate("text", x=1, y=735, label="a", size=7) +
  annotate("text", x=2, y=675, label="a", size=7)
CWplot
