
##Installing and loading libraries + loading datafile####

# Install the packages if not already installed
if (!requireNamespace("tidyverse"))
  install.packages("tidyverse")
if (!requireNamespace("dunn.test"))
  install.packages("dunn.test")

# loading library
library("tidyverse")
library("dunn.test")

#Loading datafile
Data <- read_csv("data/train.csv")

##Check if there is a relationship between count and season####

#Boxplot with counts per season
CSplot <- ggplot(data = Data, mapping = aes(x = season, y = count, group=season)) +
  geom_boxplot(alpha = 0) + theme_bw()
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
