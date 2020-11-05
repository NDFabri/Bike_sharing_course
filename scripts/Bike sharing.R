rm(list=ls())
##Installing and loading libraries####

# Install and load package devtools
if (!requireNamespace("devtools"))
  install.packages("devtools")
library("devtools")

# Install and load package Bikecourse
install_github('NDFabri/Bike_sharing_course/packages/Bikecourse@main')
library("Bikecourse")

# Install and load package tidyverse
if (!requireNamespace("tidyverse"))
  install.packages("tidyverse")
library("tidyverse")

# Install and load package plyr
if (!requireNamespace("plyr"))
  install.packages("plyr")
library("plyr")

##Loading datafile####
Data <- read_csv("data/train.csv")

##Check if there is an association between count and season####

#Making season as a factor and renaming values in season
Data$season <- as.character(Data$season)
Data$season <- revalue(Data$season, c("1"="1:January-March", "2"="2:April-June", "3"="3:July-September", "4"="4:October-December"))

#See if there is an association between count and season
#And if so, make a boxplot
bikecourse_function(Data$count, Data$season, data=Data,
                    threshold = 0.05,
                    xlab = "season", ylab = "count",
                    yax = 900, size = 7)

##Check if there is an association between count and holiday####

#Making holiday as a factor and renaming values in holiday
Data$holiday <- as.character(Data$holiday)
Data$holiday <- revalue(Data$holiday, c("1"="Yes", "0"="No"))

#See if there is an association between count and holiday
#And if so, make a boxplot
bikecourse_function(Data$count, Data$holiday, data=Data,
                    threshold = 0.05,
                    xlab = "holiday", ylab = "count",
                    yax = 900, size = 7)

##Check if there is an association between count and workingday####

#Making workingday as a factor and renaming values in workingday
Data$workingday <- as.character(Data$workingday)
Data$workingday <- revalue(Data$workingday, c("1"="Yes", "0"="No"))

#See if there is an association between count and workingday
#And if so, make a boxplot
bikecourse_function(Data$count, Data$workingday, data=Data,
                    threshold = 0.05,
                    xlab = "workingday", ylab = "count",
                    yax = 900, size = 7)

##Check if there is an association between count and weather####

#Making weather as a factor
Data$weather <- as.character(Data$weather)

#See if there is an association between count and weather
#And if so, make a boxplot
bikecourse_function(Data$count, Data$weather, data=Data,
                    threshold = 0.05,
                    xlab = "weathertype", ylab = "count",
                    yax = 900, size = 7)

##Check if there is an association between count and temp####

#See if there is an association between count and temp
#And if so, make a scatterplot with regressionline
bikecourse_function(Data$count, Data$temp, data=Data,
                    threshold = 0.05,
                    xlab = "temp", ylab = "count",
                    yax = 900, size = 7)

##Check if there is an association between count and atemp####

#See if there is an association between count and atemp
#And if so, make a scatterplot with regressionline
bikecourse_function(Data$count, Data$atemp, data=Data,
                    threshold = 0.05,
                    xlab = "atemp", ylab = "count",
                    yax = 900, size = 7)

##Check if there is an association between count and humidity####

#See if there is an association between count and humidity
#And if so, make a scatterplot with regressionline
bikecourse_function(Data$count, Data$humidity, data=Data,
                    threshold = 0.05,
                    xlab = "humidity", ylab = "count",
                    yax = 900, size = 7)

##Check if there is an association between count and windspeed####

#See if there is an association between count and windspeed
#And if so, make a scatterplot with regressionline
bikecourse_function(Data$count, Data$windspeed, data=Data,
                    threshold = 0.05,
                    xlab = "windspeed", ylab = "count",
                    yax = 900, size = 7)