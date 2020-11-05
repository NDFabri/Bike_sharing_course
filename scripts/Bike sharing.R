rm(list=ls())
##Installing and loading libraries (RUN THIS PART LINE BY LINE!!)####

# Install and load package devtools
if (!requireNamespace("devtools"))
  install.packages("devtools")
library("devtools") #Use of version 2.0.2

# Install and load package Bikecourse
install_github('NDFabri/Bike_sharing_course/packages/Bikecourse@main')
library("Bikecourse") #Use of version 0.1.0

# Install and load package tidyverse
if (!requireNamespace("tidyverse"))
  install.packages("tidyverse")
library("tidyverse") #Use of version 1.2.1

# Install and load package plyr
if (!requireNamespace("plyr"))
  install.packages("plyr")
library("plyr") #Use of version 1.8.4

##Loading datafile####
Data <- read_csv("data/train.csv")

##Check if there is an association between count and season####

#Making season as a factor and renaming values in season
Data$season <- as.character(Data$season)
Data$season <- revalue(Data$season, c("1"="1:January-March", "2"="2:April-June", "3"="3:July-September", "4"="4:October-December"))

#See if there is an association between count and season
CS <- bikecourse_function(Data$count, Data$season, data=Data,
                    threshold = 0.05,
                    xlab = "season", ylab = "count",
                    yax = 900, size = 7)
CS #Make a boxplot of the association

#Saving the boxplot as a jpeg
jpeg(file="plots/count_vs_season.jpeg")
CS
dev.off()

##Check if there is an association between count and holiday####

#Making holiday as a factor and renaming values in holiday
Data$holiday <- as.character(Data$holiday)
Data$holiday <- revalue(Data$holiday, c("1"="Yes", "0"="No"))

#See if there is an association between count and holiday
CH <- bikecourse_function(Data$count, Data$holiday, data=Data,
                    threshold = 0.05,
                    xlab = "holiday", ylab = "count",
                    yax = 900, size = 7)

##Check if there is an association between count and workingday####

#Making workingday as a factor and renaming values in workingday
Data$workingday <- as.character(Data$workingday)
Data$workingday <- revalue(Data$workingday, c("1"="Yes", "0"="No"))

#See if there is an association between count and workingday
CW <- bikecourse_function(Data$count, Data$workingday, data=Data,
                    threshold = 0.05,
                    xlab = "workingday", ylab = "count",
                    yax = 900, size = 7)

##Check if there is an association between count and weather####

#Making weather as a factor
Data$weather <- as.character(Data$weather)

#See if there is an association between count and weather
CWe <- bikecourse_function(Data$count, Data$weather, data=Data,
                    threshold = 0.05,
                    xlab = "weathertype", ylab = "count",
                    yax = 900, size = 7)
CWe #Make a boxplot of the association

#Saving the boxplot as a jpeg
jpeg(file="plots/count_vs_weather.jpeg")
CWe
dev.off()

##Check if there is an association between count and temp####

#See if there is an association between count and temp
CT <- bikecourse_function(Data$count, Data$temp, data=Data,
                    threshold = 0.05,
                    xlab = "temp", ylab = "count",
                    yax = 900, size = 7)
CT #Make a scatterplot of the association

#Saving the scatterplot as a jpeg
jpeg(file="plots/count_vs_temp.jpeg")
CT
dev.off()

##Check if there is an association between count and atemp####

#See if there is an association between count and atemp
CA <- bikecourse_function(Data$count, Data$atemp, data=Data,
                    threshold = 0.05,
                    xlab = "atemp", ylab = "count",
                    yax = 900, size = 7)
CA #Make a scatterplot of the association

#Saving the scatterplot as a jpeg
jpeg(file="plots/count_vs_atemp.jpeg")
CA
dev.off()

##Check if there is an association between count and humidity####

#See if there is an association between count and humidity
CHu <-bikecourse_function(Data$count, Data$humidity, data=Data,
                    threshold = 0.05,
                    xlab = "humidity", ylab = "count",
                    yax = 900, size = 7)
CHu #Make a scatterplot of the association

#Saving the scatterplot as a jpeg
jpeg(file="plots/count_vs_humidity.jpeg")
CHu
dev.off()

##Check if there is an association between count and windspeed####

#See if there is an association between count and windspeed
CWi <-bikecourse_function(Data$count, Data$windspeed, data=Data,
                    threshold = 0.05,
                    xlab = "windspeed", ylab = "count",
                    yax = 900, size = 7)
CWi #Make a scatterplot of the assocation

#Saving the scatterplot as a jpeg
jpeg(file="plots/count_vs_windspeed.jpeg")
CWi
dev.off()
