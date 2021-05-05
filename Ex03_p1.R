setwd("E:\\FA20\\Stats\\Exercise 3")
rm(list = ls())
dev.off()
CO <- read.csv("E:\\FA20\\Stats\\Exercise 3\\G5050x03_data\\CO_County.csv")
names(CO) 
CO$PctVote<-CO$TotVote/CO$TotPop
summary(CO$PctVote)
CO$PctVote2<-CO$PctVote*100
CO$PctPov2<-CO$PctPov*100
attach(CO)
plot(PctPov,PctVote)
plot(PctVote ~ PctPov, cex=.7, pch=19, xlab='Percent in Poverty',
     ylab='Percent of Population Voting in the 2012 Election',main="Voting and
 Poverty by County in Colorado")
Mod01<-lm (PctVote~PctPov) 
summary(Mod01) 
abline (lm(PctVote~PctPov), col='red') 

CO$PctOv65 <- (CO$TotOv65/CO$TotPop)
attach(CO)
plot(PctOv65,PctVote)
plot(PctVote ~ PctOv65, cex=.7, pch=19, xlab='Percent Over 65', ylab='Percent of Population Voting in the 2012 Election', main="Voting and Seniors by County in Colorado")
abline (lm(PctVote ~ PctOv65), col='blue')
cor(CO$PctOv65, CO$PctVote)

tapply(PctVote, Region, summary)
tapply(PCTOBM, Region, summary)

boxplot(PctVote ~ Region)
boxplot(PCTOBM ~ Region)

# anova
aov1 <- aov(PctVote ~ Region) 
aov1
summary(aov1)

# homoscedasticity
hov1 <- bartlett.test(PctVote ~ Region)
hov1

# kruskal-wallis
kruskal.test(PctVote ~ Region) 

install.packages('ggplot2')
library(ggplot2)

ggplot(CO,aes(x=PctPov2,y=PctVote2,color=Region))+geom_point()+geom_smooth(method="lm")

orstationc <- read.csv("E:\\FA20\\Stats\\Exercise 3\\G5050x03_data\\orstationc.csv", as.is=T)
attach(orstationc)
plot(lon,lat)
text(lon, lat, labels=station)
plot(elev, pann)
plot(orstationc[,2:10])
plot(elev, tjan)
plot(elev, tjul)
plot(elev, tann)
plot(elev, pjan)

cor(elev,pann)
cor(orstationc[,2:10])

detach(orstationc)

orsim <- read.csv("E:\\FA20\\Stats\\Exercise 3\\G5050x03_data\\orsim.csv")

# t-tests among groups with different variances
attach(orsim)
boxplot(TJan1x, TJan2x)
print(mean(TJan2x)-mean(TJan1x)) # the temperature difference
tt.TJan <- t.test(TJan2x, TJan1x)
tt.TJan

# two-tailed test
boxplot(PJan1x, PJan2x)
mean(PJan2x)-mean(PJan1x)
tt.PJan.1 <- t.test(PJan2x, PJan1x)
tt.PJan.1

# one-tailed test
boxplot(PJan1x, PJan2x)
mean(PJan2x)-mean(PJan1x)
tt.PJan.2 <- t.test(PJan2x, PJan1x, alternative="greater")
tt.PJan.2 

# part 2

install.packages("sf", dep = TRUE)
library(sf)
install.packages("tmap", dep = TRUE)
library(tmap)
install.packages("GISTools", dep = TRUE)
library(GISTools)
install.packages("raster", dep = TRUE)
library(raster)
install.packages("OpenStreetMap", dep = TRUE)
library(OpenStreetMap)
install.packages("RgoogleMaps", dep = TRUE)
library(RgoogleMaps)
install.packages("grid", dep=TRUE)
library(grid)
install.packages("rgdal", dep=TRUE)
library(rgdal)
install.packages("tidyverse", dep = TRUE)
library(tidyverse)
install.packages("reshape2", dep=TRUE)
library(reshape2)
install.packages("ggmosaic", dep=TRUE)
library(ggmosaic)

tmap_mode('plot')
data(georgia)
georgia_sf <- st_as_sf(georgia)
qtm(georgia, fill = 'red', style = 'natural')
data(newhaven)
ls()
blocks_sf <- st_as_sf(blocks)
breach_sf <- st_as_sf(breach)
tracts_sf <- st_as_sf(tracts)
summary(blocks_sf)
tm_shape(blocks_sf) + tm_polygons("P_OWNEROCC", breaks=seq(0, 100, by=25)
                                  
tm_shape(blocks_sf) + tm_polygons("P_OWNEROCC", breaks=c(10, 40, 60, 90)                           
                                  
tm_shape(blocks_sf) + tm_polygons("P_OWNEROCC", title = "Owner Occ") + tm_layout(legend.title.size = 1, legend.text.size = 1, legend.position = c(0.1, 0.1))                     
tm_shape(blocks_sf) + 
      tm_polygons("P_18_24", title = "Population: 18 - 24", breaks = c(0, 5, 10, 20, 40), palette = "Purples", legend.hist = T) + 
      tm_layout(frame = F, title = "New Haven, Georgia", title.size = 1.4, title.position = c(0.45, "top"), legend.hist.size = 0.5, legend.title.size = 1, legend.text.size = .75, legend.position = c(0.02, 0.01)) +
      tm_compass(position = c(0.8, 0.07)) + tm_scale_bar(width = 0.22)
