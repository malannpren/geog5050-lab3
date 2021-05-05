setwd("E:\\FA20\\Stats\\Exercise 3")
rm(list = ls())
CO <- read.csv("E:\\FA20\\Stats\\Exercise 3\\G5050x03_data\\CO_County.csv")
names(CO)

# calculating new columns to look at voting behavior
CO$PctVote <- CO$TotVote/CO$TotPop
summary(CO$PctVote)
hist(CO$PctVote)
boxplot(CO$PctVote)

# expressed as percentages:
CO$PctVote2 <- CO$PctVote * 100
CO$PctVote <- CO$PctPov * 100

attach(CO)
summary(CO)

# building plots

plot(PctPov, PctVote)
# x-axis variable first, y-axis variable second

# alternatively, use the tilde ~ (as it varies with/as a function of)
# notice that this reverses the order!
# tilde parameters:
# cex = point size as a proportion of the default
# pch = a nominal number that changes the type of character type)
# xlab = x-axis label
# ylab = y-axis label

plot(PctVote ~ PctPov, cex=.7, pch=10, xlab='Percent in Poverty', ylab='Percent of Population Voting in the 2012 Election', main="Voting and Poverty by County in Colorado")

# performing a basic univariate linear regression line to determine how two variables are related:
Mod01 <- lm (PctVote~PctPov)
summary(Mod01)

# seeing the lm on the plot: use the function abline()
# abline ([intercept], slope]) - this is a general example

abline(lm(PctPov~PctVote), col='red')
