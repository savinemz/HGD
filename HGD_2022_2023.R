#### Les Chemins des Ducs ####
#Schema de dispersion postnatal du HGD en region HDF

setwd("C:/Git/HGD")

library(data.table)
library(ggplot2)

data_HGD <- read.csv2("data_HGD.csv", head = T, sep = ";", stringsAsFactors = T)
data_HGD <- subset(data_HGD, Affichage == "OUI")
summary(data_HGD)

#supression colonnes
data_HGD <- data_HGD[,-c(1,5,27,28)]

attach(r)
summary(r)
shapiro.test(r$X2001)
shapiro.test(r$X2005)
boxplot(r, notch = T)

library(car)
library(faraway)
library(hier.part)
library(MASS)
library(mgcv)
library(MuMIn)
library(multcomp)
library(shiny)
library(vegan)
library(xtable)
library(glmmTMB)