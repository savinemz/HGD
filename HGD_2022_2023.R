#### Les Chemins des Ducs ####
#Schema de dispersion postnatal du HGD en region HDF
library(sf); library(dplyr)
library(data.table)
library(ggplot2)
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
library(dplyr)


setwd("C:/Git/HGD")


data_HGD_original <- read.csv2("data_HGD.csv", head = T, sep = ";", stringsAsFactors = T)
plot(data_HGD_original$Affichage)
summary(data_HGD_original)

data_HGD <- subset(data_HGD_original, Affichage != "NON") # j'enlève tous les NON
summary(data_HGD)
data_HGD <- subset(data_HGD, Commentair != "Pas de coordonnées GPS")
summary(data_HGD)
attach(data_HGD)

#supression colonnes
data_HGD <- data_HGD[,-c(1,5,27,28)]#device_id, datatype, Affichage, Commentair
summary(data_HGD)
names(data_HGD)[25] <- "bird_id"
data_HGD <- data_HGD %>% relocate(bird_id, .after = UTC_dateti)
data_HGD <- data_HGD %>% relocate(UTC_dateti, .after = bird_id)


hist(bird_id)
boxplot(Altitude_m~bird_id, notch = T)


setDT(data_HGD)
nb_data_HH <- data_HGD [,.(nb_data_HH = .N), by = .(bird_id, date_HH)] #voir pour modifier date_HH voir le format sur script original
nb_data_HH[,nb_data_HH := as.numeric(nb_data_HH)]
#bp <- boxplot (nb_data_HH$nb_data_HH ~ nb_data_HH$bird_id)

