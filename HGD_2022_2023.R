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


setwd("C:/Git/HGD")


### Data Habitat ####
habitat <- st_read("SIG/CLC_HDF_2018.shx", stringsAsFactors = T)
habitat <- habitat[,-4]
summary(habitat)
habitat <- st_transform(habitat,crs=3832)

gg <- plot(habitat$code_18)
ggsave("Rplot/hab.png",gg)#marche pas

#habitat$area_poly <- st_area(habitat)

#calcul des surfaces totales par habitat
area_hab <- aggregate(area_poly~habitat$code_18, habitat, sum)


#calcul des surfaces par motu
area_tot <- aggregate(habitat$area_poly, habitat, sum)
names(area_motu)[2] <- "area_motu"
rangi <- merge(rangi, area_motu, by = "id_motu")




### Data HGD ####
data_HGD_original <- read.csv2("data_HGD.csv", head = T, sep = ";", stringsAsFactors = T)
str(data_HGD_original)
plot(data_HGD_original$Affichage)
summary(data_HGD_original)

data_HGD <- subset(data_HGD_original, Affichage != "NON") # j'enlève tous les NON
summary(data_HGD)

data_HGD <- subset(data_HGD, Commentair != "Pas de coordonnées GPS")
summary(data_HGD)


#Nettoyage des données
data_HGD <- data_HGD[,-c(1,2,5,27,28)]#device_id,UTC_dateti, datatype, Affichage, Commentair
names(data_HGD)[24] <- "bird_id"
data_HGD <- data_HGD %>% relocate(bird_id, .after = UTC_date)
data_HGD <- data_HGD %>% relocate(UTC_date, .after = bird_id)
summary(data_HGD)

setDT(data_HGD)
data_HGD[,UTC_date:= as.character(UTC_date)]
data_HGD[,UTC_time:= as.character(UTC_time)]
str(data_HGD)
attach(data_HGD)

plot(bird_id)
#boxplot(Altitude_m~bird_id, notch = T)

## ajout de la colonne date et la colonne heure manuellement
data_HGD$heure_HH <- substr(data_HGD$UTC_time,1,2)
data_HGD$date_HH <- paste0(data_HGD$UTC_date, "_", data_HGD$heure_HH)

# Transformation des coordonnees en donnees spatiales + modification de la projection
HGD_sf <- st_as_sf(data_HGD, coords = c("Longitude","Latitude"))
st_crs(HGD_sf) <- 4326
HGD_sf <- st_transform(HGD_sf,crs=3832)
sum_loc <- st_intersection(habitat, HGD_sf)


nb_data_HH <- data_HGD [,.(nb_data_HH = .N), by = .(bird_id, date_HH)]
nb_data_HH[,nb_data_HH := as.numeric(nb_data_HH)]
#bp <- boxplot (nb_data_HH$nb_data_HH ~ nb_data_HH$bird_id)


# figure nombre de données par heure par oiseaux
library(ggplot2)
gg <- ggplot(nb_data_HH,aes(x=nb_data_HH,y=bird_id)) + geom_violin()
gg <- gg + labs(y = "Bird_id", x = "Number of data per hour")
gg

ggsave("Rplot/nb_data_HH2.png",gg)

gg2 <- ggplot(nb_data_HH,aes(x=nb_data_HH)) + geom_histogram() + facet_grid(bird_id~.)
gg2



### Tableau description donnees HGD ########################################

library(data.table)
sum_HGD <- data_HGD[,.(nb_data = .N,
                              first = min(UTC_date),
                              last = max(UTC_date)), by =.(bird_id)] # par oiseau: combien de donnees, premiere date et derniere date

sum_HGD[,duration_days := difftime(last, first, unit = "days")]#difference de temps entre la premiere et la derniere donnee. le ":=" veut dire pas de regroupement

nb_day <- data_HGD [,.(j = 1), by = .(bird_id, UTC_date)] # regroupement par oiseau et par date
nb_day <- nb_day [,.(nb_day= .N), by = .(bird_id)] # nombre de jour de données 
sum_HGD <- merge(sum_HGD, nb_day, bx = "bird_id")
#fwrite(sum_HGD, "table/sum_HGD_OrniTrack.csv")
