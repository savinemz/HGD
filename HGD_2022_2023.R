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
#ggsave("Rplot/hab.png",gg)

habitat$area_poly <- st_area(habitat)

#calcul des surfaces totales par habitat
area_hab <- aggregate(area_poly~habitat$code_18, habitat, sum)



### Data HGD ####
data_HGD_original <- read.csv2("data_HGD.csv", head = T, sep = ";", stringsAsFactors = T)
str(data_HGD_original)


#Nettoyage des données
#Suppression des data ayant des problemes d'affichage
plot(data_HGD_original$Affichage)
data_HGD <- subset(data_HGD_original, Affichage != "NON") # j'enlève tous les NON
data_HGD <- subset(data_HGD, Commentair != "Pas de coordonnées GPS") #j'enleve tous les "Pas de coordonnées GPS"
summary(data_HGD)

#Suppression des colonnes inutiles + Renommer certaines colonnes
data_HGD <- data_HGD[,-c(1,2,5,27,28)] # supression colonnes device_id,UTC_dateti, datatype, Affichage, Commentair
names(data_HGD)[24] <- "bird_id"
data_HGD <- data_HGD %>% relocate(bird_id, .after = UTC_date)
data_HGD <- data_HGD %>% relocate(UTC_date, .after = bird_id)
names(data_HGD)[3] <- "time"
names(data_HGD)[21] <- "day_night"
summary(data_HGD)


#creation d'une colonne date avec separateur virgule
setDT(data_HGD)
data_HGD$datej <- substr(data_HGD$UTC_date,1,2)
data_HGD$datem <- substr(data_HGD$UTC_date,4,5)
data_HGD$datea <- substr(data_HGD$UTC_date,7,8)
data_HGD$datea2 <- substr(data_HGD$UTC_date,9,10)
data_HGD$date <- paste0(data_HGD$datea,"", data_HGD$datea2,"-",data_HGD$datem, "-", data_HGD$datej)
data_HGD <- data_HGD %>% relocate(date, .after = bird_id)
data_HGD[,date:= as.character(date)]
data_HGD[,time:= as.character(time)]
data_HGD <- data_HGD[,-c(3,27,28,29,30)]
str(data_HGD)

attach(data_HGD)
plot(bird_id)


## ajout de la colonne date et la colonne heure manuellement
data_HGD$heure_HH <- substr(data_HGD$time,1,2)
data_HGD$date_HH <- paste0(data_HGD$date, "_", data_HGD$heure_HH)

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
#ggsave("Rplot/nb_data_HH2.png",gg)
#gg2 <- ggplot(nb_data_HH,aes(x=nb_data_HH)) + geom_histogram() + facet_grid(bird_id~.)
#gg2



### Tableau description donnees HGD ########################################

library(data.table)
sum_HGD <- data_HGD[,.(nb_data = .N,
                              first = min(date),
                              last = max(date)), by =.(bird_id)] # par oiseau: combien de donnees, premiere date et derniere date

sum_HGD[,duration_days := difftime(last, first, unit = "days")]#difference de temps entre la premiere et la derniere donnee. le ":=" veut dire pas de regroupement

nb_day <- data_HGD [,.(j = 1), by = .(bird_id, date)] # regroupement par oiseau et par date
nb_day <- nb_day [,.(nb_day= .N), by = .(bird_id)] # nombre de jour de données 
sum_HGD <- merge(sum_HGD, nb_day, bx = "bird_id")
#fwrite(sum_HGD, "table/sum_HGD_OrniTrack.csv")

### figure nombre de donnees par oiseau par jour par J/N ##########################################################################################################

nb_data_j_daynight <- data_HGD [,.(nb_data_jour = .N), by = .(bird_id, date, day_night)]#comme on parle de J/N après, c'est interessant de savoir comment sont distribues les donnees
all_date <- expand.grid(bird_id = unique(data_HGD[, bird_id]), date = seq(as.Date(min(data_HGD$date)), as.Date(max(data_HGD$date)),1), day_night = c("Jour", "Nuit"))#pb d'affichage
all_date$date <- as.character(all_date$date)
nb_data_j_daynight <- merge(nb_data_j_daynight, all_date, by = c("bird_id","date", "day_night"), all = T)
nb_data_j_daynight$nb_data_jour[is.na(nb_data_j_daynight$nb_data_jour)] <- 0

library(ggplot2)
gg <- ggplot(data = nb_data_j_daynight, aes(x = as.Date(date), y = nb_data_jour, colour = day_night, group = day_night))
gg <- gg + geom_point()
gg <- gg + geom_line()
gg <- gg + facet_grid(bird_id~.)
gg <- gg + geom_point(data = subset(nb_data_j_daynight, nb_data_jour == 0), colour = "white", size = 0.8, alpha = 0.5)
gg <- gg + labs(x= "week", y= "number of localisation per week")
gg
#ggsave("Rplot/OrniTrack/nb_data_daynight.png",gg)

