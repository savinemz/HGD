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

#calculs des surfaces par polygones
habitat <- st_read("SIG/CLC_HDF_2018.shx", stringsAsFactors = T)
habitat <- habitat[,-c(1,2,4,6)]# colonne non utilise
habitat <- st_transform(habitat,crs=3832)#transformation des donnees
habitat$area_poly <- st_area(habitat)
summary(habitat)

#identifiant des polygones
habitat$id_poly <- 1: nrow(habitat)
habitat <- habitat %>% relocate(id_poly, .after = code_18)


#suppression des habitats non utilise par le HGD
habitat <- subset(habitat, habitat$code_18 != "111")
habitat <- subset(habitat, habitat$code_18 != "122")
habitat <- subset(habitat, habitat$code_18 != "123")
habitat <- subset(habitat, habitat$code_18 != "124")
habitat <- subset(habitat, habitat$code_18 != "133")
habitat <- subset(habitat, habitat$code_18 != "141")
habitat <- subset(habitat, habitat$code_18 != "221")
habitat <- subset(habitat, habitat$code_18 != "222")
habitat <- subset(habitat, habitat$code_18 != "321")
habitat <- subset(habitat, habitat$code_18 != "331")
habitat <- subset(habitat, habitat$code_18 != "333")
habitat <- subset(habitat, habitat$code_18 != "412")
habitat <- subset(habitat, habitat$code_18 != "421")
habitat <- subset(habitat, habitat$code_18 != "423")
habitat <- subset(habitat, habitat$code_18 != "511")
habitat <- subset(habitat, habitat$code_18 != "521")
habitat <- subset(habitat, habitat$code_18 != "522")
habitat <- subset(habitat, habitat$code_18 != "523")


gg <- plot(habitat$code_18)
#ggsave("Rplot/hab.png",gg)


#calcul des surfaces totales par habitat
area_hab_HDF <- aggregate(area_poly~habitat$code_18, habitat, sum)
names(area_hab_HDF)[1] <- "code_18"
names(area_hab_HDF)[2] <- "area_tot_hab"

#calcul de l'aire totale (meme calcul pour les deux lignes)
area_HDF <- sum(area_hab_HDF$area_tot_hab)
area_HDF_poly <- sum(habitat$area_poly)


#proportion par habitat en HDF
area_hab_HDF$proportion <- (area_hab_HDF$area_tot_hab/area_HDF)
sum(area_hab_HDF$proportion)
ggprop <- plot(area_hab_HDF$proportion~area_hab_HDF$code_18)

#proportion des habitats par polygone en HDF
#on aimerait savoir si le HGD utilise des petits polygone d'habitat ou plutot des grands polygones d'habitat en region de France
habitat$proportion <- (habitat$area_poly/area_HDF)
sum(habitat$proportion)
habitat <- habitat %>% relocate(code_18, .after = id_poly)





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
names(data_HGD)[24] <- "bird_id" #(les oiseaux avec une balise portent le nom du lieu_dit ou ils ont ete bague + balise)
data_HGD <- data_HGD %>% relocate(bird_id, .after = UTC_date)
data_HGD <- data_HGD %>% relocate(UTC_date, .after = bird_id)
names(data_HGD)[3] <- "time"
names(data_HGD)[21] <- "day_night"
summary(data_HGD)


#creation d'une colonne date au format aaaa-mm-jj(cuisine a la maniere de savine :) )
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

#Assemblage des couches habitats + points GPS HGD
sum_loc <- st_intersection(habitat, HGD_sf)



#nombre de donnes par heure par oiseau pour expliquer le choix des heures
setDT(data_HGD)
nb_data_HH <- data_HGD [,.(nb_data_HH = .N), by = .(bird_id, date_HH)]
nb_data_HH[,nb_data_HH := as.numeric(nb_data_HH)]
#bp <- boxplot (nb_data_HH$nb_data_HH ~ nb_data_HH$bird_id)


# figure nombre de donnees par heure par oiseaux
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
#fwrite(sum_HGD, "table/sum_HGD_OrniTrack.csv") #deja enregistre

#nombre de donnees par oiseau par J/N
sum_HGD_daynight <- data_HGD[,.(nb_data = .N), by =.(bird_id, day_night)]
#fwrite(sum_HGD_daynight, "table/sum_HGD_daynight.csv") #deja enregistre


#stat sur les donnees par heure par oiseau
#nombre de donnees par oiseau et par jour et par heure
#nb_data_j <- data_HGD [,.(nb_data_jour = .N), by = .(bird_id, date, date_HH)]

#mean_data_j <- mean(nb_data_j$nb_data_jour)# moyenne du nombre de donnees par heure = 1,38
#min_data_j <- min(nb_data_j$nb_data_jour)# le plus petit nombre de donnees par heure = 1
#max_data_j <- max(nb_data_j$nb_data_jour)# le plus grand nombre de donnees par heure = 13



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


boxplot(Altitude_m~bird_id)# a voir comment retravailler ce graph
#une etude sur l'altitude des HGD et le positionnement des champs eoliens serait interessantes



#stat sur les donnees sum_HGD
nb_data_tot <- sum(sum_HGD$nb_data)# 36288 donnees

median_nb_data <- median(sum_HGD$nb_data)# mediane du nombre de data par oiseau = 191
mean_nb_data <- mean(sum_HGD$nb_data)# nombre moyen de data par oiseau = 2969,5
min_nb_data <- min(sum_HGD$nb_data) # le plus petit nombre de data = 1259
max_nb_data <- max(sum_HGD$nb_data) # le plus grand nombre de data = 5246

min_data_date <- min(sum_HGD$last)# la plus petite periode d'emission pour une balise = 2022_08_28 = 3 mois max par Glageon Bocahut
max_data_date <- max(sum_HGD$last)# la plus grande periode d'emission pour une balise = 2023_02_12 = 6 mois max par loos A,loos B,quelmes,Germignie b








## Utilisation du paysage par les oiseaux ##########################################################################################################

### Comparaison des habitats composant les motus occupes et non occupes ###################################################################################################

# regroupement du nombre d'occurence par polygone, par oiseau, par jour et par heure
setDT(sum_loc)
sum_loc_poly <- sum_loc[,.(occurence = .N),by=.(id_poly,bird_id,date_HH)][,.(occurence = .N),by=.(id_poly)]

habitat <- merge(habitat, sum_loc_poly, bx = "id_poly", all.x = T)
habitat$occurence[is.na(habitat$occurence)] <- 0
habitat <- habitat %>% relocate(occurence, .after = code_18)


#création data.table pour d_gg
habitat_DT <- habitat
setDT(habitat_DT)
habitat_DT[,occupation := occurence>0]

#creation d'un tableau a partir de habitat_DT
habitat_DT[,proportion := as.numeric(proportion)]
d_gg <- habitat_DT[,.(prop_mean = mean(proportion),prop_med = median(proportion),inf95 = quantile(proportion, 0.025),sup95 = quantile(proportion, 0.975)), by=.(code_18,occupation)]



# Figure comparaison des habitats composant les polygones occupes et non occupes
library(ggplot2); library(units)
gg <- ggplot(data = d_gg, aes(x = code_18, y = prop_mean,fill = occupation,colour=occupation,group=occupation))
gg <- gg + geom_errorbar(aes(ymin = inf95, ymax = sup95),width = 0.5,alpha=.5,linewidth =1)
gg <- gg +  geom_point(alpha=.8,size=2)
gg <- gg + labs(y = "Proportion mean", x = "Habitats")
gg
#ggsave("Rplot/prop_mean.png",gg)


