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

### Data HGD #########################################################################################################################################

#Importation des donnees
data_HGD_original <- read.csv2("data_HGD_W2.csv", head = T, sep = ";", stringsAsFactors = T)
str(data_HGD_original)
summary(data_HGD_original)
plot(data_HGD_original$Affichage)
plot(data_HGD_original$Commentair)

#Nettoyage des données
#Suppression des data ayant des problemes d'affichage
data_HGD <- subset(data_HGD_original, Affichage != "NON") # j'enlève tous les NON
data_HGD <- subset(data_HGD, Commentair != "Pas de coordonnées GPS")#j'enleve tous les "Pas de coordonnées GPS"
data_HGD <- subset(data_HGD, Commentair != "Donnée avant balisage")#j'enleve tous les "Donnée avant balisage"
data_HGD <- subset(data_HGD, Commentair != "Données avant balisage")#j'enleve tous les "Données avant balisage"
summary(data_HGD)


#Suppression des colonnes inutiles + Renommer certaines colonnes
data_HGD <- data_HGD[,-c(1,2,5,27,28)] # supression colonnes device_id,UTC_dateti, datatype, Affichage, Commentair
names(data_HGD)[24] <- "bird_id" #(les oiseaux avec une balise portent le nom du lieu_dit ou ils ont ete bague + balise)
data_HGD <- data_HGD %>% relocate(bird_id, .after = UTC_date)
data_HGD <- data_HGD %>% relocate(UTC_date, .after = bird_id)
data_HGD <- data_HGD %>% relocate(Latitude, .after = Longitude)
names(data_HGD)[3] <- "time"
names(data_HGD)[21] <- "day_night"
summary(data_HGD)

plot(data_HGD$Quinzaine)
boxplot(data_HGD$temperatur)

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

#suppression des dates après le 15 janvier = transition entre la période post-natale et prénuptiale
data_HGD <- filter(data_HGD,date < "2023-01-16")

#selection des satcount = nombre de satellite = fiabilite du point (minimum 8)
data_HGD <- filter(data_HGD,satcount > 7)
plot(data_HGD$satcount)
boxplot(data_HGD$satcount)
summary(data_HGD)


#selection des hdop = précision vur l'horizontalite du point (0,8 < hdop < 1.3)
#plus le hdop est proche de 1 mieux c'est
data_HGD <- filter(data_HGD,hdop > 0.79)
data_HGD <- filter(data_HGD,hdop < 1.31)
plot(data_HGD$hdop)
boxplot(data_HGD$hdop)
summary(data_HGD$hdop)

## ajout de la colonne date et la colonne heure manuellement
data_HGD$heure_HH <- substr(data_HGD$time,1,2)
data_HGD$date_HH <- paste0(data_HGD$date, "_", data_HGD$heure_HH)

#importation des donnees sur le jour de depart de chaque oiseau
data_depart <- read.csv("Date_de_depart.csv", head = T, sep = ";", stringsAsFactors = T)
data_HGD <- merge(data_HGD, data_depart, by = "bird_id")
data_HGD <- data_HGD %>% relocate(date_depart, .after = time)


#creation colonne dispersion/dependance alimentaire par bird_id
#creation boucle pour separer les donnees dependance alimentaire/dispersion en fonction du jour de depart de chaque oiseau
HGD_DT <- data_HGD[,-c(1,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)]
setDT(HGD_DT)
periode <- ifelse(as.Date(HGD_DT$date) < as.Date(HGD_DT$date_depart), "Dependance alimentaire", "Dispersion")
data_HGD <- cbind(data_HGD,periode)
data_HGD <- data_HGD %>% relocate(periode, .after = date_depart)


#Nombre de donnees par oiseau en fonction de la periode (dependance alimentaire/dispersion)
nb_data_periode_bird <- data_HGD[,.(nb = .N),by=.(bird_id, periode)]
#fwrite(nb_data_periode_bird, "table/nb_data_periode_bird.csv")

#Nombre de donnees par heure par oiseau sur l'ensemble de la periode jusqu'au 15 janvier
setDT(data_HGD)
nb_data_HH <- data_HGD [,.(nb_data_HH = .N), by = .(bird_id, date_HH)]
nb_data_HH[,nb_data_HH := as.numeric(nb_data_HH)]

# figure nombre de donnees par heure par oiseaux sur l'ensemble de la periode jusqu'au 15 janvier
library(ggplot2)
gg <- ggplot(nb_data_HH,aes(x=nb_data_HH,y=bird_id)) + geom_violin()
gg <- gg + labs(y = "Bird_id", x = "Nombre de données par heure")
gg
#ggsave("Rplot/nb_data_HH.png",gg)



# Figure nombre de donnees par oiseau par jour par J/N sur l'ensemble de la periode jusqu'au 15 janvier
nb_data_j_daynight <- data_HGD [,.(nb_data_jour = .N), by = .(bird_id, date, day_night)]
all_date <- expand.grid(bird_id = unique(data_HGD[, bird_id]), date = seq(as.Date(min(data_HGD$date)), as.Date(max(data_HGD$date)),1), day_night = c("Jour", "Nuit"))
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
#ggsave("Rplot/nb_data_daynight.png",gg, width = 25, height = 13)



# Data HGD en dispersion #############################################################################################################################
HGD_Disp <- filter(data_HGD,periode == "Dispersion")


#Nombre de donnees par heure par oiseau sur la periode dispersion post-natale
setDT(HGD_Disp)
nb_data_HH <- HGD_Disp [,.(nb_data_HH = .N), by = .(bird_id, date_HH)]
nb_data_HH[,nb_data_HH := as.numeric(nb_data_HH)]

# figure nombre de donnees par heure par oiseaux sur la periode dispersion post-natale
library(ggplot2)
gg <- ggplot(nb_data_HH,aes(x=nb_data_HH,y=bird_id)) + geom_violin()
gg <- gg + labs(y = "Bird_id", x = "Nombre de données par heure")
gg
#ggsave("Rplot/nb_data_HH_dispersion.png",gg)



# Figure nombre de donnees par oiseau par jour par J/N sur la periode de dispersion post-natale
nb_data_j_daynight <- HGD_Disp [,.(nb_data_jour = .N), by = .(bird_id, date, day_night)]
all_date <- expand.grid(bird_id = unique(HGD_Disp[, bird_id]), date = seq(as.Date(min(HGD_Disp$date)), as.Date(max(HGD_Disp$date)),1), day_night = c("Jour", "Nuit"))
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
#ggsave("Rplot/nb_data_daynight_dispersion.png",gg, width = 25, height = 13)



##### Tableau description donnees HGD en Dispersion post-natale #####################################################################################################
library(data.table)
sum_HGD <- HGD_Disp[,.(nb_data = .N,
                       first = min(date),
                       last = max(date)), by =.(bird_id)] # par oiseau: combien de donnees, premiere date et derniere date

sum_HGD[,duration_days := difftime(last, first, unit = "days")]#difference de temps entre la premiere et la derniere donnee. le ":=" veut dire pas de regroupement

nb_day <- HGD_Disp [,.(j = 1), by = .(bird_id, date)] # regroupement par oiseau et par date
nb_day <- nb_day [,.(nb_day= .N), by = .(bird_id)] # nombre de jour de données 
sum_HGD <- merge(sum_HGD, nb_day, bx = "bird_id")
#fwrite(sum_HGD, "table/sum_HGD_OrniTrack.csv")

#nombre de donnees par oiseau par J/N en dispersion
sum_HGD_daynight <- HGD_Disp[,.(nb_data = .N), by =.(bird_id, day_night)]
#fwrite(sum_HGD_daynight, "table/sum_HGD_daynight.csv")


#stat sur les donnees sum_HGD
nb_data_tot <- sum(sum_HGD$nb_data)# 4666 donnees

median_nb_data <- median(sum_HGD$nb_data)# mediane du nombre de data par oiseau = 402
mean_nb_data <- mean(sum_HGD$nb_data)# nombre moyen de data par oiseau = 389
min_nb_data <- min(sum_HGD$nb_data) # le plus petit nombre de data = 5
max_nb_data <- max(sum_HGD$nb_data) # le plus grand nombre de data = 808

min_data_date <- min(sum_HGD$last)# la plus petite periode d'emission pour une balise = 2022_08_28 = 3 mois max par Glageon Bocahut
max_data_date <- max(sum_HGD$last)# la plus grande periode d'emission pour une balise = 2023_02_12 = 6 mois max par loos A,loos B,quelmes,Germignie b


#Suppression de l'individu Glageon Bocahut (pas assez de donnée)
HGD_Disp <- subset(HGD_Disp, HGD_Disp$bird_id != "Glageon Bocahut")
HGD_Disp$bird_id <- as.character(HGD_Disp$bird_id)
table(HGD_Disp$bird_id)
summary(HGD_Disp)


# Nombre de donnees par oiseaux en dispersion
nb_data_disp <- HGD_Disp[,.(nb = .N),by=bird_id]
#fwrite(nb_data_disp, "table/nb_data_disp.csv")
#fwrite(HGD_Disp, "table/HGD_Disp.csv")


### Estimation des DV en dispersion (Kernel : LSCV) ############################################################################################################
library(adehabitatHR)
library(sf)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(tidyverse)
library(data.table)
library(ggrepel)
library(scales)

# Fond de carte departement Nord Pas-de-Calais
NPDC <- st_read("SIG/Departement NPDC.shp")
NPDC <- st_make_valid(NPDC)
NPDC <- st_transform(NPDC,crs=2154)
NPDC <- st_make_valid(NPDC)

# Fond de carte departement region Hauts-de-France
HDF <- st_read("SIG/Departement HDF.shp")
HDF <- st_make_valid(HDF)
HDF <- st_transform(HDF,crs=2154)
HDF <- st_make_valid(HDF)

# Delimitation bassin minier
Bassin_minier <- st_read("SIG/Limites bassin miniers fusionnées.shp")
Bassin_minier <- Bassin_minier[,-c(1:11)]
Bassin_minier$habitat = "bassin_minier"

# Trame verte region nord et pas de calais
Trame_verte <- st_read("SIG/RB_SRCE_TVB.shp")
table(Trame_verte$SOUSTRAME)
Trame_verte <- subset(Trame_verte, SOUSTRAME != "coteaux calcaires")
Trame_verte <- subset(Trame_verte, SOUSTRAME != "dunes et estrans sableux")
Trame_verte <- subset(Trame_verte, SOUSTRAME != "estuaires")
Trame_verte <- subset(Trame_verte, SOUSTRAME != "falaises et estrans rocheux")
Trame_verte <- subset(Trame_verte, SOUSTRAME != "terrils et autres milieux anthropiques")
Trame_verte <- subset(Trame_verte, SOUSTRAME != "zones humides")
Trame_verte <- subset(Trame_verte, SOUSTRAME != "landes et pelouses acidiphiles")
Trame_verte <- subset(Trame_verte, SOUSTRAME != "autres milieux")
table(Trame_verte$SOUSTRAME)

Trame_verte_sf <- st_transform(Trame_verte,crs=2154)
Trame_verte_sf <- st_union (Trame_verte_sf)
Trame_verte_sf <- st_make_valid(Trame_verte_sf)
#st_write(Trame_verte_sf, dsn = "Trame_verte_sf", layer = "Trame_verte_sf.shp", driver = "ESRI Shapefile", overwrite_layer = T)
Trame_verte_sf <- st_as_sf(Trame_verte_sf)


#Creation DV kernel Disp all
Disp_HGD_sf <- st_as_sf(HGD_Disp, coords = c("Longitude","Latitude"))
st_crs(Disp_HGD_sf) <- 4326
Disp_HGD_sf <- st_transform(Disp_HGD_sf,crs= 2154)
Disp_sf_NPDC <- Disp_HGD_sf[,c("bird_id")]
Disp_sf_NPDC <- st_crop(Disp_sf_NPDC,st_bbox(NPDC))
Disp_sf_NPDC_k <- as(Disp_sf_NPDC,'Spatial')

kdh_Disp_all <- kernelUD(Disp_sf_NPDC_k, h="LSCV", grid = 1000)
#image(kdh_Disp_l)


# creating SpatialPolygonsDataFrame
# Polygone spatial 95%
kd_names_Disp_all <- names(kdh_Disp_all)
ud_95_Disp_all <- lapply(kdh_Disp_all, function(x) try(getverticeshr(x, 95)))


sapply(1:length(ud_95_Disp_all), function(i) {
  row.names(ud_95_Disp_all[[i]]) <<- kd_names_Disp_all[i]
})
sdf_poly_95_Disp_all <- Reduce(rbind, ud_95_Disp_all)
df_95_Disp_all <- fortify(sdf_poly_95_Disp_all)
df_95_Disp_all$bird_id <- df_95_Disp_all$id


# Polygone spatial 50%
ud_50_Disp_all <- lapply(kdh_Disp_all, function(x) try(getverticeshr(x, 50)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_50_Disp_all), function(i) {
  row.names(ud_50_Disp_all[[i]]) <<- kd_names_Disp_all[i]
})
sdf_poly_50_Disp_all <- Reduce(rbind, ud_50_Disp_all)
df_50_Disp_all <- fortify(sdf_poly_50_Disp_all)
df_50_Disp_all$bird_id <- df_50_Disp_all$id


# Polygone spatial 30%
ud_30_Disp_all <- lapply(kdh_Disp_all, function(x) try(getverticeshr(x, 30)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_30_Disp_all), function(i) {
  row.names(ud_30_Disp_all[[i]]) <<- kd_names_Disp_all[i]
})
sdf_poly_30_Disp_all <- Reduce(rbind, ud_30_Disp_all)
df_30_Disp_all <- fortify(sdf_poly_30_Disp_all)
df_30_Disp_all$bird_id <- df_30_Disp_all$id




#### Cartographie ###########################################################################################################################

#Vu generale des kernel Disp departement NPDC 95%, 50%, 30%
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) 
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Domaines vitaux des HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95%, 50% et 30%)")
gg
ggsave("Rplot/Kernel/kernel_NPDC_LSCV.png",gg, width = 25, height = 13)

#Vu generale des kernel Disp departement NPDC 95%
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) 
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Domaines vitaux des HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95%)")
gg
ggsave("Rplot/Kernel/kernel_NPDC_LSCV_95.png",gg, width = 20, height = 11)

#Vu generale des kernel Disp departement NPDC 95% sans les pôints GPS
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
#gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) 
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Domaines vitaux des HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95%)")
gg
ggsave("Rplot/Kernel/kernel_NPDC_LSCV_95_simple.png",gg, width = 20, height = 11)

#Vu generale des kernel Disp region HDF
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = HDF, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) 
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Domaines vitaux des HGD en dispersion en région Hauts-de-France (Kernel 95%, 50% et 30%)")
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/kernel_HDF_LSCV.png",gg, width = 25, height = 13)


#vu par HGD des kernel departement NPDC
gg <- ggplot()  + theme_bw() + facet_wrap(.~bird_id)
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Domaines vitaux par HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95%, 50% et 30%)")
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/kernel_NPDC_bird_LSCV.png",gg, width = 25, height = 13)







#### ZST (DV 95%) ######################################################################################################################
#Creation des DV kernel ZST
Disp_HGD_sf <- st_as_sf(HGD_Disp, coords = c("Longitude","Latitude"))
st_crs(Disp_HGD_sf) <- 4326
Disp_HGD_sf <- st_transform(Disp_HGD_sf,crs=2154)
Disp_sf_NPDC <- Disp_HGD_sf[,c("bird_id")]
Disp_sf_NPDC <- st_crop(Disp_sf_NPDC,st_bbox(NPDC))
Disp_sf_NPDC$bird_id <- as.character(Disp_sf_NPDC$bird_id)
Disp_sf_NPDC_k <- as(Disp_sf_NPDC,'Spatial')

kdh_Disp_l <- kernelUD(Disp_sf_NPDC_k, h="LSCV", grid = 1000)
#image(kdh_Disp_l)

# creating SpatialPolygonsDataFrame
# Polygone spatial 95%
kd_names_Disp_l <- names(kdh_Disp_l)
ud_95_Disp_l <- lapply(kdh_Disp_l, function(x) try(getverticeshr(x, 95)))


sapply(1:length(ud_95_Disp_l), function(i) {
  row.names(ud_95_Disp_l[[i]]) <<- kd_names_Disp_l[i]
})
sdf_poly_95_Disp_l <- Reduce(rbind, ud_95_Disp_l)
df_95_Disp_l <- fortify(sdf_poly_95_Disp_l)
df_95_Disp_l$bird_id <- df_95_Disp_l$id


sdf_poly_95_Disp_l <- Reduce(rbind, ud_95_Disp_l)
sf_poly_95_Disp_l <- st_as_sf(sdf_poly_95_Disp_l)
sf_poly_95_Disp_l_DV <- st_cast(sf_poly_95_Disp_l, "MULTIPOLYGON") %>% st_cast("POLYGON")#transforme les listes emboitees



sf_poly_95_Disp_l_DV <- sf_poly_95_Disp_l_DV %>% 
  rownames_to_column(var="group")
sf_poly_95_Disp_l_DV$area_DV <- st_area(sf_poly_95_Disp_l_DV)

#st_write(sf_poly_95_Disp_l_DV, dsn = "sf_poly_95_Disp_l_DV", layer = "sf_poly_95_Disp_l_DV", driver = "CSV", overwrite_layer = T)
#complété le fichier csv avec une colonne oiseaux (remplir à la main)

DV_Disp <- read.csv("C:/Git/HGD/sf_poly_95_Disp_l_DV/sf_poly_95_Disp_l_DV.csv", head = T, sep = ";", stringsAsFactors = T)
DV_Disp <- DV_Disp[,-c(2,3,4)]
DV_Disp <- merge(DV_Disp, sf_poly_95_Disp_l_DV, by = "group")
DV_Disp <- DV_Disp[,-c(3,4)]
DV_Disp <- DV_Disp %>% relocate(group, .after = bird_id)

#Sauvegarde des DV globaux
#st_write(DV_Disp, dsn = "DV_Disp2", layer = "DV_Disp2.shp", driver = "ESRI Shapefile", overwrite_layer = T)
DV_Disp<- st_as_sf(DV_Disp)
DV_Disp <- st_transform(DV_Disp,crs=2154)
#st_crs(DV_Disp)


#Croisement de mes DV avec les points pour récupérer les dates
# liste unique des oiseaux
list_oiseaux = unique(Disp_HGD_sf$bird_id)

res = lapply(list_oiseaux, function(x) {
  # filtrer les deux jeux de données pour l'oiseau x
  gps_oiseau = Disp_HGD_sf[Disp_HGD_sf$bird_id == x,]
  dv_oiseau = DV_Disp[DV_Disp$bird_id == x,]
  
  inter_oiseau= st_intersection(dv_oiseau , gps_oiseau )
  return(inter_oiseau)})

res = do.call("rbind", res)

# Grouper par domaine vital, et compter le nombre de jours de presence dans les DV
res = group_by(res, group) %>%
  mutate(nb_jour = length(unique(date)))

# Ajouter la colone ZST avec la selection des DV dont le nombre de jours de presence est > 5
res$ZST = "No"
res$ZST[res$nb_jour > 5] = "Yes"



#tableau recapitulatif des DV > 5
#Methode A: ne prend pas en compte les excursions (dates de début et fin + durée de séjour souvent fausse )
res1 <- data.frame(res)
res1 <- unique(res1[,c("bird_id", "group", "nb_jour", "area_DV", "ZST")])
res2 <- merge(res1, DV_Disp, by = "group")
resSup5 = res2[res2$nb_jour > 5,]
resSup5 <- resSup5[,-c(6,7)]
names(resSup5)[2] <- "bird_id"
names(resSup5)[4] <- "area_DV"
resSup5 <- resSup5 %>% relocate(group, .after = bird_id)


#recapitulatif des ZST officiel mais les dates sont mauvaises (a modifier grace a la methode B)
resSup5_summary<- setDT(res)
resSup5_summary <- resSup5_summary[,.(first = min(date),last = max(date)), by =.(group)]
resSup5_summary <- merge(resSup5_summary, resSup5, by = "group")
resSup5_summary <- resSup5_summary %>% relocate(bird_id, .after = group)
resSup5_summary <- resSup5_summary %>% relocate(group, .after = bird_id)

#Renommer les ZST
# Doc cree avec les abreviations des ZST
rename_ZST <- read.csv("Rename_ZST.csv", head = T, sep = ";", stringsAsFactors = T)

#Renommer les ZST du recapitulatif 2
resSup5_summary <- merge(resSup5_summary, rename_ZST, by = "group")
resSup5_summary <- resSup5_summary %>% relocate(rename, .after = group)
resSup5_summary <- resSup5_summary[,-c(1)]
names(resSup5_summary)[1] <- "group"
resSup5_summary <- resSup5_summary %>% relocate(group, .after = bird_id)
resSup5_summary <- resSup5_summary[,-c(7)]
#st_write(resSup5_summary, dsn = "resSup5_summary", layer = "resSup5_summary.csv", driver = "CSV", overwrite_layer = T)

#Renommer les ZST du recapitulatif 1
resSup5 <- merge(resSup5, rename_ZST, by = "group")
resSup5 <- resSup5 %>% relocate(rename, .after = group)
resSup5 <- resSup5[,-c(1)]
names(resSup5)[1] <- "group"
resSup5 <- resSup5 %>% relocate(group, .after = bird_id)
resSup5 <- resSup5[,-c(5)]
#Sauveagarde 
#st_write(resSup5, dsn = "resSup5", layer = "resSup5.shp", driver = "ESRI Shapefile", overwrite_layer = T)
#resSup5_sf <- read_sf("resSup5/resSup5.shp")



#Methode B: difference entre deux dates afin de mieux estimer la duree de sejour et donc date de début et date de fin
# selection des DV > a 5 jours (on ne garde que les DV > 5)
restest <- data.frame(res)
resSup5test = restest[restest$nb_jour > 5,]
setDT(resSup5test)


#### Difference de jours entre les dates #########################################################
# Mise en evidence des jours de presence pas forcement consecutif et des excursions hors des DV 
# ajout de condition pour qu'un DV soit une ZST
# Un ZST est strictement superieur a 5 jours consecutif avec possibilite d'excursion hors de la ZST < 3 jours consecutifs


# Trie des dates avec un ordre par group et par date la plus ancienne a la plus recente 
#on voit l'aspect consecutif des dates dans les ZST
# On compte le nombre de donnees par oiseau, par group et par date
nb_data_datetest3 <- resSup5test [,.(nb_data = .N), by = .(bird_id, group, date)]
# Classification des donnees par group dans un premier temps puis par date dans l'ordre chronologique
nb_data_datetest4 <- nb_data_datetest3[order(nb_data_datetest3[,c(2,3)], decreasing=FALSE)]
nb_data_datetest5 <- setDT(nb_data_datetest4)
#Renommer les ZSt
nb_data_datetest5 <- merge(nb_data_datetest5, rename_ZST, by = "group")
nb_data_datetest5 <- nb_data_datetest5 %>% relocate(rename, .after = group)
nb_data_datetest5 <- nb_data_datetest5[,-c(1)]
names(nb_data_datetest5)[1] <- "group"
nb_data_datetest5 <- nb_data_datetest5 %>% relocate(group, .after = bird_id)

# la variable "date" est passée au format "date" de R pour calculer la différence avec la fonction "diff"
nb_data_datetest5$date <- as.Date(nb_data_datetest5$date)
# la variable "bird_id" est passée au format "factor" pour servir d'indice dans la fonction "by"
nb_data_datetest5$bird_id <- factor(nb_data_datetest5$bird_id )


# création d'une fonction "maison" qui permet de faire le "diff" des dates et ajouter en même temps les NA nécessaires
myfun <- function(x)
{
  z <- as.numeric(diff(x))
  if(length(z)==0)
  {
    s <- NA
  }
  else
  {
    s <- c(NA, z)
  }
  return(s)
}

# calcul d'un vecteur de différences
df <- by(nb_data_datetest5$date, nb_data_datetest5$bird_id, myfun)
nb_data_datetest5 <- data.frame(nb_data_datetest5, diff = unlist(df))
setDT(nb_data_datetest5)
#grace a ce dernier tableau, on determine les dates de debut et de fin en fonction de la duree des excursions (nb_jour<3 avec exception pour les haltes prolongees)

#date debut - date fin ZST (condition: strictement > 5 jours consecutifs avec des excursions < 3 jours)
#importation des dates pose a la main
date_ZST <- read.csv("date_ZST.csv", head = T, sep = ";", stringsAsFactors = T)

#mise au format date
date_ZST$date_debut <- as.Date(date_ZST$date_debut)
date_ZST$date_fin <- as.Date(date_ZST$date_fin)
setDT(date_ZST)

#nombre de jour entre la date de debut et de fin de la ZST
date_ZST[,nb_jour := difftime(date_fin, date_debut)]

# On compte le nombre de jour de presence dans chaque ZST par HGD
nb <- nb_data_datetest5 [,.(nb_data = .N), by = .(bird_id, group)]

#on regarde la difference entre intervalle de temps entre les deux dates et le nomnre de jours de presence nb
#plus la difference est grande = halte prolonge... cad que la présence du piaf est reguliere mais pas assez pour que l'on considere la zone comme une vrai ZST
# excursion frequente et longue en dehors de la zone = souvent lie a un phenomene de balancier quand les ZST sont tres rapprochees.


#Graphique: duree de sejour moyen dans les ZST
boxplot(resSup5$nb_jour)
boxplot(resSup5$nb_jour~resSup5$bird_id, ylab="Nombre de jour" , xlab="Bird")
summary(resSup5)
ggsave("Rplot/nb_jour_ZST.png",resSup5, width = 10, height = 5)




##### Cartographie ###########################################################################################################################

#Fond de carte global
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = Bassin_minier, fill = "#bd9b95", color= NA,alpha=.5)
gg <- gg + geom_sf(data = Trame_verte, fill = "#41cc76", color= NA,alpha= 0.5)
gg

#Cartographie des kernel Disp departement NPDC ZST > 5 sans les point gps mais avec étiquette
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = Bassin_minier, fill = "#bd9b95", color= NA,alpha=.5)
gg <- gg + geom_sf(data = Trame_verte, fill = "#41cc76", color= NA,alpha= 0.5)
gg <- gg +   geom_sf(data = resSup5_sf, aes(color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="ZST des HGD en dispersion dans les départements du Nord et du Pas-de-Calais issus des domaines vitaux (Kernel 95%)")
gg <- gg + geom_label_repel(data = resSup5_sf, aes(label = group, geometry = geometry), stat = "sf_coordinates", min.segment.length = 0,colour = "black",segment.colour = "black") 
gg
ggsave("Rplot/Kernel/kernel_NPDC_LSCV_sup5_paysage_loc.png",gg, width = 20, height = 11)


#Cartographie des kernel Disp departement NPDC ZST > 5 avec point gps
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_sf(data = resSup5_sf, aes(color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="ZST des HGD en dispersion dans les départements du Nord et du Pas-de-Calais issus des domaines vitaux (Kernel 95%)")
gg
ggsave("Rplot/Kernel/kernel_NPDC_LSCV_sup5_complete.png",gg, width = 25, height = 13)



#vu par HGD des ZST departement NPDC
gg <- ggplot()  + theme_bw() + facet_wrap(.~bird_id)
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_sf(data = resSup5_sf, aes(color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
#gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="ZST des HGD en dispersion dans les départements du Nord et du Pas-de-Calais issus des domaines vitaux (Kernel 95%)")
gg <- gg + geom_label_repel(data = resSup5_sf, aes(label = group, geometry = geometry), stat = "sf_coordinates", min.segment.length = 0,colour = "black",segment.colour = "black")
gg
ggsave("Rplot/Kernel/kernel_NPDC_bird_LSCV_sup5.png",gg, width = 25, height = 13)


#Vu generale des kernel Disp region HDF
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = HDF, size=0.2, alpha=.5)
gg <- gg +   geom_sf(data = resSup5_sf, aes(color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
#gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) 
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
#gg <- gg + labs(x="",y="",colour="Birds",title="Domaines vitaux des HGD en dispersion en région Hauts-de-France (Kernel 95%, 50% et 30%)")
gg
ggsave("Rplot/Kernel/kernel_HDF_LSCV_sup5.png",gg, width = 25, height = 13)


### Test Kernel h = href ####################################################################################
kdh_Disp_href <- kernelUD(Disp_sf_NPDC_k, h="href", grid = 1000)
kd_names_Disp_href <- names(kdh_Disp_href)
ud_95_Disp_href <- lapply(kdh_Disp_href, function(x) try(getverticeshr(x, 95)))


sapply(1:length(ud_95_Disp_href), function(i) {
  row.names(ud_95_Disp_href[[i]]) <<- kd_names_Disp_href[i]
})
sdf_poly_95_Disp_href <- Reduce(rbind, ud_95_Disp_href)
df_95_Disp_href <- fortify(sdf_poly_95_Disp_href)
df_95_Disp_href$bird_id <- df_95_Disp_href$id

#Vu generale des kernel Disp departement NPDC 95% h = href sans loc GPS
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_href, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
#gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) 
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Domaines vitaux des HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95% href)")
gg
ggsave("Rplot/Kernel/kernel_NPDC_href95.png",gg, width = 20, height = 11)

#Vu generale des kernel Disp departement NPDC 95% h = href avec loc GPS
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_href, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) 
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Domaines vitaux des HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95% href)")
gg
ggsave("Rplot/Kernel/kernel_NPDC_href95_complet.png",gg, width = 20, height = 11)





#### Selection des habitats J/N ################################################################################################
### Data Habitat ##################################################################################################################################

#calculs des surfaces par polygones
CLC <- st_read("SIG/CLC_HDF_2018.shx", stringsAsFactors = T)
CLC_code <- read.csv2("code_habitat.csv", head = T, sep = ";", stringsAsFactors = T)
CLC <- merge(CLC, CLC_code, by = "code_18")
CLC <- st_transform(CLC,crs=2154)#transformation des donnees
summary(CLC)


#Assemblage des couches CLC et DV
#st_crs(CLC)
#class(resSup5)
resSup5_sf <- st_as_sf(resSup5)
resSup5_sf <- st_transform(resSup5_sf,crs=2154)
#Assemblage des couches habitats + et ZST
hab <- st_intersection(CLC,resSup5_sf)
hab$area <- st_area(hab)
hab <- hab[,-c(4,5,10,12)]
hab$proportion <- (hab$area/hab$area_DV)
summary(hab)
#st_write(hab, dsn = "hab", layer = "hab.shp", driver = "ESRI Shapefile", overwrite_layer = T)

#Assemblage des couches habitats + points GPS HGD
# liste unique de tes oiseaux
list_oiseaux2 = unique(Disp_HGD_sf$bird_id)

sum_loc = lapply(list_oiseaux2, function(x) {
  # filtrer les deux jeux de données pour l'oiseau x
  gps_oiseau = Disp_HGD_sf[Disp_HGD_sf$bird_id == x,]
  dv_hab = hab[hab$bird_id == x,]
  
  inter_oiseau2= st_intersection(dv_hab , gps_oiseau )
  return(inter_oiseau2)})

sum_loc = do.call("rbind", sum_loc)


# regroupement du nombre d'occurence par polygone, par oiseau, par jour
setDT(sum_loc)
sum_loc_poly <- sum_loc[,.(occurence = .N),by=.(objectid,bird_id)][,.(occurence = .N),by=.(objectid)]

hab <- merge(hab, sum_loc_poly, bx = "objectid", all.x = T)
hab$occurence[is.na(hab$occurence)] <- 0
#hab <- hab %>% relocate(occurence, .after = habitat)

hab_DT <- hab
setDT(hab_DT)
hab_DT[,occupation := occurence>0]

## la methode en une ligne de RL
hab_DT[,proportion := as.numeric(proportion)]
d_gg <- hab_DT[,.(prop_mean = mean(proportion),prop_med = median(proportion),inf95 = quantile(proportion, 0.025),sup95 = quantile(proportion, 0.975)), by=.(habitat,occupation)]




# Figure comparaison des habitats composant les motus occupes et non occupes
library(ggplot2); library(units)
gg <- ggplot(data = d_gg, aes(x = habitat, y = prop_mean,fill = occupation,colour=occupation,group=occupation))
gg <- gg + geom_errorbar(aes(ymin = inf95, ymax = sup95),width = 0.5,alpha=.5,linewidth=1)
gg <- gg +  geom_point(alpha=.8,size=2)
gg <- gg + labs(y = "Proportion mean", x = "Habitats")
gg



#Analyse sans daynight
#Distribution des localisations par habitat
distri_loc_hab <- sum_loc[,-c(1,2,3,4,7,8,9,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37)]# j'enleve toutes les colonne sauf habitat, proportion, bird_id et geometry
distri_loc_hab <- distri_loc_hab %>% relocate(habitat, .after = bird_id)
distri_loc_hab <- distri_loc_hab %>% relocate(proportion, .after = habitat)


#  tableau occurence par habitat par oiseau
setDT(distri_loc_hab)
tab_hab <- distri_loc_hab[,.(nb = .N),by=.(bird_id,habitat)]#.N = on compte le nombre de fois que chaque habitat revient pour chaque oiseau


# barre de reference : habitat de l'ensemble des ZST
area_habitat <- aggregate(area~habitat, hab, sum)
area_habitat$proportion <- area_habitat$area/sum(area_habitat$area)
area_habitat <- area_habitat[,-c(2)]
setDT(area_habitat)
area_habitat[,bird_id := "ZST habitat"]
area_habitat[,nb := as.numeric(proportion)]

setcolorder(area_habitat,c("bird_id","habitat","nb"))
tab_hab <- bind_rows(tab_hab, area_habitat)


#couleur par habitat
tab_fill  <- read.csv("library/colour_habitat.csv", sep = ";")
vec_fill <- tab_fill$colour
names(vec_fill) <- tab_fill$habitat

#nombre de donnees par oiseau
tab_bird <- distri_loc_hab[,.(nb = .N),by=bird_id]
tab_bird[,label := paste0(bird_id," (",nb,")")]

setDF(distri_loc_hab)


# representation graphique de la distribution des localisations par habitat et par habitat des motus occupes
ggdistrib <- ggplot(data = tab_hab,aes(x = nb, y = bird_id, fill = habitat))
ggdistrib <- ggdistrib + geom_bar( colour = NA, stat="identity", position = "fill")
ggdistrib <- ggdistrib + scale_fill_manual(values = vec_fill)
#ggdistrib <- ggdistrib + scale_y_discrete(breaks = c("habitat_DV", "habitat",tab_bird[,bird_id]),labels= c("habitat_DV", "habitat",tab_bird[,label]))
ggdistrib <- ggdistrib + labs(fill ="", y = "", x="")
ggdistrib

#ggsave("Rplot/distrib_loc.png",gg)




# Analyse avec daynight
distri_loc_hab <- sum_loc[,-c(1,2,3,4,7,8,9,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,32,33,34,35,36,37)]
#distri_loc_hab <- distri_loc_hab %>% relocate(habitat, .after = bird_id)
#distri_loc_hab <- distri_loc_hab %>% relocate(proportion, .after = habitat)

library(data.table)
setDT(distri_loc_hab)
#  tableau occurence par habitat par oiseau en fonction du jour et de la nuit
tab_daynight <- distri_loc_hab[,.(nb = .N),by=.(bird_id,habitat,day_night)]


setDF(area_habitat)
# barre de reference : habitat de l'ensemble des DV par J/N
area_habitat_day <- area_habitat
setDT(area_habitat_day)
area_habitat_day [,day_night := "Jour"]

area_habitat_night <- area_habitat
setDT(area_habitat_night)
area_habitat_night [,day_night := "Nuit"]

setDT(area_habitat)

area_habitat_dn <- bind_rows(area_habitat_day, area_habitat_night)
tab_daynight <- bind_rows(tab_daynight, area_habitat_dn)


#nombre de donnees par oiseau en fonction J/N
library(tidyr)
tab_bird_dn <- sum_loc[,.(occurence = .N),by=.(bird_id, day_night)]
tab_bird_dn <- pivot_wider(tab_bird_dn, names_from = "day_night", values_from = "occurence")

setDT(tab_bird_dn)
tab_bird_dn[,label := paste0(bird_id," (",Jour," , ",Nuit,")")]



# representation graphique de la distribution des localisations par habitat et par habitat des motus occupes en fonction J/N
ggdistrib1 <- ggplot(data = tab_daynight,aes(x = nb, y = bird_id, fill = habitat))+facet_grid(.~day_night)
ggdistrib1 <- ggdistrib1 + geom_bar( colour = NA, stat="identity", position = "fill")
ggdistrib1 <- ggdistrib1 + scale_fill_manual(values = vec_fill)
#ggdistrib <- ggdistrib1 + scale_y_discrete(breaks = c("habitat_DV", "habitat",tab_bird_dn[,bird_id]),labels= c("habitat_DV", "habitat",tab_bird_dn[,label]))
ggdistrib1 <- ggdistrib1 + labs(fill ="", y = "", x="")
ggdistrib1

ggsave("Rplot/ggdistribx.png",ggdistrib1, width = 10, height = 5)


ggdistrib2 <- ggplot(data = tab_daynight,aes(x = nb, y = bird_id, fill = habitat))+facet_grid(.~day_night)
ggdistrib2 <- ggdistrib2 + geom_bar( colour = NA, stat="identity", position = "fill")
ggdistrib2 <- ggdistrib2 + scale_fill_manual(values = vec_fill)
#ggdistrib2 <- ggdistrib2 + scale_y_discrete(breaks = c("ZST habitat", tab_bird_dn[,bird_id]),labels= c("ZST habitat", tab_bird_dn[,label]))
ggdistrib2 <- ggdistrib2 + labs(fill ="", y = "", x="")
ggdistrib2







### Pression electrocution #################################################################################################################################################
##### Proportion habitat par ZST ################################################################################################################
#importation de ma couche buffer de 100m autour de mes lignes cree a partir de qgis + modification de la projection
BT <- st_read("SIG/Buffer_100m_ligne.shp")
BT_sf <- st_transform(BT,crs=2154)
BT_sf <- st_union (BT_sf)
BT_sf <- st_make_valid(BT_sf)
#st_write(BT_sf, dsn = "BT_sf", layer = "BT_sf.shp", driver = "ESRI Shapefile", overwrite_layer = T)
BT_sf <- st_as_sf(BT_sf)

# modification de la projection de la couche habitat
hab_sf <- st_as_sf(hab)
hab_sf <- st_transform(hab_sf,crs=2154)

#Intersection entre la couche habitat et mes buffer de 100m
BT_hab <- st_intersection(BT_sf,hab_sf)
st_write(BT_hab, dsn = "BT_hab", layer = "BT_hab.shp", driver = "ESRI Shapefile", overwrite_layer = T)
BT_hab <- BT_hab[,-c(8,9,10)]

# Calcul des aires par polygone dans les buffers de 100m
BT_hab$area_poly <- st_area(BT_hab)

# Somme des aires par habitat par ZST (group)
area_hab <- aggregate(area_poly~BT_hab$habitat + BT_hab$group, BT_hab, sum)
names(area_hab)[1] <- "habitat"
names(area_hab)[2] <- "group"
setDT(area_hab)

# Aire totale des buffer de 100m par ZST (group)
area_group <- aggregate(area_poly~BT_hab$group, BT_hab, sum)
names(area_group)[1] <- "group"
names(area_group)[2] <- "area_buffer"
setDT(area_group)

#merge + calcul des proportions de chaque habitat des buffer par ZST (group)
BT_area <- merge(area_hab, area_group, by = "group")
# Calcul des proportions d'habitat par buffer de 100m par ZST (group)
BT_area$proportion <- (BT_area$area_poly/BT_area$area_buffer)
BT_prop_hab <- unique(BT_area[,c("group", "habitat", "proportion")])
BT_prop_hab$proportion <- as.numeric(BT_prop_hab$proportion)
BT_prop_hab$pourcentage <- percent(BT_prop_hab$proportion, accuracy = 1)
BT_prop_hab <- subset(BT_prop_hab, BT_prop_hab$pourcentage!= "0%")


# representation graphique de la proportion des habitats sur un rayon de 100m autour des lignes basses tensions par ZST (group)
ggdistrib <- ggplot(data = BT_prop_hab,aes(x = proportion, y = group, fill = habitat))
ggdistrib <- ggdistrib + geom_bar( colour = NA, stat="identity", position = "fill")
ggdistrib <- ggdistrib + scale_fill_manual(values = vec_fill)
ggdistrib <- ggdistrib + labs(fill ="Habitats", y = "", x="")
ggdistrib <- ggdistrib + geom_text(data = BT_prop_hab, aes(label = pourcentage), size=4, position = position_stack(vjust = 0.5))
ggdistrib
ggsave("Rplot/BT_prop_hab.png",ggdistrib, width = 15, height = 7)





##### Proportion habitat ensemble des ZST #######################################################################################################
# Somme des aires par habitat pour l'ensemble de mes ZST
area_hab_tot <- aggregate(area_poly~BT_hab$habitat, BT_hab, sum)
names(area_hab_tot)[1] <- "habitat"
setDT(area_hab_tot)

# Aire totale des buffer de 100m pour l'ensemble de mes ZST
area_hab_tot = group_by(area_hab_tot) %>%
  mutate(tot_area = sum(area_poly))

## Calcul des proportions d'habitat par buffer de 100m pour l'ensemble de mes ZST
area_hab_tot$proportion <- (area_hab_tot$area_poly/area_hab_tot$tot_area)
area_hab_tot$proportion <- as.numeric(area_hab_tot$proportion)
area_hab_tot$pourcentage <- percent(area_hab_tot$proportion, accuracy = 1)

