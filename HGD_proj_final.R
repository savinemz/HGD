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
data_HGD_original <- read.csv2("data_HGD.csv", head = T, sep = ";", stringsAsFactors = T)# certains points aberrants ont été enlevés dans l'excel (oiseaux morts)
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


#selection des hdop = précision Sur l'horizontalite du point (0,8 < hdop < 1.3)
#plus le hdop est proche de 1 mieux c'est
data_HGD <- filter(data_HGD,hdop > 0.79)
data_HGD <- filter(data_HGD,hdop < 1.31)
plot(data_HGD$hdop)
boxplot(data_HGD$hdop)
summary(data_HGD$hdop)

## ajout de la colonne date et la colonne heure manuellement
data_HGD$heure_HH <- substr(data_HGD$time,1,2)
data_HGD$date_HH <- paste0(data_HGD$date, "_", data_HGD$heure_HH)

#importation des donnees sur le jour de depart de chaque oiseau (visuellement au point près (voir excel à part) sur la viosionneuse ornitrack)
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
gg <- gg + labs(y = "Oiseaux", x = "Nombre de données par heure")
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



library(adehabitatHR)
library(sf)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(tidyverse)
library(data.table)
library(ggrepel)
library(scales)

#Importation des donnees cartographiques paysage
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
Trame_verte_sf <- st_as_sf(Trame_verte_sf)


# Ligne electrique moyenne tension Enedis
LBT_Enedis <- st_read("SIG/LBT_Enedis.shp")



# Ligne electrique moyenne tension Enedis a risque
LBT_Enedis_risque <- st_read("SIG/LBT_Enedis_risque.shp")
LBT_Enedis_risque <- st_read("SIG/Ligne_BT_buf_ZST.shp")

#Aire de nidification 2022 sans Glageon
aire_2022 <- st_read("SIG/aire_2022_complet.shp")
aire_2022 <- st_transform(aire_2022,crs=2154)
aire_2022_sf <- st_make_valid(aire_2022)


# Cartographie generale #####################################################################################################################
# Repartition geographique des points gps NPDC avec BM + TV
gg <- ggplot() + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = Bassin_minier, aes(fill = "#BD9B95"), color= NA,alpha=.5)
gg <- gg + geom_sf(data = Trame_verte_sf, aes(fill = "#41CC76"), color= NA,alpha= 0.5)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="Répartition géographique des localisations GPS dans le Nord et le Pas-de-Calais")
gg <- gg + scale_fill_manual("Paysage", values = c("#41CC76","#BD9B95"),labels = c("Trame Verte", "Bassin minier"))
gg <- gg + theme (legend.title = element_text(size = 15), legend.text = element_text(size = 13))
gg
ggsave("Rplot/Carto/loc_bird.png",gg, width = 20, height = 11)




#test ajout des aire de nidification
gg <- ggplot() + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = Bassin_minier, aes(fill = "#BD9B95"), color= NA,alpha=.5)
gg <- gg + geom_sf(data = Trame_verte_sf, aes(fill = "#41CC76"), color= NA,alpha= 0.5)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.5)
gg <- gg + geom_sf(data = aire_2022_sf,aes(colour= bird_id), size = 6, alpha = 1) + labs (colour = "Aires de nidification HGD")
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="Répartition géographique des localisations GPS dans le Nord et le Pas-de-Calais")
gg <- gg + scale_fill_manual("Paysage", values = c("#41CC76","#BD9B95"),labels = c("Trame Verte", "Bassin minier"))
gg <- gg + theme (legend.title = element_text(size = 15), legend.text = element_text(size = 13))
gg
ggsave("Rplot/Carto/loc_birdtest.png",gg, width = 20, height = 11)



gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = aire_2022_sf,aes(colour= bird_id), size = 6, alpha = 1) + labs (colour = "Aires de nidification HGD")
gg
ggsave("Rplot/Carto/aire_nidif_2022.png",gg, width = 20, height = 11)




# Repartition geographique des points gps NPDC avec LMT (ligne moyenne tension)
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = LBT_Enedis, fill = "#1e63e3", color= "#1e63e3",alpha=.5) 
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="Répartition géographique des localisations GPS dans le Nord et le Pas-de-Calais")
gg
ggsave("Rplot/Carto/LBT.png",gg, width = 20, height = 11)



### Estimation des DV en dispersion (Kernel : LSCV) ############################################################################################################
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




#### Cartographie DV ###########################################################################################################################

#Vu generale des kernel Disp departement NPDC 95%, 50%, 30%
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) 
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="Domaines vitaux des HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95%, 50% et 30%)")
gg
ggsave("Rplot/Kernel/kernel_NPDC_LSCV.png",gg, width = 25, height = 13)

#Vu generale des kernel Disp departement NPDC 95%
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1,fill=NA,alpha = 1)
#gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) 
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="Domaines vitaux des HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95%)")
gg <- gg + theme (legend.title = element_text(size = 15), legend.text = element_text(size = 13))
gg
ggsave("Rplot/Kernel/kernel_NPDC_LSCV_95t.png",gg, width = 20, height = 11)

#Vu generale des kernel Disp departement NPDC 95% sans les pôints GPS
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_all, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
#gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) 
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="Domaines vitaux des HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95%)")
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
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="Domaines vitaux des HGD en dispersion en région Hauts-de-France (Kernel 95%, 50% et 30%)")
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
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="Domaines vitaux par HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95%)")
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/kernel_NPDC_bird_LSCV_95.png",gg, width = 25, height = 13)







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
# liste unique des oiseaux pour prendre en compte les points a l'intersection de 2 domaines vitaux
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


#Graphique: duree de sejour dans les ZST
gg <- ggplot(data = resSup5,aes(x = group, y = nb_jour, fill = bird_id))
gg <- gg + geom_bar(colour = NA, stat="identity")
gg <- gg + labs(x="",y="Nombre de jour",fill ="Identifiant Grand-duc",title="Durée de séjour dans les ZST par HGD")
gg <- gg + theme (legend.title = element_text(size = 15), legend.text = element_text(size = 13))
gg
ggsave("Rplot/Nb_jour_ZST.png",gg, width = 15, height = 7)

#Graphique: surface des ZST
gg <- ggplot(data = resSup5,aes(x = group, y = area_DV, fill = bird_id))
gg <- gg + geom_bar(colour = NA, stat="identity")
gg <- gg + labs(x="",y="Surface de la ZST",fill ="Identifiant Grand-duc",title="Surface des ZST par HGD")
gg <- gg + theme (legend.title = element_text(size = 15), legend.text = element_text(size = 13))
gg
ggsave("Rplot/Surface_ZST.png",gg, width = 15, height = 7)



mean_duree_ZST <- mean(resSup5$nb_jour)
min_duree_ZST <- min(resSup5$nb_jour)
max_duree_ZST <- max(resSup5$nb_jour)

mean_surface <- mean(resSup5$area_DV)
min_surface <- min(resSup5$area_DV)
max_surface <- max(resSup5$area_DV)


# Cartographie ZST ###########################################################################################################################

#Fond de carte global
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = Bassin_minier, fill = "#bd9b95", color= NA,alpha=.5)
gg <- gg + geom_sf(data = Trame_verte, fill = "#41cc76", color= NA,alpha= 0.5)
gg


#Cartographie des kernel Disp departement NPDC ZST > 5 sans les point gps mais avec étiquette

gg <- ggplot() + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = Bassin_minier, aes(fill = "#BD9B95"), color= NA,alpha=.5)
gg <- gg + geom_sf(data = Trame_verte_sf, aes(fill = "#41CC76"), color= NA,alpha= 0.5)
gg <- gg + geom_sf(data = resSup5_sf, aes(color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
#gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="Répartition géographique des localisations GPS dans le Nord et le Pas-de-Calais")
gg <- gg + geom_label_repel(data = resSup5_sf, aes(label = group, geometry = geometry), stat = "sf_coordinates", min.segment.length = 0,colour = "black",segment.colour = "black")
gg <- gg + scale_fill_manual("Paysage", values = c("#41CC76","#BD9B95"),labels = c("Trame Verte", "Bassin minier"))
gg <- gg + theme (legend.title = element_text(size = 15), legend.text = element_text(size = 13))
gg
ggsave("Rplot/Carto/loc_bird_paysage.png",gg, width = 20, height = 11)


#vu par HGD des ZST departement NPDC
gg <- ggplot()  + theme_bw() + facet_wrap(.~bird_id)
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_sf(data = resSup5_sf, aes(color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
#gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="ZST des HGD en dispersion dans les départements du Nord et du Pas-de-Calais issus des domaines vitaux (Kernel 95%)")
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
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="ZST des HGD en dispersion dans les départements du Nord et du Pas-de-Calais issus des domaines vitaux (Kernel 95%)")
gg
ggsave("Rplot/Kernel/kernel_HDF_LSCV_sup51.png",gg, width = 20, height = 11)


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
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="Domaines vitaux des HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95% href)")
gg <- gg + theme (legend.title = element_text(size = 15), legend.text = element_text(size = 13))
gg
ggsave("Rplot/Kernel/kernel_NPDC_href95.png",gg, width = 20, height = 11)

#Vu generale des kernel Disp departement NPDC 95% h = href avec loc GPS
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_href, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) 
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Identifiant Grand-duc",title="Domaines vitaux des HGD en dispersion dans les départements du Nord et du Pas-de-Calais (Kernel 95% href)")
gg
ggsave("Rplot/Kernel/kernel_NPDC_href95_complet.png",gg, width = 20, height = 11)



#### ZST (DV 50%) ######################################################################################################################
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
ud_50_Disp_l <- lapply(kdh_Disp_l, function(x) try(getverticeshr(x, 50)))


sapply(1:length(ud_50_Disp_l), function(i) {
  row.names(ud_50_Disp_l[[i]]) <<- kd_names_Disp_l[i]
})
sdf_poly_50_Disp_l <- Reduce(rbind, ud_50_Disp_l)
df_50_Disp_l <- fortify(sdf_poly_50_Disp_l)
df_50_Disp_l$bird_id <- df_50_Disp_l$id


sdf_poly_50_Disp_l <- Reduce(rbind, ud_50_Disp_l)
sf_poly_50_Disp_l <- st_as_sf(sdf_poly_50_Disp_l)
sf_poly_50_Disp_l_DV <- st_cast(sf_poly_50_Disp_l, "MULTIPOLYGON") %>% st_cast("POLYGON")#transforme les listes emboitees



sf_poly_50_Disp_l_DV <- sf_poly_50_Disp_l_DV %>% 
  rownames_to_column(var="group")
sf_poly_50_Disp_l_DV$area_DV <- st_area(sf_poly_50_Disp_l_DV)

#st_write(sf_poly_50_Disp_l_DV, dsn = "sf_poly_50_Disp_l_DV", layer = "sf_poly_50_Disp_l_DV", driver = "CSV", overwrite_layer = T)
#complété le fichier csv avec une colonne oiseaux (remplir à la main)

DV_Disp50 <- read.csv("C:/Git/HGD/sf_poly_50_Disp_l_DV/sf_poly_50_Disp_l_DV.csv", head = T, sep = ";", stringsAsFactors = T)
DV_Disp50 <- DV_Disp50[,-c(2,3,4)]
DV_Disp50 <- merge(DV_Disp50, sf_poly_50_Disp_l_DV, by = "group")
DV_Disp50 <- DV_Disp50[,-c(3,4)]
DV_Disp50 <- DV_Disp50 %>% relocate(group, .after = bird_id)

#Sauvegarde des DV globaux
#st_write(DV_Disp50, dsn = "DV_Disp50", layer = "DV_Disp50.shp", driver = "ESRI Shapefile", overwrite_layer = T)
DV_Disp50 <- st_as_sf(DV_Disp50)
DV_Disp50 <- st_transform(DV_Disp50,crs=2154)
#st_crs(DV_Disp)








#### Selection des habitats J/N ################################################################################################
##### Data Habitat ##################################################################################################################################

#calculs des surfaces par polygones
CLC <- st_read("SIG/CLC_HDF_2018.shx", stringsAsFactors = T)
CLC_code <- read.csv2("code_habitat.csv", head = T, sep = ";", stringsAsFactors = T) #fait manuellement, correspondance code habitat
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
# liste unique de tes oiseaux pour prendre en compte les points a l'intersection de 2 domaines vitaux
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
#library(ggplot2); library(units)
#gg <- ggplot(data = d_gg, aes(x = habitat, y = prop_mean,fill = occupation,colour=occupation,group=occupation))
#gg <- gg + geom_errorbar(aes(ymin = inf95, ymax = sup95),width = 0.5,alpha=.5,linewidth=1)
#gg <- gg +  geom_point(alpha=.8,size=2)
#gg <- gg + labs(y = "Proportion mean", x = "Habitats")
#gg



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
area_habitat[,bird_id := "ZST TOTAL*"]
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
ggdistrib1 <- ggdistrib1 + scale_fill_manual(values = vec_fill, labels = c("Cultures arables", "Forêts de conifères", "Forêts de feuillus", "Prairies", "Zones urbaines"))
#ggdistrib1 <- ggdistrib1 + scale_y_discrete(breaks = c("ZST TOTAL*", "habitat",tab_bird_dn[,bird_id]),labels= c(tab_bird_dn[,label]))
ggdistrib1 <- ggdistrib1 + labs(fill ="", y = "", x="")
ggdistrib1

ggsave("Rplot/ggdistribx.png",ggdistrib1, width = 10, height = 5)





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
#st_write(BT_hab, dsn = "BT_hab", layer = "BT_hab.shp", driver = "ESRI Shapefile", overwrite_layer = T)
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

# barre de reference: proportion des habitats en dessous des lignes pour l'ensemble des ZST
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

#Assemblage des proportions par ZST + proportion ZST totales
BT_prop_hab_tot <- setDT(area_hab_tot)
BT_prop_hab_tot <- BT_prop_hab_tot[,-c(2,3)]
setDT(BT_prop_hab_tot)
BT_prop_hab_tot[,group := "ZST TOTAL*"]
BT_prop_hab_tot[,nb := as.numeric(proportion)]
BT_prop_hab_tot <- BT_prop_hab_tot %>% relocate(group, .after = habitat)
BT_prop_hab_tot <- BT_prop_hab_tot %>% relocate(habitat, .after = group)

setcolorder(BT_prop_hab_tot,c("group","habitat","nb"))
BT_prop_hab_complet <- bind_rows(BT_prop_hab, BT_prop_hab_tot)


# representation graphique de la proportion des habitats sur un rayon de 100m autour des lignes basses tensions par ZST (group)
ggdistrib <- ggplot(data = BT_prop_hab_complet,aes(x = proportion, y = group, fill = habitat))
ggdistrib <- ggdistrib + geom_bar( colour = NA, stat="identity", position = "fill")
ggdistrib <- ggdistrib + scale_fill_manual(values = vec_fill, labels = c("Cultures arables", "Forêts de feuillus", "Prairies", "Zones urbaines"))
ggdistrib <- ggdistrib + labs(fill ="", y = "", x="")
ggdistrib <- ggdistrib + geom_text(data = BT_prop_hab_complet, aes(label = pourcentage), size=4, position = position_stack(vjust = 0.5))
ggdistrib
ggsave("Rplot/BT_prop_hab1.png",ggdistrib, width = 10, height = 6)






##### Cartographie ZST + LBT #############################################################################################################################

# Zone a securiser en priorite  (zone tampon de 2000 m autour des ZST)
Buffer_resSup5_sf <- st_buffer(resSup5_sf, dist= 2000)
Buffer_resSup5_sfu <- st_union (Buffer_resSup5_sf)
Buffer_resSup5_sfu <- st_make_valid(Buffer_resSup5_sfu)
Buffer_resSup5_sfu <- st_as_sf(Buffer_resSup5_sfu)
#st_write(Buffer_resSup5_sfu, dsn = "Buffer_resSup5_sfu", layer = "Buffer_resSup5_sfu.shp", driver = "ESRI Shapefile", overwrite_layer = T)

#decoupage sur Qgis pour les lignes BT a securiser en priorité
LBT_Enedis_risque_buf <- st_read("SIG/Ligne_BT_buf_ZST.shp")

#Importation Zone d'activité intense (50% d'UD)
Zone_act_int <- st_read("SIG/Zone d'activité intense (50% d'UD).shp")

#Carte simple du reseau MT
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = LBT_Enedis, aes(fill = "#1e63e3"), color= "#1e63e3",alpha=.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + scale_fill_manual("", values = c("#1e63e3"),labels = c("Réseau électrique Moyenne Tension"))
gg <- gg + theme (legend.position = 'bottom', legend.title = element_text(size = 15), legend.text = element_text(size = 13))
gg

ggsave("Rplot/Carto/reseau_moyenne_tension.png",gg, width = 20, height = 11)


#le mieux
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = LBT_Enedis, aes(color = "#1e63e3"),alpha=.5)
gg <- gg + geom_sf(data = LBT_Enedis_risque, aes(color = "#f50000"),alpha=1)
gg <- gg + geom_sf(data = Buffer_resSup5_sfu, aes(fill = "#080808"), color = "#080808",linewidth =1,fill=NA,alpha = 1)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour= "" ,title="Zone à sécuriser en priorité")
gg <- gg + scale_color_manual(values = c("#1e63e3","#f50000", "#080808"),
                             labels = c("Réseau électrique Enedis", "Lignes électriques Enedis à risque", "Zone à sécuriser en priorité"))
gg <- gg + theme (legend.position = 'bottom', legend.title = element_text(size = 15), legend.text = element_text(size = 13))
gg
ggsave("Rplot/Carto/zone_a_risque_LBT_buff_ludoi.png",gg, width = 20, height = 11)



#test2
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = LBT_Enedis, aes(color = "#1e63e3"),alpha=.5)
gg <- gg + geom_sf(data = LBT_Enedis_risque, aes(color = "#f50000"),alpha=1)
gg <- gg + geom_sf(data = Buffer_resSup5_sfu, aes(fill = "#080808"), color = "#080808",linewidth =1,fill=NA,alpha = 1)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour= "Zone à sécuriser en priorité" ,title="Zone à sécuriser en priorité")
gg <- gg + scale_color_manual(values = c("#1e63e3","#f50000"),labels = c("Ligne électrique Enedis", "Ligne électrique Enedis à risque"))
gg <- gg + scale_fill_manual(values = c("#080808"), labels = c("Zone à sécuriser en priorité"))
gg
ggsave("Rplot/Carto/zone_a_risque_LBT_buff_ludor.png",gg, width = 20, height = 11)



gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg + geom_sf(data = resSup5_sf, aes(fill = "#FAA005"), color= NA,alpha= 0.5)
gg <- gg + geom_sf(data = Zone_act_int, aes(fill = "#B30502"), color= NA,alpha= 0.5)
gg <- gg + geom_sf(data = Buffer_resSup5_sfu, aes(fill = "#080808"), color = "#080808",linewidth =1,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Buffer_resSup5_sfu, aes(fill = "#080808"), color = NA, alpha = 1, fill = NA, show.legend = "polygon")
gg <- gg + scale_fill_manual(values = c("#B30502","#FAA005","#080808"),
                             labels = c("Zone d'activité intense du HGD dans sa ZST (50% d'UD)",
                                        "Zone d'activité globale du HGD dans sa ZST (95% d'UD)",
                                        "Zone tampon de 2 km"))
gg <- gg + labs(fill = "")
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + theme (legend.position = 'bottom', legend.title = element_text(size = 15), legend.text = element_text(size = 13))
#gg <- gg + geom_label_repel(data = resSup5_sf, aes(label = group, geometry = geometry), stat = "sf_coordinates", min.segment.length = 0,colour = "black",segment.colour = "black")
gg <- gg + geom_text_repel(point.padding = NA)
gg
ggsave("Rplot/Carto/zonage_ZST_ss.png",gg, width = 20, height = 11)




### GLMM #######################################################################################################################################
sum_loc_DV <- sum_loc [,.(occurence =.N), by =.(objectid, day_night, bird_id)]

#ajout des absences utilise par individu = permet de definir le domaine vital (DV)= zone utilise pour toutes les activites
bird_DV <- unique(sum_loc[,.(group, bird_id)])#on a toutes les possibilites de ZST par oiseau
poly_DV <- hab[,.(group, objectid)]#on a toutes les possibilites de polygones dans mes ZST
bird_poly <- merge(bird_DV, hab[,.(group, objectid)], by= "group", allow.cartesian= T)

#ajout daymight
#bird_daynight <- rbind (bird_poly[,daynight :="day"], bird_poly[,daynight :="night"])
setDF(bird_poly)
bird_daynight_day <- bird_poly
setDT(bird_daynight_day)
bird_daynight_day [,day_night := "Jour"]

bird_daynight_night <- bird_poly
setDT(bird_daynight_night)
bird_daynight_night [,day_night := "Nuit"]

setDT(bird_poly)
bird_daynight <- bind_rows(bird_daynight_day, bird_daynight_night)



sum_loc_DV <- merge(sum_loc_DV, bird_daynight, all =T, allow.cartesian= T)
setDT(sum_loc_DV)
sum_loc_DV$occurence[is.na(sum_loc_DV$occurence)] <- 0
sum_loc_DV <- sum_loc_DV %>% relocate(group, .after = objectid)
sum_loc_DV <- sum_loc_DV %>% relocate(objectid, .after = group)

tab_glmm_i <- merge(sum_loc_DV, hab[,c(1,5,9)], by = "objectid", allow.cartesian= T)
tab_glmm_i[,area_poly := as.numeric(area)]
tab_glmm_i[,area_poly_st := scale(area)]



hist(tab_glmm_i$occurence)
# glmm de ref dans le rapport ziformula = day_night
##library(glmm)
library(glmmTMB)
library(glmm)
library(DHARMa)
library(ggeffects)


#bof pour l'AIC
glmm1 <- glmmTMB(occurence~habitat * day_night + area_poly_st + (1|group) + (1|bird_id), ziformula = ~day_night,
                 family = "poisson", data=tab_glmm_i)
sglmm1 <- summary(glmm1)
print(sglmm1)

ggpred <- ggpredict(glmm1,terms = c("habitat","day_night"))
print(ggpred)
plot(ggpred)

#verification des conditions d'application
sim_glm <- simulateResiduals(glmm1)
testResiduals(sim_glm)
plot(sim_glm)



# top 
glmm2 <- glmmTMB(occurence~habitat * day_night + area_poly_st + (1|group) + (1|bird_id), ziformula = ~day_night,
                 family = "nbinom2", data=tab_glmm_i)
sglmm2 <- summary(glmm2)
print(sglmm2)

ggpred <- ggpredict(glmm2,terms = c("habitat","day_night"))
print(ggpred)
plot(ggpred)




# encore plus top
library(glmm)
glmm2 <- glmmTMB(occurence~habitat * day_night + area_poly_st + (1|group) + (1|bird_id),
                 family = "nbinom2", data=tab_glmm_i[bird_id != "Licques",])
sglmm2 <- summary(glmm2)
print(sglmm2)

ggpred <- ggpredict(glmm2,terms = c("habitat","day_night"))
print(ggpred)
plot(ggpred)


#verification des conditions d'application
sim_glm <- simulateResiduals(glmm2)
testResiduals(sim_glm)
plot(sim_glm)


#(Intercept) ***
exp(1.933009)
# exp(1.933009) = 6.910272
# 6.910272 occurence en moyenne le jour pour mon habitat de référence qui est l'habitat culture arable

#habitatforets_coniferes 
exp(1.933009 + (0.157624))
# exp(1.933009 + (0.157624)) = 8.090035 occurence en moyenne le jour pour une foret de conifères

# habitatforets_feuillus ***
exp(1.933009 + (1.410378))
# exp(1.933009 + (1.410378)) = 28.31487 occurence en moyenne le jour pour une foret de feuillus


#habitatprairie .
exp(1.933009 + (-0.454998))
# exp(1.933009 + (-0.454998)) = 4.384217 occurence en moyenne le jour pour une prairie

# habitaturbain ***
exp(1.933009 + (-3.488356)) 
# exp(1.933009 + (-3.488356)) = 0.2111161 occurence en moyenne le jour pour l'habitat urbain

#day_nightNuit ***
exp(1.933009 + (1.435395))
# exp(1.933009 + (1.435395)) = 29.03215 occurence en moyenne la nuit pour l'habitat culture arable

#area_poly_st ***
exp(1.933009 + (0.671318))
# à chaque fois que l'aire augmente de 1, l'abondance est multipliée par exp(1.933009 + (0.671318)) = 13.52212

#habitatforets_coniferes:day_nightNuit .
exp(1.933009 + (-2.350049))
# exp(1.933009 + (-2.350049)) = 0.6589946 occurence en moyenne la nuit pour l'habitat foret de conifères

#habitatforets_feuillus:day_nightNuit ***
exp(1.933009 + (-1.603377))
# exp(1.933009 + (-1.603377)) = 1.390456 occurence en moyenne la nuit pour l'habitat foret de feuillus

#habitatprairie:day_nightNuit
exp(1.933009 + (-0.092701))
# exp(1.933009 + (-0.092701)) = 6.298478 occurence en moyenne la nuit pour l'habitat prairie

#habitaturbain:day_nightNuit
exp(1.933009 + (-0.003576))
# exp(1.933009 + (-0.003576)) = 6.885605 occurence en moyenne la nuit pour l'habitat urbain


# Likelihood ratio test
anova(glmm1, glmm2, test="LRT")

## model with interaction is significantly better than without


# Effet des rats sur la présence des courlis sur chaque habitats


## pour interaction pent mais ici ne fonctinne pas
## library(effects)
## plot(predictorEffects(glmm))

kruskal.test(tab_glmm_i$habitat, tab_glmm_i$area_poly_st)
kruskal.test(tab_glmm_i$area_poly_st, tab_glmm_i$habitat)

library(multcomp)
summary(glht(glmm1))
# Histogrammes des occurrences


TukeyHSD(aov(occurence~ habitat*day_night, data=tab_glmm_i))

tuk <- TukeyHSD(aov(occurence~ habitat,data=tab_glmm_i))
plot(tuk)

ggsave("Rplot/tuk.png",tuk_plot, width = 20, height = 11)





#glmm <- glmm( occurence ~ habitat * day_night + area + (1| group) + (1|bird_id),family = "nbinom2", data=tab_glmm_i)



# Original model
#glmm <- glmm(occurence~ area_poly_st + habitat*day_night +  (1|group) + (1|bird_id),
#family = "poisson", data=tab_glmm_i)
#sglmm <- summary(glmm)
#print(sglmm)
simulationOutput <- simulateResiduals(fittedModel = glmm2, plot = F)
testZeroInflation(simulationOutput)
plot(simulationOutput)
