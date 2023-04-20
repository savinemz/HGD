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


### Data Habitat ##################################################################################################################################

#calculs des surfaces par polygones
CLC <- st_read("SIG/CLC_HDF_2018.shx", stringsAsFactors = T)
CLC_code <- read.csv2("code_habitat.csv", head = T, sep = ";", stringsAsFactors = T)
CLC <- CLC[,-c(1,2,4,6)]# colonne non utilise
CLC <- merge(CLC, CLC_code, by = "code_18")
CLC <- st_transform(CLC,crs=3832)#transformation des donnees
CLC$area_poly <- st_area(CLC)
summary(CLC)

#identifiant des polygones
CLC$id_poly <- 1: nrow(CLC)
CLC <- CLC %>% relocate(id_poly, .after = area_ha)
CLC <- CLC %>% relocate(code_18, .after = habitat)
CLC <- CLC %>% relocate(area_ha, .after = area_poly)


gg <- plot(CLC$code_18)
gg <- plot(CLC$habitat)
#ggsave("Rplot/hab.png",gg)


#calcul des surfaces totales par code_18
area_hab_HDF <- aggregate(area_poly~CLC$code_18, CLC, sum)
names(area_hab_HDF)[1] <- "code_18"
names(area_hab_HDF)[2] <- "area_tot_hab"

#calcul de l'aire totale (meme calcul pour les deux lignes)
area_HDF <- sum(area_hab_HDF$area_tot_hab)
area_HDF_poly <- sum(CLC$area_poly)


#proportion par code_18 en HDF
area_hab_HDF$proportion <- (area_hab_HDF$area_tot_hab/area_HDF)
sum(area_hab_HDF$proportion)
CLC <- merge(CLC, area_hab_HDF, by = "code_18")
ggprop <- plot(area_hab_HDF$proportion~area_hab_HDF$code_18)







#calcul des surfaces totales par habitat
area_hab_HDF <- aggregate(area_poly~CLC$habitat, CLC, sum)
names(area_hab_HDF)[1] <- "habitat"
names(area_hab_HDF)[2] <- "area_tot_hab"

#calcul de l'aire totale (meme calcul pour les deux lignes)
area_HDF <- sum(area_hab_HDF$area_tot_hab)
area_HDF_poly <- sum(CLC$area_poly)


#proportion par habitat en HDF
area_hab_HDF$proportion <- (area_hab_HDF$area_tot_hab/area_HDF)
sum(area_hab_HDF$proportion)
CLC <- merge(CLC, area_hab_HDF, by = "habitat")
ggprop <- plot(area_hab_HDF$proportion~area_hab_HDF$habitat)






### Data HGD #########################################################################################################################################

#Importation des donnees
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
HGD_DT <- data_HGD[,-c(1,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)]
setDT(HGD_DT)
periode <- ifelse(as.Date(HGD_DT$date) < as.Date(HGD_DT$date_depart), "Dependance alimentaire", "Dispersion")
data_HGD <- cbind(data_HGD,periode)
data_HGD <- data_HGD %>% relocate(periode, .after = date_depart)


# Transformation des coordonnees en donnees spatiales + modification de la projection
#HGD_sf <- st_as_sf(data_HGD, coords = c("Longitude","Latitude"))
#st_crs(HGD_sf) <- 4326
#HGD_sf <- st_transform(HGD_sf,crs=3832)

#distance entre deux points
library(maps)#package map

HGD_DT_Dist <- data_HGD
setDT(HGD_DT_Dist)
HGD_DT_Dist <- HGD_DT_Dist[order(bird_id, date_HH),]
HGD_DT_Dist <- st_as_sf(HGD_DT_Dist, crs = 4326L, coords = c("Longitude", "Latitude"))
HGD_DT_Dist <- HGD_DT_Dist %>% group_by(bird_id) %>%
  mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T),)

#Separation des donnees depali et disp pour calculer la distance moyenne par periode
library(dplyr)
HGD_Dist_Disp <- filter(HGD_DT_Dist,periode == "Dispersion")
summary(HGD_Disp)#distance moyenne = 753.28m ==> buffer 1000m

HGD_Dist_Depali <- filter(HGD_DT_Dist,periode == "Dependance alimentaire")
summary(HGD_Depali)#distance moyenne = 83.798m ==> buffer 100m

summary(HGD_DT_Dist$dist)
boxplot(HGD_DT_Dist$dist~periode)


#Creation des buffer autour de mes points GPS
#Creation des buffer de 100 m autour de mes data en dependance alimentaire
data_HGD_Depali <- filter(data_HGD,periode == "Dependance alimentaire")
data_HGD_Depali <- st_as_sf(data_HGD_Depali, coords = c("Longitude","Latitude"))
st_crs(data_HGD_Depali) <- 4326
data_HGD_Depali <- st_transform(data_HGD_Depali,crs=3832)
Buffer_Depali <- st_buffer(data_HGD_Depali, dist=100)

#sauvegarde
write.csv(Buffer_Depali, file = "Buffer_Depali.csv")
fwrite(Buffer_Depali, "Buffer_Depali2.csv")
st_write(Buffer_Depali, "Buffer_Depali3.shp")


#Creation des buffer de 1000 m autour de mes data en dispersion
data_HGD_Disp <- filter(data_HGD,periode == "Dispersion")
data_HGD_Disp <- st_as_sf(data_HGD_Disp, crs = 4326L, coords = c("Longitude", "Latitude"))
Buffer_Disp <- st_buffer(data_HGD_Disp, dist=1000)
write.csv(Buffer_Disp, file = "Buffer_Disp.csv")

#sauvegarde
write.csv(Buffer_Disp, file = "Buffer_Disp.csv")
fwrite(Buffer_Disp, "Buffer_Disp.csv")
st_write(Buffer_Disp, "Buffer_Disp.shp")






# Transformation des coordonnees en donnees spatiales + modification de la projection
data_HGD_sf <- st_as_sf(data_HGD, coords = c("Longitude","Latitude"))
st_crs(data_HGD_sf) <- 4326
data_HGD_sf <- st_transform(data_HGD_sf,crs=3832)

#Assemblage des couches habitats + points GPS HGD
sum_loc <- st_intersection(CLC, data_HGD_sf)










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








### Utilisation du paysage par les oiseaux ##########################################################################################################

###### Comparaison des habitats composant les motus occupes et non occupes ###################################################################################################

# regroupement du nombre d'occurence par polygone, par oiseau, par jour et par heure
setDT(sum_loc)
sum_loc_poly <- sum_loc[,.(occurence = .N),by=.(id_poly,bird_id,date_HH)][,.(occurence = .N),by=.(id_poly)]

CLC <- merge(CLC, sum_loc_poly, bx = "id_poly", all.x = T)
CLC$occurence[is.na(CLC$occurence)] <- 0
CLC <- CLC %>% relocate(occurence, .after = code_18)


#création data.table pour d_gg
CLC_DT <- CLC
setDT(CLC_DT)
CLC_DT[,occupation := occurence>0]

#creation d'un tableau a partir de CLC_DT
CLC_DT[,proportion := as.numeric(proportion)]
d_gg <- CLC_DT[,.(prop_mean = mean(proportion),prop_med = median(proportion),inf95 = quantile(proportion, 0.025),sup95 = quantile(proportion, 0.975)), by=.(habitat,occupation)]



# Figure comparaison des habitats composant les polygones occupes et non occupes
library(ggplot2); library(units)
gg <- ggplot(data = d_gg, aes(x = habitat, y = prop_mean,fill = occupation,colour=occupation,group=occupation))
gg <- gg + geom_errorbar(aes(ymin = inf95, ymax = sup95),width = 0.5,alpha=.5,linewidth =1)
gg <- gg +  geom_point(alpha=.8,size=2)
gg <- gg + labs(y = "Proportion mean", x = "Habitats")
gg
#ggsave("Rplot/prop_mean.png",gg)




### Estimation des domaines vitaux ############################################################################################################
##### Polygone convex en periode de depali ##################################################################################################

# Packages
library(dplyr)  # To make code easier to read
library(ggplot2)

# Data Generation
#set.seed(1)  # For reproducibility of example

#df = data.frame(x = rnorm(100, 10, 5), y = rnorm(100, 10, 5), z = sample(letters[1:5], size =  100, replace = T))
#?rnorm
# Plot without convex hulls
#p = ggplot(df, aes(x, y, colour = z)) +
  #geom_point()
#p
# Computing convex hulls
#s = df %>%
  #split(df$z)  # Tranform data.frame in lists of data.frames depending on column 'z'

#ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  #lapply(., function(el) chull(el$x, el$y))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
#ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  #do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
#p.ch = ggplot(df, aes(x, y, colour = z)) +
  #geom_point() +
  #geom_polygon(data = ch, aes(fill = z), alpha = 0.2)


# Data Generation
library(dplyr)
HGD_DT <- data_HGD
HGD_DT <- data_HGD[,-c(2,3,4,6,7,8,9,10,13,14,15,16,17,18,19,20,21,22,23,25,26,27,28,29)]
HGD_Disp <- filter(HGD_DT,periode == "Dispersion")
HGD_Depali <- filter(HGD_DT,periode == "Dependance alimentaire")


df = HGD_Depali
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = bird_id)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$bird_id)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = bird_id)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = bird_id), alpha = 0.2)
p.ch



p.ch = ggplot(df, aes(Longitude, Latitude, colour = bird_id)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = bird_id), alpha = 0.2)
p.ch
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)

### Domaine vitaux sous Kernel href ##################################################################################################

##### DV all (Kernel) ############################################################################################################################
#install.packages("adehabitatHR")
library(adehabitatHR)
library(sf)
library(ggplot2)

# Fond de carte departement Nord Pas-de-Calais
NPDC <- st_read("SIG/Departement NPDC.shp")
NPDC <- st_make_valid(NPDC)
NPDC <- st_transform(NPDC,crs=3832)
NPDC <- st_make_valid(NPDC)

# Fond de carte departement region Hauts-de-France
HDF <- st_read("SIG/Departement HDF.shp")
HDF <- st_make_valid(HDF)
HDF <- st_transform(HDF,crs=3832)
HDF <- st_make_valid(HDF)

#Creation DV kernel all
data_HGD_sf <- st_as_sf(data_HGD, coords = c("Longitude","Latitude"))
st_crs(data_HGD_sf) <- 4326
data_HGD_sf <- st_transform(data_HGD_sf,crs=3832)
HGD_sf_NPDC <- data_HGD_sf[,c("bird_id")]
HGD_sf_NPDC <- st_crop(HGD_sf_NPDC,st_bbox(NPDC))
HGD_sf_NPDC_k <- as(HGD_sf_NPDC,'Spatial')

kdh_all_h <- kernelUD(HGD_sf_NPDC_k, h="href", grid = 1000)
image(kdh_all_h)


# creating SpatialPolygonsDataFrame
# Polygone spatial 95%
kd_names_all_h <- names(kdh_all_h)
ud_95_all_h <- lapply(kdh_all_h, function(x) try(getverticeshr(x, 95)))


sapply(1:length(ud_95_all_h), function(i) {
  row.names(ud_95_all_h[[i]]) <<- kd_names_all_h[i]
})
sdf_poly_95_all_h <- Reduce(rbind, ud_95_all_h)
df_95_all_h <- fortify(sdf_poly_95_all_h)
df_95_all_h$bird_id <- df_95_all_h$id


# Polygone spatial 50%
ud_50_all_h <- lapply(kdh_all_h, function(x) try(getverticeshr(x, 50)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_50_all_h), function(i) {
  row.names(ud_50_all_h[[i]]) <<- kd_names_all_h[i]
})
sdf_poly_50_all_h <- Reduce(rbind, ud_50_all_h)
df_50_all_h <- fortify(sdf_poly_50_all_h)
df_50_all_h$bird_id <- df_50_all_h$id


# Polygone spatial 30%
ud_30_all_h <- lapply(kdh_all_h, function(x) try(getverticeshr(x, 30)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_30_all_h), function(i) {
  row.names(ud_30_all_h[[i]]) <<- kd_names_all_h[i]
})
sdf_poly_30_all_h <- Reduce(rbind, ud_30_all_h)
df_30_all_h <- fortify(sdf_poly_30_all_h)
df_30_all_h$bird_id <- df_30_all_h$id


#install.packages("ggspatial")
library(ggplot2)
library(ggspatial)



#Vu generale des kernel depali + disp departement NPDC
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
##gg <- gg + geom_sf(data = COUCHE_LAGON_BLEU,aes(fill=habitat), colour=NA, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_all_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_all_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_all_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = data_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/All/kernel_NPDC_href.png",gg, width = 25, height = 13)


#Vu generale des kernel depali + disp region HDF
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = HDF, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_all_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_all_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_all_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = data_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/All/kernel_HDF_href.png",gg, width = 25, height = 13)


#vu par HGD des kernel departement NPDC
gg <- ggplot()  + theme_bw() + facet_wrap(.~bird_id)
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_all_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_all_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_all_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = data_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg

ggsave("Rplot/Kernel/All/kernel_NPDC_bird_href.png",gg, width = 25, height = 13)

##### DV dependance alimentaire (Kernel) ############################################################################################################################
HGD_Depali <- filter(data_HGD,periode == "Dependance alimentaire")

#Creation DV kernel Depali
Depali_HGD_sf <- st_as_sf(HGD_Depali, coords = c("Longitude","Latitude"))
st_crs(Depali_HGD_sf) <- 4326
Depali_HGD_sf <- st_transform(Depali_HGD_sf,crs=3832)
Depali_sf_NPDC <- Depali_HGD_sf[,c("bird_id")]
Depali_sf_NPDC <- st_crop(Depali_sf_NPDC,st_bbox(NPDC))
Depali_sf_NPDC_k <- as(Depali_sf_NPDC,'Spatial')

kdh_Depali_h <- kernelUD(Depali_sf_NPDC_k, h="href", grid = 1000)
image(kdh_Depali_h)


# creating SpatialPolygonsDataFrame
# Polygone spatial 95%
kd_names_Depali_h <- names(kdh_Depali_h)
ud_95_Depali_h <- lapply(kdh_Depali_h, function(x) try(getverticeshr(x, 95)))


sapply(1:length(ud_95_Depali_h), function(i) {
  row.names(ud_95_Depali_h[[i]]) <<- kd_names_Depali_h[i]
})
sdf_poly_95_Depali_h <- Reduce(rbind, ud_95_Depali_h)
df_95_Depali_h <- fortify(sdf_poly_95_Depali_h)
df_95_Depali_h$bird_id <- df_95_Depali_h$id


# Polygone spatial 50%
ud_50_Depali_h <- lapply(kdh_Depali_h, function(x) try(getverticeshr(x, 50)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_50_Depali_h), function(i) {
  row.names(ud_50_Depali_h[[i]]) <<- kd_names_Depali_h[i]
})
sdf_poly_50_Depali_h <- Reduce(rbind, ud_50_Depali_h)
df_50_Depali_h <- fortify(sdf_poly_50_Depali_h)
df_50_Depali_h$bird_id <- df_50_Depali_h$id


# Polygone spatial 30%
ud_30_Depali_h <- lapply(kdh_Depali_h, function(x) try(getverticeshr(x, 30)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_30_Depali_h), function(i) {
  row.names(ud_30_Depali_h[[i]]) <<- kd_names_Depali_h[i]
})
sdf_poly_30_Depali_h <- Reduce(rbind, ud_30_Depali_h)
df_30_Depali_h <- fortify(sdf_poly_30_Depali_h)
df_30_Depali_h$bird_id <- df_30_Depali_h$id


#Vu generale des kernel depali departement NPDC
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
##gg <- gg + geom_sf(data = COUCHE_LAGON_BLEU,aes(fill=habitat), colour=NA, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Depali_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Depali_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Depali_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Depali_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Depali/kernel_NPDC_href.png",gg, width = 25, height = 13)


#Vu generale des kernel depali region HDF
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = HDF, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Depali_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Depali_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Depali_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Depali_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Depali/kernel_HDF_href.png",gg, width = 25, height = 13)


#vu par HGD des kernel departement NPDC
gg <- ggplot()  + theme_bw() + facet_wrap(.~bird_id)
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Depali_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Depali_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Depali_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Depali_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Depali/kernel_NPDC_bird_href.png",gg, width = 25, height = 13)



##### DV dispersion (Kernel)############################################################################################################################
HGD_Disp <- filter(data_HGD,periode == "Dispersion")

#Creation DV kernel Depali
Disp_HGD_sf <- st_as_sf(HGD_Disp, coords = c("Longitude","Latitude"))
st_crs(Disp_HGD_sf) <- 4326
Disp_HGD_sf <- st_transform(Disp_HGD_sf,crs=3832)
Disp_sf_NPDC <- Disp_HGD_sf[,c("bird_id")]
Disp_sf_NPDC <- st_crop(Disp_sf_NPDC,st_bbox(NPDC))
Disp_sf_NPDC_k <- as(Disp_sf_NPDC,'Spatial')

kdh_Disp_h <- kernelUD(Disp_sf_NPDC_k, h="href", grid = 2000, extent = 100)#très long
image(kdh_Disp_h)


# creating SpatialPolygonsDataFrame
# Polygone spatial 95%
kd_names_Disp_h <- names(kdh_Disp_h)
ud_95_Disp_h <- lapply(kdh_Disp_h, function(x) try(getverticeshr(x, 95)))


sapply(1:length(ud_95_Disp_h), function(i) {
  row.names(ud_95_Disp_h[[i]]) <<- kd_names_Disp_h[i]
})
sdf_poly_95_Disp_h <- Reduce(rbind, ud_95_Disp_h)
df_95_Disp_h <- fortify(sdf_poly_95_Disp_h)
df_95_Disp_h$bird_id <- df_95_Disp_h$id


# Polygone spatial 50%
ud_50_Disp_h <- lapply(kdh_Disp_h, function(x) try(getverticeshr(x, 50)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_50_Disp_h), function(i) {
  row.names(ud_50_Disp_h[[i]]) <<- kd_names_Disp_h[i]
})
sdf_poly_50_Disp_h <- Reduce(rbind, ud_50_Disp_h)
df_50_Disp_h <- fortify(sdf_poly_50_Disp_h)
df_50_Disp_h$bird_id <- df_50_Disp_h$id


# Polygone spatial 30%
ud_30_Disp_h <- lapply(kdh_Disp_h, function(x) try(getverticeshr(x, 30)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_30_Disp_h), function(i) {
  row.names(ud_30_Disp_h[[i]]) <<- kd_names_Disp_h[i]
})
sdf_poly_30_Disp_h <- Reduce(rbind, ud_30_Disp_h)
df_30_Disp_h <- fortify(sdf_poly_30_Disp_h)
df_30_Disp_h$bird_id <- df_30_Disp_h$id


#Vu generale des kernel depali departement NPDC
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
##gg <- gg + geom_sf(data = COUCHE_LAGON_BLEU,aes(fill=habitat), colour=NA, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Disp_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Disp_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Disp/kernel_NPDC_href.png",gg, width = 25, height = 13)


#Vu generale des kernel depali region HDF
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = HDF, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Disp_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Disp_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Disp/kernel_HDF_href.png",gg, width = 25, height = 13)


#vu par HGD des kernel departement NPDC
gg <- ggplot()  + theme_bw() + facet_wrap(.~bird_id)
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Disp_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Disp_h, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Disp/kernel_NPDC_bird_href.png",gg, width = 25, height = 13)


### Domaine vitaux sous Kernel LSCV ##################################################################################################

##### DV all (Kernel) ############################################################################################################################
#install.packages("adehabitatHR")
library(adehabitatHR)
library(sf)
library(ggplot2)

# Fond de carte departement Nord Pas-de-Calais
NPDC <- st_read("SIG/Departement NPDC.shp")
NPDC <- st_make_valid(NPDC)
NPDC <- st_transform(NPDC,crs=3832)
NPDC <- st_make_valid(NPDC)

# Fond de carte departement region Hauts-de-France
HDF <- st_read("SIG/Departement HDF.shp")
HDF <- st_make_valid(HDF)
HDF <- st_transform(HDF,crs=3832)
HDF <- st_make_valid(HDF)

#Creation DV kernel all
data_HGD_sf <- st_as_sf(data_HGD, coords = c("Longitude","Latitude"))
st_crs(data_HGD_sf) <- 4326
data_HGD_sf <- st_transform(data_HGD_sf,crs=3832)
HGD_sf_NPDC <- data_HGD_sf[,c("bird_id")]
HGD_sf_NPDC <- st_crop(HGD_sf_NPDC,st_bbox(NPDC))
HGD_sf_NPDC_k <- as(HGD_sf_NPDC,'Spatial')

kdh_all_l <- kernelUD(HGD_sf_NPDC_k, h="LSCV", grid = 1000)
image(kdh_all_l)


# creating SpatialPolygonsDataFrame
# Polygone spatial 95%
kd_names_all_l <- names(kdh_all_l)
ud_95_all_l <- lapply(kdh_all_l, function(x) try(getverticeshr(x, 95)))


sapply(1:length(ud_95_all_l), function(i) {
  row.names(ud_95_all_l[[i]]) <<- kd_names_all_l[i]
})
sdf_poly_95_all_l <- Reduce(rbind, ud_95_all_l)
df_95_all_l <- fortify(sdf_poly_95_all_l)
df_95_all_l$bird_id <- df_95_all_l$id


# Polygone spatial 50%
ud_50_all_l <- lapply(kdh_all_l, function(x) try(getverticeshr(x, 50)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_50_all_l), function(i) {
  row.names(ud_50_all_l[[i]]) <<- kd_names_all_l[i]
})
sdf_poly_50_all_l <- Reduce(rbind, ud_50_all_l)
df_50_all_l <- fortify(sdf_poly_50_all_l)
df_50_all_l$bird_id <- df_50_all_l$id


# Polygone spatial 30%
ud_30_all_l <- lapply(kdh_all_l, function(x) try(getverticeshr(x, 30)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_30_all_l), function(i) {
  row.names(ud_30_all_l[[i]]) <<- kd_names_all_l[i]
})
sdf_poly_30_all_l <- Reduce(rbind, ud_30_all_l)
df_30_all_l <- fortify(sdf_poly_30_all_l)
df_30_all_l$bird_id <- df_30_all_l$id


#install.packages("ggspatial")
library(ggplot2)
library(ggspatial)



#Vu generale des kernel depali + disp departement NPDC
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
##gg <- gg + geom_sf(data = COUCHE_LAGON_BLEU,aes(fill=habitat), colour=NA, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_all_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_all_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_all_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = data_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/All/kernel_NPDC_LSCV.png",gg, width = 25, height = 13)


#Vu generale des kernel depali + disp region HDF
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = HDF, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_all_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_all_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_all_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = data_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/All/kernel_HDF_LSCV.png",gg, width = 25, height = 13)


#vu par HGD des kernel departement NPDC
gg <- ggplot()  + theme_bw() + facet_wrap(.~bird_id)
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_all_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_all_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_all_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = data_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg

ggsave("Rplot/Kernel/All/kernel_NPDC_bird_LSCV.png",gg, width = 25, height = 13)

##### DV dependance alimentaire (Kernel) ############################################################################################################################
HGD_Depali <- filter(data_HGD,periode == "Dependance alimentaire")

#Creation DV kernel Depali
Depali_HGD_sf <- st_as_sf(HGD_Depali, coords = c("Longitude","Latitude"))
st_crs(Depali_HGD_sf) <- 4326
Depali_HGD_sf <- st_transform(Depali_HGD_sf,crs=3832)
Depali_sf_NPDC <- Depali_HGD_sf[,c("bird_id")]
Depali_sf_NPDC <- st_crop(Depali_sf_NPDC,st_bbox(NPDC))
Depali_sf_NPDC_k <- as(Depali_sf_NPDC,'Spatial')

kdh_Depali_l <- kernelUD(Depali_sf_NPDC_k, h="LSCV", grid = 1000)
image(kdh_Depali_l)


# creating SpatialPolygonsDataFrame
# Polygone spatial 95%
kd_names_Depali_l <- names(kdh_Depali_l)
ud_95_Depali_l <- lapply(kdh_Depali_l, function(x) try(getverticeshr(x, 95)))


sapply(1:length(ud_95_Depali_l), function(i) {
  row.names(ud_95_Depali_l[[i]]) <<- kd_names_Depali_l[i]
})
sdf_poly_95_Depali_l <- Reduce(rbind, ud_95_Depali_l)
df_95_Depali_l <- fortify(sdf_poly_95_Depali_l)
df_95_Depali_l$bird_id <- df_95_Depali_l$id


# Polygone spatial 50%
ud_50_Depali_l <- lapply(kdh_Depali_l, function(x) try(getverticeshr(x, 50)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_50_Depali_l), function(i) {
  row.names(ud_50_Depali_l[[i]]) <<- kd_names_Depali_l[i]
})
sdf_poly_50_Depali_l <- Reduce(rbind, ud_50_Depali_l)
df_50_Depali_l <- fortify(sdf_poly_50_Depali_l)
df_50_Depali_l$bird_id <- df_50_Depali_l$id


# Polygone spatial 30%
ud_30_Depali_l <- lapply(kdh_Depali_l, function(x) try(getverticeshr(x, 30)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_30_Depali_l), function(i) {
  row.names(ud_30_Depali_l[[i]]) <<- kd_names_Depali_l[i]
})
sdf_poly_30_Depali_l <- Reduce(rbind, ud_30_Depali_l)
df_30_Depali_l <- fortify(sdf_poly_30_Depali_l)
df_30_Depali_l$bird_id <- df_30_Depali_l$id


#Vu generale des kernel depali departement NPDC
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
##gg <- gg + geom_sf(data = COUCHE_LAGON_BLEU,aes(fill=habitat), colour=NA, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Depali_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Depali_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Depali_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Depali_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Depali/kernel_NPDC_LSCV.png",gg, width = 25, height = 13)


#Vu generale des kernel depali region HDF
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = HDF, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Depali_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Depali_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Depali_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Depali_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Depali/kernel_HDF_LSCV.png",gg, width = 25, height = 13)


#vu par HGD des kernel departement NPDC
gg <- ggplot()  + theme_bw() + facet_wrap(.~bird_id)
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Depali_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Depali_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Depali_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Depali_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Depali/kernel_NPDC_bird_LSCV.png",gg, width = 25, height = 13)



##### DV dispersion (Kernel)############################################################################################################################
HGD_Disp <- filter(data_HGD,periode == "Dispersion")

#Creation DV kernel Depali
Disp_HGD_sf <- st_as_sf(HGD_Disp, coords = c("Longitude","Latitude"))
st_crs(Disp_HGD_sf) <- 4326
Disp_HGD_sf <- st_transform(Disp_HGD_sf,crs=3832)
Disp_sf_NPDC <- Disp_HGD_sf[,c("bird_id")]
Disp_sf_NPDC <- st_crop(Disp_sf_NPDC,st_bbox(NPDC))
Disp_sf_NPDC_k <- as(Disp_sf_NPDC,'Spatial')

kdh_Disp_l <- kernelUD(Disp_sf_NPDC_k, h="LSCV", grid = 1000)
image(kdh_Disp_l)


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


# Polygone spatial 50%
ud_50_Disp_l <- lapply(kdh_Disp_l, function(x) try(getverticeshr(x, 50)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_50_Disp_l), function(i) {
  row.names(ud_50_Disp_l[[i]]) <<- kd_names_Disp_l[i]
})
sdf_poly_50_Disp_l <- Reduce(rbind, ud_50_Disp_l)
df_50_Disp_l <- fortify(sdf_poly_50_Disp_l)
df_50_Disp_l$bird_id <- df_50_Disp_l$id


# Polygone spatial 30%
ud_30_Disp_l <- lapply(kdh_Disp_l, function(x) try(getverticeshr(x, 30)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud_30_Disp_l), function(i) {
  row.names(ud_30_Disp_l[[i]]) <<- kd_names_Disp_l[i]
})
sdf_poly_30_Disp_l <- Reduce(rbind, ud_30_Disp_l)
df_30_Disp_l <- fortify(sdf_poly_30_Disp_l)
df_30_Disp_l$bird_id <- df_30_Disp_l$id


#Vu generale des kernel depali departement NPDC
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
##gg <- gg + geom_sf(data = COUCHE_LAGON_BLEU,aes(fill=habitat), colour=NA, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Disp_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Disp_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Disp/kernel_NPDC_LSCV.png",gg, width = 25, height = 13)


#Vu generale des kernel depali region HDF
gg <- ggplot()  + theme_bw()
gg <- gg + geom_sf(data = HDF, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Disp_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Disp_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="Birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Disp/kernel_HDF_LSCV.png",gg, width = 25, height = 13)


#vu par HGD des kernel departement NPDC
gg <- ggplot()  + theme_bw() + facet_wrap(.~bird_id)
gg <- gg + geom_sf(data = NPDC, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95_Disp_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50_Disp_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30_Disp_l, aes(x = long, y = lat, color = bird_id, group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = Disp_HGD_sf,aes(group=bird_id,colour= bird_id),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",colour="birds",title="Kernel 95%, 50% et 30%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
#gg <- gg + scale_fill_manual(values=vec_colour)
gg
ggsave("Rplot/Kernel/Disp/kernel_NPDC_bird_LSCV.png",gg, width = 25, height = 13)



















gg <- ggplot()  + theme_bw() + facet_wrap(.~bird_id)
##gg <- gg + geom_sf(data = COUCHE_LAGON_BLEU,aes(fill=habitat), colour=NA, size=0.2, alpha=.5)
gg <- gg +   geom_polygon(data = df_95, aes(x = long, y = lat, color = "red", group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_50, aes(x = long, y = lat, color = "orange", group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg +   geom_polygon(data = df_30, aes(x = long, y = lat, color = "yellow", group = group),linewidth =1.2,fill=NA,alpha = 1)
gg <- gg + geom_sf(data = data_HGD_sf,aes(group=bird_id,colour=""),linewidth =0.8) #+ geom_path(data=dd,aes(x=X,y=Y,group=bird_id,colour= bird_id),alpha=0.2,size=0.5)
gg <- gg + annotation_scale()
gg <- gg + labs(x="",y="",colour="birds",title="Kernel 95% and 50%")
#gg <- gg + coord_sf(xlim = c(7284148,7288089), ylim = c( -1673693, -1671352))
gg <- gg + scale_fill_manual(values=vec_colour)
gg