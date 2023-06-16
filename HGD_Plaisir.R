





# Distance entre deux points ##########################################################################################################
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







# Polygone convex en periode de depali ##################################################################################################

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



df = HGD_Bachant
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




#MCP Bachant
HGD_Bachant <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Bachant <- filter(HGD_Bachant,bird_id == "Bachant Malakoff")
df = HGD_Bachant
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth = 0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Bachant")
gg
ggsave("Rplot/MCP_Depali/MCP_Bachant.png",gg, width = 15, height = 10)



#MCP Quelmes
HGD_Quelmes <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Quelmes <- filter(HGD_Quelmes,bird_id == "Quelmes")
df = HGD_Quelmes
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth =0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Quelmes")
gg
ggsave("Rplot/MCP_Depali/MCP_Quelmes.png",gg, width = 15, height = 10)



#MCP Custodelle
HGD_Custodelle <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Custodelle <- filter(HGD_Custodelle,bird_id == "Custodelle")
df = HGD_Custodelle
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth =0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Custodelle")
gg
ggsave("Rplot/MCP_Depali/MCP_Custodelle.png",gg, width = 15, height = 10)



#MCP Germignies A
HGD_Germignies_A <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Germignies_A <- filter(HGD_Germignies_A,bird_id == "Germignies A")
df = HGD_Germignies_A
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth =0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Germignies A")
gg
ggsave("Rplot/MCP_Depali/MCP_Germignies_A.png",gg, width = 15, height = 10)


#MCP Germignies B
HGD_Germignies_B <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Germignies_B <- filter(HGD_Germignies_B,bird_id == "Germignies B")
df = HGD_Germignies_B
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth =0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Germignies B")
gg
ggsave("Rplot/MCP_Depali/MCP_Germignies_B.png",gg, width = 15, height = 10)



#MCP Glageon Bocahut
HGD_Glageon_Bocahut <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Glageon_Bocahut <- filter(HGD_Glageon_Bocahut,bird_id == "Glageon Bocahut")
df = HGD_Glageon_Bocahut
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth =0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Glageon Bocahut")
gg
ggsave("Rplot/MCP_Depali/MCP_Glageon_Bocahut.png",gg, width = 15, height = 10)



#MCP Guemy
HGD_Guemy <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Guemy <- filter(HGD_Guemy,bird_id == "Guemy")
df = HGD_Guemy
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth =0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Guemy")
gg
ggsave("Rplot/MCP_Depali/MCP_Guemy.png",gg, width = 15, height = 10)



#MCP Licques
HGD_Licques <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Licques <- filter(HGD_Licques,bird_id == "Licques")
df = HGD_Licques
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth =0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Licques")
gg
ggsave("Rplot/MCP_Depali/MCP_Licques.png",gg, width = 15, height = 10)


#MCP Loos_en_Gohelle_74A
HGD_Loos_en_Gohelle_74A <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Loos_en_Gohelle_74A <- filter(HGD_Loos_en_Gohelle_74A,bird_id == "Loos-en-Gohelle 74A")
df = HGD_Loos_en_Gohelle_74A
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth =0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Loos-en-Gohelle 74A")
gg
ggsave("Rplot/MCP_Depali/MCP_Loos_en_Gohelle_74A.png",gg, width = 15, height = 10)


#MCP Loos_en_Gohelle_74B
HGD_Loos_en_Gohelle_74B <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Loos_en_Gohelle_74B <- filter(HGD_Loos_en_Gohelle_74B,bird_id == "Loos-en-Gohelle 74B")
df = HGD_Loos_en_Gohelle_74B
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth =0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Loos-en-Gohelle 74B")
gg
ggsave("Rplot/MCP_Depali/MCP_Loos_en_Gohelle_74B.png",gg, width = 15, height = 10)


#MCP Nabringhen
HGD_Nabringhen <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Nabringhen <- filter(HGD_Nabringhen,bird_id == "Nabringhen")
df = HGD_Nabringhen
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth =0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Nabringhen")
gg
ggsave("Rplot/MCP_Depali/MCP_Nabringhen.png",gg, width = 15, height = 10)


#MCP Wallers
HGD_Wallers <- filter(HGD_DT,periode == "Dependance alimentaire")
HGD_Wallers <- filter(HGD_Wallers,bird_id == "Wallers")
df = HGD_Wallers
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch

gg <- ggplot(df, aes(Longitude, Latitude, colour = Quinzaine))
gg <- gg + geom_point()
gg <- gg + geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2, linewidth =0.5)
gg <- gg + annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm"))
gg <- gg + labs(x="",y="",title="CMP Wallers")
gg
ggsave("Rplot/MCP_Depali/MCP_Wallers.png",gg, width = 15, height = 10)



#test all oiseaux depali
df = HGD_Depali
# Plot without convex hulls
p = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point()
p
# Computing convex hulls
s = df %>%
  split(df$Quinzaine)  # Tranform data.frame in lists of data.frames depending on column 'bird_id'

ch = s %>%
  # Compute which points are on the convex hull of each data.frame /!\ Check how 'chull' works
  lapply(., function(el) chull(el$Longitude, el$Latitude))  # 'ch' now contains the row numbers of points on convex hull per sub-data.frame

# Get points for each sub-data.frame using names index
ch = lapply(names(ch), function(x) s[[x]][ch[[x]],]) %>%   
  do.call(rbind, .)  # Join all convex hull points in a single data.frame

# Plot with convex hulls
p.ch = ggplot(df, aes(Longitude, Latitude, colour = Quinzaine)) +
  geom_point() +
  facet_wrap(.~bird_id) +
  geom_polygon(data = ch, aes(fill = Quinzaine), alpha = 0.2)
p.ch






##### DV all (Kernel) ############################################################################################################################
#install.packages("adehabitatHR")
library(adehabitatHR)
library(sf)
library(ggplot2)
library(ggspatial)

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

#Creation DV kernel Disp
Disp_HGD_sf <- st_as_sf(HGD_Disp, coords = c("Longitude","Latitude"))
st_crs(Disp_HGD_sf) <- 4326
Disp_HGD_sf <- st_transform(Disp_HGD_sf,crs=3832)
Disp_sf_NPDC <- Disp_HGD_sf[,c("bird_id")]
Disp_sf_NPDC <- st_crop(Disp_sf_NPDC,st_bbox(NPDC))
Disp_sf_NPDC_k <- as(Disp_sf_NPDC,'Spatial')

kdh_Disp_h <- kernelUD(Disp_sf_NPDC_k, h="href", grid = 1000, extent = 100)#marche pas
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


#Vu generale des kernel Disp departement NPDC
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


#Vu generale des kernel Disp region HDF
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
library(sf)



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