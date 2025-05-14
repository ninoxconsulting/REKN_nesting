#libraries
library(dplyr)
library(fs)
library(readr)
library(sf)
library(lubridate)

# read in the datasets 
# pre 2023


b23 <- st_read(path("temp", "birds_2023.gpkg"))  |> 
  select(-algorithm.marked.outlier, -date_time) 


# 2024/25

b24 <- st_read(path("temp", "birds_2024.gpkg")) |> 
  select(-algorithm.marked.outlier, -date_time)


all <- bind_rows(b24, b23) |> 
  select(-c(stopover, movement_temp, id, bearing, gcd_m, speed_mhr,
            deploy.on.latitude, deploy.on.longitude,tag.manufacturer.name,
            tag.id.order, animal.id,individual.local.identifier,
            deploy.on.date,diff, location.long_prior, location.lat_prior))


# check the number of individuals
length(unique(all$tag.id))

# 95 tags showed arrival at breding grounds 

# list of birds with the summary of movement patterns
reknbr <- read_csv(path("data", "final_tags_list_edited.csv")) |> 
  filter(breeding =="y") |> 
  filter(subspecies == "rufa")

reknbr_full <- reknbr |> 
  filter(south == "y")
  
  
# select the individuals with breeding locations 
breed <- all |> filter(tag.id %in% reknbr_full$tag.id) 
length(unique(breed$tag.id))

# 37 tags which had a full breeding season in the arctic 

breed <- breed |> 
  mutate(tag.model1 = case_when(
    tag.model == "sunbird"~ "Sunbird Solar Argos", 
    tag.model == "Sunbird"~ "Sunbird Solar Argos", 
    .default = tag.model))


aa<- breed |> 
  mutate(date = make_date(year, month, day)) 


st_write(breed, path("temp", "full_breeding_ids.gpkg"), delete_dsn = TRUE)



#############################################################################

# 95 tags total 
# 37 tags with full breeding season and return south 


## subset Breeding locations 

# filter the date based on estimate of dates 

bb <- aa |> 
 mutate(breeding = case_when(
   tag.id == "213834" & date >= "2021-06-20" & date <= "2021-07-12" ~ "y",
   tag.id == "213836" & date >= "2021-06-14" & date <= "2021-07-09" ~ "y",
   tag.id == "213838" & date >= "2021-06-17" & date <= "2021-07-15" ~ "y",
   tag.id == "228166" & date >= "2023-06-08" & date <= "2023-07-19" ~ "y",
   tag.id == "230310" & date >= "2022-06-13" & date <= "2022-07-12" ~ "y",
   tag.id == "230317" & date >= "2022-06-10" & date <= "2022-07-03" ~ "y",
   tag.id == "234233" & date >= "2022-06-19" & date <= "2022-07-03" ~ "y",
   tag.id == "236916" & date >= "2023-06-12" & date <= "2023-07-09" ~ "y",
   tag.id == "242570" & date >= "2023-06-15" & date <= "2023-07-26" ~ "y",
   tag.id == "242577" & date >= "2023-06-17" & date <= "2023-06-26" ~ "y",
   tag.id == "242580" & date >= "2023-06-05" & date <= "2023-07-30" ~ "y",
   tag.id == "242583" & date >= "2023-06-13" & date <= "2023-07-26" ~ "y",
   tag.id == "242656" & date >= "2023-06-10" & date <= "2023-08-03" ~ "y",
   tag.id == "242657" & date >= "2023-06-09" & date <= "2023-08-01" ~ "y",
   tag.id == "242658" & date >= "2023-06-16" & date <= "2023-08-10" ~ "y",
   tag.id == "260688" & date >= "2024-06-12" & date <= "2024-07-11" ~ "y",
   tag.id == "260689" & date >= "2024-06-06" & date <= "2024-07-18" ~ "y",
   tag.id == "260690" & date >= "2024-06-19" & date <= "2024-07-16" ~ "y",
   tag.id == "260692" & date >= "2024-06-11" & date <= "2024-07-21" ~ "y",
   tag.id == "260693" & date >= "2024-06-13" & date <= "2024-07-15" ~ "y",
   tag.id == "260694" & date >= "2024-06-07" & date <= "2024-07-13" ~ "y",
   tag.id == "260696" & date >= "2024-06-20" & date <= "2024-07-15" ~ "y",
   tag.id == "260697" & date >= "2024-06-15" & date <= "2024-08-07" ~ "y",
   tag.id == "260698" & date >= "2024-06-12" & date <= "2024-07-10" ~ "y",
   tag.id == "260803" & date >= "2024-06-05" & date <= "2024-07-08" ~ "y",
   tag.id == "241167" & date >= "2023-06-21" & date <= "2023-07-16" ~ "y",
   tag.id == "232981" & date >= "2022-06-07" & date <= "2022-07-10" ~ "y",
   tag.id == "232982" & date >= "2023-06-12" & date <= "2023-07-12" ~ "y",
   tag.id == "232984" & date >= "2022-06-19" & date <= "2022-07-17" ~ "y",
   tag.id == "232985" & date >= "2022-06-17" & date <= "2022-07-22" ~ "y",
   tag.id == "238544" & date >= "2023-06-07" & date <= "2023-08-06" ~ "y",
   tag.id == "238546" & date >= "2023-06-10" & date <= "2023-07-10" ~ "y",
   tag.id == "240161" & date >= "2023-06-15" & date <= "2023-07-20" ~ "y",
   tag.id == "240164" & date >= "2023-06-29" & date <= "2023-07-21" ~ "y",
   tag.id == "240167" & date >= "2023-06-23" & date <= "2023-07-26" ~ "y",
   tag.id == "240168" & date >= "2023-06-06" & date <= "2023-08-05" ~ "y",
   tag.id == "240170" & date >= "2024-06-21" & date <= "2024-07-18" ~ "y",
   tag.id == "241167" & date >= "2023-06-21" & date <= "2023-07-16" ~ "y",
   .default = NA
 ))


st_write(bb, path("temp", "full_breeding_ids_label.gpkg"), delete_dsn = TRUE)


###############################

## generate centroid as plot example


bb <- bb |> filter(breeding == 'y')

bc <- bb |> 
  group_by(tag.id) |> 
  summarise(centroid = st_centroid(st_union(geom))) |> 
  st_as_sf() |> 
  st_transform(crs = 4326) |> 
  select(tag.id, centroid)


st_write(bc, path("temp", "centroid_breeding.gpkg"), delete_dsn = TRUE)



# plot the centroids for context 


###################################################

# individual plots 


bb <- st_read(path("temp", "full_breeding_ids_label.gpkg"), quiet = TRUE) |> 
  filter(breeding == "y") |> 
  filter(tag.id == 260803) 

bb <- cbind(st_coordinates(bb), bb)
  
st_write(bb, path("temp", "tagid_260803.gpkg") )


# plot 
world <- ne_countries(scale = "medium", returnclass = "sf")

Americas <- world %>% dplyr::filter(continent == "North America")

# entire north America 
global_north <- ggplot(data = Americas) +
  geom_sf(color = "grey") +
  geom_point(data = bb, aes(x = X, y = Y, colour = argos.lc), size = 2) +#colour = "dark blue") +
  scale_color_viridis_d() + 
  #facet_wrap(~tag.id)+
  # geom_point(ru, aes(x = lng, y = lat), size = 4) +
  # xlab("Longitude") + ylab("Latitude") +
  #coord_sf(xlim = c(-130, -20), ylim = c(-50, 80), expand = FALSE)+
  coord_sf(xlim = c(-89, -81), ylim = c(62.5, 65), expand = FALSE)+
  theme_bw()+
  #labs(colour = "Subpopulation") + 
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    #legend.position="none"
    legend.position = "bottom",
    legend.key.width = unit(3, "lines")
  )

global_north

#



## Kernal density estimates 

library(sf)
library(mapview)
library(adehabitatHR)
library(sp)
library(dplyr)
library(ggplot2)


tdfgeo <- bb |>  
  filter(argos.lc %in% c(2,3)) |> 
  dplyr::select(tag.id) |>
  as("Spatial")

## 3.1 kde: h reference parameter 
# define the parameters (h, kern, grid, extent) 

kde_href  <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)

kde_href

ver95 <- getverticeshr(kde_href,95) # get vertices for home range
ver95_sf <- st_as_sf(ver95)         # convert to sf object 

ver75 <- getverticeshr(kde_href,75)
ver75_sf <- st_as_sf(ver75 )

ver50 <- getverticeshr(kde_href,50)
ver50_sf<- st_as_sf(ver50)

# plot the outputs 
mapview(ver50_sf, zcol = "id") 
mapview (ver75_sf, zcol = "id") 
mapview (ver95_sf, zcol = "id") 


## 3.2 kde: Least Squares Cross Validation (lscv) method.

kde_lscv  <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"), grid = 500, extent = 2)

ver95ls <- getverticeshr(kde_lscv,95) # get vertices for home range
ver95ls_sf <- st_as_sf(ver95ls) 

ver50ls <- getverticeshr(kde_lscv,50)
ver50ls_sf <- st_as_sf(ver50ls) 

# plot the outputs 
mapview(ver50ls_sf, zcol = "id") 



## 3.3 kde: variable smoothing parameters (h)
kde_h1000  <- kernelUD(tdfgeo, h = 1000, kern = c("bivnorm"), grid = 500,extent = 10000)
kde_h500  <- kernelUD(tdfgeo, h = 500, kern = c("bivnorm"), grid = 500,extent = 10)
kde_h3000  <- kernelUD(tdfgeo, h = 3000, kern = c("bivnorm"), grid = 500,extent = 5)
kde_h5000  <- kernelUD(tdfgeo, h = 5000, kern = c("bivnorm"), grid = 500,extent = 5)


# kde - href = 1000
ver95_1000 <- getverticeshr(kde_h1000, 95) # get vertices for home range
ver95_1000_sf <- st_as_sf(ver95_1000) |>     mutate(h = 1000) # convert to sf object 

# kde - href = 500
ver95_500 <- getverticeshr(kde_h500, 95) # get vertices for home range
ver95_500_sf <- st_as_sf(ver95_500)  |> 
  mutate(h = 500) # convert to sf object 

# kde - href = 3000
ver95_5000 <- getverticeshr(kde_h5000, 95) # get vertices for home range
ver95_3000_sf <- st_as_sf(ver95_3000)  |> 
  mutate(h = 3000) # convert to sf object 

# bind all data together 
all_verts <- bind_rows(ver95_1000_sf,  ver95_500_sf,  ver95_3000_sf)

# lets plot the output 
ggplot(data = all_verts) +
  geom_sf(
    aes(colour = id), 
    alpha = 0.1
  ) + 
  scale_colour_viridis_d() + 
  facet_wrap(vars(h)) +
  theme_bw()



##################################################################
# recurse packages 
#https://cran.r-project.org/web/packages/recurse/vignettes/recurse.html

require(recurse)
require(scales)
library(dplyr)
library(sf)
library(fs)

data(martin)
plot(martin$x, martin$y, col = viridis_pal()(nrow(martin)), pch = 20, 
     xlab = "x", ylab = "y", asp = 1)

martinvisit = getRecursions(martin, 2) 

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(martinvisit, martin, legendPos = c(13, -10))

drawCircle(-15, -10, 2)

hist(martinvisit$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 2)")
summary(martinvisit$revisits)

data(wren)
animals = rbind(martin, wren)
plot(animals$x, animals$y, col = c("red", "darkblue")[as.numeric(animals$id)], 
     pch = ".", xlab = "x", ylab = "y", asp = 1)
popvisit = getRecursions(animals, 2) 

head(popvisit$revisitStats)
plot(popvisit, animals, legendPos = c(15, -10))

visitThreshold = quantile(popvisit$revisits, 0.8)
popCluster = kmeans(animals[popvisit$revisits > visitThreshold,c("x", "y")], centers = 3)

plot(animals$x, animals$y, col = c("red", "darkblue")[as.numeric(animals$id)], 
     pch = ".", xlab = "x", ylab = "y", asp = 1)

with(animals[popvisit$revisits > visitThreshold,],
     points(x, y, col = c(alpha("red", 0.5), alpha("darkblue", 0.5))[as.numeric(id)], 
            pch = c(15:17)[popCluster$cluster]) )
legend("topleft", pch = 15:17, legend = paste("cluster", 1:3), bty = "n")


## actual data analysis

bb <- st_read(path("temp", "full_breeding_ids_label.gpkg"), quiet = TRUE) |> 
  filter(breeding == "y") |> 
  filter(tag.id == 260803) |> 
  st_transform(bb, crs = 3573)

bb <- cbind(st_coordinates(bb), bb)

bbsp <- bb |>  
  sf::st_drop_geometry() |>
  dplyr::filter(argos.lc %in% c(2,3)) |> 
  dplyr::select(X,Y, timestamp, tag.id) |> 
  dplyr::mutate(t = lubridate::ymd_hms(timestamp)) |> 
  dplyr::select(-timestamp) |> 
  mutate(tag.id = as.character(tag.id)) |> 
  select(X, Y, t, tag.id) 

plot(bbsp$X, bbsp$Y, col = viridis_pal()(nrow(bbsp)), pch = 20, 
     xlab = "x", ylab = "y", asp = 1)

bbvisit = getRecursions(bbsp, 1500) 

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(bbvisit, bbsp, legendPos = c(13, -10))

drawCircle(-15, -10, 2)

hist(bbvisit$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 1500m)")
summary(bbvisit$revisits)

head(bbvisit$revisitStats)


visitThreshold = quantile(bbvisit$revisits, 0.8)
popCluster = kmeans(bbsp[bbvisit$revisits > visitThreshold,c("X", "Y")], centers =3)



plot(bbsp$X, bbsp$Y, #col = c("red", "darkblue")[as.numeric(bbsp$tag.id)],
     pch = ".", xlab = "x", ylab = "y", asp = 1)
with(bbsp[bbvisit$revisits > visitThreshold,],
     points(X, Y, col = alpha("red", 0.5), #c(alpha("red", 0.5), alpha("darkblue", 0.5))[as.numeric(bbsp$tag.id)], 
            pch = c(15:17)[popCluster$cluster]) )
legend("topleft", pch = 15:17, legend = paste("cluster", 1:3), bty = "n")


















### 2023 dataset 

# subset the nesting time locations 

# 213834 
#2021-06-20 - 2021-07-12

# 213836
#2021-06-14 - 2021-07-09

# 213838
#2021-06-17 - 2021-07-15

# 228166 # newstead 
#2023-06-08 - 2023-07-19

#230310
#2022-06-13 - 2022-07-12

#230317
#2022-06-09 - 2022-07-03

#234233
#2022-06-19 - 2022-07-03

# 236916
# 2023-06-12 - 2023-07-09 

# 242577 - potential non - breeding ? departing south in 2023-07-01 
#2023-06-17- 2023-06-26

#242570
#


###############################

## sunbirds - detailed tags 

#232981
#2023-06-07 - 2023-07-10

#232982
#2023-06-12 - 2023-07-12

#232984
#2022-06-19 - 2022-07-17

#232985
#2022-06-17 - 2022-07-22

#238544
#2023-06-07 - 2023-08-06

# 238546
#2023-06-10 -  2023-07-10

#240161
#2023-06-15 - 2023-07-20

#240164 - TDF
#2023-06-29 - 2023-07-21

#240167 - TDF
#2023-06-23 - 2023-07-26

# 240168
#2023-06-06 - 2023-08-05


#240170 
#2024-06-18 - 2024-07-18

# 242570
#2023-06-15 - 2023-07-26

# 242580
#2023-06-05 - 2023-07-30

# 242583
#2023-06-13 - 2023-07-26

#242656
#2023-06-10 - 2023-08-03

#242657
#2023-06-09 - 2023-08-01

#242658
#2023-06-16 - 2023-08-10

#260688
#2024-06-12 - 2024-07-11

#260689
#2024-06-05 - 2024-07-18

#260690
#2024-06-18 - 2024-07-16

#260692
#2024-06-11 - 2024-07-21

#260693
#2024-06-13 - 2024-07-16

#260694
#2024-06-07 - 2024-07-13

#260696
#2024-06-19 - 2024-07-15

#260697
#2024-06-14 - 2024-08-08

#260698
#2024-06-12 - 2024-07-10

#260803
#2024-06-04 - 2024-07-08



###############################

## solar PTT  - detailed tags 

#241167
#2023-06-21 - 2023-07-16



