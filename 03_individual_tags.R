#libraries
library("rnaturalearth")
library("rnaturalearthdata")
library(ggplot2)
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

# add other specific tags 
reknbr_fullid <- c(reknbr_full$tag.id, 260805,262940)
                
  
# select the individuals with breeding locations 
breed <- all |> filter(tag.id %in% reknbr_fullid) 
length(unique(breed$tag.id))

# 37 tags which had a full breeding season in the arctic 

breed <- breed |> 
  mutate(tag.model1 = case_when(
    tag.model == "sunbird"~ "Sunbird Solar Argos", 
    tag.model == "Sunbird"~ "Sunbird Solar Argos", 
    .default = tag.model))

st_write(breed, path("temp", "full_breeding_ids.gpkg"), delete_dsn = TRUE)

aa <- breed |> 
  mutate(date = make_date(year, month, day)) 




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
   tag.id == "242577" & date >= "2023-06-17" & date <= "2023-06-26" ~ "y", #potential non - breeding ? departing south in 2023-07-01 
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
   tag.id == "260803" & date >= "2024-06-05" & date <= "2024-07-04" ~ "y",
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
   tag.id == "260805" & date >= "2024-06-07" & date <= "2024-08-03" ~ "y",  # ended in August - no return sth 
   tag.id == "262940" & date >= "2024-06-15" & date <= "2024-10-18" ~ "y", #dropped tag?
   .default = NA
 ))


st_write(bb, path("temp", "full_breeding_ids_label.gpkg"), delete_dsn = TRUE)


bb <- st_read(path("temp", "full_breeding_ids_label.gpkg"))


###############################

## generate centroid as plot example


bb <- bb |> filter(breeding == 'y')

# get duration for each tag id

bb_dir <- bb %>% 
  group_by(tag.id) %>% 
  summarise(start = min(date), end = max(date)) |>
  mutate(duration = as.numeric(end - start)) |> 
  st_drop_geometry() 

min(bb_dir$duration) 
mean(bb_dir$duration) # 37 days
max(bb_dir$duration) # 60 days
median(bb_dir$duration)



# get centroids

bc <- bb |> 
  group_by(tag.id) |> 
  summarise(centroid = st_centroid(st_union(geom))) |> 
  st_as_sf() |> 
  st_transform(crs = 4326) |> 
  select(tag.id, centroid)


st_write(bc, path("temp", "centroid_breeding.gpkg"), delete_dsn = TRUE)



# plot the centroids for context 









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



