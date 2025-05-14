#01_review_exising_data


# review the data which was compiled in 2024 (up to 2023 data)
library(dplyr)
library(fs)
library(readr)
library(sf)

ref_data <- read_csv(path("data", "reference_data_edited.csv"))
loc <- read_csv(path("data", "location_data_raw.csv"))


# list of birds with the summary of movement patterns
reknbr <- read_csv(path("data", "final_tags_list_edited.csv")) |> 
  filter(breeding =="y") |> 
  filter(subspecies == "rufa")

# 72 birds were recorded in the Breeding grounds
#length(reknbr$tag.id)

#reknbr_full <- reknbr |> 
#  filter(south == "y")

#length(reknbr_full$tag.id)

# # 26 individuals with a breeding and a return southwards 

#reknbr_id <- reknbr_full |> 
#  select(proj, tag.id, animal.id) 

#re_pr <- reknbr_full |> 
#  group_by(proj) |> 
#  count()

# this is from the compiled data up to dec 2023. 



# check the type of tags for the breeding birds 

refsub <- ref_data |> 
  filter(tag.id %in% reknbr$tag.id) |> 
  select(-c(animal.life.stage, "animal.mass","animal.ring.id" ,
            "animal.sex",  "animal.taxon","deploy.on.person" , "deployment.comments",
            attachment.type, capture.timestamp, duty.cycle,
            tag.beacon.frequency, deploy.off.date, tag.mass,
            deploy.on.measurements ,tag.comments))

# check type of tags 
# 
# refsub |> 
#   group_by(tag.model) |> 
#   count()
#   
# 
# tag_type <- refsub |> 
#   select(tag.id, tag.model)



# locations dataset

locsub <- loc |> 
  filter(tag.id %in% reknbr$tag.id) |> 
  select(-c(height.above.ellipsoid, 
            argos.lat1, argos.lat2, argos.lon1, argos.lon2, argos.nb.mes,
            argos.nb.mes.120, argos.nopc, argos.orientation,
            argos.semi.major, argos.semi.minor, argos.sensor.1, 
            argos.sensor.2, argos.sensor.3, argos.sensor.4,
            argos.altitude, argos.best.level,
            bearing, gcd_m, speed_mhr, Event,argos.calcul.freq,
            argos.error.radius, argos.gdop, argos.iq, import.marked.outlier,
            argos.pass.duration, argos.valid.location.algorithm, '...1'))

refsub<- refsub |> 
  select(-c("...1","tag.model", "study.site","proj" ,"deploy_date_time", "track_data"  ))

out <-  left_join(locsub, refsub, by = "tag.id") 

locsf <- st_as_sf(out, coords = c("location.long", "location.lat"), crs = 4326)

st_write(locsf, path("temp", "birds_2023.gpkg"), append = FALSE)


#b23 <- st_read(path("temp", "birds_2023.gpkg"))

# latest time stamp 
#b23|> 
#  group_by(tag.id) |> 
#  summarise(max_time = max(date_time)) |> 
#  arrange(desc(max_time))

# the latest date here is 2023-12-31

