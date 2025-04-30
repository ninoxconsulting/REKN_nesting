#01_review_exising_data


# review the data which was compiled in 2024 (up to 2023 data)

library(dplyr)
library(fs)
library(readr)
library(sf)

ref_data <- read_csv(path("data", "reference_data_edited.csv"))
loc <- read_csv(path("data", "location_data_raw.csv"))


reknbr <- read_csv(path("data", "final_tags_list_edited.csv")) |> 
  filter(breeding =="y") |> 
  filter(subspecies == "rufa")

length(reknbr$tag.id)

reknbr_full <- reknbr |> 
  filter(south == "y")

length(reknbr_full$tag.id)


# # 57 individuals with a breeding and a return southwards 

reknbr_id <- reknbr_full |> 
  select(proj, tag.id, animal.id) 

re_pr <- reknbr_full |> 
  group_by(proj) |> 
  count()

# 

# check the type of tags for the breeding birds 

refsub <- ref_data |> 
  filter(tag.id %in% reknbr_id $tag.id) |> 
  select(-c(animal.life.stage, "animal.mass","animal.ring.id" ,
            "animal.sex",  "animal.taxon","deploy.on.person" , "deployment.comments",
            attachment.type, capture.timestamp, duty.cycle,
            tag.beacon.frequency, deploy.off.date, tag.mass,
            deploy.on.measurements ,tag.comments))

# check type of tags 

refsub |> 
  group_by(tag.model) |> 
  count()
  

tag_type <- refsub |> 
  select(tag.id, tag.model)





# locations 

locsub <- loc |> 
  filter(tag.id %in% reknbr_id$tag.id) 


locsf <- st_as_sf(locsub, coords = c("location.long", "location.lat"), crs = 4326)

st_write(locsf, path("temp", "birds_2023.gpkg"), append = FALSE)





