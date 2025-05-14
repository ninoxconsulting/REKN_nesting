
# review the movebank data - 2024/2025 
library(dplyr)
library(fs)
library(readr)
library(sf)
library(lubridate)
library(geosphere)

ref_data <- read_csv(path("data", "reference_data_edited.csv"))
loc <- read_csv(path("data", "location_data_raw.csv"))


## review the reference data for ECCC
raw_dat <- file.path("data", "movebank_locations_20250430")
filesoi <- list.files(raw_dat)

######################################
# data_set1: ECCCC
key = "ECCC"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# read in reference data # ECCC
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep |>  dplyr::select(tag.id,  tag.model, deploy.on.date,tag.serial.no )

# read in the location data 
btemp <- read.csv(file.path(raw_dat, filesoi))
bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                argos.altitude, import.marked.outlier, 
                gps.fix.type.raw, lotek.crc.status,
                sensor.type, individual.local.identifier,
                argos.lc, algorithm.marked.outlier,
                tag.local.identifier)%>%
  mutate(date_time = ymd_hms(timestamp)) |> 
  mutate(tag.id = tag.local.identifier) |> 
  filter(visible =="true")

# calculate time differences
bout <- bout  |> 
  mutate(date_time = ymd_hms(timestamp)) |> 
  mutate(year = year(date_time )) |> 
  mutate(month = month(date_time ),
         day = day(date_time),
         hour = hour(date_time),
         minute = minute(date_time))|> 
  filter(year >2023) |> 
  filter(year <2026) |> 
arrange(date_time, by_group = tag.id ) |> 
  left_join(brep) |> 
 mutate(proj  = "ECCC") 

no.tags <- unique(bout$tag.local.identifier) #114
no.ids <- unique(bout$individual.local.identifier) # 19 + blanks

bout_eccc <- bout

#################################################################
# data_set2: "Southeast USA to Arctic"
key = "Southeast USA to Arctic"

filesoi <- list.files(raw_dat, pattern = key)
filesoi_ref <- filesoi[1]
filesoi <- filesoi[2]

# read in reference data # ECCC
brep <- read.csv(file.path(raw_dat, filesoi_ref))
brep <- brep |>  dplyr::select(tag.id,  tag.model, deploy.on.date )

# read in the location data 
btemp <- read.csv(file.path(raw_dat, filesoi))
bout <- btemp %>%
  dplyr::select(visible, timestamp, location.long, location.lat,
                argos.altitude, format.type,
                sensor.type, individual.local.identifier,
                argos.lc, algorithm.marked.outlier,
                tag.local.identifier)%>%
  mutate(date_time = ymd_hms(timestamp)) |> 
  mutate(tag.id = tag.local.identifier) |> 
  filter(visible =="true")

# calculate time differences
bout <- bout  |> 
  mutate(date_time = ymd_hms(timestamp)) |> 
  mutate(year = year(date_time )) |> 
  mutate(month = month(date_time ),
         day = day(date_time),
         hour = hour(date_time),
         minute = minute(date_time))|> 
  filter(year >2023) |> 
  filter(year <2026) |> 
  arrange(date_time, by_group = tag.id ) |> 
  left_join(brep) |> 
  mutate(proj  = "Southeast USA to Arctic") 


no.tags <- unique(bout$tag.local.identifier) #27
no.ids <- unique(bout$individual.local.identifier) #27


# Join data together
head(bout)
head(bout_eccc)

out <- bind_rows(bout, bout_eccc) 

out <- out |> 
  select(-visible, -argos.altitude, -tag.local.identifier, -import.marked.outlier, -format.type)

length(unique(out$tag.id))


############################################################################
## Calculate distance between points and bearing

out <- out %>%
  group_by(tag.id) |> 
  mutate(diff = difftime(date_time, lag(date_time),  units = c("hours")), 
         diff = as.numeric(diff)) 

bdd_det <- out  |> 
  group_by(tag.id) |> 
  mutate(location.long_prior = lag(location.long, 1L),
         location.lat_prior = lag(location.lat, 1L))

bdd_det <- bdd_det |> 
  rowwise() |> 
  dplyr::mutate(gcd_m = distHaversine(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
                bearing = bearing(c(location.long_prior,location.lat_prior), c(location.long, location.lat)),
                speed_mhr = round((gcd_m/diff)/1000,1))%>% 
  ungroup()


#cwrite out so this can be compared for tracks and update the final_tags_document

# # write out the entire dataset 
clean_sf <- st_as_sf(bdd_det, coords = c("location.long", "location.lat"), crs = st_crs(4326))
st_write(clean_sf, file.path("temp", "birds_2024_raw.gpkg"), append = F)
#st_write(clean_sf, file.path(raw_dat, "locations_final", "loc_20240712_johnson_final.gpkg"), append = F)


## use this to fill in the proj name with tag.id 
id <- bdd_det |> 
  select(tag.id, proj) |> 
  distinct() |> 
  arrange(tag.id)



# list of birds with the summary of movement patterns
reknbr <- read_csv(path("data", "final_tags_list_edited.csv")) |> 
  filter(breeding =="y") |> 
  filter(subspecies == "rufa")

ids <- reknbr$tag.id

br_sf <- clean_sf |> 
  filter(tag.id %in% ids)


st_write(br_sf, file.path("temp", "birds_2024.gpkg"), append = F)































#############################################

### Duration statistics 

# df - location data 
# ref = reference data 
# ref_u = reference data with usable tags 


# calcualte the duration of tags 
dur_all <- df_all %>% 
  filter(!is.na(movement_final)) %>%
  group_by(tag.id) |> 
  mutate(min_ts = min(date_time),
         max_ts = max(date_time))%>% 
  select(tag.id,min_ts, max_ts)%>% 
  st_drop_geometry() |> 
  distinct() %>% 
  mutate(min = ymd_hms(min_ts)) |> 
  mutate(max = ymd_hms(max_ts)) %>% 
  rowwise() |> 
  dplyr::mutate(duration = max - min ) |> 
  mutate(dur_min = round(as.numeric(duration)/60,1))%>%  
  mutate(dur_hrs = round(as.numeric(dur_min)/60,1))%>%  
  mutate(dur_days = round( dur_hrs/24,1))%>%  
  mutate(year = year(min)) %>% 
  dplyr::select(tag.id, min, max, dur_hrs, dur_days, year )


ref_due <- ref %>% 
  select(proj, tag.id, tag.model) %>% 
  left_join(ref_id)


dur_summary <- left_join(ref_due, dur_all)


write.csv(dur_summary, file.path(out.plots, "duration_per_tag_353.csv"))




