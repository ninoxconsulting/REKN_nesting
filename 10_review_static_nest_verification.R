# 01-review nest static gps testing

library(dplyr)
library(fs)
library(readr)
library(sf)
library(lubridate)
library(tidyverse)

data_dir <- path("data", "static_nest_trial")

# read in list of files that end in .csv
fss <- list.files(data_dir, pattern = "\\.xls$")

dd <- purrr::map(fss, ~ readxl::read_excel(path(data_dir, .x), .name_repair = "unique")) |>
  bind_rows()

dd[sapply(dd, function(x) all(is.na(x)))] <- NULL

ddsf <- st_as_sf(dd, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

st_write(ddsf, path(data_dir, "static_nest_trial_test.gpkg"), delete_dsn = TRUE)

# prep gps locations
locs <- readxl::read_excel(path(data_dir, "Sunbird Testing Locations.xlsx"), .name_repair = "unique")

get_time <- function(time = now()) {
  time %>%
    str_split(" ") %>%
    map_chr(2) %>%
    hms()
}

# merge data and time columns
locs <- locs |>
  mutate(deploy_date = ymd(`Deployment Date`)) |>
  mutate(deploy_time = get_time(`Deployment Time`)) |>
  mutate(deploy_dt = ymd_hms(deploy_date + deploy_time)) |>
  mutate(retrieve_date = ymd(`Retrieval Date`)) |>
  mutate(retrieve_time = get_time(`Retrieval Time`)) |>
  mutate(retrieve_dt = ymd_hms(retrieve_date + retrieve_time)) |>
  mutate(tag_id = as.numeric(`Tag ID`)) |>
  select(-c("Deployment Date", "Deployment Time", `Retrieval Date`, `Retrieval Time`, `Tag ID`))


locs <- locs |>
  mutate(region = case_when(
    tag_id %in% c(285989, 285991, 285990) ~ "PCI",
    tag_id %in% c(285995, 285996, 285997) ~ "CambridgeBay",
    tag_id %in% c(285994, 285993, 285992) ~ "EBM"
  ))

locsf <- st_as_sf(locs, coords = c("Deployment Longitude", "Deployment Latitude"), crs = 4326, remove = FALSE)

st_write(locsf, path(data_dir, "static_nest_trial_reference.gpkg"), delete_dsn = TRUE)


###################################################################################

# filter the records by reference data dates and times.

loc <- st_read(path(data_dir, "static_nest_trial_reference.gpkg"))

dd <- st_read(path(data_dir, "static_nest_trial_test.gpkg")) |>
  mutate(tag_id = as.numeric(`Platform.ID.No.`))


# for each location select only date and time within the deploy and retrieval date times.

tids <- locs$tag_id

ddf <- purrr::map(tids, function(i) {
  # i <- tids[1]

  tag <- i
  loct <- loc |> filter(tag_id == i)
  deploy <- loct$deploy_dt
  retrieve <- loct$retrieve_dt

  out <- dd |>
    filter(tag_id == tag) |>
    mutate(include = ifelse(tag_id == tag &
      ymd_hms(Loc..date) >= deploy &
      ymd_hms(Loc..date) <= retrieve, TRUE, FALSE))

  return(out)
}) |>
  bind_rows() |>
  filter(include == TRUE) |>
  select(-include)

# note checked these manually and there is some that are filtered on date that appear near
# nest site. these are likley as date-time are offset ie loc = local time and argos = UTC. Not a
# big deal as more restricted than includive.

st_write(ddf, path(data_dir, "static_nest_trial_filtered.gpkg"), delete_dsn = TRUE)



### Summary of tags per locations type

loc <- st_read(path(data_dir, "static_nest_trial_reference.gpkg"))

dd <- st_read(path(data_dir, "static_nest_trial_filtered.gpkg"))

# join the region information
loc_id <- loc |>
  st_drop_geometry() |>
  select(tag_id, region)

dd <- dd |>
  left_join(loc_id, by = "tag_id")

dddf <- st_drop_geometry(dd)

# review the number of units
type <- dddf |>
  group_by(tag_id, region, Loc..quality) |>
  summarise(n = n())


# plot the location quality by tag type and group by region

p1 <- ggplot(type, aes(x = Loc..quality, y = n, fill = region)) +
  facet_wrap(~tag_id) + # , scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  # theme_minimal() +
  labs(
    title = "Number of Locations by Quality Type and Tag id",
    x = "Location Quality",
    y = "Number of Locations"
  ) # +
# scale_fill_brewer(palette = "Set1")
p1

# generate the percentage values instead

type_total <- dddf |>
  group_by(tag_id, region) |>
  summarise(total = n())

summ <- left_join(type, type_total, by = c("tag_id", "region")) |>
  mutate(pct = n / total * 100)

# plot the location quality by tag type and group by region

p2 <- ggplot(summ, aes(x = Loc..quality, y = pct, fill = region)) +
  facet_wrap(~tag_id) + # , scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  labs(
    title = "Percent of Locations by Quality Type and Tag id",
    x = "Location Quality",
    y = "Number of Locations"
  ) # +
# scale_fill_brewer(palette = "Set1")
p2






# Calculate the distance from the nest to the static test


#We can also review the distance to the nest from each estimated point and compare to the error class rating to see how these align. 
loc_xy <- loc |> 
  #st_drop_geometry() |> 
  select(tag_id, region)

loc_xy_df <- st_drop_geometry(loc_xy)

dda <-  st_read(path(data_dir, "static_nest_trial_filtered.gpkg"), quiet = TRUE)

dd_sub <- dda |> select(tag_id, Error.radius, Loc..quality) 

tags <- unique(dd_sub$tag_id)

alldist <- purrr::map(tags, function(i){
  #  i <- tags[1]
  idd <- dd_sub |> filter(tag_id == i)
  iloc <- loc_xy |> filter(tag_id == i)
  
  dists <- st_distance(idd, iloc)
  
  out <- cbind(idd, dists)
  
  out
  
}) |> bind_rows()

## output files for Dougs review 

# st_write(alldist, path(data_dir, "static_nest_trial_distance_cals.gpkg"), delete_dsn = TRUE)
# alldistdf <- cbind(alldist, st_coordinates(alldist)) 
# alldistdf <-st_drop_geometry(alldistdf )
# write.csv(alldistdf, path(data_dir, "static_nest_trial_distance_cals.csv"))


# generate a point chart with histances coloured by Loc..quality
alldist$Loc..quality <- factor(alldist$Loc..quality, levels=c("G", "3", "2", "1", "0", "A", "B", "Z"))
alldist <- alldist |> 
  filter(Loc..quality != "Z")

p3 <- ggplot(alldist, aes(Loc..quality, as.numeric(dists)))+
  geom_jitter( alpha = 0.2)+
  facet_wrap(~tag_id, scales = "free_y")+
  labs(title = "Distance from Nest by Location Quality",
       x = "Location Quality",
       y = "Distance from Nest (m)")

p3

#st_write(alldist, path(data_dir, "static_nest_trial_distance_check.gpkg"), delete_dsn = TRUE)


# error radius 
p4 <- ggplot(alldist, aes(Error.radius, as.numeric(dists), colour = Loc..quality))+
  geom_point()+
  facet_wrap(~tag_id, scales = "free")+
  labs(title = "Distance from Nest by Error Radius metric",
       x = "Error.radius",
       y = "Distance from Nest (m)")

p4






#####################################################
### Generate kde metrics for static nest locations.
#####################################################

library(sf)
library(adehabitatHR)
library(sp)
library(dplyr)
library(ggplot2)

##########################################################################
## method 1: Kernal density estimates

### Summary of tags per locations type

loc <- st_read(path(data_dir, "static_nest_trial_reference.gpkg"))

dd <- st_read(path(data_dir, "static_nest_trial_filtered.gpkg"))

# join the region information
loc_id <- loc |>
  st_drop_geometry() |>
  select(tag_id, region)

dd <- dd |>
  left_join(loc_id, by = "tag_id")


out_dir <- fs::path("output", "hr_kde")



### Generate kde metrics for static nest locations.
# note this does not currently have the filter for the level of accuracy.
# could be rerun with the high quality location only

bb <- dd |>
  select(tag_id, Longitude, Latitude, Loc..date, Loc..quality)

tdfgeo <- bb |>
  # filter(Loc..quality %in% c(2, 3)) |>
  dplyr::select(tag_id)

ids <- unique(tdfgeo$tag_id)

# test loop for a single tag
for (ii in ids) {
   #ii = 285989
  td <- tdfgeo |>
    dplyr::filter(tag_id == ii) |>
    as("Spatial")

  # take these brken bones and make them whole we still have time.


  ## 3.1 kde: h reference parameter
  # define the parameters (h, kern, grid, extent)

  kde_href <- kernelUD(td, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)
  #kde_href

  #ver95 <- getverticeshr(kde_href, 95) # get vertices for home range
  #ver95_sf <- st_as_sf(ver95) # convert to sf object

  #ver75 <- getverticeshr(kde_href, 75)
  #ver75_sf <- st_as_sf(ver75)

  ver50 <- getverticeshr(kde_href, 50)
  ver50_sf <- st_as_sf(ver50)
  
  ver25 <- getverticeshr(kde_href, 25)
  ver25_sf <- st_as_sf(ver25)
  # plot the outputs
  # mapview(ver50_sf, zcol = "id")
  # mapview(ver75_sf, zcol = "id")
  # mapview(ver95_sf, zcol = "id")

  #st_write(ver95_sf, fs::path(out_dir, paste0(ii, "_href_95.gpkg")), append = FALSE)
  #st_write(ver75_sf, fs::path(out_dir, paste0(ii, "_href_75.gpkg")), append = FALSE)
  st_write(ver50_sf, fs::path(out_dir, paste0(ii, "_href_50.gpkg")), append = FALSE)
  st_write(ver25_sf, fs::path(out_dir, paste0(ii, "_href_25.gpkg")), append = FALSE)

  ## 3.2 kde: Least Squares Cross Validation (lscv) method.

  kde_lscv <- kernelUD(td, h = "LSCV", kern = c("bivnorm"), grid = 500, extent = 2)

  #ver95ls <- getverticeshr(kde_lscv, 95) # get vertices for home range
  #ver95ls_sf <- st_as_sf(ver95ls)

  ver50ls <- getverticeshr(kde_lscv, 50)
  ver50ls_sf <- st_as_sf(ver50ls)

  ver25ls <- getverticeshr(kde_lscv, 25)
  ver25ls_sf <- st_as_sf(ver25ls)
  
  # plot the outputs
  mapview(ver50ls_sf, zcol = "id")
  #st_write(ver95ls_sf, fs::path(out_dir, paste0(ii, "_lscv_95.gpkg")), append = FALSE)
  st_write(ver50ls_sf, fs::path(out_dir, paste0(ii, "_lscv_50.gpkg")), append = FALSE)
  st_write(ver25ls_sf, fs::path(out_dir, paste0(ii, "_lscv_25.gpkg")), append = FALSE)
}

## This was not run for test sites as needs to be specific for each tag id.
#
# ## 3.3 kde: variable smoothing parameters (h)
# #kde_h1000 <- kernelUD(td, h = 1000, kern = c("bivnorm"), grid = 500, extent = 10000)
# kde_h500 <- kernelUD(td, h = 500, kern = c("bivnorm"), grid = 500, extent = 500)
# kde_h3000 <- kernelUD(td, h = 3000, kern = c("bivnorm"), grid = 500, extent = 5)
# kde_h5000 <- kernelUD(td, h = 5000, kern = c("bivnorm"), grid = 500, extent = 5)
#
# # kde - href = 1000
# #ver95_1000 <- getverticeshr(kde_h1000, 95) # get vertices for home range
# #ver95_1000_sf <- st_as_sf(ver95_1000) |> mutate(h = 1000) # convert to sf object
#
# # kde - href = 500
# ver95_500 <- getverticeshr(kde_h500, 95) # get vertices for home range
# ver95_500_sf <- st_as_sf(ver95_500) |>
#   mutate(h = 500) # convert to sf object
#
# # kde - href = 3000
# ver95_5000 <- getverticeshr(kde_h5000, 95) # get vertices for home range
# ver95_3000_sf <- st_as_sf(ver95_3000) |>
#   mutate(h = 3000) # convert to sf object
#
# # bind all data together
# all_verts <- bind_rows(ver95_1000_sf, ver95_500_sf, ver95_3000_sf)
#
# # lets plot the output
# ggplot(data = all_verts) +
#   geom_sf(
#     aes(colour = id),
#     alpha = 0.1
#   ) +
#   scale_colour_viridis_d() +
#   facet_wrap(vars(h)) +
#   theme_bw()
#





############################################

### NestR package #########################

remotes::install_github("picardis/nestR", build_vignettes = TRUE, force = TRUE)

# note need to ensure that Jags is also downloaded
# http://www.sourceforge.net/projects/mcmc-jags/files

# file:///C:/Users/genev/AppData/Local/R/win-library/4.4/nestR/doc/nestR.html

# https://github.com/picardis/nestR/blob/master/vignettes/nestR.Rmd
# vignette("nestR")

library(nestR)
library(dplyr)
library(fs)
library(sf)


### Summary of tags per locations type

loc <- st_read(path(data_dir, "static_nest_trial_reference.gpkg"))

dd <- st_read(path(data_dir, "static_nest_trial_filtered.gpkg"))

# join the region information
loc_id <- loc |>
  st_drop_geometry() |>
  select(tag_id, region)

dd <- dd |>
  left_join(loc_id, by = "tag_id")

out_dir <- fs::path("output", "hr_nestr")

### Generate nesr  metrics for static nest locations.
# note this does not currently have the filter for the level of accuracy.
# could be rerun with the high quality location only

bb <- dd |>
  select(tag_id, Longitude, Latitude, Loc..date, Loc..quality)



# loop through all the test tags...
ids <- unique(bb$tag_id)
ii <- ids[1]


bb <- bb  |>
  filter(tag_id ==ii) |>
  filter(Loc..quality %in% c("3", "2")) |>
  dplyr::mutate(date = lubridate::ymd_hms(Loc..date)) |>
  mutate(burst = paste0(tag_id))

bb <- cbind(st_coordinates(bb), bb)
bb <- bb |>
  rename(
    "long" = "X",
    "lat" = "Y"
  ) |>
  st_drop_geometry() |>
  select(burst, date, tag_id, long, lat)


## Attempt to find nests

ws_output_1 <- find_nests(
  gps_data = bb,
  sea_start = "06-15",  # season breeding season 
  sea_end = "08-03",    # ends season breeding season 
  nest_cycle = 34,      # duration in days of complete nesting attempt 
  buffer = 10000,       # spatial scale at which revisitation pattern is calculated. buffer (in meters)
  # this is meant to account for spatial error around the nest from imprecise GPS location and pings near but not on the nest. 
  # the value of the buffer needs to be set higher than GPS error. 
  # could experiment with this one based on accuracy metrics of GPS class
  
  min_pts = 2,          # how many points need to be within the buffer to be considered. Can reduce influence of isolated points
  min_d_fix = 5,        # used to counteract impact of missed consecutive days to next 
  min_consec = 2,       # number of consecutive days a location is visited  
  min_top_att = 1,      # % of fixes at the location on the day with maximum attendance 
  min_days_att = 1,     # percent of days a location is visited between first anf last visit 
  discard_overlapping = FALSE
)

ws_output_1

head(ws_output_1$nests)

table(ws_output_1$nests$burst)


# output nests as an sf object
ws_nests <- ws_output_1$nests |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  mutate(nest_id = row_number())

st_write(ws_nests, path("output", paste0( "tag_", ii, "_nests.gpkg")), delete_dsn = TRUE)



# we can further explore these parameters to identify nest and non-nest locations. 
## situation 1 : when nest location is known 

ws_output_1$nests |> 
  filter(burst == ii) |> 
  head()

can_coords <- ws_output_1$nests |> 
  filter(burst == ii) |> 
  slice(1) |> 
  select(long, lat)


# lets check the distance between the predicted nest and the real nest 

ii_loc <- loc |> 
  filter(tag_id == ii) |> 
  mutate(long = Deployment.Longitude,
         lat = Deployment.Latitude) |> 
  st_drop_geometry() |> 
  select(long, lat)

dist_m <- geosphere::distGeo(can_coords, ii_loc)


## We can then parametise the outputs in fine detail by slecting a location close to but not within the buffer. 
## Note this can only be done when you know the exact location. 
## situation 2 : when nest location is not known. 



can_coords <- ws_output_1$nests |> 
  filter(burst == ii) 

id_known = data.frame(burst = ii, 
                      loc_id = 1)

exploded_nest <- get_explodata(candidate_nests = can_coords,
                               known_ids = id_known, 
                               pick_overlapping = FALSE)












## Step 3: Identifying nests among revisited locations

ws_output_2 <- find_nests(
  gps_data = bb,
  sea_start = "06-01",
  sea_end = "07-05",
  nest_cycle = 34,
  buffer = 85000,
  min_pts = 2,
  min_d_fix = 5,
  min_consec = 2,
  min_top_att = 1,
  min_days_att = 1,
  discard_overlapping = TRUE
)

ws_nests <- ws_output_2


# output nests as an sf object
ws_nests2 <- ws_output_2$nests |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  mutate(nest_id = row_number())

st_write(ws_nests2, path("temp", "tag_260803_nests22.gpkg"), delete_dsn = TRUE)







#####################################################
## Test the one above but with a limited distribution


bb <- st_read(path("temp", "full_breeding_ids_label.gpkg"), quiet = TRUE) |>
  filter(breeding == "y") |>
  filter(tag.id == 262940) |>
  filter(argos.lc %in% c("3", "2")) |>
  dplyr::mutate(date = lubridate::ymd_hms(timestamp)) |>
  mutate(burst = paste0(tag.id, "-", year))

# st_write(bb, path("temp", "tag_262940.gpkg"), delete_dsn = TRUE)

bb <- cbind(st_coordinates(bb), bb)
bb <- bb |>
  rename(
    "long" = "X",
    "lat" = "Y"
  ) |>
  st_drop_geometry() |>
  select(burst, date, tag.id, long, lat)


# atempt to find nests

ws_output_1 <- find_nests(
  gps_data = bb,
  sea_start = "06-15",
  sea_end = "08-03",
  nest_cycle = 34,
  buffer = 10000,
  min_pts = 2,
  min_d_fix = 5,
  min_consec = 2,
  min_top_att = 1,
  min_days_att = 1,
  discard_overlapping = FALSE
)


ws_output_1

head(ws_output_1$nests)

table(ws_output_1$nests$burst)


# output nests as an sf object
ws_nests <- ws_output_1$nests |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  mutate(nest_id = row_number())

st_write(ws_nests, path("temp", "tag_262940_nests1_dist.gpkg"), delete_dsn = TRUE)


## Step 3: Identifying nests among revisited locations

ws_output_2 <- find_nests(
  gps_data = bb,
  sea_start = "06-15",
  sea_end = "08-03",
  nest_cycle = 34,
  buffer = 10000,
  min_pts = 2,
  min_d_fix = 5,
  min_consec = 2,
  min_top_att = 1,
  min_days_att = 1,
  discard_overlapping = TRUE
)

ws_nests <- ws_output_2


# output nests as an sf object
ws_nests2 <- ws_output_2$nests |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  mutate(nest_id = row_number())

st_write(ws_nests2, path("temp", "tag_262940_nests2_dist.gpkg"), delete_dsn = TRUE)


# changing the date doesnt make a difference to location
# also changing distance did not the date doesnt make a difference to location
