#01-review nest static gps testing 

library(dplyr)
library(fs)
library(readr)
library(sf)
library(lubridate)
library(tidyverse)

data_dir <- path("data", "static_nest_trial")

# read in list of files that end in .csv
fss <- list.files(data_dir, pattern = "\\.xls$") 


dd <-  purrr::map(fss, ~ readxl::read_excel(path(data_dir, .x), .name_repair = "unique")) |> 
  bind_rows()

dd[sapply(dd, function(x) all(is.na(x)))] <- NULL

ddsf <- st_as_sf(dd, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

st_write(ddsf, path(data_dir, "static_nest_trial_test.gpkg"), delete_dsn = TRUE)


# prep gps locations 

locs <- readxl::read_excel(path(data_dir, "Sunbird Testing Locations.xlsx"),.name_repair = "unique")

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
  mutate(deploy_dt =ymd_hms(deploy_date + deploy_time)) |> 
  mutate(retrieve_date = ymd(`Retrieval Date`)) |> 
  mutate(retrieve_time = get_time(`Retrieval Time`)) |> 
  mutate(retrieve_dt =ymd_hms(retrieve_date + retrieve_time)) |> 
  mutate(tag_id = as.numeric(`Tag ID`)) |> 
  select(-c("Deployment Date" , "Deployment Time",`Retrieval Date`,`Retrieval Time`,`Tag ID`))


locs <- locs |> 
mutate(region = case_when(
  tag_id %in% c(285989, 285991, 283990) ~ "PCI",
  tag_id %in% c(285995, 285996, 285997) ~ "CambridgeBay",
  tag_id %in% c(285994, 285993, 285992) ~ "EBM"
))

locsf <- st_as_sf(locs, coords = c("Deployment Longitude", "Deployment Latitude"), crs = 4326, remove = FALSE)

st_write(locsf , path(data_dir, "static_nest_trial_reference.gpkg"), delete_dsn = TRUE)

###################################################################################


# filter the records by reference data dates and times. 

loc <- st_read( path(data_dir, "static_nest_trial_reference.gpkg"))

dd <- st_read(path(data_dir, "static_nest_trial_test.gpkg")) |> 
  mutate(tag_id = as.numeric(`Platform.ID.No.`))


# for each location select only date and time within the deploy and retrieval date times.

tids <- locs$tag_id

ddf <- purrr::map(tids, function(i){

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

loc <- st_read( path(data_dir, "static_nest_trial_reference.gpkg"))

dd <-  st_read(path(data_dir, "static_nest_trial_filtered.gpkg"))

# join the region information 
loc_id <- loc |> 
  st_drop_geometry() |> 
  select(tag_id, region)

dd <- dd |> 
  left_join(loc_id, by = "tag_id")

dddf <- st_drop_geometry(dd)

# review the number of units 
type <- dddf |> 
  group_by(tag_id, region,Loc..quality) |> 
  summarise(n = n()) 


# plot the location quality by tag type and group by region 

p1 <- ggplot(type, aes(x = Loc..quality, y = n, fill = region )) +
  facet_wrap(~tag_id)+#, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  #theme_minimal() +
  labs(title = "Number of Locations by Quality Type and Tag id",
       x = "Location Quality",
       y = "Number of Locations")# +
# scale_fill_brewer(palette = "Set1")


# generate the percentage values instead 

type_total <- dddf |> 
  group_by(tag_id, region) |> 
  summarise(total = n())

summ <- left_join(type, type_total, by = c("tag_id", "region")) |> 
  mutate(pct = n/total * 100) 
 
# plot the location quality by tag type and group by region 

p2 <- ggplot(summ , aes(x = Loc..quality, y = pct, fill = region )) +
  facet_wrap(~tag_id)+#, scales = "free_y") +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  labs(title = "Percent of Locations by Quality Type and Tag id",
       x = "Location Quality",
       y = "Number of Locations")# +
 # scale_fill_brewer(palette = "Set1")



# generate a measure of how h










