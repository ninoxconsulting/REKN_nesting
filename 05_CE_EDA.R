# Casey's exploratory REKN data analysis

library(tidyverse)
library(moveHMM)
library(fs)
library(lubridate)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(mapview)
library(argosfilter)
library(aniMotum)
library(crawl)
library(momentuHMM)
library(nestR)


# read in the data-----------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

# read in the metadata
bird_meta <- read_csv(path("data", "reference_data_edited.csv")) 

# read in the move data
move23 <- st_read(path("temp", "birds_2023.gpkg"))
move24 <- st_read(path("temp", "birds_2024.gpkg"))


# wrangle and filter the move data -----------------------------------------------

# combine the two years of data, changing the column types to be compatible
move23 <- move23 |> 
  mutate(tag.id = as.character(tag.id),
         date_time = as.POSIXct(date_time, tz = "UTC"))

move24 <- move24 |> 
  mutate(algorithm.marked.outlier = as.logical(algorithm.marked.outlier),
         tag.id = as.character(tag.id))

# combine the two years
move <- bind_rows(move23, move24)

# filter to Arctic locations
arctic_move <- move |> 
  filter(st_coordinates(geom)[,2] > 60)

# filter to full summer birds, with regular observations throughout the duration of June and July
summer_birds_summary <- arctic_move |> 
  filter(month %in% 6:7) |> 
  group_by(tag.id, tag.model) |> 
  summarise(n = n(),
            mindate = min(date_time),
            maxdate = max(date_time)) |> # 95 birds at this stage
  ungroup() |> 
  arrange(n) |> 
  filter(n>50)

# filter the move data to only include the observations from during nesting season
summer_move <- arctic_move |> 
  filter(tag.id %in% summer_birds_summary$tag.id) |> 
  mutate(yday = yday(date_time),
         argos.lc = na_if(argos.lc, ""), # convert empty strings to NA, important for next step
         lotek.crc.status = na_if(lotek.crc.status, ""),
         obs.type = case_when(
           !is.na(argos.lc) & is.na(lotek.crc.status) ~ "Doppler",
           is.na(argos.lc) & !is.na(lotek.crc.status) ~ "GPS",
           TRUE ~ "Both"
         )) 

# sanity check: do all observations have either a Doppler or GPS level of quality?
summer_move |> 
  filter(is.na(argos.lc) & is.na(lotek.crc.status)) |> 
  nrow() # yes

# filter to only include observations with a level of quality
quality_smove <- summer_move |> 
  filter(argos.lc %in% c("3","2") | lotek.crc.status %in% c("G"))



# exploratory analysis -------------------------------------------------

# plot all the raw data
ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +
  geom_sf(data = summer_move, aes(color = tag.id, shape = tag.model), size = 1) +
  coord_sf(xlim = c(-120, -70), ylim = c(60, 75)) +
  theme_minimal()


# how many birds have multiple years?
birds.w.multiyear <- quality_smove |> 
  distinct(tag.id, year) |> 
  st_drop_geometry() |> 
  count(tag.id) |> 
  filter(n==2) |> # no birds with more than 2 years of data
  pull(tag.id) 

quality_smove |> 
  st_drop_geometry() |> 
  filter(tag.id %in% birds.w.multiyear) |> 
  count(tag.id, year, obs.type)


# which birds have both Doppler and GPS?
quality_smove |> 
  st_drop_geometry() |> 
  group_by(proj, tag.id) |>
  summarise(n_doppler = sum(!is.na(argos.lc)),
            n_gps = sum(!is.na(lotek.crc.status))) |>
  ungroup() |>
  group_by(proj, tag.id) |> 
  arrange(desc(n_gps), desc(n_doppler)) |> # 6 birds have some of both GPS and doppler
  print(n=54)

tags.w.gps.and.doppler <- c("238570", "213841", "238579", "236449", "236453", "213838")

# map the birds in this subset showing the doppler and GPS data in different colors
quality_smove |> 
  filter(tag.id %in% tags.w.gps.and.doppler) |> 
  arrange(desc(obs.type)) |> # plot doppler on top
  ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +
  geom_sf(aes(color = obs.type)) +
  coord_sf(xlim = c(-120, -70), ylim = c(60, 75)) +
  facet_wrap(~tag.id) +
  theme_minimal()


## individual birds ------------

# plot tag.id 232982 colored by argos.lc (level of quality of the location)
ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white") +
  geom_sf(data = quality_smove |> filter(tag.id == "232982"), aes(color = argos.lc), size = 1) +
  coord_sf(xlim = c(-120, -70), ylim = c(60, 75)) +
  theme_minimal() +
  labs(color = "Argos Location Class")



# list of Gen's IDs to look at
#"232981", "232982", 
# "238544", good candidate for accurate prediction
# "242577", failed nest, short duration 
# "260803", "260805",
# "262940" stationary-- dead bird or dropped?
my.id <- "262940"

my_dat <- quality_smove |> 
  filter(tag.id == my.id,
         yday %in% 166:181) # jun 15-31

# plot with lines connecting consecutive points
make_lines <- function(df) {
  df <- df %>%
    mutate(lead_geom = lead(geom)) %>%
    filter(!is.na(lead_geom)) %>%
    rowwise() %>%
    mutate(line = st_sfc(st_cast(st_union(c(geom, lead_geom)), "LINESTRING"), crs = st_crs(df))) %>%
    ungroup() %>%
    st_as_sf()  # convert back to sf with LINESTRING geometries
  
  st_geometry(df) <- "line" # change the active geometry col
  return(df)
}

# wrapper for applying to each individual in a dataframe
make_lines_wrapper <- function(df) {
  df %>%
    group_by(tag.id) %>%
    do(make_lines(.)) %>%
    ungroup()
}

#draw an interactive map
mapview(make_lines(my_dat), lwd = 2, map.types = "Esri.WorldImagery") +
  mapview(my_dat, color = "red", cex = 2) 
# caveat: mapview dosen't show the error around each geolocation

lc_codes <- tribble(
  ~accuracy, ~argos.lc,~ord,~color,
  "<250", "3",1,"darkgreen",
  "250-500", "2",2,"green",
  "500-1500", "1",3,"yellow",
  ">1500", "0",4,"orange",
  "unknown (poor)", "A",5,"red",
  "unknown (poor)", "B",6,"darkred"
)

ggplot() +
  geom_sf(data = make_lines(my_dat), aes(geometry = line), color = "blue") +
  geom_sf(data = my_dat |> 
            left_join(lc_codes) |> 
            mutate(accuracy=fct_reorder(accuracy, ord)), 
          aes(color = accuracy), size = 2) +
  scale_color_manual(values = lc_codes$color) 
  # add satellite image basemap??

# look at only high quality points for an individual
hiq <- my_dat |> 
  filter(argos.lc %in% c("3", "2"))

mapview(make_lines(hiq), lwd = 2, map.types = "Esri.WorldImagery") +
  mapview(hiq, color = "red", cex = 2) 



# look at a GPS collared individual
my.gps.id <-  213841
my_gps_dat <- quality_smove |> 
  filter(tag.id == my.gps.id,
         obs.type == "GPS",
         yday %in% 170:180)

mapview(make_lines(my_gps_dat), lwd = 2, map.types = "Esri.WorldImagery") +
  mapview(my_gps_dat, color = "red", cex = 2) 




# coarse search for nest sites with nestR ----------------------------------------------------

# objective is to get rough estimates of nest locations for each bird
# to generate ROIs (buffered nest points) for covariate extraction in GEE
# EDIT: did not end up using these points, the resulting raster files were too large
# just extracted rasters for Southhampton Island, NU

nestr_data <- quality_smove |> 
  mutate(long = st_coordinates(geom)[,1],
         lat = st_coordinates(geom)[,2],
         burst = paste0(tag.id, "-", year)) |>
  rename(date = date_time) |> 
  st_drop_geometry() |>
  select(burst, date, tag.id, long, lat) |> 
  as_tibble()



# attempt to find nests (slow,takes a minute to run)
ws_output_1 <- find_nests(
  gps_data = nestr_data,
  sea_start = "06-01",
  sea_end = "07-05",
  nest_cycle = 34,
  buffer = 85000,
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

# plot all nestR estimated nest locations next to their movement data as sanity check
pts_dopp <- quality_smove |> 
  filter(obs.type == "Doppler")
pts_gps <- quality_smove |> 
  filter(obs.type == "GPS")

mapview(pts_dopp, color = "red", cex = 2, map.types = "Esri.WorldImagery") + # movement points
  mapview(pts_gps, color = "green", cex = 2) + # GPS points
  mapview(ws_nests, color = "blue", cex = 3) # nest points

# obviously there are some problems with these coarse estimates, revisit  
# another method, or better parameters might be necessary


# Write output files ------------------------


# export the filtered data for use in other scripts
st_write(quality_smove, path("temp", "casey_filtered_birds.gpkg"), delete_dsn = TRUE)

# write nest coarse estimate points as csv for Earth Engine
st_write(ws_nests, path("temp", "coarse_nests.csv"), layer_options = "GEOMETRY=AS_XY")




  