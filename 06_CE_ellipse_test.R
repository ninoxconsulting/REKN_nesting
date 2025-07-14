# run this script with different birds to get a feel for how the SSM outputs differ with and without ellipse data

library(tidyverse)
library(fs)
library(lubridate)
library(sf)
library(rnaturalearth)
library(mapview)
library(leaflet)
library(aniMotum)
library(nestR)

move <- st_read(path("temp", "casey_filtered_birds.gpkg"))


# the raw data
raw_dat <- file.path("data", "movebank_locations_20250430", "Red Knot Migration Study ECCC.csv")
raw <- read_csv(raw_dat)
colnames(raw)

# select a bird (I tried 232982, 238544, and 242577 (gappy))
my_bird <- 232982

# choose an individual, filter to where we have doppler ellipse data 
# format these data for aniMotum
ell_test <- raw |> 
  filter(yday(timestamp) %in% seq(166, 194)) |> # jun 15 - jul 13
  filter(`tag-local-identifier`==my_bird) |> # select one bird
  select(id = `tag-local-identifier`,
         date = timestamp,
         lon = `location-long`,
         lat = `location-lat`,
         lc = `argos:lc`,
         smaj = `argos:semi-major`,
         smin = `argos:semi-minor`,
         eor = `argos:orientation`) |> 
  filter(!is.na(smin) & !is.na(smaj)) |> 
  filter(year(date)==2023)



# fit with the ellipse data with aniMotum SSM
fit_ellipse <- aniMotum::fit_ssm(ell_test, 
                         vmax = 10, # maximum speed in m/s, 36 km/h
                         model = "crw", 
                         time.step = 1) # prediction interval, in hours. NA to only estimate at observation times 

# fit without the ellipse data
fit_without <- ell_test |> 
  select(-c(smaj, smin, eor)) |> 
  aniMotum::fit_ssm(vmax = 10, # maximum speed in m/s, 36 km/h
                    model = "crw", 
                    time.step = 1) # prediction interval, in hours. NA to only estimate at observation times 

# plot time-series of the fitted values, comparing with and without the ellipse data
plot(fit_ellipse, what = "fitted", type = 1, pages = 1) + labs(title = "With ellipse data")
plot(fit_without, what="fitted", type = 1, pages = 1)+ labs(title = "WithOUT ellipse data")
# orange = fitted, blue = observed. Orange ribbon = SE. black X = prefiltered. units in Mercator kilometers


# compare the standard error of the fitted values with and without the ellipse data
# i.e. compare the area of the light orange area in the plots
fit_ellipse_locs <- grab(fit_ellipse, what = "pred", as_sf = FALSE)
fit_without_locs <- grab(fit_without, what = "pred", as_sf = FALSE)


# measure error area at each time step
fit_ellipse_mean_se <- fit_ellipse_locs |> 
  mutate(se_area = pi*x.se*y.se) |> 
  summarise(mean_se_area = mean(se_area, na.rm = TRUE),
            median_se_area = median(se_area, na.rm = TRUE))
fit_without_mean_se <- fit_without_locs |> 
  mutate(se_area = pi*x.se*y.se) |> 
  summarise(mean_se_area = mean(se_area, na.rm = TRUE),
            median_se_area = median(se_area, na.rm = TRUE))
print(paste("lacking ellipse data, the estimated geolocation error is",
      round((fit_without_mean_se$mean_se_area/fit_ellipse_mean_se$mean_se_area)*100,1),
      "% lower then when the ellipse data is included"))

# plot fitted values as a 2-d track
plot(fit_ellipse, what = "predicted", type = 2, pages = 1) + labs(title = "With ellipse data")
plot(fit_without, what = "predicted", type = 2, pages = 1) + labs(title = "WithOUT ellipse data")
# lacking ellipse data, the true error is likely underestimated (relies on Argos default values)


# how does the point estimate of geolocation change between the two?
compare <- fit_ellipse_locs |> 
  add_column(type = "ellipse") |> 
  bind_rows(fit_without_locs |> 
              add_column(type = "without")) 
  
ggplot(compare, aes(x = x, y = y, color = type)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_path() +
  coord_fixed()

# plot the x and y coordinates separately as a function of time, colored by type
ggplot(compare, aes(x = date, y = x, color = type)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "X coordinate (km)") +
  theme_minimal()
ggplot(compare, aes(x = date, y = y, color = type)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "Y coordinate (km)") +
  theme_minimal()


# try running nestR on SSM output
nestr_data_ssm <- compare |> 
  mutate(burst = paste0(id, "-", year(date), "-", type)) |>
  rename(long = lon, tag.id=id) |> 
  dplyr::select(burst, date, tag.id, long, lat)

# attempt to find nests from SSM out
nest_preds_ssm <- nestR::find_nests(
  gps_data = nestr_data_ssm,
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

# compare with the raw nestR predictions
nestr_data_raw <- ell_test |>
  mutate(burst = paste0(id, "-", year(date), "-raw")) |>
  rename(long = lon, tag.id=id) |> 
  dplyr::select(burst, date, tag.id, long, lat)

nest_preds_raw <- find_nests(
  gps_data = nestr_data_raw,
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

# output nests as an sf object
ssm_nests <- nest_preds_ssm$nests |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  mutate(nest_id = row_number())
ssm_nests_ellipse <- ssm_nests |> filter(str_detect(burst,"ellipse"))
ssm_nests_without <- ssm_nests |> filter(str_detect(burst,"without"))
raw_nests <- nest_preds_raw$nests |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  mutate(nest_id = row_number())
print(paste("the coords of the raw nests are:", raw_nests$geometry))
print(paste("the coords of the SSM+ellipse nests are:", ssm_nests_ellipse$geometry))
print(paste("the coords of the SSM nests are:", ssm_nests_without$geometry))
distm <- st_distance(bind_rows(ssm_nests, raw_nests) |> st_transform(3857))  # return units in meters
distm[upper.tri(distm)] # the distances between nest estimates, in meters

ell_test_sf <- ell_test |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  mutate(nest_id = row_number())

# plot nestR estimated nest locations next to their movement data
mapview(raw_nests, color = "blue", cex = 2, map.types = "Esri.WorldImagery") +
  mapview(ssm_nests_ellipse, color = "red", cex = 2) + 
  mapview(ssm_nests_without, color = "pink", cex = 2) + 
  mapview(ell_test_sf, color = "green", cex = 3) # raw movement data
  
