# o4 testing methodology for nest locations:

library(sf)
library(mapview)
library(adehabitatHR)
library(sp)
library(dplyr)
library(ggplot2)

out_dir <- fs::path("output", "hr_kde")

##########################################################################
## method 1: Kernal density estimates


bb <- st_read(path("temp", "full_breeding_ids_label.gpkg"))


tdfgeo <- bb |>
  filter(argos.lc %in% c(2, 3)) |>
  dplyr::select(tag.id) |>
  as("Spatial")


## 3.1 kde: h reference parameter
# define the parameters (h, kern, grid, extent)

kde_href <- kernelUD(tdfgeo, h = "href", kern = c("bivnorm"), grid = 500, extent = 2)

kde_href

ver95 <- getverticeshr(kde_href, 95) # get vertices for home range
ver95_sf <- st_as_sf(ver95) # convert to sf object

ver75 <- getverticeshr(kde_href, 75)
ver75_sf <- st_as_sf(ver75)

ver50 <- getverticeshr(kde_href, 50)
ver50_sf <- st_as_sf(ver50)

# plot the outputs
mapview(ver50_sf, zcol = "id")
mapview(ver75_sf, zcol = "id")
mapview(ver95_sf, zcol = "id")


## 3.2 kde: Least Squares Cross Validation (lscv) method.

kde_lscv <- kernelUD(tdfgeo, h = "LSCV", kern = c("bivnorm"), grid = 500, extent = 2)

ver95ls <- getverticeshr(kde_lscv, 95) # get vertices for home range
ver95ls_sf <- st_as_sf(ver95ls)

ver50ls <- getverticeshr(kde_lscv, 50)
ver50ls_sf <- st_as_sf(ver50ls)

# plot the outputs
mapview(ver50ls_sf, zcol = "id")

## 3.3 kde: variable smoothing parameters (h)
kde_h1000 <- kernelUD(tdfgeo, h = 1000, kern = c("bivnorm"), grid = 500, extent = 10000)
kde_h500 <- kernelUD(tdfgeo, h = 500, kern = c("bivnorm"), grid = 500, extent = 10)
kde_h3000 <- kernelUD(tdfgeo, h = 3000, kern = c("bivnorm"), grid = 500, extent = 5)
kde_h5000 <- kernelUD(tdfgeo, h = 5000, kern = c("bivnorm"), grid = 500, extent = 5)

# kde - href = 1000
ver95_1000 <- getverticeshr(kde_h1000, 95) # get vertices for home range
ver95_1000_sf <- st_as_sf(ver95_1000) |> mutate(h = 1000) # convert to sf object

# kde - href = 500
ver95_500 <- getverticeshr(kde_h500, 95) # get vertices for home range
ver95_500_sf <- st_as_sf(ver95_500) |>
  mutate(h = 500) # convert to sf object

# kde - href = 3000
ver95_5000 <- getverticeshr(kde_h5000, 95) # get vertices for home range
ver95_3000_sf <- st_as_sf(ver95_3000) |>
  mutate(h = 3000) # convert to sf object

# bind all data together
all_verts <- bind_rows(ver95_1000_sf, ver95_500_sf, ver95_3000_sf)

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
# method 2: recurse packages
# https://cran.r-project.org/web/packages/recurse/vignettes/recurse.html
####################################################################

require(recurse)
require(scales)
library(dplyr)
library(sf)
library(fs)

## test data set
# data(martin)
# plot(martin$x, martin$y, col = viridis_pal()(nrow(martin)), pch = 20,
#      xlab = "x", ylab = "y", asp = 1)
#
# martinvisit = getRecursions(martin, 2)
#
# par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
# plot(martinvisit, martin, legendPos = c(13, -10))
#
# drawCircle(-15, -10, 2)
#
# hist(martinvisit$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 2)")
# summary(martinvisit$revisits)
#
# data(wren)
# animals = rbind(martin, wren)
# plot(animals$x, animals$y, col = c("red", "darkblue")[as.numeric(animals$id)],
#      pch = ".", xlab = "x", ylab = "y", asp = 1)
# popvisit = getRecursions(animals, 2)
#
# head(popvisit$revisitStats)
# plot(popvisit, animals, legendPos = c(15, -10))
#
# visitThreshold = quantile(popvisit$revisits, 0.8)
# popCluster = kmeans(animals[popvisit$revisits > visitThreshold,c("x", "y")], centers = 3)
#
# plot(animals$x, animals$y, col = c("red", "darkblue")[as.numeric(animals$id)],
#      pch = ".", xlab = "x", ylab = "y", asp = 1)
#
# with(animals[popvisit$revisits > visitThreshold,],
#      points(x, y, col = c(alpha("red", 0.5), alpha("darkblue", 0.5))[as.numeric(id)],
#             pch = c(15:17)[popCluster$cluster]) )
# legend("topleft", pch = 15:17, legend = paste("cluster", 1:3), bty = "n")
#
#


## actual data analysis

bb <- st_read(path("temp", "full_breeding_ids_label.gpkg"), quiet = TRUE) |>
  filter(breeding == "y") |>
  filter(tag.id == 260803) |>
  st_transform(bb, crs = 3573)

bb <- cbind(st_coordinates(bb), bb)

bbsp <- bb |>
  sf::st_drop_geometry() |>
  dplyr::filter(argos.lc %in% c(2, 3)) |>
  dplyr::select(X, Y, timestamp, tag.id) |>
  dplyr::mutate(t = lubridate::ymd_hms(timestamp)) |>
  dplyr::select(-timestamp) |>
  mutate(tag.id = as.character(tag.id)) |>
  select(X, Y, t, tag.id)

plot(bbsp$X, bbsp$Y,
  col = viridis_pal()(nrow(bbsp)), pch = 20,
  xlab = "x", ylab = "y", asp = 1
)

bbvisit <- getRecursions(bbsp, 1500)

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(bbvisit, bbsp, legendPos = c(13, -10))

drawCircle(-15, -10, 2)

hist(bbvisit$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 1500m)")
summary(bbvisit$revisits)

head(bbvisit$revisitStats)

# K - means clustering
visitThreshold <- quantile(bbvisit$revisits, 0.8)
popCluster <- kmeans(bbsp[bbvisit$revisits > visitThreshold, c("X", "Y")], centers = 3)


plot(bbsp$X, bbsp$Y, # col = c("red", "darkblue")[as.numeric(bbsp$tag.id)],
  pch = ".", xlab = "x", ylab = "y", asp = 1
)
with(
  bbsp[bbvisit$revisits > visitThreshold, ],
  points(X, Y,
    col = alpha("red", 0.5), # c(alpha("red", 0.5), alpha("darkblue", 0.5))[as.numeric(bbsp$tag.id)],
    pch = c(15:17)[popCluster$cluster]
  )
)
legend("topleft", pch = 15:17, legend = paste("cluster", 1:3), bty = "n")









## Locate Nests using NestR



# Load the package

#remotes::install_github("picardis/nestR", build_vignettes = TRUE, force = TRUE)

# note need to ensure that Jags is also downloaded
# http://www.sourceforge.net/projects/mcmc-jags/files

# file:///C:/Users/genev/AppData/Local/R/win-library/4.4/nestR/doc/nestR.html

# https://github.com/picardis/nestR/blob/master/vignettes/nestR.Rmd
# vignette("nestR")

library(nestR)
library(dplyr)
library(fs)
library(sf)


bb <- st_read(path("temp", "full_breeding_ids_label.gpkg"), quiet = TRUE) |>
  filter(breeding == "y") |>
  filter(tag.id == 260803) |>
  filter(argos.lc %in% c("3", "2")) |>
  dplyr::mutate(date = lubridate::ymd_hms(timestamp)) |>
  mutate(burst = paste0(tag.id, "-", year))

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
  sea_start = "06-01",
  sea_end = "07-05",
  nest_cycle = 34,
  buffer = 1000,
  min_pts = 2,
  min_d_fix = 5,
  min_consec = 2,
  min_top_att = 1,
  min_days_att = 1,
  discard_overlapping = TRUE # when this is FALSE there are many more options
)

ws_output_1

head(ws_output_1$nests)

table(ws_output_1$nests$burst)


# output nests as an sf object
ws_nests <- ws_output_1$nests |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  mutate(nest_id = row_number())

st_write(ws_nests, path("temp", "tag_260803_1000buf_T_nests.gpkg"), delete_dsn = TRUE)




# atempt to find nests

ws_output_1 <- find_nests(
  gps_data = bb,
  sea_start = "06-01",
  sea_end = "07-05",
  nest_cycle = 34,
  buffer = 1000,
  min_pts = 2,
  min_d_fix = 5,
  min_consec = 2,
  min_top_att = 1,
  min_days_att = 1,
  discard_overlapping = TRUE
)

ws_output_1

head(ws_output_1$nests)

table(ws_output_1$nests$burst)


# output nests as an sf object
ws_nests <- ws_output_1$nests |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  mutate(nest_id = row_number())

st_write(ws_nests, path("temp", "tag_260803_1000buf_T_nests.gpkg"), delete_dsn = TRUE)



## Attempt 2 : parameters 

## Step 3: Identifying nests among revisited locations

ws_output_2 <- find_nests(
  gps_data = bb,
  sea_start = "06-01",
  sea_end = "07-05",
  nest_cycle = 34,
  buffer = 500,
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

st_write(ws_nests2, path("temp", "tag_260803_500buf_T_nests.gpkg"), delete_dsn = TRUE)







#####################################################
## Test the one above but with a limited distribution 


bb <- st_read(path("temp", "full_breeding_ids_label.gpkg"), quiet = TRUE) |>
  filter(breeding == "y") |>
  filter(tag.id == 262940) |>
  filter(argos.lc %in% c("3", "2")) |>
  dplyr::mutate(date = lubridate::ymd_hms(timestamp)) |>
  mutate(burst = paste0(tag.id, "-", year))

#st_write(bb, path("temp", "tag_262940.gpkg"), delete_dsn = TRUE)

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








