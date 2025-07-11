
# test MoveHMM ######################################################################  


# create a MoveHMM object for individual bird
hmm_data <- my_dat |> 
  # convert to UTM for southern baffin island, UTM zone 18N, EPSG 32618
  st_transform(crs = 32618) |>
  mutate(ID = tag.id, 
         timestamp = date_time, 
         x = st_coordinates(geom)[,1], 
         y = st_coordinates(geom)[,2], 
         argos.lc) |>
  # average the locations by timestamp if they have the same quality, if not choose the one with best quality
  arrange(timestamp) |> 
  st_drop_geometry() |> 
  select(ID, timestamp, x, y, argos.lc) |> 
  # normalize dt
  prepData(type = "UTM") |> # calculates step and turning angle
  # since the fix time varies, normalize 
  mutate(dt = as.numeric(difftime(lead(timestamp), timestamp, units = "hours")),
         step = step / dt) |> 
  filter(!is.na(step)) # see mergetime, delete this eventually

View(hmm_data)
# Inspect to get idea of scale
summary(hmm_data$step)
hist(hmm_data$step, breaks = 50)

# Initial parameter guesses for step length distribution:
# gamma parameters: mean, SD -> converted to shape, scale internally
stepPar0 <- c(5, 30, 2, 20)  # means and SDs for states 1 (nest) and 2 (forage)

# Initial guesses for angle distribution: concentration parameter (von Mises)
# Higher = more concentrated near 0 (linear movement), lower = random
anglePar0 <- c(0.5, 2)

fitHMM()

|> 
  select(timestamp, x, y, argos.lc) |> 
  mutate(timestamp = as.POSIXct(timestamp, tz = "UTC")) |> 
  arrange(timestamp)

