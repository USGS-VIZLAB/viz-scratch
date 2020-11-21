
library(sf)
source("shift_oconus_states/map_shift_utils.R")

# Projection to use
proj_albers <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

# State shifting information
shift_details_sf <- list(AK = list(obj_sf = to_sf("world", "USA:alaska", projection = proj_albers), 
                                   scale = 0.31, shift = c(90,-460), rotation_deg = -50),
                         HI = list(obj_sf = to_sf("world", "USA:hawaii", projection = proj_albers),
                                   scale = 1, shift = c(520, -110), rotation_deg = -35),
                         PR = list(obj_sf = to_sf("world", "Puerto Rico", projection = proj_albers),
                                   scale = 2.3, shift = c(-140, 90), rotation_deg=20))

# Using the code to shift states and add to CONUS map
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
plot(st_geometry(st_transform(st_as_sf(maps::map("usa", fill=TRUE, plot = FALSE)), proj_albers)))
purrr::map(shift_details_sf, function(obj_ls) {
  obj_shifted <- do.call(shift_sf, c(obj_ls, projection = proj_albers))
  plot(st_geometry(obj_shifted), add = TRUE)
})
