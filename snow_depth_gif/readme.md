# This code creates a rough gif of snow depth if you provide dates and states

Change the dates used and the states you would like it to be cropped to. The code will download the data, process it, and then plot in a map. To use the functions stored in `fxns.R`, run the following code. This example takes about 20 minutes to run in total.

```r
library(sf)
library(raster)
source("fxns.R")

# The rest of the code operates on whatever you put for `snow_dir` being a directory in your working directory
snow_dir <- "test_snow"
if(!dir.exists(snow_dir)) dir.create(snow_dir)

# 0. Config: Setup the dates to use and the states to crop to
dates_to_see <- seq(as.Date("2021-02-20"), as.Date("2021-03-10"), by = 1)
states_sf <- st_as_sf(maps::map("state", c("ohio", "pennsylvania", "maryland", "new york", "west virginia", "virginia"), plot=FALSE, fill=TRUE))

# 1. Fetch and Process: Download and process data for each day
raster_fn_all <- download_data(dates_to_see, states_sf, snow_dir)

# 2. Visualize: Create individual timestep PNGs of the snow depth maps
frames_all <- create_frames(dates_to_see, raster_fn_all, snow_dir)

# 3. Animate: Create the animation
make_gif("snow_anim.gif", frames_all)
```

![](https://user-images.githubusercontent.com/13220910/113920576-8fef6f00-97aa-11eb-9475-f86cb021b982.gif)

# Further development / considerations

Per Colleen's comments to the original PR, there is more work that can be done to refine this approach. Please read the considerations below before using the code as-is.

1. The scale bar moves throughout the animation, reason is unknown
2. Consider a continuous instead of discrete color scale
3. This is currently set up to use snow depth, but snow-water equivalent (SWE) might be more appropriate for hydrology applications, especially flooding. Consider using SWE instead (this would involve downloading different data, see [Colleen's comment here](https://github.com/USGS-VIZLAB/viz-scratch/pull/69#issuecomment-815992879) for a bit more detail)
