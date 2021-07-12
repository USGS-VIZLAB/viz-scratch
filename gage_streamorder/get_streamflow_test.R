# test script, found at https://usgs-r.github.io/nhdplusTools/reference/get_streamorder.html
source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

test_flowline <- prepare_nhdplus(walker_flowline, 0, 0, FALSE)
#> Warning: removing geometry
#> Warning: Removed 0 flowlines that don't apply.
#>  Includes: Coastlines, non-dendritic paths, 
#> and networks with drainage area less than 0 sqkm, and drainage basins smaller than FALSE

test_flowline <- data.frame(
  ID = test_flowline$COMID,
  toID = test_flowline$toCOMID)

(order <- get_streamorder(test_flowline))
#>  [1] 4 4 4 4 4 3 3 3 3 2 2 2 2 2 2 1 3 3 3 3 2 2 1 1 2 2 1 1 1 2 1 2 1 1 1 1 1 2
#> [39] 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 2

walker_flowline$order <- order

plot(sf::st_geometry(walker_flowline), lwd = walker_flowline$order, col = "blue")


