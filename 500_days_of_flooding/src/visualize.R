make_site_location_map <- function(filename, soi_info, ref_info, proj = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") {
  conus_sf <- st_geometry(st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))) %>% st_transform(proj)
  
  soi_sf <- st_as_sf(soi_info, coords = c("dec_long_va", "dec_lat_va"))
  st_crs(soi_sf) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  soi_sf_proj <- soi_sf %>% st_transform(proj)
  
  ref_site_sf <- st_as_sf(ref_info, coords = c("dec_long_va", "dec_lat_va"))
  st_crs(ref_site_sf) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  ref_site_sf_proj <- ref_site_sf %>% st_transform(proj)
  
  png(filename, width = 800, height = 500)
  par(mar = c(0,0,0,0))
  plot(st_geometry(conus_sf), col = "lightgrey", border = "white")
  plot(st_geometry(soi_sf_proj), add = TRUE, pch = 20, cex = 3, col = "cornflowerblue")
  plot(st_geometry(ref_site_sf_proj), add = TRUE, pch = 20, cex = 1.5, col = "salmon")
  
  # Place text for labels:
  dims <- st_bbox(conus_sf)
  x_pos <- dims$xmin + diff(c(dims$xmin, dims$xmax))*0.95 # Place at 70% along x
  y_pos1 <- dims$ymin + diff(c(dims$ymin, dims$ymax))*0.30
  y_pos2 <- dims$ymin + diff(c(dims$ymin, dims$ymax))*0.25
  text(x_pos, c(y_pos1, y_pos2), labels = c("Site of interest", "Reference sites"), 
       col = c("cornflowerblue", "salmon"), cex = 1.4, font = 2)
  title("Sites used in streamgraph", cex.main = 2, col.main = "#323232", line = -2)
  dev.off()
}

make_streamgraph <- function(soi_data, ref_data) {
  # TODO: ordering so that the soi is always the correct color
  bind_rows(soi_data, ref_data) %>%
    streamgraph(key="site_no", value="max_flood_evt_duration", date="year") %>% 
    sg_fill_manual(c("#F0D830", "#D8F0FF", "#909090", "#C0A890", "#C0C0C0")) # 500 days palette
    # sg_fill_manual(c("#3f5f5c", "#9f817f", "#D8C0C0", "#A8A8A8", "#D8D8D8")) %>% # teal/rose palette
}

save_png <- function(widget, filename) {
  htmltools::html_print(widget) %>%
    normalizePath(.,winslash="/") %>%
    gsub(x=.,pattern = ":/",replacement="://") %>%
    paste0("file:///",.) %>%
    webshot( file = filename, delay = 2 )
}
