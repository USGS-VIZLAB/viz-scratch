

make_arc <- function(x0, y0, r, from_angle, to_angle){
  theta <- seq(from_angle, to_angle, by = 0.002)
  x_out <- rep(NA, length.out = length(theta))
  y_out <- rep(NA, length.out = length(theta))
  for (i in 1:length(theta)){
    x_out[i] = x0 + r*cos(theta[i])
    y_out[i] = y0 + r*sin(theta[i])
  }
  return(list(x = x_out, y = y_out))
}


plot_state_map <- function(state_sp, county_sp, dots_sp, metadata, watermark_file, filename){
  
  png(filename, width = metadata[1], height = metadata[2], res=metadata[3], units = 'in')
  
  par(mai=c(0,0,0,0), omi=c(0,0,0,0)) #, xaxs = 'i', yaxs = 'i'
  
  plot(county_sp, col = NA, border = "grey60")
  plot(state_sp, col = NA, border = "black", lwd = 1.2, add = TRUE)
  
  plot(dots_sp, add = TRUE, col = 'red', pch = 20, cex = 3)
  dot_to_circle(dots_sp)
  add_watermark(watermark_file)
  
  
  dev.off()
}

dot_to_circle <- function(dots){
  dots_sp
}

add_watermark <- function(watermark_file, ...){
  # --- watermark ---
  coord_space <- par()$usr
  watermark_alpha <- 0.4
  d <- png::readPNG(watermark_file)
  
  which_image <- d[,,4] != 0 # transparency
  d[which_image] <- watermark_alpha
  img_scale <- 160
  
  img_bump <- 10000
  
  x1 <- coord_space[1]+img_bump
  y1 <- coord_space[3]+img_bump
  
  rasterImage(d, x1, y1, x1+ncol(d)*img_scale, y1+nrow(d)*img_scale)
  
  text(coord_space[2], coord_space[3]+img_bump*2, 'https://owi.usgs.gov/vizlab/water-use-15/', pos = 2, cex = 0.8)
}