

make_arc <- function(x0, y0, r, from_angle, to_angle){
  theta <- seq(from_angle, to_angle, by = 0.002)
  x_out <- x0 + r*cos(theta)
  y_out <- y0 + r*sin(theta)
  
  return(list(x = x_out, y = y_out))
}

plot_dot_map <- function(state_sp, county_sp, watermark_file){
  
  
  par(mai=c(0,0,0,0), omi=c(0,0,0,0)) #, xaxs = 'i', yaxs = 'i'
  
  plot(county_sp, col = "grey97", border = "grey70")
  plot(state_sp, col = NA, border = "black", lwd = 1.2, add = TRUE)
  add_watermark(watermark_file)
}

cat_col <- function(cat){
  cols <- c("irrigation" = "#59a14f", "industrial"="#e15759", 
            "thermoelectric"="#edc948", "publicsupply"="#76b7b2", "other"="#A9A9A9")
  cols[[cat]]
}
plot_dot_transitions <- function(state_sp, county_sp, dots_sp, metadata, watermark_file, filename, cat1, cat2, frames = 5){
  
  col1 <- cat_col(cat1)
  col2 <- cat_col(cat2)
  cols <- colorRampPalette(c(col1, col2))(frames)
  interp_vals <- seq(1,0, length.out = frames)
  for (i in 1:frames){
    file_parts <- strsplit(filename, '[.]')[[1]]
    this_filename <- paste0(substr(file_parts[1], start = 1, stop = nchar(file_parts[1])-1), 
                            i-1, '.', file_parts[2]) # hack alert!
    
    png(this_filename, width = metadata[1], height = metadata[2], res=metadata[3], units = 'in')
    plot_dot_map(state_sp, county_sp, watermark_file)
    tmp_dots <- dots_sp
    tmp_dots[[cat1]] <- (interp_vals[i]*sqrt(tmp_dots[[cat1]])+ (1 - interp_vals[i])*sqrt(tmp_dots[[cat2]]))^2
    dot_to_circle(tmp_dots, cat1, cols[i])
    dev.off()
  }
  
  dev.off()
}

plot_pie_transitions <- function(state_sp, county_sp, dots_sp, metadata, watermark_file, filename, cat_to, frames = 5){
  
  
  interp_vals <- seq(1,0, length.out = frames)
  all_cats <- c("irrigation", "industrial", "thermoelectric", "publicsupply")
  cat_aways <- all_cats[!all_cats %in% cat_to]
  
  for (i in 1:frames){
    
    file_parts <- strsplit(filename, '[.]')[[1]]
    this_filename <- paste0(substr(file_parts[1], start = 1, stop = nchar(file_parts[1])-1), 
                            i-1, '.', file_parts[2]) # hack alert!
    
    png(this_filename, width = metadata[1], height = metadata[2], res=metadata[3], units = 'in')
    plot_dot_map(state_sp, county_sp, watermark_file)
    tmp_dots <- dots_sp
    
    total_now <- (interp_vals[i]*sqrt(tmp_dots$total)+ (1 - interp_vals[i])*sqrt(tmp_dots[[cat_to]]))^2
    total_diff <- dots_sp$total - (total_now - dots_sp[[cat_to]])
    
    
    tmp_dots$total <- total_now
    for (cat in cat_aways){
      orig_slice <- dots_sp$total - dots_sp[[cat_to]]
      new_slice <- total_now - dots_sp[[cat_to]]
      slice_frac <- dots_sp[[cat]]/(dots_sp$total-dots_sp[[cat_to]])
      tmp_dots[[cat]] <- dots_sp[[cat]] - (orig_slice - new_slice)*slice_frac
    }
    
    if (i == frames){
      dot_to_circle(dots_sp, cat_to, border = cat_col(cat_to))
    } else {
      dot_to_pie(tmp_dots)
    }
    
    dev.off()
  }
  
  dev.off()
}

dot_to_circle <- function(dots, cat = 'total', border = '#6495ED', col = paste0(border, 'CC')){
  scale_const <- 900
  for (j in seq_len(length(dots))){
    dot <- dots[j, ]
    r <- sqrt(dot[[cat]]) * scale_const
    
    circle_data <- make_arc(dot@coords[1], dot@coords[2], r, 0, 2*pi)
    polygon(circle_data$x, circle_data$y, col=col, border = border)
  }
  
}

dot_to_pie <- function(dots){
  
  scale_const <- 900
  
  categories <- c("irrigation","industrial", "thermoelectric", "publicsupply")
  
  for (j in seq_len(length(dots))){
    
    transparency_alpha <- 'CC'
    dot <- dots[j, ]
    r <- sqrt(dot$total) * scale_const
    
    c.x <- dot@coords[1]
    c.y <- dot@coords[2]
    
    #stole code from water-use-15
    for (cat in categories){
      cat_angle <- dot[[cat]] / dot[['total']]*2*pi
      if (cat == head(categories, 1L)){
        # start the first category mirror relative to the top
        angle_from <- pi/2 - cat_angle/2
        orig_ang <- angle_from
      } else {
        angle_from <- angle_to
      }
      angle_to <- angle_from + cat_angle
      if (!is.na(cat_angle) & cat_angle > 0.01){
        segments <- make_arc(c.x, c.y, r = r, angle_from, angle_to)
        polygon(c(c.x, segments$x, c.x), c(c.y, segments$y, c.y), 
                border = NA,
                col = paste0(cat_col(cat),transparency_alpha))
        lines(segments$x, segments$y, lwd=0.75, col = cat_col(cat))
      }
    }
    if (!is.na(r) & cat == tail(categories, 1L) & angle_to < 2*pi + orig_ang){
      
      segments <- make_arc(c.x, c.y, r = r, angle_to, 2*pi + orig_ang)
      polygon(c(c.x, segments$x, c.x), c(c.y, segments$y, c.y), 
              border = NA,
              col = paste0(cat_col('other'),transparency_alpha))
      lines(segments$x, segments$y, lwd=0.75, col = cat_col('other'))
    }
  }
}

add_watermark <- function(watermark_file,...){
  # --- watermark ---
  watermark_frac <- 0.2 # fraction of the width of the figure
  watermark_bump_frac <- 0.01
  coord_space <- par()$usr
  
  watermark_alpha <- 0.4
  d <- png::readPNG(watermark_file)
  
  which_image <- d[,,4] != 0 # transparency
  d[which_image] <- watermark_alpha
  
  coord_width <- coord_space[2]-coord_space[1]
  coord_height <- coord_space[4]-coord_space[3]
  watermark_width <- dim(d)[2]
  img_scale <- coord_width*watermark_frac/watermark_width
  
  x1 <- coord_space[1]+coord_width*watermark_bump_frac
  y1 <- coord_space[3]+coord_height*watermark_bump_frac
  
  rasterImage(d, x1, y1, x1+ncol(d)*img_scale, y1+nrow(d)*img_scale)
  
  text(coord_space[2], y1+coord_height*watermark_bump_frac, 'https://owi.usgs.gov/vizlab/water-use-15/', pos = 2, cex = 0.8, col = 'grey50')
}