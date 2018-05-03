
get_state_layout <- function(sp, plot_metadata){
  
  state_bb <- bbox(sp)

  layout_out <- list(figure = list(width = plot_metadata[1], height = plot_metadata[2], res = plot_metadata[3]),
                     map = list(xlim = c(NA_integer_, NA_integer_), ylim = c(NA_integer_, NA_integer_)),
                     legend = list(xpct = NA_integer_, ypct = NA_integer_, box_h = 0.055, y_bump = 0.015,
                                   title_pos = 'top', title = names(sp)))
  aspect_map <- diff(state_bb[c(1,3)])/diff(state_bb[c(2,4)])
  aspect_fig <- plot_metadata[1]/plot_metadata[2]
  
  map_ratio <- aspect_map / aspect_fig
  if (map_ratio < 0.76){ # is "tall"
    x_remain <- 0.69
    x1 <- state_bb[1]
    x2 <- x1 + diff(state_bb[c(1,3)])/x_remain
    y_remain <- 0.94 # to get it above the watermark
    map_span <- diff(state_bb[c(2,4)])/y_remain
    y2 <- state_bb[4]
    y1 <- y2 - map_span
    layout_out$map$ylim <- c(y1, y2)
    layout_out$map$xlim <- c(x1, x2)
    layout_out$legend$xpct <- 0.67 # percentage relative to left for the left edge of the legend
    layout_out$legend$ypct <- 0.40 # percentage relative to bottom for the bottom of the legend
  } else if (map_ratio >= 1.7){ # is wide, and will hug the top of the plot
    y_remain <- 0.6
    map_span <- diff(state_bb[c(2,4)])/y_remain
    y2 <- state_bb[4]
    y1 <- y2 - map_span
    layout_out$map$ylim <- c(y1, y2)
    layout_out$map$xlim <- NULL
    layout_out$legend$xpct <- 0.27 # percentage relative to left for the left edge of the legend
    layout_out$legend$ypct <- 0.015 # percentage relative to bottom for the bottom of the legend
    layout_out$legend$box_h <- 0.048
    layout_out$legend$y_bump <- 0.012
  } else { # is squarish
    y_remain <- 0.7
    map_span <- diff(state_bb[c(2,4)])/y_remain
    y2 <- state_bb[4]
    y1 <- y2 - map_span
    layout_out$map$ylim <- c(y1, y2)
    layout_out$map$xlim <- NULL
    layout_out$legend$xpct <- 0.67 # percentage relative to left for the left edge of the legend
    layout_out$legend$ypct <- 0.015 # percentage relative to bottom for the bottom of the legend
    layout_out$legend$box_h <- 0.045
    layout_out$legend$y_bump <- 0.012
    layout_out$legend$title_pos <- 'left'
  }
  return(layout_out)
}

make_arc <- function(x0, y0, r, from_angle, to_angle){
  theta <- seq(from_angle, to_angle, by = 0.002)
  x_out <- x0 + r*cos(theta)
  y_out <- y0 + r*sin(theta)
  
  return(list(x = x_out, y = y_out))
}


plot_national_pies <- function(us_states, us_counties, us_dots, metadata, watermark_file, filename){
  png(filename, width = metadata[1], height = metadata[2], res=metadata[3], units = 'in')
  par(mai=c(0,0,0,0), omi=c(0,0,0,0)) #, xaxs = 'i', yaxs = 'i'
  
  plot(us_states, col = NA, border = "grey50", lwd = 0.2)

  plot(us_counties, col = "grey90", border = "grey94", lwd = 0.5, add = TRUE)
  
  # don't plot state/terr border if it is a shifted state
  plot(us_states[!names(us_states) %in% c('PR','AK','HI')], col = NA, border = "white", lwd = 0.8, add = TRUE)
  
  #add_watermark(watermark_file)
  dot_to_pie(us_dots)
  dev.off()
}


plot_dot_map <- function(state_sp, county_sp, watermark_file, layout){
  
  
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), bg = '#eaedef') #, xaxs = 'i', yaxs = 'i'
  
  plot(county_sp, col = "white", border = "grey60", lwd=0.75, xlim = layout$map$xlim, ylim = layout$map$ylim) 
  plot(state_sp, col = NA, border = "grey50", lwd = 1.2, add = TRUE)
  add_watermark(watermark_file, layout)
}

calc_frame_filenames <- function(frames, ...){
  cats <- c(...)
  filenames <- rep(NA_character_, (frames-1) * length(cats))
  filenames[seq(1, to = length(filenames), by = frames -1)] <- paste0(cats, "_00.png")
  cats <- c(cats, cats[1L])
  cat_i <- 1
  subframe_i <- 1
  
  for (i in 2:length(filenames)){
    if (is.na(filenames[i])){
      subframe_char <- stringr::str_pad(sprintf('%s', subframe_i), width = 2, pad = "0")
      filenames[i] <- paste(cats[cat_i], cats[cat_i+1], paste0(subframe_char, '.png'), sep = '_', collapse = "")
      subframe_i <- subframe_i + 1
    } else {
      cat_i <- cat_i + 1
      subframe_i <- 1
    }
  }
  return(filenames)
}

build_wu_gif <- function(state_sp, county_sp, dots_sp, state_totals, state_layout, watermark_file, gif_filename, frames = 10, ...){
  
  frame_filenames <- calc_frame_filenames(frames, ...)
  
  trans_delay <- "10"
  pause_delay <- "180"
  
  gifsicle_out <- c('')
  temp_dir <- tempdir()
  frame_num <- 0
  legend_cats <- legend_categories()
  for (filename in frame_filenames){
    
    base_map_plot(state_sp, county_sp, state_layout, watermark_file, file.path(temp_dir, filename))
    
    basefile <- strsplit(filename, '[.]')[[1]][1]
    file_pieces <- strsplit(basefile, "[_]")[[1]]
    frame <- as.numeric(tail(file_pieces, 1L)) + 1
    
    if (grepl("pie_", x = basefile)){ # case where we transition to/from pie:
      cat_to_i <- 2L
      if (grepl("_pie_",  x = basefile)){ # to pie, special case of reversing the order
        
        frame <- frames - frame + 1
        cat_to_i <- 1L
      }
      if (length(file_pieces) == 2){
        # is a "PAUSE" frame for pie charts
        dot_to_pie(dots_sp)
        add_legend(legend_cats, state_totals, layout = state_layout)
        gifsicle_out <- paste0(gifsicle_out, sprintf('-d%s "#%s" ', pause_delay, frame_num))
      } else {
        cat_to <- file_pieces[cat_to_i]
        plot_pie_transitions(dots_sp, cat_to, frames, frame, state_totals, layout = state_layout)
        gifsicle_out <- paste0(gifsicle_out, sprintf('-d%s "#%s" ', trans_delay, frame_num))
      }
      
    } else {
      if (length(file_pieces) == 2){
        # is a "PAUSE" frame for a category
        cat <- file_pieces[1]
        dot_to_circle(dots_sp, cat = cat, col = cat_col(cat))
        
        cat_frames <- rep(frames, length(legend_cats))
        cat_frames[legend_cats == cat] <- 1
        add_legend(legend_cats, state_totals, frame = cat_frames, frames = frames, layout = state_layout)
        gifsicle_out <- paste0(gifsicle_out, sprintf('-d%s "#%s" ', pause_delay, frame_num))
      } else {
        plot_dot_transitions(dots_sp, file_pieces[1], file_pieces[2], frames, frame, state_totals, layout = state_layout)
        gifsicle_out <- paste0(gifsicle_out, sprintf('-d%s "#%s" ', trans_delay, frame_num))
      }
    }
    frame_num <- frame_num + 1
    dev.off()
    
  }
  
  system(paste0("convert -loop 0 -delay 15 ", paste(file.path(temp_dir, frame_filenames), collapse = " "), " ", gif_filename))
  
  system(sprintf('gifsicle -b %s %s --colors 256', gif_filename, gifsicle_out))
}

categories <- function(){
  c("irrigation", "industrial", "thermoelectric", "publicsupply")
}

legend_categories <- function(){
  c("other", rev(categories()))
}
cat_title <- function(cat){
  titles <- c("irrigation" = "Irrigation", "industrial"="Industrial", 
            "thermoelectric"="Thermoelectric", "publicsupply"="Public supply", "other"="Other")
  titles[[cat]]
}

cat_col <- function(cat){
  cols <- c("irrigation" = "#59a14f", "industrial"="#e15759", 
            "thermoelectric"="#edc948", "publicsupply"="#76b7b2", "other"="#A9A9A9", 
            "dead"='#dcdcdc', 'text'= '#A9A9A9')
  cols[[cat]]
}

fill_col <- function(col){
  paste0(col, 'CC')
}

base_map_plot <- function(state_sp, county_sp, layout, watermark_file, filename){
  png(filename, width = layout$figure$width, height = layout$figure$height, res=layout$figure$res, units = 'in')
  plot_dot_map(state_sp, county_sp, watermark_file, layout)
}

plot_dot_transitions <- function(dots_sp, cat1, cat2, frames = 5, frame, state_totals, layout){
  legend_cats <- legend_categories()
  col1 <- cat_col(cat1)
  col2 <- cat_col(cat2)
  cols <- colorRampPalette(c(col1, col2))(frames)
  interp_vals <- seq(1,0, length.out = frames)
  
  tmp_dots <- dots_sp
  tmp_dots[[cat1]] <- (interp_vals[frame]*sqrt(tmp_dots[[cat1]])+ (1 - interp_vals[frame])*sqrt(tmp_dots[[cat2]]))^2
  dot_to_circle(tmp_dots, cat1, cols[frame])
  cat_frames <- rep(frames, length(legend_cats))
  cat_frames[legend_cats == cat1] <- frame
  cat_frames[legend_cats == cat2] <- frames - frame + 1
  add_legend(legend_cats, state_totals, frame = cat_frames, frames = frames, layout = layout)
}

plot_pie_transitions <- function(dots_sp, cat_to, frames = 5, frame, state_totals, layout){
  
  
  interp_vals <- seq(1,0, length.out = frames)
  all_cats <- categories()
  cat_aways <- all_cats[!all_cats %in% cat_to]
  
  tmp_dots <- dots_sp
  
  total_now <- (interp_vals[frame]*sqrt(tmp_dots$total)+ (1 - interp_vals[frame])*sqrt(tmp_dots[[cat_to]]))^2
  total_diff <- dots_sp$total - (total_now - dots_sp[[cat_to]])
  
  
  tmp_dots$total <- total_now
  for (cat in cat_aways){
    orig_slice <- dots_sp$total - dots_sp[[cat_to]]
    new_slice <- total_now - dots_sp[[cat_to]]
    denom <- (dots_sp$total-dots_sp[[cat_to]])
    slice_frac <- ifelse(denom>0, dots_sp[[cat]]/denom, 0)
    tmp_dots[[cat]] <- dots_sp[[cat]] - (orig_slice - new_slice)*slice_frac
  }
  
  dot_to_pie(tmp_dots)
  legend_cats <- legend_categories()
  cat_frames <- rep(frame, length(legend_cats))
  cat_frames[legend_cats == cat_to] <- 1
  add_legend(legend_cats, state_totals, frame = cat_frames, frames = frames, layout = layout)
  
}

add_legend <- function(categories, state_totals, frame = rep(1, length(categories)), frames = 5, layout){
  alpha_hex <- rev(c("00", "1A", "33", "4D", "66", "80", "99", "B3", "CC", "E6", "FF"))
  
  this_legend <- layout$legend
  coord_space <- par()$usr
  plot_width <- diff(coord_space[c(1,2)])
  plot_height <- diff(coord_space[c(3,4)])
  strt_x <- coord_space[1]+plot_width*this_legend$xpct
  strt_y <- coord_space[3]+plot_height*this_legend$ypct
  box_w <- plot_width*0.32
  box_h <- plot_height*this_legend$box_h
  y_bump <- plot_height*this_legend$y_bump
  text_st <- 0
  cat_nums <- sapply(categories, function(x)as.numeric(gsub(",", "", state_totals[[x]])))
  sorted_idx <- sort(cat_nums, index.return = TRUE)$ix
  categories <- categories[sorted_idx]
  frame <- frame[sorted_idx]
  for (cat in categories){
    this_frame <- frame[cat == categories]
    this_width <- box_w - (this_frame-1)/frames * plot_width*0.015
    border <- colorRampPalette(c(cat_col(cat), cat_col('dead')))(frames) [frame[cat == categories]]
    text_col <- colorRampPalette(c("black", cat_col('text')))(frames) [frame[cat == categories]]
    num_col <- paste0("#000000", alpha_hex[ceiling(this_frame/frames*length(alpha_hex))])
    
    polygon(c(strt_x, strt_x+this_width, strt_x+this_width, strt_x, strt_x), 
            c(strt_y, strt_y, strt_y+box_h, strt_y+box_h, strt_y), 
            col = fill_col(border), 
            border = border,
            lwd=0.75)
    text(x = strt_x+text_st, y = strt_y+box_h/2.2, labels = cat_title(cat), cex = 1.0, pos = 4, col = text_col)
    text(x = strt_x+this_width, y = strt_y+box_h/2.2, labels = state_totals[[cat]], cex = 1.0, pos = 2, col = num_col)
    strt_y <- strt_y+y_bump+box_h
  }
  if (this_legend$title_pos == 'top'){
    text(x = strt_x+box_w/2, y = strt_y+box_h, labels = simpleCap(this_legend$title), cex = 1.2)
    text(x = strt_x+box_w/2, y = strt_y+box_h/4, labels = "(water withdrawals, million gallons per day)", cex = 0.7)
  } else if (this_legend$title_pos == 'left'){
    text(x = coord_space[1]+plot_width*0.45, y = strt_y-box_h, labels = simpleCap(this_legend$title), cex = 1.4)
    text(x = coord_space[1]+plot_width*0.45, y = strt_y-2*box_h, labels = "(water withdrawals, million gallons per day)", cex = 0.7)
  } else {
    stop(this_legend$title_pos, ' not supported')
  }
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

scale_const <- 1050

dot_to_circle <- function(dots, cat = 'total', col){
  
  all_cats <- categories()
  for (j in seq_len(length(dots))){
    dot <- dots[j, ]
    r <- sqrt(dot[[cat]]) * scale_const
    plot_slice(dot@coords[1], dot@coords[2],r, 0, 2*pi, col = col)
  }
}

dot_to_pie <- function(dots){
  
  categories <- categories()
  
  for (j in seq_len(length(dots))){
    
    dot <- dots[j, ]
    r <- sqrt(dot$total) * scale_const
    
    c.x <- dot@coords[1]
    c.y <- dot@coords[2]
    
    #stole code from water-use-15
    for (cat in categories){
      cat_angle <- dot[[cat]] / dot[['total']]*2*pi
      if (is.infinite(cat_angle)){
        cat_angle <- NA # happens with some really small categories
      }
      if (cat == head(categories, 1L)){
        # start the first category mirror relative to the top
        angle_from <- pi/2 - cat_angle/2
        orig_ang <- angle_from
      } else {
        angle_from <- angle_to
      }
      angle_to <- angle_from + ifelse(is.na(cat_angle), 0, cat_angle)
      if (!is.na(cat_angle) & cat_angle > 0.01){
        plot_slice(c.x, c.y, r = r, angle_from, angle_to, cat)
      }
    }
    
    if (r > 0 & !is.na(r) & cat == tail(categories, 1L) & angle_to < 2*pi + orig_ang){
      plot_slice(c.x, c.y, r = r, angle_to, 2*pi + orig_ang, 'other')
    }
  }
}

plot_slice <- function(x,y,r,angle_from, angle_to, cat, col = NULL){
  segments <- make_arc(x, y, r = r, angle_from, angle_to)
  if (is.null(col)){
    col <- cat_col(cat)
  }
  polygon(c(x, segments$x, x), c(y, segments$y, y), 
          border = NA,
          col = fill_col(col))
  lines(segments$x, segments$y, lwd=0.4, col = col)
}

add_watermark <- function(watermark_file, layout, ...){
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
  
  if (layout$legend$title_pos == 'left'){
    text(coord_space[1]+diff(coord_space[c(1,2)])*0.40, y1+coord_height*watermark_bump_frac, 'https://owi.usgs.gov/vizlab/water-use-15/', cex = 0.8, col = 'grey50')
  } else{
    text(coord_space[2], y1+coord_height*watermark_bump_frac, 'https://owi.usgs.gov/vizlab/water-use-15/', pos = 2, cex = 0.8, col = 'grey50')
  }
  
}
