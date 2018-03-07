plot_ww_map <- function(filename, us_states, col_sites, watermark_file){
  png(filename = filename, width = 10, height = 6.5, res = 300, units = 'in')
  
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
  
  alpha_hex <- 'CC'
  state_fill <- "#E0DED7" # play w/ this
  shore_border <- "#7A8567"
  plot(us_states, col = state_fill, border = 'white', expandBB = c(0.1, 0, 0, 0))
  
  
  float_states <- us_states[names(us_states) %in% c('HI','PR','AK'), ]
  conus_states <- us_states[!names(us_states) %in% c('HI','PR','AK'), ]
  
  plot(float_states, add = TRUE,
       col = state_fill, border = NA)

  
  non_ww <- is.na(col_sites$col)
  non_cols <- rep(NA, length(col_sites$col))
  non_cols[non_ww] <- paste0('#9f9f9f', alpha_hex)
  
  
  
  plot(col_sites, add = TRUE, col = non_cols, 
       pch = 20, cex = 0.5, lwd=0)
  
  ext_cut <- c(0.1, 0.9)
  med_sites <- col_sites[is.na(col_sites$per) | (col_sites$per < ext_cut[2] & col_sites$per > ext_cut[1]), ]
  
  ext_sites <- col_sites[!is.na(col_sites$per) & (col_sites$per >= ext_cut[2] | col_sites$per <= ext_cut[1]), ]
  
  
  plot(med_sites, add = TRUE, col = med_sites$col,
       bg = ifelse(!is.na(med_sites$col), paste0(med_sites$col, alpha_hex), NA),
       pch = 21, cex = 0.6, lwd = 0.5)
  
  # put extreme sites on the top of the plot z-index:
  plot(ext_sites, add = TRUE, col = ext_sites$col,
       bg = ifelse(!is.na(ext_sites$col), paste0(ext_sites$col, alpha_hex), NA),
       pch = 21, cex = 0.6, lwd = 0.5)
  
  
  coord_space <- par()$usr
  
  # --- watermark ---
  watermark_alpha <- 0.4
  d <- png::readPNG(watermark_file)
  
  which_image <- d[,,4] != 0 # transparency
  d[which_image] <- watermark_alpha
  img_scale <- 300
  
  img_bump <- 60000
  
  x1 <- coord_space[1]+img_bump
  y1 <- coord_space[3]+img_bump
  
  rasterImage(d, x1, y1, x1+ncol(d)*img_scale, y1+nrow(d)*img_scale)
  
  
  # --- legend ---
  
  percs <- seq(0,1, by=0.05)
  
  leg_cols <- sapply(percs, FUN = function(x){
    rgb(col_fun()(x), maxColorValue = 255)
  })
  
  xs <- seq(mean(coord_space[1:2])+120000, by = 80000, length.out = length(leg_cols))
  ys <- rep(coord_space[3]+85000, length(xs))
  
  low_norm <- xs[percs == 0.25]
  mid <- xs[percs == 0.5]
  high_norm <- xs[percs == 0.75]
  
  points(xs, ys, bg = paste0(leg_cols, alpha_hex), col = leg_cols, pch = 21, cex = 1.8, lwd = 1)
  
  lines(c(low_norm, low_norm), c(ys[1]+55000, ys[1]+140000), col = 'black', lwd = 1.4)
  text(mid, ys[1]+100000, 'Normal')
  text(low_norm-298000, ys[1]+100000, 'Lower flows')
  text(high_norm+310000, ys[1]+94000, 'Higher flows')
  lines(c(high_norm, high_norm), c(ys[1]+55000, ys[1]+140000), col = 'black', lwd = 1.4)
  
  dev.off()
  
}

col_fun <- function(){
  colorRamp(c('#ca0020','#f4a582','#efefef','#efefef','#92c5de','#034064'))
}

color_sites <- function(sp_sites, dv_stats){
  sp_site_nms <- sp_sites@data
  
  col_fun <- col_fun()
  sites <- left_join(sp_site_nms, dv_stats)
  rmv.i <- is.na(sites$per)
  sites <- sites[!rmv.i,]
  sites <- sites %>% 
    mutate(col = rgb(col_fun(per), maxColorValue = 255))
  
  sp_sites@data$col <- NA
  sp_sites@data$per <- NA
  
  for (i in 1:length(sites$site_no)){
    #needed to loop to keep these in order? sp...
    which.i <- sp_sites$site_no == sites$site_no[i]
    sp_sites$col[which.i] <- sites$col[i]
    sp_sites$per[which.i] <- sites$per[i]
  }
  
  return(sp_sites)
}
