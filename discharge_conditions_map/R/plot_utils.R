plot_ww_map <- function(filename, us_states, col_sites){
  png(filename = filename, width = 10, height = 6, res = 300, units = 'in')
  
  par(mai=c(0,0,0,0), omi=c(0,0,0,0), xaxs = 'i', yaxs = 'i')
  
  alpha_hex <- 'CC'
  state_fill <- "#E0DED7" # play w/ this
  shore_border <- "#7A8567"
  plot(us_states, col = state_fill, border = 'white')
  
  
  float_states <- us_states[names(us_states) %in% c('HI','PR','AK'), ]
  conus_states <- us_states[!names(us_states) %in% c('HI','PR','AK'), ]
  
  plot(float_states, add = TRUE,
       col = state_fill, border = NA)

  
  non_ww <- is.na(col_sites$col)
  non_cols <- rep(NA, length(col_sites$col))
  non_cols[non_ww] <- paste0('#888888', alpha_hex)
  
  
  
  plot(col_sites, add = TRUE, col = non_cols, 
       pch = 20, cex = 0.5, lwd=0)
  
  med_sites <- col_sites[is.na(col_sites$per) | (col_sites$per < 0.9 & col_sites$per > 0.1), ]
  
  ext_sites <- col_sites[!is.na(col_sites$per) & (col_sites$per >= 0.9 | col_sites$per <= 0.1), ]
  
  
  plot(med_sites, add = TRUE, col = med_sites$col,
       bg = ifelse(!is.na(med_sites$col), paste0(med_sites$col, alpha_hex), NA),
       pch = 21, cex = 0.6, lwd = 0.5)
  
  # put extreme sites on the top of the plot z-index:
  plot(ext_sites, add = TRUE, col = ext_sites$col,
       bg = ifelse(!is.na(ext_sites$col), paste0(ext_sites$col, alpha_hex), NA),
       pch = 21, cex = 0.6, lwd = 0.5)
  
  dev.off()
  
}


color_sites <- function(sp_sites, dv_stats){
  sp_site_nms <- sp_sites@data
  
  col_fun <- colorRamp(c('#ca0020','#f4a582','#f7f7f7','#92c5de','#034064'))
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
