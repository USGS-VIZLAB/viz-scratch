
plot_ice_frame <- function(filename, ice_data){
  
  png(filename, width = 1080, height = 1350, units = 'px')
  font_add_google('Abel', "abel")
  par(omi = c(0.6,0,0.1,0.1), mai = c(0.5,1,1.2,0), las = 1, mgp = c(2,1,0), cex.axis = 4, cex = 2, family = "abel", xpd = TRUE)
  showtext_begin()
  layout(matrix(c(1,1,1,1,1,1,1,2)))
  
  ice_data <- mutate(ice_data, n_days = as.numeric(DAYS))
  shortest5 <- ice_data %>% filter(!is.na(n_days)) %>% arrange(n_days) %>% pull(n_days) %>% head(10) %>% tail(1)
  longest5 <- ice_data %>% filter(!is.na(n_days)) %>% arrange(desc(n_days)) %>% pull(n_days) %>% head(10) %>% tail(1)
  
  ice_data <- ice_data %>% mutate(col = ifelse(n_days <= shortest5, '#E76F5166', ifelse(n_days >= longest5, '#2b83ba66', NA)))
  
  # the year being plotted:
  this_date <- tail(strsplit(filename,'[_]')[[1]],1) %>% strsplit(., split = '[.]') %>% .[[1]] %>% .[1] %>% as.Date(., format("%Y%m%d"))
  
  all_centered <- seq(as.Date('1855-01-01'), to = as.Date('2018-01-01'), by = 'years')
  belongs_to_yr <- all_centered[which.min(abs(this_date - all_centered))]
  
  
  plot(c(NA, 2020), c(0,NA), xlim = c(1854, 2019.5), ylim = c(18,165), xaxs = 'i', yaxs = 'i', ylab = "", xlab = "", axes = FALSE)
  text(1844, 173, 'Lake Mendota: A History of Ice', cex = 6, font = 2, pos = 4)
  text(1844, 168, '# Days of ice cover', cex = 3.5, pos = 4)
  
  par(xpd = FALSE)
  
  axis(2, at = seq(0, to = 200, by = 20), lwd = 2.5)
  par(mgp = c(2,3,0))
  axis(1, at = seq(1840, to = 2020, by = 20), lwd = 2.5)
  
  
  
  plot_yrs <- all_centered[all_centered < belongs_to_yr] %>% format('%Y') %>% as.numeric()
  
  # make background rectangles:
  for (year in plot_yrs){
    this_year <- filter(ice_data, start == year, !is.na(n_days))
    year_col <- this_year$col
    rect(xleft = year+0.5, ybottom = 18, xright = year+1.5, ytop = 170, col = year_col, border = NA)
  }
  # plot dots on top:
  for (year in plot_yrs){
    this_year <- filter(ice_data, start == year, !is.na(n_days))
    year_col <- this_year$col
    points(year+1, this_year$n_days, pch = 21, col = '#75a2a4', bg = '#75a2a4', lwd = 4, cex = 4) #col = year_col,
  }
  
  
  # plot previous years:
  years <- rev(tail(plot_yrs, 5))
  
  #100, 75, 60, 50, 45 
  col_suf <- c("FF","BF","99","80", "73")
  for (year in years){
    this_year <- filter(ice_data, start == year, !is.na(n_days))
    year_col <- this_year$col
    if (!is.na(year_col)){
      year_col <- substr(year_col, start = 1, stop = 7)
      rect(xleft = year+0.5, ybottom = 18, xright = year+1.5, ytop = 170, col = paste0(year_col, col_suf[1]), border = NA)
      
    }
    col_suf <- tail(col_suf, -1L)
  }
  
  col_suf <- c("FF","BF","99","80", "73")
  cexs <- c(7,6,5.5,5,4.5)
  for (year in years){
    this_year <- filter(ice_data, start == year, !is.na(n_days))
    year_col <- this_year$col
    if (!is.na(year_col)){
      year_col <- substr(year_col, start = 1, stop = 7)
      points(year+1, this_year$n_days, pch = 21, col = paste0(year_col, col_suf[1]), 
             bg = paste0(year_col, col_suf[1]), cex = cexs[1])  
    }
    col_suf <- tail(col_suf, -1L)
    cexs <- tail(cexs, -1L)
  }
  
  
  
  
  ice_blocks <- filter(ice_data, abs(as.numeric(close_date-belongs_to_yr)) < 100, abs(as.numeric(open_date-belongs_to_yr)) < 150)
  
  # new_mai <- current_mai <- par()$mai
  # new_mai[3] <- 0.5
  # par(mai = new_mai)
  xlim <- c(belongs_to_yr-50, belongs_to_yr+140)
  par(mai = c(0.6, 0.1, 0.85, 0))
  plot(0, NA, xlim = xlim, 
       ylim = c(0,1.48), xaxs = 'i', yaxs = 'i', ylab = "", xlab = "", axes = FALSE, xpd = FALSE)
  
  # start ticks Nov 1
  tick_spots <- seq(belongs_to_yr-61, by = 'month', length.out = 12)
  tick_spots[5] <- tick_spots[4]+29
  tick_spots[6] <- tick_spots[5]+31
  tick_spots[7] <- tick_spots[6]+30
  tick_lab <- format(tick_spots, '%b')
  tick_lab[3] <- paste0(tick_lab[3],'-    ')
  axis(1, at = tick_spots, labels = tick_lab, tick = TRUE, lwd = 2.5)
  axis(1, at = belongs_to_yr+6.5, labels = sprintf("%s", format(belongs_to_yr, '%Y')), 
       tck = 0, lwd = NA, cex.axis = 3.5, font = 2)
  
  
  for (j in 1:nrow(ice_blocks)){
    this_block <- ice_blocks[j, ]
    rect(xleft = this_block$close_date, ybottom = 0.2, xright = this_block$open_date, 1.2, 
         col = '#82B4B7') #4392F1
    rect(xleft = this_block$close_date, ybottom = 0.2, xright = this_block$open_date, 1.2, 
         col = 'white', density = 10, lwd = 3)#4392F1
  }
  par(xpd = TRUE)
  text(belongs_to_yr-48, 1.55, 'Ice duration', cex = 4, pos = 4)
  mtext(text = 'Data from Wisconsin State Climatology Office; blue bars are 10 years with longest ice-cover, red bars are the shortest 10', 
        side = 1, line = 3, at = belongs_to_yr+45, cex = 1.9, padj = 2.2, col = 'grey40')
  showtext_end()
  dev.off()
}

create_frame_plan <- function(dates){
  step1 <- create_task_step(
    step_name = 'plot',
    target_name = function(task_name, step_name, ...) {
      sprintf('figures/mendota_ice_%s101.png', task_name)
    },
    command = "plot_ice_frame(target_name, ice_data)"
  )
  task_plan <- create_task_plan(as.character(dates), list(step1),
                                final_steps='plot', add_complete = FALSE)
  return(task_plan)
}

combine_frames <- function(filename, ...){
  
  frame_names <- c(...)
  # display the last frame first
  collapse_frames <- paste(c(tail(frame_names,1), frame_names), collapse = ' ')
  
  system(sprintf('convert -delay 15 %s %s', collapse_frames, filename))
  reg_frames <- paste(sprintf('"#%s"', 1:(length(frame_names)-1)), collapse = ' ')
  system(sprintf('gifsicle -b -O3 %s -d0 "#0" -d14 %s -d400 "#%s" --colors 256', filename, reg_frames, length(frame_names)-1))
  
}