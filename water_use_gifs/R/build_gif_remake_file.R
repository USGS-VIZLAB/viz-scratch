library(scipiper)
library(dplyr)
normalize_state_name <- function(polynames){
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  simpleCap(strsplit(polynames[1], '[:]')[[1]][1])
}
task_config <- maps::state.fips %>% 
  group_by(abb) %>% summarise(state_name = normalize_state_name(polyname)) %>% 
  mutate(id = abb) %>% select(id, state_name) %>% 
  as.data.frame(stringsAsFactors = FALSE)

step1 <- create_task_step(
  step_name = 'proj',

  command = function(task_name, step_name, ...) {
    state_name <- task_config[task_config$id == task_name, 'state_name']
    sprintf("get_proj(proj_data, I('%s'))", state_name)
  }
)
step2 <- create_task_step(
  step_name = 'counties',
  command = function(target_name, task_name, ...) {
    state_name <- task_config[task_config$id == task_name, 'state_name']
    sprintf("to_sp(I('county'), I('%s'), proj.string = %s_proj)", state_name, task_name)
  }
)
step3 <- create_task_step(
  step_name = 'state',
  command = function(target_name, task_name, ...) {
    state_name <- task_config[task_config$id == task_name, 'state_name']
    sprintf("to_sp(I('state'), I('%s'), proj.string = %s_proj)", state_name, task_name)
  }
)
step4 <- create_task_step(
  step_name = 'state_totals',
  command = function(target_name, task_name, ...) {
    state_name <- task_config[task_config$id == task_name, 'state_name']
    sprintf("get_state_totals('../../water-use-15/cache/wu_state_data.json', I('%s'))", state_name)
  }
)
step5 <- create_task_step(
  step_name = 'plot_layout',
  command = function(target_name, task_name, ...) {
    sprintf("get_state_layout(%s_state, plot_metadata)", task_name)
  }
)
step6 <- create_task_step(
  step_name = 'county_dots',
  command = function(target_name, task_name, ...) {
    sprintf("get_state_dots('../../water-use-15/cache/county_centroids_USA.json', 
      '../../water-use-15/cache/county_centroids_wu.tsv',
      %s_proj, %s_state_totals)", task_name, task_name)
  }
)
step7 <- create_task_step(
  step_name = 'water_use',
  target = function(task_name, step_name, ...) {
    sprintf('figures/%s_%s.gif', task_name, step_name)
  },
  
  command = function(target_name, task_name, ...) {
    sprintf("build_wu_gif(%s_state, %s_counties, %s_county_dots, %s_state_totals, %s_plot_layout, 
      'images/usgs_logo_black.png', 
      target_name, frames = I(5), I('pie'), I('irrigation'), I('thermoelectric'), I('publicsupply'))", 
            task_name, task_name, task_name, task_name, task_name)
  }
)


task_plan <- create_task_plan(task_config$id, list(step1, step2, step3, step4, step5, step6, step7),
                              final_steps='water_use', ind_dir='gif_ind/log')
task_makefile <- create_task_makefile(
  task_plan, makefile='remake.yml',
  sources = c('R/map_utils.R','R/data_utils.R','R/draw_utils.R'),
  include = 'gif_globals.yml',
  file_extensions=c('ind'), 
  packages=c('sp','maps','maptools','rgeos','readr','stringr','dataRetrieval','lubridate','dplyr','mapdata','jsonlite'))