do_cloud_moisture_tasks <- function(final_target, dates, projection_str, ...) {
  
  # Define task table rows
  tasks <- tibble(date = dates) %>% mutate(date_str = format(date, "%Y_%m_%d"))
  
  # Define task table columns
  
  fetch_cloudmoisture <- scipiper::create_task_step(
    step_name = 'fetch_cloudmoisture',
    target_name = function(task_name, step_name, ...){
      sprintf("tmp/tmp_fetch/cloud_moisture_%s.nc", task_name)
    },
    command = function(task_name, ...){
      task_info <- filter(tasks, date_str == task_name)
      sprintf("fetch_cloud_moisture(target_name, I('%s'))", task_info$date)
    } 
  )
  
  process_cloud_moisture <- create_task_step(
    step_name = 'process_cloud_moisture',
    target_name = function(task_name, step_name, ...) {
      sprintf("tmp/tmp_process/cloud_moisture_raster_%s.rds", task_name)
    },
    command = function(..., task_name, steps) {
      task_info <- filter(tasks, date_str == task_name)
      psprintf("process_cloud_moisture_raster(", 
               "target_name = target_name,",
               "cloud_data_fn = '%s'," = steps[["fetch_cloudmoisture"]]$target_name,
               "conus_sf = conus_sf,",
               "proj = I('%s'))" = projection_str
      )
    }
  )
  
  visualize_cloud_moisture <- create_task_step(
    step_name = 'visualize_cloud_moisture',
    target_name = function(task_name, step_name, ...) {
      sprintf("tmp/tmp_visualize/cloud_moisture_%s.png", task_name)
    },
    command = function(..., task_name, steps) {
      task_info <- filter(tasks, date_str == task_name)
      psprintf("map_cloud_moisture(", 
               "target_name = target_name,",
               "date = I('%s')," = task_info$date,
               "conus_sf = conus_sf,",
               "clouds_raster_fn = '%s'," = steps[["process_cloud_moisture"]]$target_name,
               "frame_height, frame_width)"
      )
    }
  )
  
  # Create the task plan
  task_plan <- create_task_plan(
    task_names = tasks$date_str,
    task_steps = list(fetch_cloudmoisture, process_cloud_moisture, visualize_cloud_moisture),
    final_steps = c('visualize_cloud_moisture'),
    add_complete = FALSE)
  
  # Create the task remakefile
  task_makefile <- 'cloud_moisture_timelapse_tasks.yml'
  create_task_makefile(
    task_plan = task_plan,
    makefile = task_makefile,
    include = 'remake.yml',
    sources = c(...),
    packages = c('aws.s3', 'dplyr', 'ncdf4', 'raster', 'sf', 'tidyr'),
    final_targets = final_target,
    as_promises = TRUE,
    tickquote_combinee_objects = TRUE)
  
  # Build the tasks
  loop_tasks(task_plan = task_plan,
             task_makefile = task_makefile,
             num_tries = 1,
             n_cores = 1)
  
  # Clean up files created
  
  # Remove the temporary target from remake's DB; it won't necessarily be a unique  
  #   name and we don't need it to persist, especially since killing the task yaml
  scdel(sprintf("%s_promise", basename(final_target)), remake_file=task_makefile)
  # Delete task makefile since it is only needed internally for this function and  
  #   not needed at all once loop_tasks is complete
  file.remove(task_makefile)
  
}
