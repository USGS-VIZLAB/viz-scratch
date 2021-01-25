
create_video <- function(video_filename, png_file_yml, input_framerate, output_framerate, tmp_dir = "tmp") {
  
  if(!dir.exists(tmp_dir)) dir.create(tmp_dir)
  
  png_frames <- names(yaml::yaml.load_file(png_file_yml))
  png_dir <- unique(dirname(png_frames))
  file_name_df <- tibble(origName = png_frames,
                         countFormatted = sprintf("%03.f", 1:length(png_frames)),
                         newName = sprintf("%s/frame_%s.png", png_dir, countFormatted))
  file.rename(from = file_name_df$origName, to = file_name_df$newName)
  
  # added ffmpeg better code for reducing video size
  # see https://unix.stackexchange.com/questions/28803/how-can-i-reduce-a-videos-size-with-ffmpeg
  # and https://slhck.info/video/2017/02/24/crf-guide.html
  
  pre_downscale_fn <- sprintf("%s/pre_downscale_%s", tmp_dir, basename(video_filename))
  
  # Combine frames into video at full resolution
  system(
    sprintf(
      "ffmpeg -y -framerate %s -i %s/frame_%%03d.png -r %s -pix_fmt yuv420p -vcodec libx264 -crf 27 %s",
      input_framerate, png_dir, output_framerate, pre_downscale_fn)
  )
  
  # Downscale to help with resolution when building on Windows
  system(sprintf("ffmpeg -y -i %s -vf scale=iw/2:-1 %s", pre_downscale_fn, video_filename))
  
  file.rename(from = file_name_df$newName, to = file_name_df$origName)
  
}
