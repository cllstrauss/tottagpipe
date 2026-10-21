#' @export
data_pipeline <- function(folder_path, age,
                          false_movement = 5, zero_ranging = 1, lag_diff_0 = 1,
                          clean_data = TRUE, device_dates, out_folder, out_folder_clean) {

  if (length(folder_path) > 1) {

    results <- lapply(
      folder_path,
      function(path) {
        data_pipeline(
          folder_path = path,
          age = age,
          false_movement = false_movement,
          zero_ranging = zero_ranging,
          lag_diff_0 = lag_diff_0,
          device_dates = device_dates,
          clean_data = clean_data,
          out_folder = out_folder,
          out_folder_clean = out_folder_clean)})
    return(invisible(NULL)) #stop if batched run is called
  }


  #run prepare data using input folder_path and input age
  prepare_data(folder_path, age, out_folder)

  #extract 5-digit family id from folder_path outside of if statement
  #so it is identified even if no graphics file is created
  family_id <- sub(".*?([0-9]{5}).*", "\\1", folder_path)

  if (clean_data) {

    #extract dataframes from global environment and store in list
    df_names <- ls(envir = .GlobalEnv, pattern = paste0("^df_", age, "_", family_id))
    dfs <- mget(df_names, envir = .GlobalEnv)

    #clean data
    clean_data(dfs, age, false_movement, zero_ranging, lag_diff_0, out_folder_clean, device_dates)
  }

}
