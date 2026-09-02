#' @export
data_pipeline <- function(folder_path, age,
                          false_movement = 5, zero_ranging = 5,
                          create_graphics = TRUE, clean_data = TRUE,
                          rmd_file = "graphics_V5.Rmd",
                          out_folder, out_folder_graphics, out_folder_clean) {

  if (length(folder_path) > 1) {

    results <- lapply(
      folder_path,
      function(path) {
        data_pipeline(
          folder_path = path,
          age = age,
          false_movement = false_movement,
          zero_ranging = zero_ranging,
          create_graphics = create_graphics,
          clean_data = clean_data,
          rmd_file = rmd_file,
          out_folder = out_folder,
          out_folder_graphics = out_folder_graphics,
          out_folder_clean = out_folder_clean)})
    return(invisible(NULL)) #stop if batched run is called
  }


  #run prepare data using input folder_path and input age
  prepare_data(folder_path, age, out_folder)

  #extract family id from folder_path outside of if statement
  #so it is identified even if no graphics file is created
  family_id <- sub(".*?([0-9]{5}).*", "\\1", folder_path)

  #create graphics file if create_graphics == TRUE
  if (create_graphics) {

    #set directory to save graphics files
    graphics_dir <- out_folder_graphics

    #create filename for graphics file
    graphics_rmd <- file.path(graphics_dir, paste0("graphics_", family_id, "_", age, "_V5.Rmd"))

    #locate the graphics template included with the package
    graphics_template <- system.file('GraphicsV5.Rmd', package = "tottagpipe")

    #copy original graphics file into new file name
    file.copy(from = graphics_template, to = graphics_rmd, overwrite = TRUE)

    #ensure age and family id are readily available in global environment
    assign("age", age, envir = .GlobalEnv)
    assign("family_id", family_id, envir = .GlobalEnv)

    #render pdf
    rmarkdown::render(graphics_rmd, envir = globalenv())
  }

  if (clean_data) {

    #extract dataframes from global environment and store in list
    df_names <- ls(envir = .GlobalEnv, pattern = paste0("^df_", age, "_", family_id))
    dfs <- mget(df_names, envir = .GlobalEnv)

    #clean data
    clean_data(dfs, age, false_movement, zero_ranging, out_folder_clean)
  }

}
