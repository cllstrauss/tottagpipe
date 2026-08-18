#' @export
clean_data_batched <- function(folder_path, age, false_movement = 5,
                               zero_ranging = 5,out_folder_clean) {

  #load in all .Rdata files in folder_path
  files <- list.files(folder_path, pattern = "\\.RData$", full.names = TRUE)

  #read files into R
  for (file in files) {load(file, envir = .GlobalEnv)}

  #extract names of dataframes
  df_names <- ls(envir = .GlobalEnv, pattern = "^df_")

  #store dataframes in single list
  dfs <- mget(df_names, envir = .GlobalEnv)

  #create list of family ids
  family_ids <- sub(".*?(\\d{5}).*", "\\1", names(dfs))

  #split dfs by family
  dfs_by_family <- split(dfs, family_ids)

  #run clean_data seperately for each family
  results <- lapply(
    names(dfs_by_family),
    function(family_id) {
      family_dfs <- dfs_by_family[[family_id]]


      # Make sure the current family's dataframes are in the global env
      #are available in the global environment
      list2env(family_dfs,envir = .GlobalEnv)

      #run clean_data
      clean_data(dfs = family_dfs, age = age, false_movement = false_movement,
        zero_ranging = zero_ranging, out_folder_clean = out_folder_clean)

      invisible(family_dfs)
    }
  )
  invisible(results)
}
