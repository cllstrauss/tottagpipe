#' List Folders
#'
#' Creates a list of all files containing .pkl files ready to be processed.
#'
#' @param folder_path A character file path containing subfolders labeled
#' by family id which contain .pkl files.
#' @param one_kb_filter A boolean toggle set to TRUE by default. When toggled to TRUE
#' any .pkl files of size 1KB or less will be noted. These are potentially empty files.
#' The user should investigate these files, and if truly empty, delete them and
#' rerun this function before inputting into the pipeline.
#' @param file_names_filter A boolean toggle set to TRUE by default. When toggled to TRUE
#' any .pkl files that do not follow the standard naming scheme are noted.
#' The naming scheme requires all files to start with a numeric family code,
#' followed by _, followed by an alphanumeric person code.
#' Incorrectly named files may not be properly processed.
#' The user should investigate these files, and address naming concerns then
#' rerun this function before inputting into the pipeline.
#'
#' @return A character vector of file paths that contain .pkl files to be used as input into
#' the folder_path argument of data_pipeline.
#'
#' @examples
#' list_folders('X:/Daily_2/ABC/tottag R code/Chris Processed/Batch Test/testData')
#'
#' @export
list_folders <- function(folder_path, one_kb_filter = T, file_names_filter = T) {

  #creates a list of all subfolders in folder_path
  subfolders <- list.dirs(folder_path, recursive = FALSE, full.names = TRUE)

  #look for all .pkl files in subfolders
  pkl_files <- list.files(subfolders, pattern = "\\.pkl$",
                          recursive = F, full.names = T)

  #extract file sizes is filter is set to T
  if (one_kb_filter) {
  file_sizes <- file.info(pkl_files)$size

    #define small files as files at or less than 1KB
    small_files <- pkl_files[file_sizes <= 1000]

    #print warning if any files are 1KB or less
    if (length(small_files) > 0) {
      warning("The following .pkl files are 1 KB or smaller:\n", paste(small_files, collapse = "\n"))
      warning("Do NOT proceed with data_pipeline until these files are reviewed")
    } #end internal if statement
  } #end file size check

  #check that all pickle files follow appropriate naming scheme if filter is set to T
  if (file_names_filter) {
    filenames <- basename(pkl_files)

    #valid naming schemes involve a numeric code followed by "_" followed by an alphanumeric code
    valid_pattern <- "^[0-9]{5}_[A-Za-z0-9]+\\.pkl$"

    #look for invalid names
    invalid_names <- pkl_files[!grepl(valid_pattern, filenames)]

    if (length(invalid_names) > 0) {
      warning("The following .pkl files have invalid filenames:\n",
        paste(invalid_names, collapse = "\n"))
      warning("Do NOT proceed with data_pipeline until these files are correctly named")
    } #end internal if statement
  } #end file naming scheme check

  return(unique(dirname(pkl_files)))
}
