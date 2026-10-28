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
list_folders <- function(folder_path, file_names_filter = T, one_file_filter = T) {

  #creates a list of all subfolders in folder_path
  subfolders <- list.dirs(folder_path, recursive = FALSE, full.names = TRUE)

  #look for all .pkl files in subfolders
  pkl_files <- list.files(subfolders, pattern = "\\.pkl$",
                          recursive = F, full.names = T)

  #initialize diagnostics
  invalid_names <- character(0)
  one_file_folders <- character(0)

  #check that all pickle files follow appropriate naming scheme if filter is set to T
  if (file_names_filter) {
    filenames <- basename(pkl_files)

    #valid naming schemes involve a numeric code followed by "_" followed by an alphanumeric code
    valid_pattern <- "^[0-9]{5}_[A-Za-z0-9]+\\.pkl$"

    #look for invalid names
    invalid_names <- pkl_files[!grepl(valid_pattern, filenames)]

    if (length(invalid_names) > 0) {
      warning(
      length(invalid_names), ' .pkl file(s) have invalid filenames. ',
      'Access with attr(result, "invalid_names"). ',
      'Folders containing these .pkl files will be dropped from the final folder list.')
    } #end internal if statement
  } #end file naming scheme check

  #extract file sizes is filter is set to T
  if (one_file_filter) {
    #count the number of pkl files in each folder
    file_counts <- table(dirname(pkl_files))

    #identify folders containing only one .pkl file
    one_file_folders <- names(file_counts[file_counts == 1])

    #print warning if any folders contain only one .pkl file
    if (length(one_file_folders) > 0) {
      warning(
        length(one_file_folders), ' folder(s) contain only one .pkl file. ',
        'Access with attr(result, "one_file_folders"). ',
        'These folders will be dropped from the final folder list.')
    } #end internal if statement

  } #end check for folders with only one file

  #locate all problematic .pkl files
  problem_folders <- c(dirname(invalid_names), one_file_folders)

  #remove problematic folders from outputted folder list
  final_list <- setdiff(unique(dirname(pkl_files)), problem_folders)

  #attach diagnostic information
  attr(final_list, "invalid_names") <- invalid_names
  attr(final_list, "one_file_folders") <- one_file_folders

  return(final_list)
}
