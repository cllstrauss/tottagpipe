#' @export
prepare_data <- function(folder_path, age, out_folder) {

  library(reticulate)
  library(data.table)
  library(tidyverse)
  library(zoo)

  pickle <- reticulate::import("pickle")
  os <- reticulate::import("os")

  process_pkls <- function(folder_path, age) {
    #Get list of all .pkl files in the folder
    pkl_files <- list.files(folder_path, pattern = "\\.pkl$",
                            full.names = TRUE)

    print('reading .pkls into R')
    process_single_pkl <- function(pkl_file) {

      print(paste("Converting", basename(pkl_file),
                  "into R dataframe with data quality check variables"))

      #Read the .pkl file
      py_file <- reticulate::py_eval("open", convert = FALSE)
      file <- py_file(pkl_file, "rb")
      large_list <- pickle$load(file)

      if (length(large_list) > 0) {
        large_list <- lapply(large_list, function(x) {

          #process ranging data
          if (!is.null(x$r) && length(x$r) > 0) { #only look where r exists

            nm_r <- names(x$r) #extract code of nearby person

            for (j in seq_along(nm_r)) {
              ranging_tottag <- nm_r[j] #store nearby person
              mm <- x$r[[ranging_tottag]] #store mm closeness

              suffix <- sub(".*_", "", ranging_tottag) #extract alphanumeric code (e.g., 'TC')
              suffix <- toupper(suffix) #force letters to be capitalized

              #create and store new variable with name of person and distance
              x[[paste0("rmm_", suffix)]] <- mm
            }

            #drop original list of r values
            x$r <- NULL

          } else {
            x$r <- NULL #set to null if no r values exist
          }

          #process BLE data
          if (!is.null(x$b) && length(x$b) > 0) {

            for (BLE_tottag in x$b) {

              suffix <- sub(".*_", "", BLE_tottag)
              suffix <- toupper(suffix)

              x[[paste0("BLE_", suffix)]] <- TRUE
            }

            x$b <- NULL

          } else {
            x$b <- NULL
          }

          #process accelerameter data
          if (!is.null(x$i) && length(x$i) >= 3) { #only search where i exists
            x$accel_x <- x$i[[1]] #x-axis
            x$accel_y <- x$i[[2]] #y-axis
            x$accel_z <- x$i[[3]] #z-axis
            x$i <- NULL
          } else {
            x$i <- NULL #set to null if no i exists
          }
          x #return updated list ready to convert to dataframe
        })

        #convert list to data.table
        datarbind <- data.table::rbindlist(large_list, fill = TRUE)

        #ensure t, v, and m are in a consistent order
        datarbind <- datarbind %>%
          dplyr::relocate(any_of(c("t", "v", "m")))

        #order 'b' variables and 'r' variables
        datarbind <- datarbind %>%
          dplyr::relocate(dplyr::starts_with("BLE"), .before = dplyr::starts_with("rmm"))

        #print variable names so user can see if anything is missing
        print(paste0("Variable names in ", basename(pkl_file), " are: ", paste(names(datarbind), collapse = ", ")))

        #add family and person ids to dataframe as new variables
        family_id <- sub("_.*", "", basename(pkl_file)) #extract five digit family code
        person_id <- sub("\\.pkl$", "", sub("^[^_]+_", "", basename(pkl_file))) #extract alphanumeric person code

        #add to dataframe
        datarbind$family_id <- rep(family_id, times = nrow(datarbind))
        datarbind$person_id <- rep(person_id, times = nrow(datarbind))

        #add age
        datarbind$age <- rep(age, times = nrow(datarbind))

        #relocate to beginning of dataframe
        datarbind <- datarbind %>%
          dplyr::relocate(c(family_id, person_id, age), .before = t)

        ###variable addition 1: flag time jumps in OG ordering
        datarbind <- datarbind %>%
          dplyr::mutate(time_diff_min_og = c(NA, diff(t) / 60))

        datarbind <- datarbind %>%
          dplyr::relocate(time_diff_min_og, .after = t)

        ###variable addition 2: time difference between measurement occasions
        #create variable that computes the difference between the current and previous timestamp
        datarbind <- datarbind %>%
          dplyr::arrange(t) %>%  # Ensure data is sorted by time
          dplyr::mutate(time_diff_min = c(NA, diff(t) / 60))

        #relocate to after t
        datarbind <- datarbind %>%
          dplyr::relocate(c(time_diff_min), .after = t)

        ###variable addition 3: POSIXct timestamp
        #create variables to convert Unix time to readable time
        #Convert Unix time to POSIXct
        datarbind$datetime <- as.POSIXct(datarbind$t, origin = "1970-01-01", tz = "America/Chicago")

        #relocate to after t
        datarbind <- datarbind %>%
          dplyr::relocate(datetime, .after = t)

        ###variable addition 4: flag when time difference is greater than 4.9
        #as this likely implies tottag is on charger
        datarbind$time_diff_min_4.9 <- datarbind$time_diff_min >= 4.9

        #relocate to after time_diff_min
        datarbind <- datarbind %>%
          dplyr::relocate(time_diff_min_4.9, .after = time_diff_min)

        ###variable addition 5: voltage power check
        #create voltage check using standard metric
        if ("v" %in% names(datarbind)) {
          datarbind$v_check <- ifelse(datarbind$v >= 4200, 'fully charged',
                                      ifelse(datarbind$v <= 3680 & datarbind$v > 3500, 'battery low',
                                             ifelse(datarbind$v <= 3500, 'battery dead', NA)))

          #relocate to after v
          datarbind <- datarbind %>%
            dplyr::relocate(v_check, .after = v)

          ###variable addition 6: voltage frequency check
          valid_v_idx <- which(!is.na(datarbind$v)) #identify nonmissing voltage readings
          valid_t <- datarbind$t[valid_v_idx] #identify UNIX timestamps for nonmissing voltage readings
          valid_vt_diff <- c(NA, diff(valid_t)) #compute time difference between successive valid voltage readings

          datarbind$v_freq <- rep(NA, nrow(datarbind)) #initial voltage check to NA
          datarbind$v_freq[valid_v_idx] <- valid_vt_diff #difference in seconds from last voltage check

          #relocate to after v_check
          datarbind <- datarbind %>%
            dplyr::relocate(v_freq, .after = v_check)
        } else {
          warning(paste('No voltage readings in', basename(pkl_file)))
        }

        ###variable addition 7: movement toggle check violations
        if ("m" %in% names(datarbind)) {
          datarbind$m_check <- NA #create an NA-filled vector first
          m_idx <- which(!is.na(datarbind$m)) #Get the index of non-NA movement rows
          same_as_prev <- datarbind$m[m_idx[-1]] == datarbind$m[m_idx[-length(m_idx)]] #compare current and previous non-NA values
          datarbind$m_check[m_idx[-1][same_as_prev]] <- "movement_violation" #mark the second of each offending pair

          #relocate to after m
          datarbind <- datarbind %>%
            dplyr::relocate(m_check, .after = m)
        } else {
          warning(paste('No movement data in ', basename(pkl_file)))
        }

        ###variable addition 8: lagged ranging measures
        rmm_cols <- grep("^rmm", names(datarbind), value = TRUE) #extract columns containing ranging data

        if (length(rmm_cols) > 0) {
          for (col in rmm_cols) {
            new_col <- paste0(col, "_lag_diff") #create lagged difference
            datarbind[[new_col]] <- with(datarbind, datarbind[[col]] - dplyr::lag(datarbind[[col]])) #compute and store in df
          }

          #relocate to after closeness
          datarbind <- datarbind %>%
            dplyr::relocate(
              dplyr::all_of(paste0(rmm_cols, "_lag_diff")),
              .after = max(match(rmm_cols, names(datarbind))))
        } else {
          warning(paste('No ranging data in ', basename(pkl_file)))
        }

        #store number of rows of datarbind for subsequent code
        n <- nrow(datarbind)

        ###variable addition 9: flag and time where tottag is not moving
        if ("m" %in% names(datarbind)) {
          datarbind$m_FALSE <- NA_real_ #define an populate new variable

          m_state <- zoo::na.locf(datarbind$m, na.rm = FALSE) #forward fill NAs with F state
          #compute number of rows in datarbind

          m_true_idx <- which(m_state == TRUE) #define rows with T state
          m_true_idx <- c(0, m_true_idx, n + 1)

          m_starts <- m_true_idx[-length(m_true_idx)] + 1 #index start of F segments
          m_ends   <- m_true_idx[-1] - 1 #index end

          m_valid <- m_starts <= m_ends #clear out empty intervals
          m_starts <- m_starts[m_valid]
          m_ends   <- m_ends[m_valid]

          m_ends <- pmin(m_ends + 1, nrow(datarbind)) #extend run by one

          m_duration <- (datarbind$t[m_true_idx[-1][m_valid]] - datarbind$t[m_starts]) / 60

          idx <- unlist(mapply(seq, m_starts, m_ends)) #expand runs into row indices
          datarbind$m_FALSE[idx] <- rep(m_duration, times = m_ends - m_starts + 1) #populate variable

          datarbind <- datarbind %>%
            dplyr::relocate(m_FALSE, .after = m)
        }

        ###variable addition 10: flag and time states ranging 0 mms from at least one other tottag

        if (length(rmm_cols) > 0) {
          datarbind$rmm_0 <- NA_real_ #create new variable
          rmm_mat <- as.matrix(datarbind[, rmm_cols, with = FALSE]) #create matrix of rmm_cols #create matrix of rmm_cols
          has_zero <- rowSums(rmm_mat == 0, na.rm = TRUE) > 0 #define 0-state within matrix

          rmm_0_r <- rle(has_zero) #identify runs using run length encoding

          rmm_0_ends <- cumsum(rmm_0_r$lengths) #define segment ends
          rmm_0_starts <- c(1, head(rmm_0_ends, -1) + 1) #define segment starts

          rmm_0_true_runs <- which(rmm_0_r$values) #select all NA runs

          rmm_0_starts_t <- rmm_0_starts[rmm_0_true_runs] #row index start segment
          rmm_0_ends_t   <- rmm_0_ends[rmm_0_true_runs] #row index end segment
          rmm_0_ends_t <- pmin(rmm_0_ends_t + 1, nrow(datarbind)) #extend each run by one

          rmm_0_duration <- (datarbind$t[rmm_0_ends_t] - datarbind$t[rmm_0_starts_t]) / 60
          rmm_0_idx <- unlist(Map(seq, rmm_0_starts_t, rmm_0_ends_t)) #expand to rows

          datarbind$rmm_0[rmm_0_idx] <- rep(rmm_0_duration, times = rmm_0_ends_t - rmm_0_starts_t + 1)

          #relocate to after original rmm variables
          datarbind <- datarbind %>%
            dplyr::relocate(rmm_0,
                     .after = max(match(rmm_cols, names(datarbind))))
        }

        ###variable addition 11: flag and time states ranging with no other tottags
        if (length(rmm_cols) > 0) {
          datarbind$rmm_NA <- NA_real_
          all_na <- rowSums(!is.na(rmm_mat)) == 0

          rmm_na_r <- rle(all_na)

          rmm_na_ends <- cumsum(rmm_na_r$lengths)
          rmm_na_starts <- c(1, head(rmm_na_ends, -1) + 1)

          na_runs <- which(rmm_na_r$values)

          rmm_na_starts_t <- rmm_na_starts[na_runs]
          rmm_na_ends_t <- rmm_na_ends[na_runs]
          rmm_na_ends_t <- pmin(rmm_na_ends_t + 1, nrow(datarbind))

          rmm_na_duration <- (datarbind$t[rmm_na_ends_t] - datarbind$t[rmm_na_starts_t]) / 60
          rmm_na_idx <- unlist(Map(seq, rmm_na_starts_t,rmm_na_ends_t))

          datarbind$rmm_NA[rmm_na_idx] <- rep(rmm_na_duration, times = rmm_na_ends_t - rmm_na_starts_t + 1)

          #relocate to after original rmm_0
          datarbind <- datarbind %>%
            dplyr::relocate(rmm_NA, .after = rmm_0)
        }

        ###variable addition 12: count bluetooth detections during each rmm NA run

        ble_cols <- grep("^BLE_", names(datarbind), value = TRUE) #identify all BLE cols

        if (length(ble_cols) > 0) {

          #create new variable
          datarbind$BLE_NA_count <- NA_integer_

          #operate during a non-zero rmm_na run
          if (length(na_runs) > 0) {

            #convert multiple BLE columns to a logical matrix once
            ble_mat <- as.matrix(datarbind[, ble_cols, drop = FALSE])

            #count total BLE pings across ALL tottags within each rmm_na run
            ble_counts <- mapply(
              function(s, e) {sum(ble_mat[s:e, ], na.rm = TRUE)},
              rmm_na_starts_t,
              rmm_na_ends_t - 1
            )

            #store the count one time at the end of the run
            datarbind$BLE_NA_count[rmm_na_ends_t] <- ble_counts

            #relocate to after original rmm_0
            datarbind <- datarbind %>%
              dplyr::relocate(BLE_NA_count, .after = rmm_NA)
          }
        }

        print(paste(sub("\\.pkl$", "", basename(pkl_file)), "completed"))

        return(datarbind)

      } else
        print(paste("No data in", basename(pkl_file)))
      warning(paste("No data in", basename(pkl_file), ". Dropping file.
                      There may be data loss in this family without further investigation"))
    }

    #process each .pkl file and store results in a named list
    results <- lapply(pkl_files, process_single_pkl)
    results <- Filter(Negate(is.null), results) #drop empty dataframes if they exist
    names(results) <- paste0('df_', age, '_', sub("\\.pkl$", "", basename(pkl_files))) # Name each dataframe by filename

    return(results)
  }

  person_dfs <- process_pkls(folder_path, age)
  person_dfs <- person_dfs[!vapply(person_dfs, is.character, logical(1))] #drop empty files

  #assign indiviudal data files to global environment
  list2env(person_dfs, envir = .GlobalEnv)

  if (length(person_dfs) > 1) {
    print('creating merged family data')

    dfs_names <- names(person_dfs) #extract names of dataframes
    person_ids <- sub(".*_([^_]+)$", "\\1", dfs_names) #extract person ids

    #create vector of keys used for merging
    key_cols <- c("t", "datetime", "family_id", "age")

    #Assign prefixes to variables in og data to differentiate
    person_dfs_prefixed <- Map(function(df, prefix) {
      df %>%
        dplyr::rename_with(~ paste0(prefix, "_", .), .cols = -dplyr::all_of(key_cols))
    }, person_dfs, person_ids) #prefix is the person id (e.g., TC)

    #merge dataframes by pre-defined keys
    family_df <- purrr::reduce(person_dfs_prefixed, dplyr::full_join, by = key_cols)

    #arrange by timestamp
    family_df <- family_df %>%
      dplyr::arrange(t)

    #order first four variables
    family_df <- family_df %>%
      dplyr::relocate(family_id, age, t, datetime)

    #code mismatched distance measures
    matched_pairs <- combn(person_ids, 2, simplify = FALSE) #create all pairwise matches

    #find columns that contain distance measures for each matched pair
    matched_columns <- lapply(matched_pairs, function(pair) {
      cols <- names(family_df)
      matched <- cols[sapply(cols, function(col) all(sapply(pair, function(p) grepl(p, col))))]
      matched[!grepl("_lag_diff$", matched) & !grepl("BLE", matched)]
    })

    #create named variables for all distances
    names(matched_columns) <- sapply(matched_pairs, paste, collapse = "_")

    #Loop through each pair of matched columns
    for (pair_name in names(matched_columns)) {
      cols <- matched_columns[[pair_name]]

      #catch in case not all matched pairs are included in merged data
      if (length(cols) == 2 && all(cols %in% names(family_df))) {
        col1 <- cols[1]
        col2 <- cols[2]

        #Create a new column with the difference
        diff_col_name <- paste0("diff_", pair_name)
        family_df[[diff_col_name]] <- family_df[[col1]] - family_df[[col2]]

        #Create a new column to store differences when only one tottag is ranging and the other is NA
        na_diff_col_name <- paste0("na_diff_", pair_name)
        family_df[[na_diff_col_name]] <- ifelse(
          xor(is.na(family_df[[col1]]), is.na(family_df[[col2]])),
          coalesce(family_df[[col1]], 0) - coalesce(family_df[[col2]], 0),
          NA
        )

      } else {
        #error message for when troubleshooting within function
        warning(paste("Skipping pair", pair_name, "for family", family_df$family_id[1], ". There is potentially missing ranging data between this pair, please carefully review rmm columns"))
      }
    }

    family_id <- family_df$family_id[1] #extract five digit family id to name dataframe

    #assign merged family data to global environment
    assign(paste0('df_', age, '_', family_id), family_df, envir = .GlobalEnv)
  } else {
    warning(paste('This folder contains only one file with data. No merged family dataset will be created.'))
  }

  #save all dataframes to folder_path
  all_dfs <- ls(envir = .GlobalEnv, pattern = paste0("^df_", age, "_", family_id))

  #save to shared drive
  print('saving all data to server')
  for (df in all_dfs){
    setwd(out_folder)
    save(list = df, file = paste0(df, '.RData'))

  }

}
