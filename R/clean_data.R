#' @export
clean_data <- function(dfs, age, false_movement, zero_ranging, out_folder_clean) {

  #create independent copy of dfs to retain original dataframes
  dfs_original <- dfs

  #ensure original dataframes are restored to environment
  #even if function times out
  #if function continues, these will be restored alongside cleaned data
  #with _cleaned appended
  on.exit(list2env(dfs_original, envir = .GlobalEnv), add = TRUE)

  #the original dataframes will be edited throughout to allow for live
  #line by line debugging within the function

  #extract five digit code from dataframes
  family_id <- sub(".*?(\\d{5}).*", "\\1", names(dfs[1]))

  #read in device dates if not already in global environment
  ###edit internally to clean CARE data
  if (!exists("device_dates", envir = .GlobalEnv)) {
    device_dates <- readxl::read_excel(
      'X:/Daily_2/ABC/tottag R code/Current Cleaning Process/Device Date Use Generator/Device_Date_Tracking.xlsx')
  }

  #find corresponding family and wave to extract start
  #and end date
  device_dates_fam <- device_dates %>%
    dplyr::filter(.data$family_id == .env$family_id,
           .data$age == .env$age)

  #set start and end date
  #eventually extract these from file
  start_date <- device_dates_fam$start_date[1]
  start_date <- as.POSIXct(paste(start_date, "00:00:00"), tz = "America/Chicago")

  end_date   <- device_dates_fam$end_date[1]
  end_date <- as.POSIXct(paste(end_date, "23:59:59"), tz = "America/Chicago")

  #trim all data by study period and place back in global environment
  for (df_name in names(dfs)) {
    assign(df_name, subset(
      get(df_name, envir = .GlobalEnv),
      datetime >= start_date & datetime <= end_date),
      envir = .GlobalEnv)}

  #repopulate dfs from global environment after trimming
  dfs <- mget(names(dfs), envir = .GlobalEnv)

  #store all trimmed dfs except family df into a generic named list to loop through
  person_dfs <- dfs[!grepl("\\d{5}$", names(dfs))]

  #store ids of all persons
  person_ids <- sub(".*_([^_]+)$", "\\1", names(person_dfs))

  #Store the family dataframe into a generic name by extracting
  #df ending in the family code
  family_df <- dfs[[grep("\\d{5}$", names(dfs), value = TRUE)]]

  #initialize list to store off_person timestamps
  off_person_vectors <- list()

  print("Cleaning person-level data")

  #next, identify "problematic states" including
  ##1: allowable times during deployment periods for which a given dataframe
  #has no dat
  ##2: off body states as defined by 0 ranging and/or false movement states
  ##3: on charger states as defined by time differences greater than 4.9 minutes

  for (i in 1:length(person_ids)) {
    df <- person_dfs[[i]] #store dataframe in object df
    person_id <- sub(".*_", "", names(person_dfs)[i]) #extract person id for specific df

    print(paste('Creating off body and on charger vectors for', person_id))

    #convert POSIXct dates to Unix numeric timestamps
    start_time <- as.numeric(start_date)
    end_time <- as.numeric(end_date)

    #identify observed data boundaries
    first_time <- min(df$t, na.rm = TRUE)
    last_time <- max(df$t, na.rm = TRUE)

    #create vector of time within allowable window
    no_record <- c(if (start_time < first_time) seq(start_time, first_time - 0.5, by = 0.5),
                   if (last_time < end_time) seq(last_time + 0.5, end_time, by = 0.5))

    #flag potential on charger rows where time difference is greater than 4.9 minutes
    df$charging_status <- ifelse(df$time_diff_min_4.9, 'charging', NA)

    #find the first TRUE in each run
    first_on_charger <- df$time_diff_min_4.9 &
      !dplyr::coalesce(lag(df$time_diff_min_4.9), FALSE)

    #index row before first charging row
    on_idx <- which(first_on_charger) - 1
    on_idx <- on_idx[on_idx > 0]   #avoid row 0

    #clarify tottag was placed on charger before first 5 minute lag reading
    df$charging_status[on_idx] <- 'placed on charger'

    #find the last TRUE in each run
    last_on_charger <- df$time_diff_min_4.9 &
      !dplyr::coalesce(lead(df$time_diff_min_4.9), FALSE)

    #index of the row after each run
    off_idx <- which(last_on_charger) + 1
    off_idx <- off_idx[off_idx <= nrow(df)]   #avoid going past the last row

    #clarify tottag was removed off charger after last 5 minute lag reading
    df$charging_status[off_idx] <- 'taken off charger'

    #first row will remain NA
    #impute first row based on second row
    if (df$charging_status[2] %in% c("charging", "taken off charger")) {
      df$charging_status[1] <- "charging"
    } else {
      df$charging_status[1] <- NA
    }

    #flag potential off-body rows using user-input false_movement cut off
    #and 0mm ranging cutoff

    #define boolean off body vector
    off_body_boolean <- df$m_FALSE > false_movement | df$rmm_0 > zero_ranging

    #find first and last TRUE in ach run
    first_off_body <- off_body_boolean &
      !dplyr::coalesce(lag(off_body_boolean), FALSE)

    last_off_body <- off_body_boolean &
      !dplyr::coalesce(lead(off_body_boolean), FALSE)

    #label entire run to start
    df$off_body_status <- ifelse(off_body_boolean, "off body", NA)

    #relabel boundaries of each run
    df$off_body_status[first_off_body] <- "taken off body"
    df$off_body_status[last_off_body]  <- "placed on body"

    #create boolean runs of charging status and off body status
    #to cooperate with rle
    df$on_charger_boolean <- ifelse(df$charging_status == 'placed on charger' |
                                      df$charging_status == 'charging' |
                                      df$charging_status == 'taken off charger', T, NA)

    df$off_body_boolean <- ifelse(df$off_body_status == 'taken off body' |
                                    df$off_body_status == 'off body' |
                                    df$off_body_status == 'placed on body', T, NA)

    expand_runs <- function(t, flag, start_offset = 0, end_offset = 0, by = 0.5) {

      #treat NA flags as FALSE for purposes of identifying runs
      flag[is.na(flag)] <- FALSE

      r <- rle(flag)

      ends <- cumsum(r$lengths)
      starts <- c(1, head(ends, -1) + 1)

      #keep only TRUE runs
      run_keep <- which(r$values)

      if (length(run_keep) == 0)
        return(numeric(0))

      #define run boundaries with offsets
      start_times <- t[starts[run_keep]] + start_offset
      end_times   <- t[ends[run_keep]] + end_offset

      #remove runs with no timestamps after applying offsets
      valid_runs <- start_times <= end_times

      start_times <- start_times[valid_runs]
      end_times   <- end_times[valid_runs]

      #identify empty runs to troubleshoot when running internally
      empty_runs <- which(!valid_runs)

      if (length(start_times) == 0)
        return(numeric(0))

      #expand each run to every 0.5-second timestamp
      unique(unlist(
        Map(function(start, end) {
          seq(start, end, by = by)
        }, start_times, end_times),
        use.names = FALSE
      ))
    } #end expand runs function

    #Expand runs to include all missing 0.5-second timestamps
    on_charger_t <- expand_runs(
      t = df$t,
      flag = df$on_charger_boolean,
      start_offset = 0.5, #start .5 seconds after placed on charger to allow ranging right before device began charging
      end_offset = -0.5 #end .5 seconds before the row flagged as taken off charger - this row is the row that follows the final time difference greater tha 4.9 minutes (so it is likely a time difference less than 4.9 minutes) - this row is probably ok to allow ranging so ranging should only be prohibited UP TO this row
    )

    off_body_t <- expand_runs(
      t = df$t,
      flag = df$off_body_boolean,
      start_offset = 0, #start right at taken off body - this row represent the beginning of an off-body state and should not allow ranging with other tottags
      end_offset = -0.5 #start .5 seconds before placed on body - this row will contain either a movement toggle at TRUE or non-zero ranging and represents a row that is valid for allowable ranging so ranging should only be prohibited UP TO this row
    )

    #store all states in a list
    lst_off_person <- list(
      off_body   = off_body_t,
      on_charger = on_charger_t,
      no_record  = no_record)

    #assign list to environment with person code in name
    assign(paste0("lst_off_person_", person_id), list(off_body_t, on_charger_t, no_record))

    #store vector of problematic timestamps
    vector_off_person <- unique(unlist(lst_off_person))

    #store in list rather than global environment
    off_person_vectors[[person_id]] <- vector_off_person

    #update dataframe in person_dfs list
    person_dfs[[i]] <- df

    #keep master dfs list synchronized
    dfs[[names(person_dfs)[i]]] <- df

    #update dataframes in global environment for troubleshooting
    assign(names(person_dfs)[i], df, envir = .GlobalEnv)
  }

  #define all pairways combination of tottag ids
  matched_pairs <- combn(person_ids, 2, simplify = FALSE)

  #extract difference variables from family data
  family_diff_vars <- family_df %>% dplyr::select(t, matches("^(diff|na_diff)"))

  #next, go through all matched pairs and edit/clean datasets in pairs
  #define first pair below to run without loop

  for (pair in matched_pairs) {
    #assign person ids as objects
    person1 <- pair[1]
    person2 <- pair[2]

    #retrieve dataframes of each person
    df1 <- person_dfs[[grep(paste0("_", person1, "$"), names(person_dfs))]]
    df2 <- person_dfs[[grep(paste0("_", person2, "$"), names(person_dfs))]]

    #retrieve vectors of "problematic" states
    v1 <- off_person_vectors[[person1]]
    v2 <- off_person_vectors[[person2]]

    #combine into single vector only retaining unique timestamps
    #these comprise all timestamps where these two tottags
    #should not be allowed to range
    v <- unique(c(v1, v2))

    #define target rmm variables to populate with NAs during these states
    #uses opposite target as this is the ranging tottag
    target_rmm_person1 <- paste0("rmm_", person2)
    target_rmm_person2 <- paste0("rmm_", person1)

    #only proceed if both target variables exist
    if (target_rmm_person1 %in% names(df1) &&
        target_rmm_person2 %in% names(df2)) {

      #set ranging values during these times to NA
      df1[[target_rmm_person1]][df1$t %in% v] <- NA
      df2[[target_rmm_person2]][df2$t %in% v] <- NA

      #extract difference variables for this pair
      pair_cols <- names(family_diff_vars)[grepl(person1, names(family_diff_vars)) &
                                             grepl(person2, names(family_diff_vars))]

      person_family_diff_vars <- family_diff_vars %>%
        select(t, all_of(pair_cols))

      #drop rows that are in v, or that are involved in a state where
      #ranging should not be allowable
      person_family_diff_vars <- person_family_diff_vars %>%
        filter(!t %in% v)

      #first, impute ranging data when only one tottag is ranging

      #extract t and NA ranging collumn
      na_cols <- person_family_diff_vars %>%
        dplyr::select(t, starts_with("na"))

      #drop missing data
      na_cols <- na.omit(na_cols)

      #merge dfs
      df1 <- full_join(df1, na_cols, by = join_by(t))
      df2 <- full_join(df2, na_cols, by = join_by(t))

      #impute ranged readings
      df1[[target_rmm_person1]] <- ifelse(!is.na(df1[[ncol(df1)]]), #if the last variable is not na
                                          abs(df1[[ncol(df1)]]), #impute mm using absolute value to account for difference computation
                                          df1[[target_rmm_person1]]) #otherwise keep original value

      df2[[target_rmm_person2]] <- ifelse(!is.na(df2[[ncol(df2)]]), #if the last variable is not na
                                          abs(df2[[ncol(df2)]]), #impute mm using absolute value to account for difference computation
                                          df2[[target_rmm_person2]]) #otherwise keep original value

      #second, take minimum for discrepant ranging readings

      #extract column of discrepant readigns
      diff_cols <- person_family_diff_vars %>%
        dplyr::select(t, starts_with("diff"))

      #drop missing data
      diff_cols <- na.omit(diff_cols)

      #drop differences of 0 (these are not discrepant)
      diff_cols <- diff_cols %>%
        dplyr::filter(diff_cols[[2]] != 0)

      #find corresponding original ranging data in family_df
      family_df_rmms <- family_df %>%
        dplyr::select(t, paste0(person1, "_rmm_", person2),
               paste0(person2, "_rmm_", person1))

      #join with discrepant differences columns
      diff_cols <- dplyr::left_join(diff_cols, family_df_rmms)

      #create new variable that stores the minimum mm reading
      diff_cols$rmm_min <- pmin(diff_cols[[3]], diff_cols[[4]], na.rm = TRUE)

      #merge dfs
      df1 <- dplyr::full_join(df1, diff_cols, by = join_by(t))
      df2 <- dplyr::full_join(df2, diff_cols, by = join_by(t))

      #impute ranged readings
      df1[[target_rmm_person1]] <- ifelse(!is.na(df1[[ncol(df1)]]), #if the last variable is not na
                                          df1[[ncol(df1)]], #impute mm
                                          df1[[target_rmm_person1]]) #otherwise keep original value

      df2[[target_rmm_person2]] <- ifelse(!is.na(df2[[ncol(df2)]]), #if the last variable is not na
                                          df2[[ncol(df2)]], #impute mm
                                          df2[[target_rmm_person2]]) #otherwise keep original value

      #since ranging data has been corrected, drop flagging variables
      df1 <- df1[, 1:(grep("^na", names(df1))[1] - 1)]
      df2 <- df2[, 1:(grep("^na", names(df2))[1] - 1)]

      #sort on t to fold in potential new rows
      df1 <- df1 %>%
        dplyr::arrange(t)

      df2 <- df2 %>%
        dplyr::arrange(t)

      #fill out potential missing information in edited data
      #family id
      df1$family_id <- rep(family_id, times = nrow(df1))
      df2$family_id <- rep(family_id, times = nrow(df2))

      #person id
      df1$person_id <- rep(person1, times = nrow(df1))
      df2$person_id <- rep(person2, times = nrow(df2))

      #age/wave information
      df1$age <- rep(age, times = nrow(df1))
      df2$age <- rep(age, times = nrow(df2))

      #readable date/time
      df1$datetime <- as.POSIXct(df1$t, origin = "1970-01-01", tz = "America/Chicago")
      df2$datetime <- as.POSIXct(df2$t, origin = "1970-01-01", tz = "America/Chicago")

      #updated lag_diff
      target_lag_person1 <- paste0("rmm_", person2, "_lag_diff")
      target_lag_person2 <- paste0("rmm_", person1, "_lag_diff")

      df1[[target_lag_person1]] <- with(df1, df1[[target_rmm_person1]] -
                                          dplyr::lag(df1[[target_rmm_person1]]))

      df2[[target_lag_person2]] <- with(df2, df2[[target_rmm_person2]] -
                                          dplyr::lag(df2[[target_rmm_person2]]))

      #find dataframe names in person_dfs to replace
      df1_name <- names(person_dfs)[grep(paste0("_", person1, "$"), names(person_dfs))]
      df2_name <- names(person_dfs)[grep(paste0("_", person2, "$"), names(person_dfs))]

      #replace dataframes in global environment with cleaned data
      assign(df1_name, df1, envir = .GlobalEnv)
      assign(df2_name, df2, envir = .GlobalEnv)

      #update dfs with edited dataframes
      dfs[[df1_name]] <- df1
      dfs[[df2_name]] <- df2

      #repopulate person_dfs from the updated list
      person_dfs <- dfs[!grepl("\\d{5}$", names(dfs))]
    }  else {

      warning(paste('No ranging data between', person1, 'and', person2))
    }
  }

  #drop rows associated with off body states in all dataframes
  #in list person_dfs, keeping NAs
  person_dfs <- lapply(person_dfs, function(df) {

    remove <-
      (df$off_body_status == "off body" &
         !dplyr::coalesce(df$charging_status %in% c("placed on charger",
                                                    "taken off charger"), FALSE)) |
      (df$charging_status == "charging" &
         !dplyr::coalesce(df$off_body_status %in% c("taken off body",
                                                    "placed on body"), FALSE))

    remove[is.na(remove)] <- FALSE
    df[!remove, ]
  })

  #create new object of cleaned data
  person_dfs_cleaned <- person_dfs

  #append "_cleaned" to dataframe names
  names(person_dfs_cleaned) <- paste0(names(person_dfs_cleaned), "_cleaned")

  #attach final cleaned data to environment with '_cleaned'
  list2env(person_dfs_cleaned, envir = .GlobalEnv)

  #create update merged family data
  if (length(person_dfs_cleaned) > 1) {
    print('Merging clean data into cleaned family-level data')

    #create vector of keys used for merging
    key_cols <- c("t", "datetime", "family_id", "age")

    #Assign prefixes to variables in og data to differentiate
    person_dfs_prefixed <- Map(function(df, prefix) {
      df %>%
        rename_with(~ paste0(prefix, "_", .), .cols = -all_of(key_cols))
    }, person_dfs_cleaned, person_ids) #prefix is the person id (e.g., TC)

    #merge dataframes by pre-defined keys
    family_df <- purrr::reduce(person_dfs_prefixed, full_join, by = key_cols)

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
          dplyr::coalesce(family_df[[col1]], 0) - dplyr::coalesce(family_df[[col2]], 0),
          NA
        )

      } else {
        #error message for when troubleshooting within function
        warning(paste("Skipping pair", pair_name, "potentially missing ranging data between this pair, please carefully review rmm columns"))
      }
    }

    family_id <- family_df$family_id[1] #extract five digit family id to name dataframe

    #assign merged family data to global environment
    assign(paste0('df_', age, '_', family_id, '_cleaned'), family_df, envir = .GlobalEnv)

    #create cleaned family dataframe name
    family_df_name <- paste0("df_", age, "_", family_id, "_cleaned")

    #add family dataframe to cleaned dataframe list
    person_dfs_cleaned[[family_df_name]] <- family_df

  }
  print('Saving cleaned data to server')

  for (df_name in names(person_dfs_cleaned)) {
    save(list = df_name, file = file.path(out_folder_clean, paste0(df_name, ".RData")))
  }

}
