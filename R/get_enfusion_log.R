#' @title Get Enfusion Log Entries
#' @description Returns Enfusion Log Entries with timestamps
#' @param enfusion_path file path to enfusion.exe
#' @param t Optional, time value to filer latest log entries
#' @importFrom stringr str_which
#' @importFrom readr read_delim
#' @importFrom stringr str_extract_all
get_enfusion_log <- function(enfusion_path, t = NULL) {
  # Set Enfusion Log Location
  path_f <- list.files(path = enfusion_path)
  log_loc <- file.path(
    enfusion_path, 
    path_f[stringr::str_which(path_f, ".log")]
  )
  # Read Enfuison Log
  log_raw <-
    readr::read_delim(
      log_loc, 
      delim = ";", 
      escape_double = FALSE, 
      col_names = FALSE, 
      trim_ws = TRUE, 
      skip = 1,
      show_col_types = FALSE
    )
  # Log Date Format
  log_day_fmt <- "[:digit:]{4}-[:digit:]{2}-[:digit:]{2}"
  log_time_fmt <- "[:digit:]{2}:[:digit:]{2}:[:digit:]{2}"
  log_subt_fmt <- ",[:digit:]{3}"
  log_tz_fmt <- "[:alpha:]{3}"
  log_date_fmt <- paste0(
    log_day_fmt, " ",
    log_time_fmt,
    log_subt_fmt, " ",
    log_tz_fmt
  )
  # Find where there are date entries
  log_entries_loc <- stringr::str_which(log_raw$X1, paste0("^\\[", log_date_fmt, "\\]"))
  # Subset Log to just entries with date and times
  log_entries <- log_raw[log_entries_loc,]
  # Get Time and Dates
  log_dates <- strptime(
    stringr::str_extract_all(log_entries$X1, paste0(log_day_fmt, " ", log_time_fmt)),
    "%Y-%m-%d %H:%M:%S"
  )

  if (is.null(t)) {
    return(log_entries$X1)
  } else {
    # Subset log for where it time > t
    return(log_entries$X1[which(log_dates > t)])
  }
}

#' @title Check If Enfusion is at Username Login Page
#' @param enfusion_path file path to enfusion.exe
#' @param t Optional, time value to filer latest log entries
#' @importFrom stringr str_detect
check_enfusion_log_un <- function(enfusion_path, t) {
  log_entries <- get_enfusion_log(enfusion_path, t)
  any(
    stringr::str_detect(
      log_entries, 
      paste0(
        "INFO  BrowserLoginComponent",
        ".+", 
        "Loaded URL:",
        ".+",
        "https://login-prod-us01.enfusionsystems.com/auth"
      )
    )
  )
}

#' @title Check If Enfusion is at Password Login Page
#' @param enfusion_path file path to enfusion.exe
#' @param t Optional, time value to filer latest log entries
#' @importFrom stringr str_detect
check_enfusion_log_pw <- function(enfusion_path, t) {
  log_entries <- get_enfusion_log(enfusion_path, t)
  any(
    stringr::str_detect(
      log_entries,
      paste0(
        "INFO  BrowserLoginComponent",
        ".+",
        "Password forms should have"
      )
    )
  )
}

#' @title Check If Enfusion is loaded
#' @param enfusion_path file path to enfusion.exe
#' @param t Optional, time value to filer latest log entries
#' @importFrom stringr str_detect
check_enfusion_log_load <- function(enfusion_path, t) {
  log_entries <- get_enfusion_log(enfusion_path, t)
  any(
    stringr::str_detect(
      log_entries,
      paste0("INFO  ReportServiceImpl \\[PreloadWorker\\] Completed metadata preloads")
    )
  )
}
