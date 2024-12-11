#' @title Launch Enfusion Application
#' @description Launch Enfusion Integrata using user name and password
#' @param username Enfusion Username
#' @param password Enfusion Password
#' @param enfusion_path Path to Enfusion.exe
#' @importFrom processx process
#' @import RDCOMClient
#' @include get_enfusion_log.R
#' @export
launch_enfusion <- function(
  username,
  password,
  enfusion_path = file.path("C:", "Program Files", "Enfusion")
) {
  library(RDCOMClient)
  # Create WScript Shell to send commands to Enfusion Login Page
  shell <- RDCOMClient::COMCreate("WScript.Shell")
  # Get Current Time for limiting Enfusion Log
  t <- Sys.time()
  # Launch Enfusion
  process <- processx::process$new(
    file.path(enfusion_path, "Enfusion.exe"),
    stdout = "|",
    stderr = "|",
    stdin = "|",
    echo_cmd = TRUE
  )
  # Get Enfusion Process Id
  pid <- process$get_pid()

  # Check if Username page has loaded
  login_un_load <- FALSE
  t_1 <- Sys.time()
  while (!login_un_load) {
    login_un_load <- check_enfusion_log_un(enfusion_path, t)
    if ((Sys.time() - t_1) > 10) {
      stop("Error Loading Enfusion")
    }
  }
  # Insert Username and press enter
  shell$AppActivate(pid)
  shell$SendKeys(paste0("^a", "{BACKSPACE}", username, "{ENTER}"))

  # Check if Password page has loaded
  login_pw_load <- FALSE
  t_1 <- Sys.time()
  while (!login_pw_load) {
    login_pw_load <- check_enfusion_log_pw(enfusion_path, t)
    if ((Sys.time() - t_1) > 10) {
      stop("Error Loading Enfusion")
    }
  }
  # Insert Password and press enter
  shell$AppActivate(pid)
  shell$SendKeys(paste0("{TAB}", password, "{ENTER}"))
  # Check if Enfusion has loaded
  login_load <- FALSE
  t_1 <- Sys.time()
  while (!login_load) {
    login_load <- check_enfusion_log_load(enfusion_path, t)
    if ((Sys.time() - t_1) > 60) {
      stop("Error Loading Enfusion - Final")
    }
  }
  return(process)
}