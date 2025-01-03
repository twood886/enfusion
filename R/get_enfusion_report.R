#' @title Download Enfusion Report Using API from Excel Add-In
#' @description This function downloads Enfusion reports using the same API as
#'  the Enfusion Excel Add-In. This cicumvents the need to use the REST API
#'  which is an additional cost the Enfusion License. It requires logging into
#'  the enfusion application which can be accomplished using the launch
#'  enfusion function.
#' @param reportWebServiceURL The Enfusion Report URL.
#'  Same as the one used when downloading reports in Excel.
#' @importFrom httr GET
#' @importFrom readr read_csv
#' @importFrom dplyr if_all
#' @importFrom dplyr everything
#' @examples
#' library(enfusion)
#' enfusion_process <- launch_enfusion("username", "password")
#' reportWebServiceURL <- "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=test.trb"
#' get_enfusion_report(reportWebServiceURL, enfusion_process)
#' @export
get_enfusion_report <- function(reportWebServiceURL) { #nolint
  if (!check_enfusion_connection()) {
    stop("Enfusion is not Running")
  }

  #if (is.null(enfusion_process)) {
  #  stop("No Enfusion Process Added")
  #}

  #if (!any(class(enfusion_process) == "process")) {
  #  stop("enfusion_process is not 'process' object")
  #}

  #if (!enfusion_process$get_status() == "running") {
  #  stop("enfusion_process is not running")
  #}

  # Change Web Service URL from rest API to app
  report_url <- gsub(
    "https://webservices.enfusionsystems.com/mobile/rest/reportservice/",
    "http://127.0.0.1:18443/",
    reportWebServiceURL
  )

  response <- httr::GET(report_url)
  if (response$status != 200) stop("No Response from Enfusion")
  raw_data <- readr::read_csv(report_url, show_col_types = FALSE)
  return(raw_data[rowSums(is.na(raw_data)) != ncol(raw_data), ])
}
