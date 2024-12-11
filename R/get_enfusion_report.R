#' @title Download Enfusion Report Using API from Excel Add-In
#' @description This function downloads Enfusion reports using the same API as
#'  the Enfusion Excel Add-In. This cicumvents the need to use the REST API
#'  which is an additional cost the Enfusion License. It requires logging into
#'  the enfusion application which can be accomplished using the launch
#'  enfusion function.
#' @param reportWebServiceURL The Enfusion Report URL.
#'  Same as the one used when downloading reports in Excel.
#' @param enfusion_process A processx process for Enfusion.exe.
#'  This is the output of launch_enfusion.
#' @importFrom httr GET
#' @importFrom readr read_csv
get_enfusion_report <- function(reportWebServiceURL, enfusion_process) { #nolint
  if (is.null(enfusion_process)) {
    stop("No Enfusion Process Added")
  }

  if (!any(class(enfusion_process) == "process")) {
    stop("enfusion_process is not 'process' object")
  }

  if (!enfusion_process$get_status() == "running") {
    stop("enfusion_process is not running")
  }

  # Change Web Service URL from rest API to app
  report_url <- gsub(
    "https://webservices.enfusionsystems.com/mobile/rest/reportservice/",
    "http://127.0.0.1:18443/",
    reportWebServiceURL
  )

  response <- httr::GET(report_url)
  if (response$status != 200) stop("No Response from Enfusion")
  raw_data <- readr::read_csv(report_url, show_col_types = FALSE)
  return(na.omit(raw_data))
}