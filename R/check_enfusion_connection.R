#' @title Check connection to Enfusion
#' @description Checks connection to Enfusion based on whether a users is logged
#'  into Enfusion or not. Returns TRUE if logged in, FALSE if not.
#' @importFrom httr GET
#' @returns Bool
#' @export
check_enfusion_connection <- function() {
  tryCatch(
    {
      httr::GET("http://127.0.0.1:18443/exportReport")
      TRUE
    },
    error = function(cond) {
      FALSE
    }
  )
}