devtools::use_package("rJava")
devtools::use_package("RSQLServer")
devtools::use_package("dplyr")


#' Title
#'
#' @param conn
#'
#' @return
#' @export
#'
#' @examples
flowcell_lane_table <- function(conn) {
  dplyr::tbl(conn, "flowcell_lane_results")
}

#' Title
#'
#' @param conn
#'
#' @return
#' @export
#'
#' @examples
flowcell_runfolder_table <- function(conn) {
  dplyr::tbl(conn, "flowcell_runfolder")
}

#' Title
#'
#' @param conn
#'
#' @return
#' @export
#'
#' @examples
sample_table <- function(conn) {
  dplyr::tbl(conn, "sample_results")
}
