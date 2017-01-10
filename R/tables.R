devtools::use_package("rJava")
devtools::use_package("RSQLServer")
devtools::use_package("dplyr")


#' Fetch the flowcell lane table
#'
#' @param conn a connection object
#'
#' @return The format of the table looks like:
#'     flowcell_id lane_num read_num raw_density pf_density error_rate raw_clusters pf_clusters cycles   pct_q30   mean_q delivered
#'     <chr>    <int>    <int>       <dbl>      <dbl>      <dbl>        <dbl>       <dbl>  <int>     <dbl>    <dbl>     <int>
#'  1  000000000-A0KD9        1        1    864521.0   696930.8       0.65      5871614     4733350    150 0.8453360 33.12884         1
#'  2  000000000-A0KD9        1        2    864521.0   696930.8       1.08      5871614     4733350    150 0.7515980 30.17472         1
#'  3  000000000-A0WUK        1        1   1272872.1  1018724.9       0.57      9332511     7469194    150 0.8728759 33.96945         1
#'  4  000000000-A0WUK        1        2   1272872.1  1018724.9       2.08      9332511     7469194    150 0.7391021 29.90063         1
#'  5  000000000-A0Y09        1        1    965732.0   843288.6       0.54      7462029     6516029    150 0.8857051 34.15779         1
#'  6  000000000-A0Y09        1        2    965732.0   843288.6       1.28      7462029     6516029    150 0.7526702 30.16574         1
#'  7  000000000-A14LW        1        1   1022702.9   862672.0       0.57      7660272     6461573    150 0.9083131 35.40900         1
#' @export
#'
#' @examples
flowcell_lane_table <- function(conn) {
  dplyr::tbl(conn, "flowcell_lane_results")
}

#' Fetch the runfolder table
#'
#' @param conn a connection object
#'
#' @return The format of the table looks like this:
#'        flowcell_id                      runfolder_name   run_date
#'    <chr>                               <chr>     <dttm>
#' 2  000000000-A0KD9 120426_M00485_0002_AMS0010415-00300 2012-04-26
#' 3  000000000-A0WUK 120511_M00485_0005_AMS2004474-00300 2012-05-11
#' 4  000000000-A0Y09 120507_M00485_0004_AMS0010471-00300 2012-05-07
#' 5  000000000-A14LW 120604_M00485_0007_A000000000-A14LW 2012-06-04
#' 6  000000000-A180F 120612_M00485_0008_A000000000-A180F 2012-06-12
#' @export
#'
#' @examples
flowcell_runfolder_table <- function(conn) {
  dplyr::tbl(conn, "flowcell_runfolder")
}

#' Fetch the sample results table
#'
#' @param conn a connection object
#'
#' @return The format of the table looks like this:
#'    flowcell_id project_id sample_name tag_seq lane_num read_num cycles  pct_lane pf_clusters  pct_q30 pct_tag_err library_name   mean_q
#'    <chr>      <chr>       <chr>   <chr>    <int>    <int>  <int>     <dbl>       <dbl>    <dbl>       <dbl>        <chr>    <dbl>
#' 1  000000000-A0KD9    LD-0049       15890  CTCGGT        1        1    150 15.806417      748173 85.32888    3.932513              33.37124
#' 2  000000000-A0KD9    LD-0049       19540  TTACGG        1        1    150 13.646423      645933 84.84964    2.017237              33.24309
#' 3  000000000-A0KD9    LD-0049       22221  GAGTAT        1        1    150  9.311397      440741 85.38124    7.834760              33.38419
#' 4  000000000-A0KD9    LD-0049       26291  GCGTCC        1        1    150 16.277901      770490 84.83561    2.805617              33.24936
#' 5  000000000-A0KD9    LD-0049       27403  ATATAC        1        1    150 20.664413      978119 84.94376    1.611051              33.26583
#' @export
#'
#' @examples
sample_table <- function(conn) {
  dplyr::tbl(conn, "sample_results")
}
