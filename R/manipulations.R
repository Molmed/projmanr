devtools::use_package("dplyr")

#' Add the instrument name to a data frame
#'
#' @param data the data frame to add the instrument name to. This has to have a column called runfolder_name
#' which contains the name of the runfolder on the format '120620_M00485_0010_A000000000-A19RW'
#'
#' @return the same data frame as input with the column instrument added to it.
#' @export
#'
#' @examples
add_instrument_name_to_dataframe <- function(data) {
  instrument_id_to_name <-
    data.frame(
      unique_identifier = c(
        "SN344",
        "SN866",
        "SN7001335",
        "D00118",
        "D00457",
        "D00458",
        "ST-E00215",
        "ST-E00216",
        "ST-E00274",
        "ST-E00279",
        "ST-E00280",
        "M00485",
        "M00629", 
        "M03379"
      ),
      instrument = c(
        "HiSeq 1",
        "HiSeq 2",
        "HiSeq 3",
        "HiSeq 4",
        "HiSeq 5",
        "HiSeq 6",
        "HiSeqX 1",
        "HiSeqX 2",
        "HiSeqX 3",
        "HiSeqX 4",
        "HiSeqX 5",
        "MiSeq 1",
        "MiSeq 2", 
        "MiSeq IMBIM"
      )
    )

  data$instrument_name <-
    sapply(strsplit(as.character(data$runfolder_name), "_"), `[`, 2)

  runfolder_names_and_instrument_names <-
    merge(
      x = data,
      y = instrument_id_to_name,
      by.x = "instrument_name",
      by.y = "unique_identifier",
      all.x = TRUE,
      sort = FALSE
    )

  runfolder_names_and_instrument_names
}

#' Fetch project summaries for the specified period
#'
#' @param connection a connection object
#' @param start_date on format '2017-01-01' inclusive
#' @param end_date on format '2017-12-31' exclusive
#'
#' @return A project summary table with the following format
#'       project_id   nbr_of_samples lanes bases_sequenced
#' 2          FU102   22             2    2.111589e+10
#' 3           FU64    1             1    5.688977e+10
#' 4           FU79   12             2    9.404285e+10
#' 5           FU87    8             4    3.076640e+11
#' 6           FU95    1             2    3.873992e+10
#' 7           FU98   19             2    8.033634e+10
#' @export
#'
#' @examples
fetch_project_summaries_for_date_range <-
  function(connection, start_date, end_date) {
    flowcells_and_run_dates_in_range <-
      flowcell_runfolder_table(connection) %>%
      dplyr::select(flowcell_id, run_date) %>%
      dplyr::filter(run_date >= as.Date(start_date), run_date < as.Date(end_date)) %>%
      dplyr::collect()

    samples <-
      sample_table(connection) %>%
      dplyr::collect(n = Inf)

    samples_in_date_range <-
      samples[samples$flowcell_id %in% flowcells_and_run_dates_in_range$flowcell_id,]

    samples_in_date_range_grouped_by_project_id  <-
      samples_in_date_range %>%
      dplyr::group_by(project_id)

    samples_per_project <-
      samples_in_date_range_grouped_by_project_id %>%
      dplyr::filter(read_num == 1) %>%
      dplyr::distinct(sample_name) %>%
      dplyr::summarise(nbr_of_samples = n())

    lanes_per_project <-
      samples_in_date_range_grouped_by_project_id %>%
      dplyr::distinct(flowcell_id, lane_num) %>%
      dplyr::summarise(lanes = n())

    bases_per_project <-
      samples_in_date_range_grouped_by_project_id %>%
      dplyr::summarise(bases_sequenced = sum(cycles * pf_clusters, na.rm = TRUE))

    merge(x = merge(x = samples_per_project, y = lanes_per_project),
          y = bases_per_project)

  }

#' Fetches all information about a project and returns on the format:
#'
#' @param connection a connection object
#' @param project_name the name of the project to fetch data for
#' @return A data frame with all information for the project on the following format:
#' flowcell_id project_id sample_name tag_seq lane_num read_num cycles  pct_lane pf_clusters  pct_q30 pct_tag_err     library_name   mean_q   runfolder_name   run_date
#' 1      HVHL3CCXX    PA-1029       LR058  ACTTGA        1        1    151 13.176618    61628805 95.73171    3.032265   SX717_LR058.v1 40.09754     160816_ST-E00274_0100_BHVHL3CCXX 2016-08-16
#' 2      HVHL3CCXX    PA-1029       LR284  TTAGGC        1        1    151 10.769056    50368320 95.62205    3.079384   SX717_LR284.v1 40.053861    160816_ST-E00274_0100_BHVHL3CCXX 2016-08-16
#'
#' @export
#'
#' @examples
fetch_information_for_project <-
  function(connection, project_name) {
    sample_level_project_info <-
      sample_table(connection) %>%
      dplyr::filter(project_id == project_name) %>%
      dplyr::collect(n = Inf)

    flowcell_level_info <-
      flowcell_runfolder_table(connection) %>%
      dplyr::collect(n = Inf)

    merge(x = sample_level_project_info,
          y = flowcell_level_info,
          by = "flowcell_id")
  }


#' Fetch aggregated data for all instruments for the specified period
#'
#' @param connection a connection object
#' @param start_date on format '2017-01-01' inclusive
#' @param end_date on format '2017-12-31' exclusive
#'
#' @return A data frame with aggregate data for each instrument
#' month   year instrument giga_bases mean_pct_q30 cummulative_giga_bases
#' <fctr> <fctr>     <fctr>      <dbl>        <dbl>                  <dbl>
#' 1      01   2016    HiSeq 3   136.2203    0.7901464               136.2203
#' 2      03   2016    HiSeq 3   784.2160    0.8173106               920.4363
#' 3      04   2016    HiSeq 3   752.7554    0.8559456              1673.1917
#' 4      05   2016    HiSeq 3   235.9002    0.7706029              1909.0919
#' 5      06   2016    HiSeq 3   500.5259    0.8773095              2409.6177
#' 6      07   2016    HiSeq 3  1040.6433    0.8605515              3450.2611
#' 7      08   2016    HiSeq 3   489.9396    0.7885864              3940.2007
#' 8      11   2016    HiSeq 3   103.4723    0.9756829              4043.6730
#' 9      02   2016    HiSeq 4   765.1809    0.9520556               765.1809
#' 10     03   2016    HiSeq 4   914.8843    0.8691078              1680.0653
#'
#' @export
#'
#' @examples
fetch_aggregated_data_per_instrument_for_period <-
  function(connection, start_date, end_date) {
    flowcells_and_run_dates_in_range <-
      flowcell_runfolder_table(connection) %>%
      dplyr::filter(run_date >= as.Date(start_date), run_date < as.Date(end_date)) %>%
      dplyr::collect(n = Inf)

    flowcell_lane_level_info <-
      flowcell_lane_table(connection) %>%
      dplyr::collect(n = Inf)

    lane_info_in_date_range <-
      merge(x = flowcells_and_run_dates_in_range,
            y = flowcell_lane_level_info,
            by = "flowcell_id")

    aggregated_information <-
      lane_info_in_date_range %>%
      add_instrument_name_to_dataframe() %>%
      dplyr::mutate(month = as.factor(format(run_date, "%m")),
                    year = as.factor(format(run_date, "%Y"))) %>%
      dplyr::group_by(month, year, instrument) %>%
      dplyr::summarise(giga_bases = sum(pf_clusters * cycles) / 10 ^ 9,
                       mean_pct_q30 = mean(pct_q30)) %>%
      dplyr::group_by(instrument) %>%
      dplyr::mutate(cummulative_giga_bases = cumsum(giga_bases)) %>%
      dplyr::arrange(instrument, year, month)

    aggregated_information

  }
