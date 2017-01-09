


#' Title
#'
#' @param data
#'
#' @return
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
        "M00629"
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
        "MiSeq 2"
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

#' Title
#'
#' @param connection
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
fetch_project_summaries_for_date_range <-
  function(connection, start_date, end_date) {
    flowcells_and_run_dates_in_range <-
      flowcell_runfolder_table(connection) %>%
      select(flowcell_id, run_date) %>%
      filter(run_date >= as.Date(start_date), run_date < as.Date(end_date)) %>%
      collect()

    samples <-
      sample_table(connection) %>%
      collect(n = Inf)

    samples_in_date_range <-
      samples[samples$flowcell_id %in% flowcells_and_run_dates_in_range$flowcell_id,]

    samples_in_date_range_grouped_by_project_id  <-
      samples_in_date_range %>%
      group_by(project_id)

    samples_per_project <-
      samples_in_date_range_grouped_by_project_id %>%
      filter(read_num == 1) %>%
      distinct(sample_name) %>%
      tally()

    lanes_per_project <-
      samples_in_date_range_grouped_by_project_id %>%
      distinct(flowcell_id, lane_num) %>%
      summarise(lanes = n())

    bases_per_project <-
      samples_in_date_range_grouped_by_project_id %>%
      summarise(bases_sequenced = sum(cycles * pf_clusters, na.rm = TRUE))

    merge(x = merge(x = samples_per_project, y = lanes_per_project),
          y = bases_per_project)

  }

#' Title
#'
#' @param connection
#' @param project_name
#'
#' @return
#' @export
#'
#' @examples
fetch_information_for_project <-
  function(connection, project_name) {
    sample_level_project_info <-
      sample_table(connection) %>%
      filter(project_id == project_name) %>%
      collect(n = Inf)

    flowcell_level_info <-
      flowcell_runfolder_table(connection) %>%
      collect(n = Inf)

    merge(x = sample_level_project_info,
          y = flowcell_level_info,
          by = "flowcell_id")
  }


#' Title
#'
#' @param connection
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
fetch_aggregated_data_per_instrument_for_period <-
  function(connection, start_date, end_date) {
    flowcells_and_run_dates_in_range <-
      flowcell_runfolder_table(connection) %>%
      filter(run_date >= as.Date(start_date), run_date < as.Date(end_date)) %>%
      collect(n = Inf)

    flowcell_lane_level_info <-
      flowcell_lane_table(connection) %>%
      collect(n = Inf)

    lane_info_in_date_range <-
      merge(x = flowcells_and_run_dates_in_range,
            y = flowcell_lane_level_info,
            by = "flowcell_id")

    aggregated_information <-
      lane_info_in_date_range %>%
      add_instrument_name_to_dataframe() %>%
      mutate(month = as.factor(format(run_date, "%m")),
             year = as.factor(format(run_date, "%Y"))) %>%
      group_by(month, year, instrument) %>%
      summarise(giga_bases = sum(pf_clusters * cycles) / 10 ^ 9)

    aggregated_information

  }
