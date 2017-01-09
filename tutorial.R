library(dplyr)

connection <- RSQLServer::src_sqlserver("MS_SQL", file='sql.yaml', database = 'ProjectMan')

start_date <- "2016-01-01"
end_date <- "2016-12-31"

fetch_aggregated_data_per_instrument_for_period(connection, start_date, end_date)

fetch_information_for_project(connection, "PA-1029")

fetch_project_summaries_for_date_range(connection, start_date, end_date)
