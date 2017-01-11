
# ------------------------------------------------------------------------
# This is a super short tutorial with examples of how to use this package
# ------------------------------------------------------------------------

library(projmanr)

# First establish a connection object. Please note that you first have to have a
# `sql.yaml` file which contains the connection information to connect to the database.
connection <- RSQLServer::src_sqlserver("MS_SQL", file='sql.yaml', database = 'ProjectMan')

# These are some nice dates, right?
start_date <- "2016-01-01"
end_date <- "2016-12-31"

# Fetch aggregated data for all instruments active in the period.
fetch_aggregated_data_per_instrument_for_period(connection, start_date, end_date)

# Fetch all available information for a specific project
fetch_information_for_project(connection, "PA-1029")

# Fetch
fetch_project_summaries_for_date_range(connection, start_date, end_date)

