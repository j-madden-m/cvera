#' Connect to the Animal Identification and Movement (AIM) SQL Database (db)
#'
#' Establishes a connection in R to the movement database on the server. Note,
#' you need to have set up a connection with the server via your PC initially,
#' with correct password etc (once-off set-up).
#'
#' @param database_name Name of the database. Default is "final1", this may change depending on Jamie T. "network" is also available.
#'
#' @return A DBI connection object.

#' @examples
#' \dontrun{
#' con_move <- connect_to_movement_db(final1)
#' }
#'
#' @export
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
connect_to_movement_db <- function(database_name = final1) {

  # Convert unquoted symbol to string, database (db) name
  db_name <- deparse(substitute(database_name))

  DBI::dbConnect(
    odbc::odbc(),
    # database on the server
    database = db_name,
    # data source name
    dsn = "Jamie_T_SQL_server"
  )
}



