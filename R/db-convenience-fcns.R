# db convenience fcns -----------------------------------------------------

#' tbls.in.schema
#'
#' lists tables in schema. use \code{DBI::dbListObjects} to view different
#' schema.
#' @param schema schema you want to check
#' @export
tbls.in.schema <- function(con, schema) {
  DBI::dbListObjects(con, prefix = Id(schema = schema))
}

