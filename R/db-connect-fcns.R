library(dbplyr)


# misc helpers ------------------------------------------------------------
# shp.dir <- "~/R/shapefiles/"

# db connect fcns ---------------------------------------------------------


#' princeton.db.connect
#'
#' Stores code and some options to connect to princeton's postgres database.
#' Supply a username and password; default options should be appropriate
#' otherwise.
#' @param db name of database to connect to.
#' @param host db host.
#' @param usr Username for connecting to the datbase.
#' @param pw password.
#' @param driver Driver to use. Parsed from text to RPostgres:Postgres() or
#'   DBI::dbDriver("PostgreSQL"). First (default) option has been more
#'   convenient for me.
#' @param pool Whether or not to open a connection as pool. Defaults to false.
#' @export
db.connect <- function(usr, pw
                       ,db = "shp"
                       ,host = c("carto.princeton.edu", "geoseg-instance-1.cyj8wtbh3rhs.us-east-2.rds.amazonaws.com")
                       ,driver = c("Postgres", "RPostgreSQL")
                       ,pool=FALSE, port = 5432) {
  require(DBI)
  require(RPostgreSQL)
  driver = driver[1]
  host = host[1]
  driver.index = c("Postgres" = RPostgres::Postgres(),
                   "RPostgreSQL" = DBI::dbDriver("PostgreSQL"))
  cat("connecting with",driver,"driver")
  driver=driver.index[[driver]]
  
  if(pool) {
    con <- pool::dbPool(driver
                        ,dbname = db
                        ,host = host
                        ,port =  port
                        ,user = usr
                        ,password = pw)
  } else {
    con <- DBI::dbConnect(driver
                          ,dbname = db
                          ,host = host
                          ,port =  port
                          ,user = usr
                          ,password = pw) }
  return(con)
}
