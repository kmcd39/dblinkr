library(dbplyr)


# misc helpers ------------------------------------------------------------
# shp.dir <- "~/R/shapefiles/"

# db connect fcns ---------------------------------------------------------

#' aws.connect
#'
#' connects to my amazon web service (aws) database. 
aws.connect <- function( usr, pw
                        ,db = "geoseg_1"
                        ,driver = c("Postgres", "RPostgreSQL")
                        ,pool=FALSE, port = 5432) {
  require(DBI)
  require(RPostgreSQL)
  driver = driver[1]
  driver.index = c("Postgres" = RPostgres::Postgres(),
                   "RPostgreSQL" = DBI::dbDriver("PostgreSQL"))
  cat("connecting with",driver,"driver")
  driver=driver.index[[driver]]
  
  #login info
  
  if(pool) {
    con <- pool::dbPool(driver
                        ,dbname = db
                        ,host = "geoseg-instance-1.cyj8wtbh3rhs.us-east-2.rds.amazonaws.com"
                        ,port =  port
                        ,user = usr
                        ,password = pw)
  } else {
    con <- DBI::dbConnect(driver
                          ,dbname = db
                          ,host = "geoseg-instance-1.cyj8wtbh3rhs.us-east-2.rds.amazonaws.com"
                          ,port =  port
                          ,user = usr
                          ,password = pw) }
  return(con)
}




#' princeton.db.connect
#'
#' Stores code and some options to connect to princeton's postgres database.
#' Supply a username and password; default options should be appropriate
#' otherwise.
#' @param db name of database to connect to.
#' @param usr Username for connecting to the datbase.
#' @param pw password.
#' @param driver Driver to use. Parsed from text to RPostgres:Postgres() or
#'   DBI::dbDriver("PostgreSQL"). First (default) option has been more
#'   convenient for me.
#' @param pool Whether or not to open a connection as pool. Defaults to false.
#' @export
princeton.db.connect <- function(usr, pw
                                 ,db = "shp"
                                 ,driver = c("Postgres", "RPostgreSQL")
                                 ,pool=FALSE, port = 5432) {
  require(DBI)
  require(RPostgreSQL)
  driver = driver[1]
  driver.index = c("Postgres" = RPostgres::Postgres(),
                   "RPostgreSQL" = DBI::dbDriver("PostgreSQL"))
  cat("connecting with",driver,"driver")
  driver=driver.index[[driver]]
  
  if(pool) {
    con <- pool::dbPool(driver
                        ,dbname = db
                        ,host = "carto.princeton.edu"
                        ,port =  port
                        ,user = usr
                        ,password = pw)
  } else {
    con <- DBI::dbConnect(driver
                          ,dbname = db
                          ,host = "carto.princeton.edu"
                          ,port =  port
                          ,user = usr
                          ,password = pw) }
  return(con)
}
