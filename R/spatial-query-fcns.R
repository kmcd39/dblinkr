# querying shapes ---------------------------------------------------------


#' query.division
#'
#' Extracts all geometries from a postgis table that lay in the bounding box of
#' the supplied \code{region}.
#' @param connection a connection, as retrieved from \code{princeton.db.connect}
#' @param region A sf object
#' @export
query.division <- function(connection, region, div.name, crs=4326) {
  require(sf)
  # extract bbox from query
  bb <- st_bbox(region)
  bb.str <- paste(bb, collapse = ", ")
  
  qry <- paste0("SELECT * FROM ",div.name," WHERE geometry && ST_MakeEnvelope(",
                bb.str,", ","4326);")
  
  error.check <- tryCatch(
    {
      out <- st_read(connection, query=qry, geometry_column="geometry")
      st_crs(out) <- crs
      return(out)
    },
    error=function(e) e
  )
  if(!inherits(error.check, "error")){
    return(error.check)
  }
  return(NULL)
}
# this works, as in:
# query.division(con, st_sf(out_set()), "shps.hwys") # called w/in app


#' persistent.query.division
#'
#' Applies query division until a non-null result is retrieved. A workaround to
#' an issue when the db is queried from within a shiny app. Not neccessary
#' outside of shiny context.
#' @export
persistent.query.division <- function(connection, region, div.name) {
  
  out <- query.division(connection, region, div.name)
  while(is.null(out))
    out <- query.division(connection, region, div.name)
  
  return(out)
}


#' query.by.geoid
#' 
#' Both slower and less convenient than above, so not included. 
query.by.geoid <- function(con, region.type, geoid, tbl_name = "geopaired_hwys") {
  require(sf)
  require(dplyr)
  
  divdb = tbl(con,
              dbplyr::in_schema("shps",
                                tbl_name))
  
  # setup dplyr for writing query
  divq = divdb %>%
    filter(region.type == local(region.type) &
             region.id == local(geoid))
  
  # extract query using dbplyr and send to st_read
  q = dbplyr::sql_render(divq)
  out <- st_read(con
                 , query = q)
  
  return(out)
}
# query.by.geoid(con, test.area$region.type, get_geoid(test.area$id))

