# checking cz -- same reso as counties ?? ---------------------------------
library(sf)
library(dplyr)
library(dblinkr)
library(purrr)

shp.dir <- "~/R/shapefiles/"
con <- princeton.db.connect(usr="km31",pw="Sh@rkey20")
# if not i will just aggregate from counties myself.
czS <- st_read(paste0(shp.dir,
                      "1990 commuting zones/cz1990.shp")) # realized this version was simplified polygons

czs <- readRDS(paste0(shp.dir,
                      "1990 commuting zones/cb_2015_us_cz_500k_sf.rds")) %>% ungroup()

pcounties <- dblinkr::query.division(con, filter(czs, place == "Philadelphia"), "regions.counties")
library(mapview)

filter(czs, place == "Philadelphia") %>% st_boundary() %>% mapview() +
  mapview(st_boundary(filter(czS, cz == 19700 ))
          , color="red") +
  mapview(st_cast(pcounties, "MULTILINESTRING"), color= "#008080")
# they're both simplified relative to counties. I will aggregate to counties.

phlcz = st_read(con, query = "select * from regions.czs_1990 where cz_name='Philadelphia';")
pcounties <- dblinkr::query.division(con, phlcz, "regions.counties")

library(mapview)
mapview(st_boundary(filter(phlcz, cz == 19700 ))
        , color="red",lwd=3) +
  mapview(st_cast(pcounties, "MULTILINESTRING"), color= "#008080")
### ahhh, that perfect overlap
