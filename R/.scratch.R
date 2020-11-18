phlcz = st_read(con, query = "select * from regions.czs1990 where name='Philadelphia';")
mapview::mapview(phlcz)
# wtf montgomery county ---------------------------------------------------
montco <- tigris::area_water(state = 42, county = 91, year = 2019)
montco %>% mapview()

montco$HYDROID %in% awater$HYDROID

# alt water shps ----------------------------------------------------------

wtr = st_read("~/R/shapefiles/water/natural earth river+lake centerlines/natl/ne_10m_rivers_north_america.shp")
wtr = wtr %>% select(1:5)
wtr <- st_transform(wtr, 4326)
#wtr <- st_intersection(wtr, czs)
wtr["name"] %>% plot()
phl <- st_transform(phl, 4326)
tmp <- st_intersection(phl, wtr)
mapview(st_boundary(phl)) + mapview(tmp, color = "red")

wtr %>%
  rmapshaper::ms_explode() %>%
  mapview(zcol="name")
# checking worldbank water ------------------------------------------------
wtr = st_read("~/R/shapefiles/majorrivers_0_0/MajorRivers.shp")
wtr <- st_transform(wtr , 4326)
czs <- divDat::geo.list$cz
czs <- czs %>% st_union() %>% st_transform(4326)
wtr <- st_intersection(wtr, czs)
wtr["NAME"] %>% plot()



# another tigris water check... -------------------------------------------
tmp <- st_intersection(awater, st_transform(phl, st_crs(awater)))

tmp2 <- st_union(tmp)
tmp2 <- st_transform(tmp2, st_crs(phl))
watercropped.phl <- st_difference(phl, tmp2)
mapview(watercropped.phl)



# checking ways to clean better tigirs file -------------------------------
tmp %>% head()
tmp %>% mapview(zcol = "AWATER")

tmp2 <- tmp %>%
  filter(!is.na(FULLNAME) |
           AWATER > 5e5)

tmp3 <- tmp %>%
  filter(!is.na(FULLNAME) &
           AWATER > 5e5)

tmp2 %>%
  filter(!HYDROID %in% tmp3$HYDROID) %>%
  mapview(color = "red") + mapview(tmp3, color = "#008080") + mapview(st_boundary(phl), color = "#000000")
tmp3 %>% mapview(zcol = "AWATER")

watercropped.phl <- st_difference(phl, 
                                  st_union(st_transform(tmp3, st_crs(phl))))

tmp = watercropped.phl %>%
  st_union() %>%
  rmapshaper::ms_explode() %>%
  st_sf(poly.id = 1:length(.)
        ,geometry = .) %>%
  mutate(aland = lwgeom::st_geod_area(.))

tmp %>%
  mutate(aland = as.numeric(aland)) %>%
  filter(aland > 1e7) %>%
  mutate(poly.id = 1:nrow(.)) %>%
  mapview(zcol="poly.id")
            #"aland"   
  

# union'ing water ---------------------------------------------------------

phl <- divDat::geo.list$cz %>% filter(region.name == "Philadelphia")
phl <- st_transform(phl, st_crs(awater))

tmp <- awater %>%
  st_crop(phl)

tmp$FULLNAME %>% is.na(.) %>% sum()
tmp$AWATER %>% summary()
tmp2 <- tmp[!is.na(tmp$FULLNAME),]

tmp3 <- tmp %>%
  filter(!is.na(FULLNAME) |
           AWATER > 5e5 )

tmp3["AWATER"] %>% plot()

library(mapview)

cleaned.phl.water <- st_union(tmp3)
cleaned.phl.water <- cleaned.phl.water %>%
  rmapshaper::ms_explode() %>%
  st_sf(geometry = .,
        id = 1:length(.))
cleaned.phl.water["id"] %>% plot()
mapview(phl)
watercropped.phl <- st_difference(phl, st_union(tmp))
mapview(watercropped.phl)

# bsic workflow -----------------------------------------------------------


con <- aws.connect()

dbListObjects(con)
tbls.in.schema(con, "divs")
tbls.in.schema(con, "regions")

library(sf)
library(dplyr)
phl <- divDat::geo.list$cz %>% filter(region.name == "Philadelphia")
phl <- st_transform(phl, 4326)

phwy <- persistent.query.division(con, phl, "divs.lac_hwys")

phwy["SIGNT1"] %>% plot(main = "limited-access hwys around philly")

dbDisconnect(con)
rm(con)



