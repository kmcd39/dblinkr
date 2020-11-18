

library(sf)
library(dplyr)
library(dblinkr)
library(purrr)

shp.dir <- "~/R/shapefiles/"
con <- princeton.db.connect(usr="km31",pw="Sh@rkey20")
# 1947 hwy plan ---------------------------------------------------------------------
# this is the 1947 hwy plan digitized by philly federal reserve ppl, after I
# made some manual edits using mapedit::editFeatures
cleaned_plan <- st_read(paste0(shp.dir, "1947plan/most addl cleans/cleaner-hwy-plan.shp"))


cleaned_plan["id"] %>% plot()
#con <- princeton.db.connect(driver = "RPostgreSQL")
#con <- aws.connect(driver = "RPostgreSQL")

dbListObjects(con)
tbls.in.schema(con, "divs")

st_write(obj = cleaned_plan, dsn = con, Id(schema="divs", table="hwyPlan"))


tmp <- st_read(con, query = 'select * from "divs"."hwyPlan1947";') # I think the " "s are necessary for tbl name if there's uppercase...

'
dbDisconnect(con)
rm(con)
'
# nhpn --------------------------------------------------------------------

nhpn <- st_read(paste0(shp.dir, "National_Highway_Planning_Network-shp/National_Highway_Planning_Network.shp"))

nhpn <- nhpn %>%
  group_by_at(vars(all_of(c("SIGNT1", "SIGN1", "FCLASS")))) %>%
  summarise(.,  do_union = TRUE)

nhpn.dn <- nhpn %>% split(.$SIGNT1)

# process set in for loop to handle memory more conservatively ------------
# (had gotten a crash)
for(hwy.type in names(nhpn.dn)) { # c("N","O")){  #
  just.cleaned <- nhpn.dn[[hwy.type]] %>%
    divFcns::denode.lines(group.cols = c("SIGNT1", "SIGN1", "FCLASS"))
  
  saveRDS(just.cleaned, 
          paste0(".intermediate-saves/denoded-",hwy.type,"-hwys.RDS"))
  rm(just.cleaned)
}

# read all rds s back in
rm(list = ls())

hwy.parts <- list.files(path = ".intermediate-saves/", pattern = "*-hwys.RDS")

hwy.parts <- paste0(".intermediate-saves/", hwy.parts)

ldf <- lapply(hwy.parts, readRDS)
ldf <- do.call("rbind", ldf)

ldf <- st_transform(ldf, 4326)

st_write(obj = ldf, dsn = con, Id(schema="divs", table="hwys"))

# redlining -----------------------------------------------------------------

redlining <- st_read(paste0(shp.dir,
                            "redlining shpfiles/shapefile/holc_ad_data.shp"))

head(redlining, 2)
colnames(redlining)
redlining <- st_transform(redlining, 4326)
redlining["holc_grade"] %>% plot()

redlining <- redlining %>% select(-area_descr)

st_write(obj = redlining, dsn = con, Id(schema="divs", table="redlining"))



# places ------------------------------------------------------------------

plc <- st_read(paste0(shp.dir,
                            "places/places.shp"))

st_write(obj = plc, dsn = con, Id(schema="divs", table="places"))





# water -------------------------------------------------------------------
library(dbplyr)
tmp = tbl(con, in_schema("regions", 
                   "counties"))
county_list <- tmp %>% select(geoid) %>% collect() %>% pull(geoid)


county_list <- data.frame( state = substr(county_list,1,2)
                           ,county = substr(county_list,3,5))
county_list <-county_list %>%
  mutate_all(as.numeric)
# skip linear water shpfile, which is mostly tiny creeks and not large rivers
#lwater <- purrr::map(county_list,
#                     ~tigris::linear_water(state = substr(.,1,2)
#                                           , county = substr(.,3,5)
#                                           , class = "sf"))

# handle errors for counties missing large water areas w/ purrr::possibly
awater <- purrr::map2_dfr(county_list$state, county_list$county,
                          purrr::possibly( ~tigris::area_water(state = .x
                                                          , county = .y
                                                          , class = "sf")
                                      , otherwise = NULL
                                      , quiet = FALSE))

# tmp=awater # backup
#awater= readRDS(file = ".intermediate-saves/awater.RDS")
library(purrr)
#awater <- awater[!map_lgl(awater, is.null)] # drop nulls (relics of counties that didn't have water)
#colnames(awater[[1]])

awater <- awater %>%
  select(c(HYDROID, AWATER, FULLNAME, geometry))
#awater <- awater %>%
#  map( ~mutate_at(., vars("HYDROID", "AWATER"), as.numeric))


# parallel / generic rbind
# from https://stackoverflow.com/questions/7224938/can-rbind-be-parallelized-in-r
rbind.parallel <- function(list,ncore) {
  library(parallel)
  do.call.rbind<-function(x){do.call(rbind,x)}
  cl<-makeCluster(ncore)
  list.split<-split(list,rep(1:ncore,length(list)+1)[1:length(list)])
  list.join<-parLapply(cl,list.split,do.call.rbind)
  stopCluster(cl)
  list.out<-do.call(rbind,list.join)
  return(list.out)
}
awater <- rbind.parallel(awater, 4)
saveRDS(awater, file = ".intermediate-saves/awater_better i think.RDS")

# union'ing water
poly_simplify <- function(x) {
  uniond <- x %>% 
    st_union()
  out <- uniond %>%
    rmapshaper::ms_explode() %>%
    mutate(id = row_number())
  return(out)
}

#saveRDS(awater, file = ".intermediate-saves/awater2.RDS")


# Water shapefile contains many tiny bodies of water not relevant to this drop
# water unnamed water features. Alternatively, we could do this clean by
# dropping small bodies of water by area, perhaps after grouping by name.
awater <- awater %>% filter(!is.na(FULLNAME) & AWATER > 5e5)
awater <- awater %>% st_transform(4326)

st_write(obj = awater, dsn = con, Id(schema="divs", table="tigris_water"))


# -------------------------------------------------------------------------


# regions -----------------------------------------------------------------


# -------------------------------------------------------------------------



# 2010 CTs ---------------------------------------------------------------------

list.files(paste0(shp.dir,"2010 CTs"))

cts <- st_read(paste0(shp.dir,
                      "2010 CTs/US_tract_2010.shp"))
stS <- readRDS(paste0(shp.dir,
                      "2010 CTs/simplified-shp.RDS"))

head(cts)

cts <- cts %>%
  select( statefp = STATEFP10
          ,countyfp = COUNTYFP10
          ,tractce = TRACTCE10
          ,geoid = GEOID10
          ,gisjoin = GISJOIN )
stS <- stS %>%
  select( statefp = STATEFP10
          ,countyfp = COUNTYFP10
          ,tractce = TRACTCE10
          ,geoid = GEOID10
          ,gisjoin = GISJOIN )
cts <- cts %>% st_transform(4326)
stS <- stS %>% st_transform(4326)


st_write(obj = cts, dsn = con, Id(schema="regions", table="cts2010"))
st_write(obj = stS, dsn = con, Id(schema="regions", table="cts2010_simplified"))

# cbsas -------------------------------------------------------------------
# remembering if i had simplified cbsa polys
pcbsa = divDat::geo.list$cbsa

ncbsa = tigris::core_based_statistical_areas()
ncbsa %>% head()

tmp1 <- ncbsa %>% filter(GEOID ==12060) %>% st_transform(4326)
tmp2 <- pcbsa %>% filter(region.id ==12060) %>% st_transform(4326)

library(ggplot2)
library(mapview)
mapview(st_boundary(tmp1)) + 
  mapview(st_boundary(tmp2),color = "#208000")
# tigirs is different; i use that
cbsa = tigris::core_based_statistical_areas(year = 2019)

st_write(obj = cbsa, dsn = con, Id(schema="regions", table="cbsa"))
rm(list=ls())
# CZs --------------------------------------------------------------------

czS <- st_read(paste0(shp.dir,
                       "1990 commuting zones/cz1990.shp")) # realized this version was simplified polygons
                      
czs <- readRDS(paste0(shp.dir,
                      "1990 commuting zones/cb_2015_us_cz_500k_sf.rds")) %>% ungroup()
library(mapview)
czs %>% filter(place == "Philadelphia") %>% mapview() +
  mapview(st_boundary(filter(czS, cz == 19700 ))
          , color="red") +
  
# checks
pcz = divDat::geo.list$cz
tmp1 = pcz %>% filter(grepl("New York", region.name)) %>% st_transform(4326)
tmp2 = czs %>% filter(cz == 19400)

mapview(st_boundary(tmp1)) + 
  mapview(st_boundary(tmp2),color = "#208000")

pcz <- pcz %>%
  select(cz = region.id
         ,name = region.name
         ,geometry)
pcz <- pcz %>% st_transform(4326)

st_write(obj = pcz, dsn = con, Id(schema="regions", table="czs1990"))



# states ------------------------------------------------------------------
states = tigris::states()
states = select(states,
                c( STATEFP 
                  ,abv = STUSPS
                  ,NAME, geometry))
colnames(states) <- tolower(colnames(states))

st_write(obj = states, dsn = con, Id(schema="regions", table="states"))

# states= st_read(con, query="select * from regions.states;")

# counties ----------------------------------------------------------------
library(purrr)
counties = map_dfr(states$statefp,
                   ~tigris::counties(state = ., year = 2019))


counties %>% tibble() %>% count(LSAD)
head(counties)
counties["GEOID"] %>% plot()
counties %>% filter(STATEFP == 36) %>% `[`("NAME") %>% plot()
counties<- counties %>% select(c(STATEFP,
                                 GEOID,
                                 NAME,
                                 LSAD,
                                 CLASSFP))
colnames(counties) <- tolower(colnames(counties))

st_write(obj = counties, dsn = con, Id(schema="regions", table="counties"))
