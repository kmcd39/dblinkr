
# setup ws ---------------------------------------------------------------------
library(sf)
library(dplyr)
library(dblinkr)
library(purrr)

shp.dir <- "~/R/shapefiles/"
con <- princeton.db.connect(usr="km31"
                            ,pw="Sh@rkey20")


# counties ----------------------------------------------------------------
# get counties, full detail; 2019
counties = tigris::counties(cb = FALSE, year = 2015)
counties <- st_transform(counties, 4326)
#counties %>% tibble() %>% count(FUNCSTAT)

colnames(counties) <- tolower(colnames(counties))
st_write(obj = counties, dsn = con, Id(schema="regions", table="counties"))

# get simplified counties
counties_simplified = tigris::counties(cb = T, 
                            resolution = "5m"
                            , year = 2015)
counties_simplified <- st_transform(counties_simplified, 4326)

colnames(counties_simplified) <- tolower(colnames(counties_simplified))
st_write(obj = counties_simplified, dsn = con, Id(schema="regions_simplified", table="counties"))


# build czs from counties -------------------------------------------------
# uses county xwalk from https://github.com/walkerke/us-boundaries/blob/master/cz_1990_v2_sp.R
# ( but i want to build from my own counties to get full detail )

# FIPS codes for counties deleted since 1990 census
counties_deleted <- c(
  "02201", "02231", "02270", "02280", "12025", "30113", "46113", "51515",
  "51560", "51780"
)
# FIPS codes for counties added since 1990 census with 1990 commuting zone
counties_added <- tribble(
  ~fips_county, ~cz_1990,
  "02068", "34115",
  "02105", "34109",
  "02158", "34112",
  "02195", "34110",
  "02198", "34111",
  "02230", "34109",
  "02275", "34111",
  "02282", "34109",
  "08014", "28900",
  "12086", "07000",
  "46102", "27704"
)
# URL for commuting zone county partition using 1990 counties
url_cz <- "https://www.ers.usda.gov/webdocs/DataFiles/48457/czlma903.xls?v=68.4"
### older link from walkerke code: "https://www.ers.usda.gov/webdocs/DataFiles/Commuting_Zones_and_Labor_Market_Areas__17970/czlma903.xls"
cz_loc <- "~/R/dblinkr/.tmp/czlma903.csv" #.xls
# Read commuting zone county partition, add place and state variables
library(stringr)
co2cz <- 
  read.csv(cz_loc) %>%
  tibble() %>%
  select(
    fips_county = contains("FIPS"),
    cz_1990 = CZ90,
    place_state = contains("largest.place")
  ) %>% 
  mutate_at(c(1,2),
            ~stringr::str_pad(., 5, side= "left", "0")) %>%
  mutate(
    place =
      place_state %>% 
      str_replace(" borough.*| CDP.*| city.*| town.*| \\(rem.*|,.*", ""),
    state = place_state %>% str_sub(start = -2L)
  ) %>% 
  select(-place_state) %>%
  rename( cz_name = place)
co2cz
# Adjust county partition for counties added and deleted since 1990
v <- 
  counties_added %>% 
  left_join(co2cz %>% select(-fips_county) %>% distinct(), by = "cz_1990")
co2cz <- 
  bind_rows(co2cz, v) %>% 
  filter(!(fips_county %in% counties_deleted))



# end adaptation from walkerke code ---------------------------------------

# write neat county-cz cwalk to db
dbWriteTable(conn = con
             ,name = Id(schema = "xwalks", table = "county2cz")
             ,value = co2cz
)


# -------------------------------------------------------------------------

# union counties to czs from my shp ---------------------------------------

czs <- co2cz %>%
  left_join(counties, by = c("fips_county" = "geoid"))

czs <- czs %>%
  rename(cz = cz_1990) %>%
  st_sf() %>%
  group_by(cz, cz_name) %>%
  summarise(., do_union=T) %>%
  st_transform(4326)

# czs["cz"] %>% plot()

# simplified
czs_simplified <- co2cz %>%
  left_join(counties_simplified, by = c("fips_county" = "geoid"))

czs_simplified <- czs_simplified %>%
  rename(cz = cz_1990) %>%
  st_sf() %>%
  group_by(cz, cz_name) %>%
  summarise(., do_union=T) %>%
  st_transform(4326)

# write
st_write(obj = czs, dsn = con, Id(schema="regions", table="czs_1990"))

st_write(obj = czs_simplified, dsn = con, Id(schema="regions_simplified", table="czs_1990"))



# -------------------------------------------------------------------------


# states ------------------------------------------------------------------
states = tigris::states()
states = select(states,
                c( STATEFP 
                   ,abv = STUSPS
                   ,NAME, geometry))
colnames(states) <- tolower(colnames(states))

st_write(obj = states, dsn = con, Id(schema="regions", table="states"))

# states= st_read(con, query="select * from regions.states;")

# cbsas -------------------------------------------------------------------

cbsa = tigris::core_based_statistical_areas(year = 2019)

st_write(obj = cbsa, dsn = con, Id(schema="regions", table="cbsa"))


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


st_write(obj = cts, dsn = con, Id(schema="regions", table="cts_2010"))
st_write(obj = stS, dsn = con, Id(schema="regions_simplified", table="cts_2010"))