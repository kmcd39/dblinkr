# setup ---------------------------------------------------------------------
library(sf)
library(dplyr)
library(dblinkr)
library(purrr)

shp.dir <- "~/R/shapefiles/"
con <- princeton.db.connect(usr="km31"
                            ,pw="Sh@rkey20")

# 1947 hwy plan ---------------------------------------------------------------------
# this is the 1947 hwy plan digitized by philly federal reserve ppl, after I
# made some manual edits using mapedit::editFeatures
cleaned_plan <- st_read(paste0(shp.dir, "1947plan/most addl cleans/cleaner-hwy-plan.shp"))

cleaned_plan["id"] %>% plot()
#con <- princeton.db.connect(driver = "RPostgreSQL")
#con <- aws.connect(driver = "RPostgreSQL")

dbListObjects(con)
tbls.in.schema(con, "divs")

st_write(obj = cleaned_plan, dsn = con, Id(schema="divs",  table="hwy_plan_1947"))


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

