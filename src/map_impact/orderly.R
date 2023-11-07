orderly2::orderly_strict_mode()
# pull in the dependency from analyse_impact
orderly2::orderly_dependency("analyse_impact", "latest", 
                             c(df_comb.rds = "df_comb.rds"))
orderly2::orderly_resource("data/map/uga_admbnda_adm2_ubos_20200824.shp")
orderly2::orderly_resource("data/map/uga_admbnda_adm1_ubos_20200824.shp")

# load packages
library(tidyverse)
library(lubridate)
library(rgdal)
library(rgeos)
library(maptools)
library(sf)
df_comb <- readRDS("df_comb.rds")

## we can do similar thing to admin1 (region) level
admin1_shp <- readOGR(dsn="data/map", # directory of the folder containing shapefiles
                      layer="uga_admbnda_adm1_ubos_20200824", # name of the admin1 file
                      stringsAsFactors = FALSE)
adm1 <- st_as_sf(admin1_shp)

## read district-level map
admin2_gadm <- readOGR(dsn="data/shapefiles/uga_gadm", # directory of the folder containing shapefiles
                      layer="gadm36_2", # name of the admin2 file
                      stringsAsFactors = FALSE)
adm2 <- st_as_sf(admin2_gadm[admin2_gadm@data$NAME_0 == "Uganda",])

## we can do similar thing to admin1 (region) level
admin2_shp <- readOGR(dsn="data/map", # directory of the folder containing shapefiles
                      layer="uga_admbnda_adm2_ubos_20200824", # name of the admin1 file
                      stringsAsFactors = FALSE)
karamoja <- admin2_shp[admin2_shp$ADM2_EN %in% c("Kaabong", "Kotido", "Abim", "Napak", 
                                      "Moroto", "Nakapiripirit", "Amudat", "Karenga", "Nabilatuk"),]%>% 
   st_as_sf()

test <- adm2 %>% left_join(df_comb,
                            by=join_by(NAME_1==district))
# averted per child
ggplot() + geom_sf(data = adm1, fill = "grey85", lwd = 0.4) +
  geom_sf(data = test, aes(fill = per_child_50)) +
  theme(panel.background = element_rect(fill = "white")) #+
  # geom_sf(data = karamoja, col = "red", alpha = 0, lwd = 0.4)
  
#proportion of cases averted annually
ggplot() + geom_sf(data = adm1, fill = "grey85", lwd = 0.4) +
  geom_sf(data = test, aes(fill = proportion_50)) +
  theme(panel.background = element_rect(fill = "white")) +
  geom_sf(data = karamoja, col = "red", alpha = 0, lwd = 0.4)

