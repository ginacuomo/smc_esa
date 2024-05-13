# create my shape file of the health districts matching the routine data
# admin2_shp <- readOGR(dsn="data/map", # directory of the folder containing shapefiles
#                       layer="uga_admbnda_adm2_ubos_20200824", # name of the admin1 file
#                       stringsAsFactors = FALSE)
# adm2_shp <- st_as_sf(admin2_shp[admin2_shp$ADM0_EN == "Uganda",])

## for Matt and Andria:
adm2_shp <- readRDS("output/shape_file.RDS")

# chirps data urls
urls <- umbrella::get_urls(year = 2022)
for(i in 1:length(urls)) {
  umbrella::download_raster(url = urls[i], 
                            destination_file = paste0("output/raster", i, ".tif.gz"))
}
#read in raster files
raster_files <- ""
for(i in 1:length(urls)) {
  raster_files <- c(raster_files, paste0("output/raster", i, ".tif"))
}

myraster <- terra::rast(raster_files[2:366])
full_data <- terra::extract(myraster, adm2_shp, fun = mean)
# rows are districts, columns are days

districts <- unique(adm2_shp$ADM2_EN)
full_data$districts <- districts
saveRDS(full_data,"input/full_data.RDS")
saveRDS(districts, "input/districts.RDS")
saveRDS(adm2_shp, "output/shape_file.RDS")
