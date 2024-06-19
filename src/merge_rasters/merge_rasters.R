require(umbrella)
orderly2::orderly_shared_resource("shape_file.RDS" = "shape_file.RDS")
orderly2::orderly_artefact("Dataset of the daily chirps rainfall data based on the shape file",
                           "full_data.RDS")

# read in shape file
adm2_shp <- readRDS("shape_file.RDS")

# chirps data urls
urls <- umbrella::get_urls(year = 2022) # chose a year without an extreme weather event
dir.create(paste0(getwd(), "/output"))
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
saveRDS(full_data,"full_data.RDS")

# remove the output files to reduce the size of the packet
for(i in 1:length(urls)) {
  file.remove(paste0("output/raster", i, ".tif"))
}
