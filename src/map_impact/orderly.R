orderly2::orderly_parameters(calibrated = TRUE, repetitions = 20)
orderly2::orderly_artefact("maps", c("averted.pdf", 
                                     "proportion.pdf", 
                                     "per_child.pdf"))
# pull in the dependency from analyse_impact
# calibrated <- this:calibrated
# repetitions <- this:repetitions

library(data.table)
orderly2::orderly_resource("shape_file.RDS")
orderly2::orderly_resource("districts.RDS")

# start with the model outputs
# read in all districts
districts <- readRDS("districts.RDS")
districts <- districts[districts != "Kampala"] # exclude Kampala b/c EIR = 0

output<- data.table()
for(i in 1:length(districts)) {
  district <- districts[i]
  metadata <- orderly2::orderly_dependency("analyse_impact",
                                           quote(latest(parameter:district == environment:district &&
                                                        parameter:calibrated == this:calibrated &&
                                                        parameter:repetitions == this:repetitions)),
                                           c('df_${district}.RDS' = "df_comb.RDS"))
  dt <- readRDS(metadata$files$here)
  output <- rbind(output, dt, fill = T)
}

# load packages
library(tidyverse)
library(lubridate)
library(rgdal)
library(rgeos)
library(maptools)
library(sf)
library(terra)
library(ggpubr)

shape <- readRDS("shape_file.RDS")
df_comb <- output

averted <- df_comb %>%
  dplyr::select(district, averted_50, year) %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(averted_annually_50 = mean(averted_50))
proportion <- df_comb %>%
  dplyr::select(district, proportion_50, year) %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(proportion_annually_50 = mean(proportion_50))
per_child <- df_comb %>%
  dplyr::select(district, per_child_50, year) %>%
  dplyr::group_by(district) %>%
  dplyr::reframe(per_child_annually_50 = mean(per_child_50))

averted <- full_join(shape, averted, join_by(ADM2_EN == district))
proportion <- full_join(shape, proportion, join_by(ADM2_EN == district))
per_child <- full_join(shape, per_child, join_by(ADM2_EN == district))

# averted per child
ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = averted, aes(fill = averted_annually_50)) +
  theme_bw() +
  # theme(panel.background = element_rect(fill = "white")) + 
  guides(fill=guide_legend(title="Cases averted \nannually")) 
ggsave("averted.pdf", dpi = 300)

ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = proportion, aes(fill = proportion_annually_50)) + 
  theme_bw() +
  guides(fill=guide_legend(title="Proportion of cases \naverted annually")) 
ggsave("proportion.pdf", dpi = 300)

ggplot() + geom_sf(data = shape, fill = "grey85", lwd = 0.4) +
  geom_sf(data = per_child, aes(fill = per_child_annually_50)) +
  theme_bw() +
  guides(fill=guide_legend(title="Cases averted \nper child per year")) 
ggsave("per_child.pdf", dpi = 300)
while (!is.null(dev.list()))  dev.off()
