############################################################
# SITE MAP of MALIBU dive sites plus fires (sf + ggplot, dplyr only)
#
# Author: Zoë Kitchel
# Last updated: 2026-01-26
#
# PURPOSE:
# Reproduce site map with:
#  - Fire footprints
#  - Location of dive sites
#  - California inset
#  - Google basemap
############################################################


#############################
# Packages
#############################
library(ggplot2)
library(ggnewscale)
library(ggrepel)
library(dplyr)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(cowplot)

#############################
# Load and site data
#############################
lat_lon_site_fix.malibu <- read.csv("lat_lon_site_fix.malibu.csv")

# Convert to sf object
lat_lon_sites_sf <- st_as_sf(lat_lon_site_fix.malibu, coords = c("Longitude", "Latitude"), crs = 4326)

#############################
# Load fire perimeters
#############################
woolsey <- st_read(file.path("data","California_Fire_Perimeters_(1950%2B)","California_Fire_Perimeters_(1950%2B).shp")) %>%
  st_transform(4326) %>%
  filter(FIRE_NAME == "WOOLSEY") %>%
  select(geometry) %>%
  mutate(IncidentNa = "2018 Woolsey")

palisades <- st_read(file.path("data","USA_Current_Wildfires","Current_Perimeters.shp")) %>%
  st_transform(4326) %>%
  filter(IncidentNa == "PALISADES") %>%
  mutate(IncidentNa = "2025 Palisades") %>%
  select(IncidentNa, geometry)

fire_perimeters <- rbind(woolsey, palisades)

#############################
# Load basemap
#############################
library(ggmap) #Make sure to get API! https://cran.r-project.org/web/packages/ggmap/readme/README.html
malibu_basemap <- get_googlemap("Malibu, CA, USA", zoom = 10, maptype = "satellite")

#############################
# Malibu map with PVR inset
#############################
malibu_firemap <- ggmap(malibu_basemap) +
  geom_sf(data = fire_perimeters, inherit.aes = FALSE,
          aes(color = IncidentNa, fill = IncidentNa), alpha = 0.5) +
  scale_color_manual(values = c("darkred","red")) +
  scale_fill_manual(values = c("darkred","red")) +
  geom_point(data = lat_lon_site_fix.malibu, aes(x = Longitude, y = Latitude), color = "white") +
  geom_text(data = lat_lon_site_fix.malibu, aes(x = Longitude, y = Latitude, label = label), size = 1.5) +
  coord_sf(xlim = c(-119.1,-118.4), ylim = c(33.9, 34.3), expand = FALSE, crs = 4326) +
  theme_classic() +
  labs(color = "Fire incident", fill = "Fire incident") +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = c(0.16,0.19)) +
  annotation_scale(location = "bl")

# California inset
states <- ne_states(country = "united states of america", returnclass = "sf")
california <- states %>% filter(name == "California")
california_inset <- ggplot() +
  geom_sf(data = california, lwd = 0.3, fill = "grey") +
  geom_rect(aes(xmin = -119.1, xmax = -118.4, ymin = 33.9, ymax = 34.3),
            color = "yellow", fill = "yellow", alpha = 0.3) +
  theme_void()

# Add inset to main map
test_fig2 <- malibu_firemap +
  annotation_custom(ggplotGrob(california_inset), xmin = -118.58, ymin = 34.15)

# Save
ggsave(test_fig2,
       filename = "test_fig2.jpg",
       path = "figures",
       width = 6, height = 4.5, units = "in", dpi = 300)
