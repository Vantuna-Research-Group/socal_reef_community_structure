############################################################
# SITE MAP WITH PVR INSET (sf + ggplot, dplyr only)
#
# Author: Zoë Kitchel
# Last updated: 2026-01-26
#
# PURPOSE:
# Reproduce a Figure 1–style site map with:
#  - Artificial vs natural reefs
#  - Depth zones
#  - California inset
#  - Palos Verdes Peninsula (PVR) zoom inset
############################################################


#############################
# 1. Packages
#############################
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(cowplot)
library(ggrepel)


#############################
# 2. Load and prepare site data
#############################

# Dive site priority list with corrected coordinates
dive_sites <- read.csv("dive_site_priority_list.csv")

# Extract Site and DepthZone from Location field
dive_sites <- dive_sites %>%
  mutate(
    DepthZone = stringr::word(Location, -1),
    Site      = stringr::word(Location, 1, -2)
  )

# Manual fixes for sites without depth zones
dive_sites <- dive_sites %>%
  mutate(
    DepthZone = case_when(
      Location == "Malibu Bluffs Eelgrass" ~ "Middle",
      Location %in% c(
        "Santa Monica Jetty",
        "Santa Monica Jetty - Interior",
        "Marina del Rey Breakwater - Exterior"
      ) ~ NA_character_,
      TRUE ~ DepthZone
    ),
    Site = case_when(
      Location == "Malibu Bluffs Eelgrass" ~ "Malibu Bluffs Eelgrass",
      Location %in% c(
        "Santa Monica Jetty",
        "Santa Monica Jetty - Interior",
        "Marina del Rey Breakwater - Exterior"
      ) ~ Location,
      TRUE ~ Site
    )
  )


#############################
# 3. Classify reef type and depth zones
#############################

dive_sites <- dive_sites %>%
  mutate(
    ReefType = if_else(DepthZone == "ARM",
                       "Artificial reef",
                       "Natural reef"),
    ReefType = factor(ReefType),
    DepthZone = factor(
      DepthZone,
      levels = c("Inner", "Middle", "Outer", "Deep", "ARM"),
      labels = c("Inner (5 m)",
                 "Middle (10 m)",
                 "Outer (15 m)",
                 "Deep (25 m)",
                 "AR")
    )
  )


#############################
# 4. Average coordinates by site
#############################

sites_avg <- dive_sites %>%
  group_by(Site, ReefType) %>%
  summarise(
    avg_lon = mean(Longitude, na.rm = TRUE),
    avg_lat = mean(Latitude,  na.rm = TRUE),
    .groups = "drop"
  )


#############################
# 5. Convert to sf objects
#############################

sites_avg <- sites_avg[complete.cases(sites_avg),] #Delete missing lat/lon values

sites_sf <- st_as_sf(
  sites_avg,
  coords = c("avg_lon", "avg_lat"),
  crs = 4326
)

dive_sites.cc <- dive_sites[complete.cases(dive_sites),]  #Delete missing lat/lon values

dive_sf <- st_as_sf(
  dive_sites.cc,
  coords = c("Longitude","Latitude"),
             crs = 4326)


#############################
# 6. Base maps
#############################

usa <- ne_countries(
  country = "United States of America",
  returnclass = "sf",
  scale = "large"
)

mexico <- ne_countries(
  country = "Mexico",
  returnclass = "sf",
  scale = "large"
)

states <- ne_states(
  country = "united states of america",
  returnclass = "sf"
)

california <- states %>% filter(name == "California")

# High-resolution CA coastline (project-specific)
CA_Map <- st_read("CA_Map_Nov2023.shp", quiet = TRUE) #I found this file on the internet


#############################
# 7. Main site map
#############################

site_map_basic <- ggplot() +
  geom_sf(data = usa, fill = "grey95", color = "grey50") +
  geom_sf(data = mexico, fill = "grey95", color = "grey50") +
  geom_sf(
    data = sites_sf,
    aes(fill = ReefType, shape = ReefType),
    size = 1.6,
    color = "black",
    alpha = 0.8
  ) +
  scale_fill_manual(values = c("darkturquoise", "brown1")) +
  scale_shape_manual(values = c(21, 24)) +
  coord_sf(
    xlim = c(-120.5, -116.75),
    ylim = c(32.6, 34.1),
    expand = FALSE
  ) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = c(0.72, 0.22),
    legend.title = element_blank()
  )


#############################
# 8. California inset
#############################

california_inset <- ggplot() +
  geom_sf(data = california, linewidth = 0.3) +
  geom_rect(
    aes(
      xmin = -120.5, xmax = -116.75,
      ymin = 32.6,   ymax = 34.3
    ),
    fill = NA,
    color = "black",
    linewidth = 0.4
  ) +
  theme_void()


#############################
# 9. PVR inset (Palos Verdes Peninsula)
#############################

# Transform sites to CA map CRS
sites_pvr_sf <- st_transform(dive_sf, st_crs(CA_Map))

PVR_inset <- ggplot() +
  geom_sf(data = CA_Map) +
  geom_sf(
    data = sites_pvr_sf ,
    aes(fill = DepthZone, shape = ReefType),
    size = 1.5,
    color = "black",
    stroke = 0.1
  ) +
  scale_fill_manual(
    values = c("#015AB5", "#785EF0", "#DC277F", "#FE6100", "white")
  ) +
  scale_shape_manual(values = c(21, 24)) +
  coord_sf(
    xlim = c(366453, 381075),
    ylim = c(3729596, 3741414),
    expand = FALSE
  ) +
  annotation_scale(location = "bl") +
  theme_classic() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line  = element_blank(),
    axis.title = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linetype = "dashed"
    ),
    legend.position = "none"
  )

#Dummy plot for final legend
PVR_inset_leg <- get_legend(
  ggplot() +
    geom_sf(
      data = sites_pvr_sf,
      aes(color = DepthZone, shape = DepthZone),
      size = 3
    ) +
    scale_color_manual(
      values = c("#015AB5", "#785EF0", "#DC277F", "#FE6100", "black")
    ) +
    scale_shape_manual(values = c(17, 17, 17, 17, 21)) +
    labs(
      color = "Depth zone /\nreef type",
      shape = "Depth zone /\nreef type"
    ) +
    theme_classic() + theme(
      legend.key.size = unit(0.3, "cm"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.spacing.y = unit(0.15, "cm")
    )
  
)



#############################
# 10. Assemble final figure
#############################

site_map_with_insets <- site_map_basic +
  annotation_custom(
    grob = ggplotGrob(california_inset),
    xmin = -117.7,
    ymin = 33.1
  )

test_fig1 <- ggdraw(site_map_with_insets) +
  draw_plot(PVR_inset, x = -0.28, y = 0.06, height = 0.55) +
  draw_plot(PVR_inset_leg, x = -0.2, y = 0.25, height = 0.3) +
  geom_segment(
    aes(x = 0.55, y = 0.73, xend = 0.37, yend = 0.64),
    arrow = arrow()
  ) +
  geom_text(
    aes(label = "Palos Verdes Peninsula", x = 0.22, y = 0.63),
    size = 5
  ) +
  geom_text(
    aes(label = "Santa Monica Bay", x = 0.46, y = 0.85),
    size = 5
  )



#############################
# 11. Save outputs
#############################

ggsave(
  test_fig1,
  filename = "test_fig1.jpg",
  path = "figures",
  width = 9,
  height = 4.5,
  units = "in",
  dpi = 500
)
