install.packages("sf")  
install.packages("viridis")
library(sf)
library(tidyverse)
library(viridis)
library(readr)

st_layers("Global_Flood_Records.gpkg")  # Polygon from DFO.
                                        # This provides the layer name

floods <- st_read("Global_Flood_Records.gpkg",
                  layer = "combined_floods")   # replace with actual name

head(floods)
names(floods)
st_geometry_type(floods)

unique(floods)

str(floods) 

floods$Notes

view(floods) 

Floods2 <- floods %>%
  filter(Country == "Mozambique") %>%
  mutate(across(c(Area, Duration, NumberOfFatalities, NumberOfDisplaced,
                  Severity, FloodImpactIndex), as.numeric))

view(Floods2)

Moz_districts <- st_read("gadm41_MOZ_2.shp") # Shapefile as a WD in Moz file.
                                             # MOZ_2 indicates data at district level.

st_crs(Moz_districts) # The CRS should match between Floods2 and Moz_districts
st_crs(Floods2)       # That is WGS 84

District_floods <- st_join(Moz_districts, Floods2, join = st_intersects)

view(District_floods)

Final_df <- st_drop_geometry(District_floods)

write.csv(Final_df, "flood_data.csv", row.names = F)


# Keeping floods to Mozambique boundaries only, since some crossed over to
# neighbor countries' boundaries.

Floods2_clipped <- st_intersection(Floods2, st_union(Moz_districts))

ggplot() + 
  geom_sf(data= District_floods, fill = "lightgrey", col = "black") +
  geom_sf(data = Floods2_clipped, aes(fill = Severity), alpha = 0.5)

# Flood mapping

ggplot() +
  geom_sf(data = Moz_districts, fill = "lightgray", color = "black") +
  geom_sf(data = Floods2_clipped, aes(fill = Severity), alpha = 0.5) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal()

  
  # Base map (clean districts)
  geom_sf(data = Moz_districts,
          fill = "white",
          color = "black",
          linewidth = 0.3) +
  
  # Flood polygons (clean style)
  geom_sf(data = Floods2_clipped,
          aes(fill = Severity),
          color = NA,        # 🔥 removes messy borders
          alpha = 0.7) +
  
  # Better color scale
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Flood Severity"
  ) +
  
  # Fix map layout
  coord_sf(expand = FALSE) +
  
  # Clean theme
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  ) +
  
  labs(
    title = "Flood Events in Mozambique",
    subtitle = "Clipped flood extents colored by severity"
  )


#####################################################################

District_summary <- District_floods %>%
  group_by(NAME_2) %>%   
  summarise(
    avg_severity = mean(Severity, na.rm = TRUE),
    total_floods = n(),
    .groups = "drop"
  )

ggplot() +
  geom_sf(data = District_summary,
          aes(fill = avg_severity),
          color = "black",
          linewidth = 0.3) +
  
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Mean Flood Severity"
  ) +
  
  coord_sf(expand = FALSE) +
  
  theme_minimal() +
  labs(
    title = "Flood Severity by District (Mozambique)",
    subtitle = "Aggregated from flood events"
  )





District_summary <- District_floods %>%
  group_by(NAME_1) %>%   
  summarise(
    avg_severity = mean(Severity, na.rm = TRUE),
    total_floods = n(),
    .groups = "drop"
  )

ggplot() +
  geom_sf(data = District_summary,
          aes(fill = avg_severity),
          color = "black",
          linewidth = 0.3) +
  
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Mean Flood Severity"
  ) +
  
  coord_sf(expand = FALSE) +
  
  theme_minimal() +
  labs(
    title = "Flood Severity by District (Mozambique)",
    subtitle = "Aggregated from flood events"
  )


####### FIXED FLOOD MAPPING

ggplot() +
  # Base layer — district fills
  geom_sf(data = Moz_districts, 
          fill = "white", 
          color = NA) +
  
  # Flood layer
  geom_sf(data = Floods2_clipped, 
          aes(fill = Severity), 
          alpha = 0.7,
          color = NA) +  # no border on flood polygons
  
  # District borders drawn ON TOP of floods — this is the key fix
  geom_sf(data = Moz_districts, 
          fill = NA,          # transparent fill
          color = "black", 
          linewidth = 0.3) +
  
  # Better color scale for severity
  scale_fill_gradientn(
    colors = c("#ffffcc", "#fd8d3c", "#bd0026"),  # yellow → orange → red
    name = "Flood Severity",
    breaks = c(1, 1.5, 2),
    labels = c("1.0\n(Large)", "1.5\n(Very Large)", "2.0\n(Extreme)")
  ) +
  
  # Clean theme
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 8),
    panel.grid = element_line(color = "grey90", linewidth = 0.2),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "grey40")
  ) +
  
  # Labels
  labs(
    title = "Flood Events in Mozambique by Severity",
    subtitle = "Source: Dartmouth Flood Observatory (DFO)",
    x = "Longitude",
    y = "Latitude"
  )






library(classInt)
library(ggrepel)

breaks <- classIntervals(District_summary$avg_severity, n = 5, style = "quantile")$brks

ggplot(District_summary) +
  geom_sf(aes(fill = cut(avg_severity, breaks = breaks, include.lowest = TRUE)),
          color = "black", linewidth = 0.2) +
  scale_fill_viridis_d(option = "plasma", direction = -1, name = "Avg Flood\nSeverity") +
  theme_minimal()

top_districts <- District_summary %>% 
  slice_max(avg_severity, n = 6)

ggplot(District_summary) +
  geom_sf(aes(fill = avg_severity), color = "black", linewidth = 0.2) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  ggrepel::geom_sf_label_repel(data = top_districts, aes(label = NAME_2),
                               size = 2.8, fill = "white") +
  theme_minimal()

# Adapt this mapping to match exactly how you coded Region in your regression data
region_lookup <- c("Cabo Delgado"="North","Niassa"="North","Nampula"="North",
                   "Zambezia"="Centre","Tete"="Centre","Manica"="Centre","Sofala"="Centre",
                   "Inhambane"="South","Gaza"="South","Maputo"="South","Maputo City"="South")

District_summary <- District_summary %>%
  mutate(Region = region_lookup[NAME_1])

region_boundaries <- District_summary %>%
  group_by(Region) %>%
  summarise(geometry = st_union(geometry))

ggplot() +
  geom_sf(data = District_summary, aes(fill = avg_severity), color = "grey60", linewidth = 0.1) +
  geom_sf(data = region_boundaries, fill = NA, color = "black", linewidth = 1) +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Avg Flood\nSeverity") +
  theme_minimal()















