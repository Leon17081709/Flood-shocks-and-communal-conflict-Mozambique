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
          color = NA,        
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





 
    
  
























