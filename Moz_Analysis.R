install.packages("sf")  
install.packages("viridis")
install.packages("scales")
install.packages("rnaturalearthdata")
install.packages("ggspatial")
install.packages("cowplot")
install.packages("stringdist")
install.packages("ggspatial")
install.packages("grid")


library(sf)
library(tidyverse)
library(viridis)
library(readr)
library(terra)
library(scales)
library(rnaturalearthdata)
library(geodata)
library(exactextractr)
library(modelsummary)
library(rnaturalearth)
library(patchwork)
library(spatial)
library(cowplot)
library(stringdist)
library(readxl)
library(haven)
library(ggspatial)
library(grid)

sf_use_s2(FALSE)


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

Moz_outline <- st_read("gadm41_MOZ_0.shp")

st_crs(Moz_districts) # The CRS should match between Floods2 and Moz_districts
st_crs(Floods2)       # That is WGS 84

District_floods <- st_join(Moz_districts, Floods2, join = st_intersects)

view(District_floods)

Final_df <- st_drop_geometry(District_floods)

write.csv(Final_df, "flood_data.csv", row.names = F)

Floods2_clipped <- st_intersection(Floods2, st_union(Moz_districts))

# Keeping floods to Mozambique boundaries only, since some crossed over to
# neighbor countries' boundaries.

Moz_districts2 <- st_read("gadm41_MOZ_2.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

Moz_districts2$NAME_1

Moz_districts2 <- Moz_districts2 %>% 
  mutate(
    Province = recode(Province,
    "Nassa" = "Niassa",
    "Maputo city" = "Cidade de Maputo"),
    District_Id = paste(Province, District, sep = "_"))

Moz_outline <- st_read("gadm41_MOZ_0.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

names(Moz_outline)

fp <- read_csv("full_panel_cgado.csv")
names(fp)

view(fp)

head(fp$District_Id)

conf <- read_xlsx("conflict_data.xlsx")

acled <- conf %>% 
  filter(
    year >= 1998, year <= 2023,
    event_type %in% c(
      "Riots",
      "Battles",
      "Violence against civilians"
    )
  ) %>% 
  filter(!is.na(longitude), !is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_intersection(Moz_outline)

flood_by_district <- fp %>% 
  group_by(District_Id) %>% 
  summarise(
    severity = max(Severity, na.rm = T),
    .groups = "drop"
  )

fp$District_Id

Moz_flood <- Moz_districts2 %>% 
  left_join(
    flood_by_district, by = "District_Id")

Moz_flood <- Moz_flood %>% 
  mutate(
    severity = replace_na(severity, 0),
    severity_bin = cut(
      severity,
      breaks = c(-Inf, 1.0,2.0,3.0,4.0, Inf),
      labels = c("No flood data",
                 "1-2",
                 "2-3",
                 "3-4",
                 ">4"),
      include.lowest = TRUE
    )
  )
  

fld_polygon <- st_read("Global_Flood_Records.gpkg", layer = "combined_floods") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

Moz_bbox <- st_bbox(Moz_outline)

flood_moz <- fld_polygon %>% 
  st_crop(Moz_bbox) %>% 
  st_intersection(Moz_outline)

dfo_clipped <- st_intersection(flood_moz, Moz_outline)
  
geom_sf(data = dfo_clipped, fill = NA,
        color = "steelblue", linewidth = 0.3, alpha = 0.4)



# A MAP OF AFRICA WITH MOZA IN IT -----------------------------------------



africa <- ne_countries(
  continent = "Africa",
  scale = "medium",
  returnclass = "sf"
)

Moz_country <- Africa %>% filter(name == "Mozambique")

Africa_Moz <- ggplot() +
  
  geom_sf(
    data = africa,
    fill = "lightblue",
    color = "white",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = Moz_country,
    fill = "#E8527A",
    color = "#C0173A",
    linewidth = 0.3
  ) +
  
  coord_sf(xlim = c(-20,52),
           ylim = c(-36, 38),
           expand = FALSE) +
  
  theme_void() +
  
  theme(
    panel.background = element_rect(         # borders and background
      fill = "white",
      color = "NA",
    ),
    plot.background = element_rect(
      fill = "white",
      color = "NA",
    ),
    plot.margin = margin(0,0,0,0)
  )



# CONFLICT MAP (1998-2023) ------------------------------------------------


conflict_map <- ggplot() +
  
  geom_sf(data      = Moz_districts2,
          fill      = "lightblue",
          color     = "white",
          linewidth = 0.3) +
  
  geom_sf(data      = Moz_outline,
          fill      = NA,
          color     = "grey25",
          linewidth = 0.6) +
  
  # conflict events in red dots
  geom_sf(data   = acled,
          color  = "#C0173A",
          fill   = "#E8527A",
          size   = 1.6,
          alpha  = 0.7,
          shape  = 21,
          stroke = 0.18) +
  
  coord_sf(xlim   = c(30, 41),
           ylim   = c(-27, -10),
           expand = FALSE) +
  
  annotation_scale(location   = "br",
                   width_hint = 0.25,
                   text_cex   = 0.6,
                   line_width = 0.5,
                   unit_category = "metric") +
  
  annotation_north_arrow(
    location    = "tl",
    which_north = "true",
    height = unit(0.8, "cm"),
    width  = unit(0.8, "cm"),
    style  = north_arrow_fancy_orienteering(text_size = 7)
  ) +
  
  annotate("point",
           x = 36.8, y = -22.5,
           shape = 21, size = 2.5,
           color = "#C0173A", fill = "#E8527A") +
  annotate("text",
           x = 37.15, y = -22.5,
           label = "Conflict events",
           hjust = 0, size = 2.5, color = "grey20") +
  
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(8, 8, 8, 8)
  )



# FLOOD EVENTS MAP --------------------------------------------------------


severity_colors <- c(
  "No flood data" = "#F2F2F2",
  "1-2" = "#FFF3B0",
  "2-3" = "#FDB366",
  "3-4" = "#E85D04",
  ">4"=   "#9D0208"
)

floods_map <- ggplot() +
  
  geom_sf(data      = Moz_flood,
          aes(fill  = severity_bin),
          color     = "white",
          linewidth = 0.18) +
  
  geom_sf(data      = Moz_outline,
          fill      = NA,
          color     = "grey25",
          linewidth = 0.6) +
  
  scale_fill_manual(
    values   = severity_colors,
    name     = "Flood Severity",
    drop     = FALSE
  ) +
  
  coord_sf(xlim   = c(30, 41),
           ylim   = c(-27, -10),
           expand = FALSE) +
  
  annotation_scale(location   = "br",
                   width_hint = 0.25,
                   text_cex   = 0.6,
                   line_width = 0.5,
                   unit_category = "metric") +
  
  annotation_north_arrow(
    location    = "tl",
    which_north = "true",
    height = unit(0.8, "cm"),
    width  = unit(0.8, "cm"),
    style  = north_arrow_fancy_orienteering(text_size = 7)
  ) +
  
  theme_void() +
  theme(
    legend.title     = element_text(size = 7, face = "bold"),
    legend.text      = element_text(size = 7.5),
    legend.key.size  = unit(0.35, "cm"),
    legend.position  = c(0.88,0.25),
    legend.justification = c(0.5,0.5),
    legend.background = element_rect(fill = "white",
                                     color = "grey70",
                                     linewidth = 0.3
    ),
    legend.margin = margin(2,3,2,3),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(8, 8, 8, 8)
  )


fig1 <- conflict_map + Africa_Moz +
  plot_layout(width = c(3, 1))

fig2 <- floods_map + Africa_Moz +
  plot_layout(width = c(3, 1))

fig1
fig2

ggsave(
  "fig1_Communal_Violence.png",
  plot = fig1,
  width = 7.5,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  "fig2_Flood_Severity.png",
  plot = fig2,
  width = 7.5,
  height = 6.5,
  dpi = 300,
  bg = "white"
)
























