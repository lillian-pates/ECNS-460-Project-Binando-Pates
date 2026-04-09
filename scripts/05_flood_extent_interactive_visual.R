# =============================================================================
# POZA RICA FLOOD EXTENT VISUALIZATIONS — HAND METHOD
# OpenStreetMap basemap + publication-ready static maps + interactive leaflet
# =============================================================================
install.packages(c("sf", "ggplot2", "leaflet", "leaflet.extras",
                    "maptiles", "tidyterra", "ggspatial",
                    "RColorBrewer", "htmlwidgets", "dplyr", "patchwork"))

library(sf)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(maptiles)
library(ggspatial)
library(RColorBrewer)
library(htmlwidgets)
library(dplyr)
library(patchwork)

# =============================================================================
# !! EDIT THESE PATHS !!
# =============================================================================
hand_dir   <- "~/Desktop/ECNS 460/GitHub/ECNS-460/ECNS-460-Project-Binando-Pates/data/raw/floodplains_HAND"
river_path <- "~/Desktop/FloodAnalysis/hecras_project/cazones_middle.shp"
output_dir <- "~/Desktop/FloodAnalysis/output/figures"
dir.create(path.expand(output_dir), showWarnings = FALSE, recursive = TRUE)

return_periods <- c(2, 5, 10, 25, 50, 100)
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# LOAD DATA
# ─────────────────────────────────────────────────────────────────────────────

cat("Loading shapefiles...\n")

flood_list <- lapply(return_periods, function(rp) {
  path <- file.path(path.expand(hand_dir),
                    paste0("floodplain_HAND_", rp, "yr.shp"))
  if (!file.exists(path)) { cat("Missing:", path, "\n"); return(NULL) }
  gdf <- st_read(path, quiet = TRUE)
  st_crs(gdf) <- 32614
  gdf$rp_label <- paste0(rp, "-year")
  gdf$rp_num   <- rp
  gdf
})
flood_list  <- Filter(Negate(is.null), flood_list)
floods_utm  <- do.call(rbind, flood_list) %>%
  mutate(rp_label = factor(rp_label,
                            levels = paste0(return_periods, "-year")))
floods_wgs  <- st_transform(floods_utm, 4326)

river_utm   <- st_read(path.expand(river_path), quiet = TRUE)
st_crs(river_utm) <- 32614
river_wgs   <- st_transform(river_utm, 4326)

cat("Loaded", length(flood_list), "flood extent layers.\n")


# ─────────────────────────────────────────────────────────────────────────────
# COLOUR PALETTE
# ─────────────────────────────────────────────────────────────────────────────

rp_colors <- c(
  "2-year"   = "#c6dbef",
  "5-year"   = "#9ecae1",
  "10-year"  = "#6baed6",
  "25-year"  = "#3182bd",
  "50-year"  = "#2171b5",
  "100-year" = "#084594"
)


# ─────────────────────────────────────────────────────────────────────────────
# PLOT 1 — SMALL MULTIPLES WITH OSM BASEMAP
# ─────────────────────────────────────────────────────────────────────────────

cat("Building small multiples with OSM basemap...\n")

# Get OSM tiles for the study area
bbox_wgs <- st_bbox(floods_wgs)
osm_tiles <- get_tiles(floods_wgs, provider = "OpenStreetMap", zoom = 12,
                        crop = TRUE)

# Convert tiles to data frame for ggplot
library(tidyterra)

p_multiples <- ggplot() +
  geom_spatraster_rgb(data = osm_tiles, alpha = 0.6) +
  geom_sf(data = floods_wgs,
          aes(fill = rp_label),
          color = NA, alpha = 0.55) +
  geom_sf(data = river_wgs,
          color = "#08306b", linewidth = 0.5) +
  scale_fill_manual(values = rp_colors, guide = "none") +
  facet_wrap(~rp_label, ncol = 3) +
  annotation_scale(location = "bl", width_hint = 0.3,
                   text_cex = 0.6) +
  annotation_north_arrow(location = "tr",
                          style = north_arrow_minimal(),
                          height = unit(0.8, "cm"),
                          width  = unit(0.8, "cm")) +
  labs(
    title    = "Cazones River Flood Extents — Poza Rica, Veracruz",
    subtitle = "HAND method | Thresholds calibrated from HEC-RAS 6.7 simulation | Real CONAGUA gauge data (1983–2025)",
    caption  = "Basemap: © OpenStreetMap contributors"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle   = element_text(size = 9, hjust = 0.5, color = "grey40"),
    plot.caption    = element_text(size = 7, color = "grey50", hjust = 1),
    strip.text      = element_text(face = "bold", size = 10, color = "#084594"),
    panel.spacing   = unit(0.5, "lines"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(10, 10, 10, 10)
  )

ggsave(file.path(path.expand(output_dir), "HAND_multiples_OSM.png"),
       p_multiples, width = 14, height = 9, dpi = 200, bg = "white")
cat("Saved: HAND_multiples_OSM.png\n")


# ─────────────────────────────────────────────────────────────────────────────
# PLOT 2 — 100-YEAR FLOOD HIGHLIGHT MAP WITH OSM
# ─────────────────────────────────────────────────────────────────────────────

cat("Building 100yr highlight map...\n")

flood_100 <- floods_wgs %>% filter(rp_num == 100)
flood_2   <- floods_wgs %>% filter(rp_num == 2)

p_100yr <- ggplot() +
  geom_spatraster_rgb(data = osm_tiles, alpha = 0.7) +
  geom_sf(data = flood_100,
          fill = "#084594", color = NA, alpha = 0.45) +
  geom_sf(data = flood_2,
          fill = "#c6dbef", color = NA, alpha = 0.6) +
  geom_sf(data = river_wgs,
          color = "#08306b", linewidth = 0.8) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tr",
                          style = north_arrow_minimal(),
                          height = unit(1, "cm"),
                          width  = unit(1, "cm")) +
  labs(
    title    = "100-year vs 2-year Flood Extent",
    subtitle = "Cazones River — Poza Rica, Veracruz",
    caption  = "Dark blue = 100yr extent  |  Light blue = 2yr extent  |  Basemap: © OpenStreetMap contributors"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle   = element_text(size = 11, hjust = 0.5, color = "grey40"),
    plot.caption    = element_text(size = 8, color = "grey50", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(10, 10, 10, 10)
  )

ggsave(file.path(path.expand(output_dir), "HAND_100yr_vs_2yr_OSM.png"),
       p_100yr, width = 10, height = 8, dpi = 200, bg = "white")
cat("Saved: HAND_100yr_vs_2yr_OSM.png\n")


# ─────────────────────────────────────────────────────────────────────────────
# PLOT 3 — AREA BAR CHART
# ─────────────────────────────────────────────────────────────────────────────

cat("Building area chart...\n")

area_df <- floods_utm %>%
  st_drop_geometry() %>%
  group_by(rp_num, rp_label) %>%
  summarise(area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop") %>%
  arrange(rp_num)

p_area <- ggplot(area_df,
                 aes(x = factor(rp_num), y = area_km2,
                     fill = rp_label)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = paste0(round(area_km2, 1), " km²")),
            vjust = -0.4, size = 3.5, fontface = "bold",
            color = "#084594") +
  scale_fill_manual(values = rp_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Inundated Area by Return Period",
    subtitle = "Cazones River Middle Reach — Poza Rica, Veracruz",
    x        = "Return period (years)",
    y        = "Inundated area (km²)",
    caption  = "HAND method | HEC-RAS calibrated thresholds | CONAGUA gauge data 1983–2025"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 10, color = "grey40"),
    plot.caption       = element_text(size = 8, color = "grey50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.margin        = margin(10, 15, 10, 10)
  )

ggsave(file.path(path.expand(output_dir), "HAND_area_barchart.png"),
       p_area, width = 8, height = 5, dpi = 200, bg = "white")
cat("Saved: HAND_area_barchart.png\n")


# ─────────────────────────────────────────────────────────────────────────────
# PLOT 4 — INTERACTIVE LEAFLET WITH OSM
# ─────────────────────────────────────────────────────────────────────────────

cat("Building interactive leaflet map...\n")

bbox       <- st_bbox(floods_wgs)
centre_lng <- mean(c(bbox["xmin"], bbox["xmax"]))
centre_lat <- mean(c(bbox["ymin"], bbox["ymax"]))

map <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
  addProviderTiles("OpenStreetMap",
                   group = "OpenStreetMap") %>%
  addProviderTiles("CartoDB.Positron",
                   group = "CartoDB (light)") %>%
  addProviderTiles("Esri.WorldImagery",
                   group = "Satellite") %>%
  setView(lng = centre_lng, lat = centre_lat, zoom = 11)

# Add flood layers — 100yr first so 2yr renders on top
for (rp in rev(return_periods)) {
  label  <- paste0(rp, "-year")
  col    <- rp_colors[[label]]
  gdf    <- floods_wgs %>% filter(rp_num == rp)
  area   <- area_df %>% filter(rp_num == rp) %>% pull(area_km2) %>% round(2)

  map <- map %>%
    addPolygons(
      data        = gdf,
      group       = paste0(rp, "-year flood"),
      fillColor   = col,
      fillOpacity = 0.5,
      color       = col,
      weight      = 1,
      opacity     = 0.8,
      popup       = paste0(
        "<b>", rp, "-year flood extent</b><br>",
        "Inundated area: <b>", area, " km²</b><br>",
        "<i>HAND method — HEC-RAS calibrated</i>"
      ),
      highlightOptions = highlightOptions(
        weight = 2.5, color = "#333",
        fillOpacity = 0.7, bringToFront = TRUE
      )
    )
}

# Add river
map <- map %>%
  addPolylines(
    data    = river_wgs,
    group   = "Cazones River",
    color   = "#08306b",
    weight  = 2.5,
    opacity = 0.9,
    label   = "Cazones River — Middle Reach"
  ) %>%
  addLegend(
    position = "bottomleft",
    colors   = unname(rp_colors),
    labels   = names(rp_colors),
    title    = "Flood return period<br>Cazones River — Poza Rica<br><i style='font-size:10px'>HAND method</i>",
    opacity  = 0.85
  ) %>%
  addLayersControl(
    baseGroups    = c("OpenStreetMap", "CartoDB (light)", "Satellite"),
    overlayGroups = c(paste0(return_periods, "-year flood"), "Cazones River"),
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%
  addScaleBar(position = "bottomleft") %>%
  addFullscreenControl() %>%
  addMeasure(primaryLengthUnit = "kilometers",
             primaryAreaUnit   = "sqmeters")

# Save interactive map
html_path <- file.path(path.expand(output_dir), "HAND_flood_map_interactive.html")
saveWidget(map, file = html_path, selfcontained = TRUE)
cat("Saved: HAND_flood_map_interactive.html\n")


# ─────────────────────────────────────────────────────────────────────────────
# SUMMARY
# ─────────────────────────────────────────────────────────────────────────────

cat("\n════════════════════════════════════════════════\n")
cat("  OUTPUT FILES:\n")
cat("  HAND_multiples_OSM.png       — 6-panel small multiples\n")
cat("  HAND_100yr_vs_2yr_OSM.png    — comparison map\n")
cat("  HAND_area_barchart.png       — area by return period\n")
cat("  HAND_flood_map_interactive.html — shareable interactive map\n")
cat("════════════════════════════════════════════════\n")

# Display in RStudio viewer
map
