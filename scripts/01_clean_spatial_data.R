############################################################
# Montana State University 
# Project: ECNS 460 Data Analytics Spring 2026
# Author: Bailey Binando & Lillian Pates
# Created: 2026-04-01
# Last Updated: 2026-04-01

# Research Question: 
# Source: Spatial Statistics for Data Science: Theory and Practice with R
# url: https://www.paulamoraga.com/book-spatial/the-sf-package-for-spatial-vector-data.html
# Output: Cropped Spatial Data files 
############################################################
# Preliminary: Load Packages
library(tidyverse)
library(sf) # vector data
library(terra) # rastor data
library(here)

############################################################
# Preliminary: Load Data
# mexico admin. boundaries
municipalities <- st_read(here("data", "raw", "mex_admin_boundaries.shp", "mex_admin2.shp"), quiet = TRUE)

# building infastructure
buildings <- st_read(here("data", "raw", "microsoft_buildings_pozarica.gpkg"), quiet = TRUE)

# river
río_cazones <- st_read(here("data", "raw", "cazones", "rio_cazones_mainstem.gpkg"), quiet = TRUE)

# río cazones floodplain extents
floodplain_2yr <- st_read(here("data", "raw", "floodplains_HAND", "floodplain_HAND_2yr.shp"), quiet = TRUE)
floodplain_5yr <- st_read(here("data", "raw", "floodplains_HAND", "floodplain_HAND_5yr.shp"), quiet = TRUE)
floodplain_10yr <- st_read(here("data", "raw", "floodplains_HAND", "floodplain_HAND_10yr.shp"), quiet = TRUE)
floodplain_25yr <- st_read(here("data", "raw", "floodplains_HAND", "floodplain_HAND_25yr.shp"), quiet = TRUE)
floodplain_50yr <- st_read(here("data", "raw", "floodplains_HAND", "floodplain_HAND_50yr.shp"), quiet = TRUE)
floodplain_100yr <- st_read(here("data", "raw", "floodplains_HAND", "floodplain_HAND_100yr.shp"), quiet = TRUE)

# digital elevation map
dem <- rast(here("data", "raw", "cazones_basin_dem.tif"))

############################################################
# Clean Data
# check crs (coordinate reference system)
st_crs(municipalities) # degrees
st_crs(buildings) # degrees
st_crs(río_cazones)  # meters
st_crs(floodplain_2yr) # meters
st_crs(floodplain_5yr)
st_crs(floodplain_10yr)
st_crs(floodplain_25yr)
st_crs(floodplain_50yr)
st_crs(floodplain_100yr)
crs(dem) # degrees

# align crs
# convert to crs = meters
align_crs <- st_crs(río_cazones) 
city_boundaries <- st_transform(municipalities, align_crs)
infrastructure <- st_transform(buildings, align_crs)
dem_m <- project(dem, align_crs$wkt)

# check
st_crs(city_boundaries) # correct
st_crs(infrastructure) # correct
crs(dem_m) # correct

# check polygons
table(st_is_valid(city_boundaries))
table(st_is_valid(infrastructure)) # invalid geometries
table(st_is_valid(río_cazones))
table(st_is_valid(floodplain_2yr))
table(st_is_valid(floodplain_5yr))
table(st_is_valid(floodplain_10yr))
table(st_is_valid(floodplain_25yr))
table(st_is_valid(floodplain_50yr))
table(st_is_valid(floodplain_100yr))

# fix invalid geometries
# infrastructure --> satellite imagery data
infrastructure_fix <- st_make_valid(infrastructure)

# check
table(st_is_valid(infrastructure_fix)) # fixed

# filter to only poza rica
poza_rica <- city_boundaries |>
  filter(adm2_name == "Poza Rica de Hidalgo")

# check
nrow(poza_rica)
plot(st_geometry(poza_rica))

# clip to only poza rica
infrastructure_clipped <- st_intersection(infrastructure_fix, poza_rica)
river_clipped <- st_intersection(río_cazones, poza_rica)
floodplain_2yr_clipped <- st_intersection(floodplain_2yr, poza_rica)
floodplain_5yr_clipped <- st_intersection(floodplain_5yr, poza_rica)
floodplain_10yr_clipped <- st_intersection(floodplain_10yr, poza_rica)
floodplain_25yr_clipped <- st_intersection(floodplain_25yr, poza_rica)
floodplain_50yr_clipped <- st_intersection(floodplain_50yr, poza_rica)
floodplain_100yr_clipped <- st_intersection(floodplain_100yr, poza_rica)

dem_crop <- crop(dem_m, vect(poza_rica))
dem_mask <- mask(dem_crop, vect(poza_rica))

############################################################
# visual check
ggplot() +
  geom_sf(data = poza_rica, fill = NA, color = "black") +
  geom_sf(data = floodplain_100yr_clipped, fill = "blue", alpha = 0.3) +
  geom_sf(data = river_clipped, color = "darkblue", linewidth = 1) +
  geom_sf(data = infrastructure_clipped, color = "red", size = 0.1) +
  theme_minimal()

plot(dem_mask)

############################################################
# write files
st_write(poza_rica, here("data", "processed", "poza_rica.gpkg"))
st_write(infrastructure_clipped, here("data", "processed", "buildings_clipped.gpkg"))
st_write(river_clipped, here("data", "processed", "river_clipped.gpkg"))

st_write(floodplain_2yr_clipped, here("data", "processed", "floodplain_2yr.gpkg"))
st_write(floodplain_5yr_clipped, here("data", "processed", "floodplain_5yr.gpkg"))
st_write(floodplain_10yr_clipped, here("data", "processed", "floodplain_10yr.gpkg"))
st_write(floodplain_25yr_clipped, here("data", "processed", "floodplain_25yr.gpkg"))
st_write(floodplain_50yr_clipped, here("data", "processed", "floodplain_50yr.gpkg"))
st_write(floodplain_100yr_clipped, here("data", "processed", "floodplain_100yr.gpkg"))

writeRaster(dem_mask, here("data", "processed", "dem_pozarica.tif"))

