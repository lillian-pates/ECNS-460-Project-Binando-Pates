
# Bailey Binando and Lillian Pates

# Source: Spatial Statistics for Data Science: Theory and Practice with R
# url: https://www.paulamoraga.com/book-spatial/the-sf-package-for-spatial-vector-data.html

# Load Packages
library(tidyverse)
library(sf) # vector data
library(terra) # rastor data
library(here)

# Load Data
buildings <- st_read(here("data", "processed", "buildings_clipped.gpkg"), quiet = TRUE)
river <- st_read(here("data", "processed", "river_clipped.gpkg"), quiet = TRUE)
floodplain_2yr <- st_read(here("data", "processed", "floodplain_2yr.gpkg"), quiet = TRUE)
floodplain_5yr <- st_read(here("data", "processed", "floodplain_5yr.gpkg"), quiet = TRUE)
floodplain_10yr <- st_read(here("data", "processed", "floodplain_10yr.gpkg"), quiet = TRUE)
floodplain_25yr <- st_read(here("data", "processed", "floodplain_25yr.gpkg"), quiet = TRUE)
floodplain_50yr <- st_read(here("data", "processed", "floodplain_50yr.gpkg"), quiet = TRUE)
floodplain_100yr <- st_read(here("data", "processed", "floodplain_100yr.gpkg"), quiet = TRUE)
dem <- rast(here("data", "processed", "dem_pozarica.tif"))

# Variable Creation
# distance from river
# elevation
# slope
# assets within flood plain

# distance from river
# distance from building centroid to river
building_centroid <- st_centroid(buildings)
river_distance <- st_distance(building_centroid, river)
dim(river_distance)
st_crs(buildings)
st_crs(river)
nrow(river)
plot(river)
