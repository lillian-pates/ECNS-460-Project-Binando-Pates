############################################################
# Montana State University 
# Project: ECNS 460 Data Analytics Spring 2026
# Author: Bailey Binando & Lillian Pates
# Created: 2026-04-01
# Last Updated: 2026-04-05

# Script 3 
# Output: Basic Visuals & Regression Analysis 
############################################################
# Preliminary: Libraries 

library(sf)
library(tidyverse)
library(tmap)
library(terra)

############################################################
# Preliminary: Data  

## Main 
dem_pz = rast("data/processed/dem_pozarica.tif")
rivers = st_read("data/processed/river_clipped.gpkg")
flooding = read_csv("data/processed/flood_timeseries_clean.csv")


## For Analysis
population = st_read("data/processed/poza_rica.gpkg")
buildings = st_read("data/processed/buildings_clipped.gpkg")
key_infrastructure = read_csv("data/processed/poza_rica_key_infrastructure.csv")
expos = st_read("data/processed/building_exposure_vulnerability.gpkg")


## Flood Plains 
flood_plain2yr = st_read("data/processed/floodplain_2yr.gpkg")
flood_plain5yr =st_read("data/processed/floodplain_5yr.gpkg")
flood_plain10yr = st_read("data/processed/floodplain_10yr.gpkg")
flood_plain25yr = st_read("data/processed/floodplain_25yr.gpkg")
flood_plain50yr = st_read("data/processed/floodplain_50yr.gpkg")
flood_plain100yr = st_read("data/processed/floodplain_100yr.gpkg")


## Flood events 
floods = read_csv("data/processed/flood_timeseries_clean.csv")
  

############################################################
# Deriving Slope, Aspect, Hill from DEM Model 

slope = terrain(dem_pz, v = "slope", unit = "degrees")

plot(slope)

aspect = terrain(dem_pz, v = "aspect")

plot(aspect)

hill = shade(
  terrain(dem_pz, "slope"),
  terrain(dem_pz, "aspect")
)

plot(hill)

############################################################
# Preliminary Visuals 


## Flood Plain 100 yr flood extent 
ggplot() +
  geom_sf(data = flood_plain100yr, fill = "#0A7398", color = NA, alpha = 0.6) +
  geom_sf(data = rivers, color = "#08306b", linewidth = 0.5) +
  theme_void() +
  labs(title = "Cazones River Flood Extent")

## Predicted river flow vs flood indications 
floods |>
  mutate(flow_bin = cut(pred_flow, breaks = 20)) |>
  group_by(flow_bin) |>
  summarise(prop_flood = mean(flood_flag),
            mid = mean(pred_flow)) |>
  ggplot(aes(x = mid, y = prop_flood)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  labs(y= "Proportion flooded", x= "Predicted flow")

## Actual river flow vs flood indications
floods |>
  mutate(flow_bin = cut(river_flow, breaks = 20)) |>
  group_by(flow_bin) |>
  summarise(prop_flood = mean(flood_flag),
            mid = mean(river_flow)) |>
  ggplot(aes(x = mid, y = prop_flood)) +
  geom_point() +
  geom_smooth(se = TRUE) +
  labs(y= "Proportion flooded", x = "Predicted flow")

## Predicted River Flow vs Flood cause 
ggplot(floods, aes(x = pred_flow , y = cause)) +
  geom_jitter(height = 0.05, alpha = 0.3)

## Histograms of building area, elevation, slope, river distance 
expos |>
  st_drop_geometry() |>
  select(river_distance, building_elevation, building_slope, building_area) |>
  pivot_longer(everything()) |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "#0A7398", color = "white") +
  facet_wrap(~name, scales = "free") +
  labs(x = NULL, y = "Count")

## Histogram of River distance by floodplain status
ggplot(expos, aes(x = river_distance, fill = factor(in_floodplain100yr))) +
  geom_histogram(bins = 30, color = "white", alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("0" = "grey70", "1" = "#0A7398")) +
  labs(fill = "In 100yr floodplain")

##Proportion of buildings in each floodplain 
expos |>
  st_drop_geometry() |>
  select(starts_with("in_floodplain")) |>
  pivot_longer(everything()) |>
  group_by(name) |>
  summarise(prop = mean(value)) |>
  ggplot(aes(x = name, y = prop)) +
  geom_col() +
  coord_flip() +
  labs(y = "Proportion of buildings in floodplain", x = NULL)

##Mapping the buildings colored by floodplain status
ggplot(expos) +
  geom_sf(aes(fill = factor(in_floodplain100yr)), size = 0.1) +
  scale_fill_manual(values = c("0" = "grey80", "1" = "#0A7398")) +
  labs(fill = "In 100yr floodplain")

##Distribution of river distance by floodplain status (100Yr)
expos |>
  st_drop_geometry() |>
  ggplot(aes(x = river_distance, fill = factor(in_floodplain100yr))) +
  geom_density(alpha = 0.5) +
  scale_x_log10() + 
  labs(fill = "In 100yr floodplain")

############################################################
# Data Behavior 




############################################################
# Regressions 



############################################################