############################################################
# Montana State University 
# Project: ECNS 460 Data Analytics Spring 2026
# Author: Bailey Binando & Lillian Pates
# Created: 2026-04-01
# Last Updated: 2026-04-05

# Script 4
# Output: Summary csv files of asset exposure
############################################################

# load packages
library(tidyverse)
library(sf)

# load data
building_exposure_vulnerability <- st_read(here("data", "processed", "building_exposure_vulnerability.gpkg"), quiet = TRUE)

# create floodplain exposure (number of buildings, building area, ratio of total assets) summary
# create total asset summary (total buildings, total surface area) for Poza Rica
glimpse(building_exposure_vulnerability)

# summarize building counts within each floodplain
floodplain_building_summary <- building_exposure_vulnerability |>
  st_drop_geometry() |> # remove
  select(in_floodplain2yr, in_floodplain5yr, in_floodplain10yr, # keep necessary cols.
         in_floodplain25yr, in_floodplain50yr, in_floodplain100yr) |>
  rename( # rename vars for later join
    fp100 = in_floodplain100yr,
    fp50 = in_floodplain50yr,
    fp25 = in_floodplain25yr,
    fp10 = in_floodplain10yr,
    fp5 = in_floodplain5yr,
    fp2 = in_floodplain2yr
  ) |>
  pivot_longer( # tidy dataset (row = building_i X floodplain_j)
    cols = everything(),
    names_to = "floodplain",
    values_to = "exposed"
  ) |>
  group_by(floodplain) |>
  summarize( # count total buildings exposed in each floodplain
    exposed_buildings = sum(exposed)
  ) 

# check
glimpse(floodplain_building_summary)

# summarize building area within each floodplain
floodplain_area_summary <- building_exposure_vulnerability |>
  st_drop_geometry() |> # remove
  select(area_floodplain2yr, area_floodplain5yr, area_floodplain10yr, # keep necessary cols.
         area_floodplain25yr, area_floodplain50yr, area_floodplain100yr) |>
  rename( # rename vars for later join
    fp100 = area_floodplain100yr,
    fp50 = area_floodplain50yr,
    fp25 = area_floodplain25yr,
    fp10 = area_floodplain10yr,
    fp5 = area_floodplain5yr,
    fp2 = area_floodplain2yr
  ) |>
  pivot_longer( # tidy dataset (row = building_i X floodplain_j)
    cols = everything(),
    names_to = "floodplain",
    values_to = "exposed_area"
  ) |>
  group_by(floodplain) |>
  summarize( # find total building area in each floodplain
    exposed_building_area = sum(exposed_area)
  ) 

# check
glimpse(floodplain_area_summary)

# summarize total assets in poza rica
total_assets <- building_exposure_vulnerability |>
  st_drop_geometry() |> # remove
  summarize(
    total_buildings = n(), # total buildings in municipality
    total_building_area = sum(building_area) # total building area in municipality
  )

# check
glimpse(total_assets)

# extract vars for ratio of assets exposed
total_buildings <- total_assets$total_buildings
total_building_area <- total_assets$total_building_area

# join
asset_exposure_summary <- floodplain_building_summary |>
  left_join(floodplain_area_summary, by = "floodplain") |>
  mutate( # create vars for ratio of assets exposed
    buildings_exposed_ratio = exposed_buildings / total_buildings,
    building_area_exposed_ratio = exposed_building_area / total_building_area
  ) |>
  arrange(desc(exposed_buildings)) # arrange in floodplain year order (2, 5, 10, ...)

# check
glimpse(asset_exposure_summary)

# Write File
write_csv(asset_exposure_summary, here("outputs", "tables", "floodplain_exposure_summary.csv"))
write_csv(total_assets, here("outputs", "tables", "poza_rica_asset_summary.csv"))
