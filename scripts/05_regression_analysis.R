############################################################
# Montana State University 
# Project: ECNS 460 Data Analytics Spring 2026
# Authors: Bailey Binando & Lillian Pates
# Created: 2026-04-22
# Last Updated: 2026-04-27

# Script 5 
# Purpose: Regression Analysis 
############################################################
# Preliminary: Libraries 

library(sf)
library(tidyverse)
library(tmap)
library(terra)
library(broom)

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

############################################################
## Feature Engineering 

##Adding elevation to buildings 
building_centroids = st_centroid(expos)
elevation_extract = extract(dem_pz, vect(building_centroids))
expos$elevation = elevation_extract[, 2]

##Compute distance to river from each buildings
building_centroids = st_centroid(buildings)
dist_matrix = st_distance(building_centroids, rivers)
buildings$dist_river = apply(dist_matrix, 1, min)
buildings$dist_river = as.numeric(buildings$dist_river)

## Prepping dataset for analysis
analysis_df = expos |> st_drop_geometry() |>
  select(exposure_100yr, elevation, river_distance, building_slope) |> drop_na() |>
  mutate(exposed = ifelse(exposure_100yr > 0, 1, 0)) |>
  as.data.frame()

############################################################
## Distributions of variables

##Convert to long format for histogram plotting
hist_long = analysis_df |>
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  )

## Plotting distributions of variables before regressing 
p_hist = ggplot(hist_long, aes(x = Value)) +
  geom_histogram(bins = 40, fill = "#0097B2", color = "black", alpha = 0.8) +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Distributions of Key Variables",
    x = "Value",
    y = "Frequency"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold")
  )

print(p_hist)

## Transforming river distance (more info in report) 
## Adding a nonlinear (exponential) elevation variable
analysis_df = analysis_df |> mutate(log_river_distance = log(river_distance+1), 
                                    elevation_sq = elevation^2)

## Histogram of river distance after transformation
ggplot(analysis_df, aes(x=log_river_distance)) + 
  geom_histogram(bins = 40, fill = "#0097B2", color = "black", alpha = 0.8) +
  labs(
    title = "Distribution of Logged River Distance",
    x = "Logged River Distance",
    y = "Frequency"
  )

## Histogram of elevation distance after transformation
ggplot(analysis_df, aes(x=elevation_sq)) + 
  geom_histogram(bins = 40, fill = "#0097B2", color = "black", alpha = 0.8) +
  labs(
    title = "Distribution of Squared Elevation",
    x = "Squared Elevation",
    y = "Frequency"
  )

############################################################
## Relationship Analysis 

## Elevation vs Exposure 
p1 = ggplot(analysis_df, aes(x = elevation, y = exposed))  +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "blue",
              linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Flood Exposure vs Elevation",
    x = "Elevation (m)",
    y = "Probability of Exposure"
  )

print(p1)

## Logged River Distance vs Exposure
p2 = ggplot(analysis_df, aes(x = log_river_distance, y = exposed))  +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "blue",
              linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Flood Exposure vs Log River Distance",
    x = "Log River Distance",
    y = "Probability of Exposure"
  )

print(p2)

## Slope vs Exposure
p3 = ggplot(analysis_df, aes(x = building_slope, y = exposed))  +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "blue",
              linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Flood Exposure vs Slope",
    x = "Slope (degrees)",
    y = "Probability of Exposure"
  )

print(p3)

## Boxplots
p4 = ggplot(analysis_df, aes(x = factor(exposed), y = elevation)) +
  geom_boxplot(fill = "#0097B2") +
  theme_minimal() +
  labs(
    title = "Elevation by Exposure Status",
    x = "Exposed (0 = No, 1 = Yes)",
    y = "Elevation (m)"
  )

print(p4)

##Binned exposure rates
analysis_df = analysis_df |> mutate(elevation_bin = cut(elevation, breaks = 10))

p5 = analysis_df |>
  group_by(elevation_bin) |>
  summarise(exposure_rate = mean(exposed)) |>
  ggplot(aes(x = elevation_bin, y = exposure_rate, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(
    title = "Exposure Rate by Elevation Bin",
    x = "Elevation Bin",
    y = "Exposure Rate"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)

############################################################
##Summary Statistics

summary_stats <- analysis_df |>
  select(elevation, river_distance, log_river_distance,elevation_sq,building_slope, exposed) |>
  summarise(
    across(
      everything(),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE)
      )
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "statistic"),
    names_sep = "_(?=[^_]+$)",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) |>
  mutate(across(where(is.numeric), ~round(.x, 3)))

print(summary_stats)

## Compute correlations
corr_matrix = analysis_df |>
  select(where(is.numeric)) |>
  cor(use = "complete.obs")

print(corr_matrix)

############################################################
## Regression Models 
## exposed = 1 if any part of the building is inside the 100-year floodplain
## exposed = 0 otherwise

## Model 1: Elevation only
model1 = glm(exposed ~ elevation, data = analysis_df, family = binomial)
summary_m1 = summary(model1)

## Model 2: River distance only
model2 = glm(exposed ~ log_river_distance,data = analysis_df,family = binomial)
summary_m2 = summary(model2)

## Model 3: Building Slope only
model3 = glm(exposed ~ building_slope, data = analysis_df,family = binomial)
summary_m3 = summary(model3)

## Model 4: Elevation + river distance 
model4 = glm(exposed ~ elevation + log_river_distance, data = analysis_df,family = binomial)
summary_m4 = summary(model4)

## Model 5: Elevation + river distance + building slope
model5 = glm(exposed ~ elevation + log_river_distance + building_slope, data = analysis_df,family = binomial)
summary_m5 = summary(model5)

## Model 6 : Interaction model: Elevation & River Distance 
model6 = glm(exposed ~ elevation * log_river_distance, data = analysis_df, family = binomial)
summary_m6 = summary(model6)

## Model 7: Full model + interaction 
model7 = glm(exposed ~ elevation * log_river_distance + building_slope,data = analysis_df,family = binomial)
summary_m7 = summary(model7)

##Model 8: Nonlinear elevation
model8 = glm( exposed ~ elevation_sq + log_river_distance + building_slope, data = analysis_df, family = binomial)
summary_m8 = summary(model8)

############################################################
## Model Analysis & Comparison

models = list(
  "Model 1: Elevation" = model1,
  "Model 2: Log River Distance" = model2,
  "Model 3: Building Slope" = model3,
  "Model 4: Elevation + Log River Distance" = model4,
  "Model 5: Elevation + Log River Distance + Slope" = model5,
  "Model 6: Elevation * Log River Distance" = model6,
  "Model 7: Interaction + Slope" = model7,
  "Model 8: Nonlinear Elevation" = model8
)

##Comparison table
model_comparison = data.frame(
  Model = names(models),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC),
  Deviance = sapply(models, deviance),
  Null_Deviance = sapply(models, function(m) m$null.deviance)
) |>
  mutate(
    Pseudo_R2 = 1 - (Deviance / Null_Deviance),
    across(where(is.numeric), ~round(.x, 4))
  ) |> 
  arrange(AIC)

print(model_comparison)

############################################################

