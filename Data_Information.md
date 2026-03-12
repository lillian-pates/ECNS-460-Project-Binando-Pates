# Data Information
The data for this project includes ERA5 cummulative basin precipitation, LISFLOOD forecasted river discharge rates, flood event dates, spatial data, and monthly oil production.

## Dataset Relation
The analysis combines several datasets to estimate the impact of flood events on oil production in Poza Rica, Veracruz, Mexico within the Río Cazones basin. Precipitation data (ERA5), forecasted river flow (LISFLOOD), and historical flood events are combined in a regression model to relationship between these variables and oil production.

The ERA5, LISFLOOD, flood events, and oil production data is utilized in a regression to identify the effect of a flood on oil production. This will be combined with spacial data to better estimate the impact. All these datasets are specifically for Poza Rica, Veracruz, Mexico and the Río Cazones basin. In combining the spatial datasets (topography, hydrology, land-use, etc.), the hope is to improve understanding and differences of flood exposure to improve economic proxy for flood damages.

## ERA5 
Daily cummulative watershed precipitation was collected from the European Centre for Medium-Range Weather Forecasts's ERA5 reanalysis model (1975-2025). This is a publically avaiable assimilation dataset, utilizing physics, model data, and station observations to reconstruct global climate and weather. Precipitation data was provided by capstone sponsor.

Source: https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=download

## LISFLOOD
The daily river flow forecast of Río Cazones (1975-2025) was modeled by the European Commission's Joint Research Centre (JRC) LISFLOOD model. This is a publicly available, grid-based hydrological rainfall-runoff-routing model. The model was trained with 5 years of a single point source river flow gauge (B27002). Forecast data was provided by capstone sponsor.

Source: https://ec-jrc.github.io/lisflood/

## Flood Events
A collection of flood events (n = 42) was found utilizing the DesInventar (1971-2013) natural disaster impact database and a manual search of flood events (2010-2025) through government reports and local news sources (La Jornada, El País, La Silla Rota Veracruz, etc.). This data provides the date of flood, primary cause, deaths, and additional reported impacts. All floods events occurred in Poza Rica, Veracruz, Mexico. In the search for events beyond the DesInvetar database, a ±7-day window were applied to avoid duplicate events between sources.

DesInventar: https://www.desinventar.net

News Sources: Available upon request

## Spatial Data
FINISH


## Oil Production
Production by petrochemical complex data was obtained in thousand tonnes from the Pemex public database provided by the Mexican government. This dataset includes production data (January 1990-December 2025) from all of the Pemex sites (23) in Mexico including crude oil refineries, petrochemical complexes, and gas processing complexes. This data consists of monthly production and facility. 

Source: https://ebdi.pemex.com/bdi/bdiController.do

