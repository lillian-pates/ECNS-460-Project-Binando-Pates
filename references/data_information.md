# Data Information
The data for this project includes ERA5 cummulative basin precipitation, LISFLOOD forecasted river discharge rates, flood event dates, spatial data, and floodplain extents.

## Dataset Relation
The analysis utilizes several datasets to descriptively analyze building exposure and vulnerability in Poza, Rica, Veracruz, México. Spatial data will be combined with LISFLOOD generated flood extents to estimate the relationship between key spatial variables, flood extents, and assets. In combining the spatial datasets (topography, hydrology, land-use, etc.), the hope is to improve understanding and differences of flood exposure to improve economic proxy for flood damages.

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

### Elevation & Geological Data 
A dataset containing global digital elevation model (DEM) was downloaded from the EarthExplorer feature on the United States Geological Survey website. This data contains elevation levels, topography, slope, drainage patterns, and watershed boundaries of the Poza Rica and its surrounding area (tile N20W098). This data was public and free to use after setting up an account with USGS. The data was originally published on  9/23/2014 but updated on 3/10/2025. 

Source: https://earthexplorer.usgs.gov

### River Data 
River path GeoPackage file was collected from Open Street Maps (OSM). This data is public and free to use.

Source: https://welcome.openstreetmap.org/working-with-osm-data/downloading-and-using/

### Municipality Boundary
The administrative municipality boundary shapefile for Poza Rica was acquired from United Nations Office for the Coordination of Humanitarian Affairs (OCHA). 

Source: https://data.humdata.org/group/mex?q=administrative&ext_page

### Floodplain Extents
Words

Source: 






