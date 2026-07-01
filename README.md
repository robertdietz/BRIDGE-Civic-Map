#BRIDGE CIVIC MAP

##PRODUCT
This project delivers a user-friendly map comparison tool paired with a non-technical data update system for the Economy League of Greater Philadelphia via an R Shiny app. The tool allows users to visualize and compare disparate datasets across Philadelphia metropolitan area’s neighborhoods and sub-regions. 

##HOW TO RUN
All necessary preprocessed data files are available in the folder 4_app.  Users are able locally run the project by downloading just the contents of this folder and executing app.R.

##HOW TO UPDATE DATA
Data Processing Pipeline & Updates
Data is preprocessed through a series of three distinct stages using a combination of automated APIs and manual data downloads.

Note on Data Sources: Data not accessible via a live API may be downloaded directly from the original sources listed in the project's external storage or restored from the local version last received by the original data provider on 7/1/2026.

###`1_prep_raw_data.R`
Brings raw data into R enviroment and standardizes it. Beginning of data processing for data that makes up the map choropleth tiles. Exports new files to `2_summary_data`.

###`2_derive_&_config.R`
Configures the standardized data tables, runs the normalization functions, and generates the main metadata that drives the information structure and visual formatting. Exports new files to `4_app`.

###`3_overlay_data.R`
Brings raw data into R environment and standardizes it. This script builds structural geographic boundaries that sit on top of the base maps as selectable toggle layers. Exports new files to `4_app`.

###`4_app.R`
Generates user interface and runs Shiny app.

##ADD NEW DATA SET
see technical documentation in the below Google Drive folder.

##EXTERNAL INFO
Raw data files last updated 7/1/2026 that are not accessible via an API and further project documentation are available here: https://drive.google.com/drive/folders/1AzV71T2ekzLvp3cHpuMptiT7BXj8s5n4?usp=drive_link
