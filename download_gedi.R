# ------------------------------------------------------------------------------------------------ #
# Spatial Querying of GEDI data in R
# Author: Hyeonmin Kang
# Last Updated: 25/04/2022
# Objective: The objective of this script is to demonstrate how to get the url for downloading
# GEDI data directly from the LP DAAC Data Pool, using NASA's Common Metadata Repository (CMR).
# 
# ------------------------------------------------------------------------------------------------ #
#------------------------------------LOAD PACKAGES -----------------------------
# Install the required packages, if not previously installed
if ("jsonlite" %in% rownames(installed.packages()) == FALSE) {install.packages("jsonlite")}
if ("glue" %in% rownames(installed.packages()) == FALSE) {install.packages("glue")}
# Load the necessary packages into R
library(jsonlite)
library(glue)
# -----------------------------------SET UP OUTPUT DIRECTORY AND QUERY PARAMETER------------------------------------------- #
# Set output dir for downloading the files
outdir <- getwd()

# set the bounding box
bbox <- "8.527,50.770,9.974,51.94" #lon_min, lat_min, lon_max, lat_max 

# name of GEDI data we need
GEDI1B <- 'GEDI01_B.002' # geolocation product 
GEDI2A <- 'GEDI02_A.002' # Full-waveform product
GEDI2B <- 'GEDI02_B.002' # Elevation and Height Metrics product

# -----------------------------------CREATE A FUNCTION TO QUERY CMR------------------------------------------- #
request_gedi <- function(product, bbox) {
  # Define the base NASA's Common Metadata Repository (CMR) granule search url, 
  # including LPDAAC provider name and max page size (2000 is the max allowed)
  cmr <- "https://cmr.earthdata.nasa.gov/search/granules.json?pretty=true&provider=LPDAAC_ECS&page_size=2000&concept_id="
  # Create a vector where GEDI's version 2 data products and CMR Concept ID are saved. 
  concept_ids <- c('GEDI01_B.002' = 'C1908344278-LPDAAC_ECS', 
                   'GEDI02_A.002' = 'C1908348134-LPDAAC_ECS', 
                   'GEDI02_B.002' = 'C1908350066-LPDAAC_ECS')
  # CMR uses pagination for queries with more features returned than the page size
  page <- "1"
  # Get CMR granule search endpoint with product concept ID, bbox & page number
  url <- glue("{cmr}{concept_ids[product]}&bounding_box={bbox}&pageNum={page}")
  # Return the CMR granule as json
  cmr_json <- jsonlite::fromJSON(url) 
  # Find the download links 
  raw_links  <-  cmr_json$feed$entry$links 
  
  # Search the download link for the date that we need. 
  for (i in 1:length(raw_links)) { 
    #href: an attribute of the anchor tag
    href <- raw_links[[i]]["href"][1,1]
    # Stop the loop when the date matches 
    if (grepl("2020.11.01", href, fixed = TRUE)){ 
      final_url <- href
      break
    } 
  }
 print(final_url)

}

# Get the urls that you can download GEDI DATA 
# WARNING:: 1B and 2A data are bigger than 1 GB. so just run the code and do not download it,
# just use the data that I downloaded already to save your laptop. 
request_gedi(GEDI1B, bbox)
request_gedi(GEDI2A, bbox)
request_gedi(GEDI2B, bbox)



