# ------------------------------------------------------------------------------------------------ #
# Spatial Querying and Analysis of GEDI data in R
# Author: Hyeonmin Kang
# Operating system: Ubuntu 20.04.4 LTS
# Last Updated: 25/04/2022
# Objective: The objective of this script is to demonstrate how to get the url 
# for downloading  NASA's Global Ecosystem Dynamics Investigation (GEDI) Data 
# directly from the LP DAAC Data Pool, using NASA's Common Metadata Repository (CMR).
# 
# ------------------------------------------------------------------------------------------------ #
#------------------------------------LOAD PACKAGES ----------------------------------------------- #
# Install the required packages, if not previously installed
if ("jsonlite" %in% rownames(installed.packages()) == FALSE) {install.packages("jsonlite")}
if ("glue" %in% rownames(installed.packages()) == FALSE) {install.packages("glue")}

# Load the necessary packages into R
library(jsonlite)
library(glue)
# -------------------------SET UP OUTPUT DIRECTORY AND QUERY PARAMETER---------------------------- #
# Set output dir for downloading the files
outdir <- getwd()

# set the bounding box 
bbox <- "8.877,51.465,9.921,51.762" #lon_min, lat_min, lon_max, lat_max 

# name of GEDI data we need 
# Here we need only GEDI2A data.
GEDI1B <- 'GEDI01_B.002' # geolocation product 
GEDI2A <- 'GEDI02_A.002' # Full-waveform product
GEDI2B <- 'GEDI02_B.002' # Elevation and Height Metrics product

# set the date time (this is just an example datetime to show how to use the below function)
datetime_ex = "2019.11.02"
# ------------------------------CREATE A FUNCTION TO QUERY CMR------------------------------------ #
# Currently, it is not available to download GEDI data directly from R and save it in 
# the wished directory. So we have to get the URL and download it by ourselves.
# Get the URLs so that you can download GEDI data. Use this URLs to download the data 
request_gedi_url <- function(product, bbox, datetime, print = TRUE) {
  # Define the base NASA's Common Metadata Repository (CMR) granule search URL, 
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
    if (grepl(as.character(datetime), href, fixed = TRUE)){ 
      final_url <- href
      break
    } 
  }
  if (print == TRUE) {
    print(final_url) 
    return(final_url)}
  if (print == FALSE) return(final_url)

}

# WARNING:: 1B and 2A data are bigger than 1 GB. 
# I recommend using the data I already downloaded to save your laptop. 
# ------------------------------EXAMPLE----------------------------------------------------------- #
# This is an example to show how to use the function 
# We do not this data! 
# Reading GEDI level2B data (h5 file)
GEDI2B_url <- request_gedi_url(GEDI2B, bbox, datetime_ex, print = TRUE)




