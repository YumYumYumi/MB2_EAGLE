## Title of Project: Detecting Forest Disturbance By Processing GEDI 2A Data
#### Author: Hyeonmin Kang
#### Institute: EAGLE master, Julius-Maximilians-University Wuerzburg
#### Email: hyunminkang88@gmail.com
#### github: https://github.com/YumYumYumi/MB2_EAGLE
#### Operating system: Ubuntu 20.04.4 LTS
#### R version : 4.2.0 (2022-04-22)
#### Last Updated: 12/05/2022
#### Objective: (1) demonstrating how to get the URL for downloading NASA's Global Ecosystem Dynamics Investigation (GEDI) Data directly from the LP DAAC Data Pool, using NASA's Common Metadata Repository (CMR). (2) visualizing and analyzing NASA's Global Ecosystem Dynamics Investigation (GEDI) 2A Data spatially and statistically. (3) detecting the forest disturbance by processing GEDI 2A DATA. 
#### Additional Input Data Provider: (1) forest type raster data inside of bounding box is converted to shapefile: https://land.copernicus.eu/pan-european/high-resolution-layers/forests   (2) median NDVI (Normalized Difference Vegetation Index) Sentinel 2 raster tiff image: https://sentinel.esa.int/web/sentinel/user-guides/sentinel-2-msi/product-types/level-2a

#### Overview of Workflow: At first, we will get URLs for downloading GEDI data. (Currently, there is no way to import GEDI Data into R directly). After downloading GEDI Data (file format: HDF5) using URLs, we will load GEDI data as dataframe by using rGEDI package. Unfortunately, only a couple of functions from the rGEDI package are available. So we have to make some functions by ourselves to process GEDI data. We will visualize GEDI shots inside of aoi, qualitative GEDI shots, and qualitative GEDI shots representing forest disturbance using Leaflet package. Next, we will plot a relative cumulative RH profile line graph by randomly picking one sample GEDI shot and RH Metrics Transects with elevation by choosing one beam. Finally, statistical analysis is performed.  

#### What is GEDI? GEDI uses near-infrared lasers, is about the size of a refrigerator, has 3 lasers, illuminates 25m diameter footprints on Earth’s surface, and measures forest canopy height. (https://gedi.umd.edu/)

Please run r scirpts in this order : (1) download_gedi.R (2) gedi2a_processing.R
