# ------------------------------------------------------------------------------------------------ #
# Spatial and Statistical Analysis of GEDI data in R
# Author: Hyeonmin Kang
# Institute: EAGLE master, Julius-Maximilians-University Wuerzburg
# Email: hyunminkang88@gmail.com
# github: https://github.com/YumYumYumi/MB2_EAGLE
# Operating system: Ubuntu 20.04.4 LTS
# R version : 4.2.0 (2022-04-22)
# Last Updated: 12/05/2022
# Objective: The objective of this script is to visualize and analyze NASA's Global 
# Ecosystem Dynamics Investigation (GEDI) 2A Data spatially and statistically and to detect the 
# forest disturbance by processing GEDI 2A DATA. 
# Data Provider: (1) https://land.copernicus.eu/pan-european/high-resolution-layers/forests
#                (2) https://sentinel.esa.int/web/sentinel/user-guides/sentinel-2-msi/product-types/level-2a
# ------------------------------------------------------------------------------------------------ #

#------------------------------------INSTALL AND LOAD REQUIRED PACKAGES -------------------------- #
# install required packages (if not installed yet)
packagelist <- c("dplyr","ggplot2","ggthemes","raster","rgdal","rgeos",
                 "sf","tidyverse","devtools","rGEDI", "ggstatsplot",
                 "sp","leaflet","leafsync","hdf5r","data.table","ggplot2","ggpubr")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

# If rGEDI is unavailable to install, use this development version of install line. 
devtools::install_git("https://github.com/carlos-alberto-silva/rGEDI", dependencies = TRUE)


#------------------------------------SET VARIABLES AND PARAMETERS -------------------------------- #

# Save your current working directory in a variable 
outdir <- getwd() 

# Set the bounding box
xmin <- 8.877 #lon_min
xmax <- 9.9219 #lon_max
ymin <- 51.465 #lat_min
ymax <- 51.762 #lat_max 

# necessary datetimes 
datetime_v <- c("2019.10.28", "2019.11.06", "2019.11.12", "2019.11.23", "2019.11.24")

# -----------------------------------IMPORT POLYGONS------------------------------------------- #
# Import forest polygons as sf object 
# Input data (1) forest type raster data inside of bounding box is converted to shapefile 
angiosperms <- st_read("./data/forest1_vec_fixed.shp") # vector of broad-leaved forests
gymnosperms <- st_read("./data/forest2_vec_fixed.shp") # vector of coniferous forests

# Combine two sf objects
forests <- rbind(angiosperms,gymnosperms)

# Input data (2) median NDVI (Normalized Difference Vegetation Index) image 
# of Sentinel-2 (May to October, 2019) 
ndvi_19 <- brick("./data/2019s2_clipped_ndvi.tif")


# (2) Above median NDVI is converted to shapefile and less than 0.5 NDVI values are extracted inside of forest area. 
forest_disturbance <- st_read("./data/ndvi19_disturbance_vec_0_fixed.shp")

# -----------------------------------PLOT MY AOI------------------------------------------- #
#plot my area of interest 
leaflet() %>% addTiles() %>%
  addRectangles(
    lng1=xmin, lat1=ymin,
    lng2=xmax, lat2=ymax,
    fillColor = "transparent"
  )
# -----------------------------------READ AND CLIP GEDI DATA------------------------------------------- #
# Get download link for GEDI data. 
# WARNING!! all of the GEDI data above are more than 10 GB. Check your storage space before you curse me.
for (d in datetime_v){
  request_gedi_url(GEDI2A, bbox, d, print = TRUE)
}

# If you downloaded all the GEDI by yourself by using above codes, please run below for loop. 
# This for loop load the GEDI data you saved in your directory and clip it by bounding box. 
# This might take more than 10 minutes, depending on your computer health and ability. 

level2A_dt <- NULL # the dataframe to be filled
for (d in datetime_v){
  GEDI2A_link <-request_gedi_url(GEDI2A, bbox, d,print = FALSE)
  GEDI2A_filename <- tail(strsplit(GEDI2A_link, '/')[[1]], n = 1)
  gedilevel2a <-readLevel2A(level2Apath = paste0(outdir,"/",GEDI2A_filename))
  new_d <-gsub("\\.","_",d)
  GEDI2A_clip_name <-paste0(outdir,"/data/level2a_clip_bb_", new_d,".h5")
  level2a_clip_bb <- clipLevel2A(gedilevel2a, xmin, xmax, ymin, ymax, output=GEDI2A_clip_name)
  level2AM<-getLevel2AM(level2a_clip_bb)
  level2AM_filtered <- level2AM[level2AM$rh100 != 0, ]
  level2A_dt <- rbind(level2A_dt, level2AM_filtered) 
}

# If you do not want to download all GEDI data and just use clipped data that I already uploaded in Github, 
# then use this for loop. This for loop will not kill your computer like above one. 
# This for loop load the GEDI data you saved in your directory and clip it by bounding box. 
level2A_dt <- NULL # the dataframe to be filled
for (d in datetime_v){
  new_d <-gsub("\\.","_",d)
  GEDI2A_clip_name <-paste0(outdir,"/data/level2a_clip_bb_", new_d,".h5")
  gedilevel2a <-readLevel2A(level2Apath = GEDI2A_clip_name)
  level2AM<-getLevel2AM(gedilevel2a)
  level2AM_filtered <- level2AM[level2AM$rh100 != 0, ]
  level2A_dt <- rbind(level2A_dt, level2AM_filtered) 
}

# Convert data table to data frame
level2A_df <- as.data.frame(level2A_dt)

# Transform the date frame as sf object 
level2A_sf <- st_as_sf(level2A_df, coords = c("lon_lowestmode","lat_lowestmode"), remove = FALSE)

# Match the coordinate Reference System 
st_crs(level2A_sf) <- st_crs(forests)


# This function intersects GEDI data by polygon. 
intersect_f <- function(GEDI_point, polygon) {
  # Match the coordinate Reference System 
  st_crs(GEDI_point) <- st_crs(polygon)
  # Intersect the gedi points by a polygon  
  intersected <- GEDI_point[st_intersects(GEDI_point, polygon) %>% lengths > 0,] 
  return(intersected)
}

# Intersect the GEDI data by forest areas  
level2A_forest <- intersect_f(level2A_sf, forests)

# Intersect the GEDI data by forest disturbances  
#level2A_disturbance<- intersect_f(level2A_forest, forest_disturbance)

# Subset forest GEDI data where quality flag 1 is. 
# A quality flag value of 1 indicates the laser shot meets criteria based on 
# energy, sensitivity, amplitude, and real-time surface tracking quality. 
level2A_forest_qf_1 <- level2A_forest[level2A_forest$quality_flag == 1, ]

# Intersect the GEDI data by forest disturbances  
level2A_disturbance_qf_1<- intersect_f(level2A_forest_qf_1, forest_disturbance)

## Visualize GEDI clipped data by leaflet (forest/disturbance GEDI data with quality flag = 1 data)
# When cursor is on the point, you can see the beam numbers. 
# If the pulses are on the same line, they have same beam number. 
leaflet() %>% addTiles() %>%
  addCircleMarkers(level2A_forest$lon_lowestmode,
                   level2A_forest$lat_lowestmode,
                   radius = 1,
                   opacity = 1,
                   color = "green")  %>%
  addCircleMarkers(level2A_forest_qf_1$lon_lowestmode,
                   level2A_forest_qf_1$lat_lowestmode,
                   radius = 1,
                   opacity = 0.5,
                   color = "orange",
                   label = paste0("Beam number: ", level2A_forest_qf_1$beam))  %>%
  addCircleMarkers(level2A_disturbance_qf_1$lon_lowestmode,
                   level2A_disturbance_qf_1$lat_lowestmode,
                   radius = 1,
                   opacity = 0.7,
                   color = "black",
                   label = paste0("Beam number: ", level2A_disturbance_qf_1$beam))  %>%
  addScaleBar(options = list(imperial = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldImagery)  %>%
  addLegend(colors = c("green","orange","black"), 
            labels= c("All forest GEDI data" ,"qf = 1 forest GEDI","qf = 1 forest disturbance GEDI"),
            title ="GEDI LEVEL2A") 


# clip forest disturbance polygon by bounding box of GEDI forest disturbance data
forest_disturbance_clipped <- st_crop(forest_disturbance, st_bbox(level2A_disturbance_qf_1))

## Visualize GEDI data with forest disturbance polygon using leaflet 
# This time, rh100 values will pop up if you click the points. 
rh100_popup <- paste0("<strong>rh100: </strong>", level2A_disturbance_qf_1$rh100)

leaflet(level2A_disturbance_qf_1) %>%  addTiles() %>%
  addPolygons(data = forest_disturbance_clipped, 
              opacity = 0.7,
              smoothFactor = 0.5,
              color = "pink")  %>%
  addCircleMarkers(~lon_lowestmode,
                   ~lat_lowestmode,
                   fillColor = "navy",
                   fillOpacity = 0.8,
                   radius = 3,
                   stroke = F,
                   popup = rh100_popup)  %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addLegend("bottomright",  # location, 
            colors = c("pink","navy"), 
            labels= c("disturbance area", "GEDI pulses")
            )
# ----------------------------BACKGROUND INFORMATION OF GEDI L2A DATA---------------------------------------------------- #
# The GEDI L2A data product provides relative height (RH) metrics, which are “lidar perceived” metrics
# rh100 = elev_highestreturn - elev_lowestmode 
# The RH metrics are intended for vegetated surfaces. 
# So it results over bare/water surfaces are still valid but may present some confusing results.
# The lower RH metrics (e.g., RH10) will often have negative values, particularly in low canopy cover conditions. 
# This is because a relatively high fraction of the waveform energy is from the ground and below elev_lowestmode. 
# For example, if the ground return contains 30% of the energy, then RH1 through 15 are likely to be 
# below 0 since half of the ground energy from the ground return is below the center of the ground return, 
# which is used to determine the mean ground elevation in the footprint (elev_lowestmode).
# --------------------------------------------Plot RH Metrics Transects-------------------------------------------------- #
## Let's import and extract a specific GEDI L2A shot, Plot Relative Height Metrics
# At first, we add new column for the orbit 
level2A_forest_qf_1$orbit_number <- level2A_forest_qf_1$shot_number*10^-13
# reorder column. So the last column (orbit_number) becomes the first column. 
level2A_forest_qf_1 <- level2A_forest_qf_1 %>% dplyr::select(orbit_number, everything())

# Each GEDI shot has a unique shot identifier (shot number) 
# (1) you can choose a certain shot number that you analyze like this 
shot <- 49580000200141006 
# 4958 : Orbit Number 
# 00 : Beam Number # Same beam number means they are in the same line 
# 0: Reserved for future use 
# 02: Sub-orbit Granule Number 
# 001 : Minor frame number 
# 41006 : Shot index 

# (2) or you can choose one random shot number using filter function in dplyr 
# see which orbit numbers we have 
dplyr::distinct(level2A_forest_qf_1, orbit_number) # same as unique function 
# see which beams my orbit number has 
level2A_forest_qf_1 %>% dplyr::filter(orbit_number == 4958) %>% dplyr::distinct(beam)   
# choose a random shot_number from the orbit and beam you selected
level2A_forest_qf_1 %>% dplyr::filter(orbit_number == 4958 & beam == "BEAM0000") %>% 
  dplyr::sample_n(1) %>% dplyr::select(shot_number) 
# because it is still a sf object, we have to extract shot_number using st_set_geometry 
shot <- (st_set_geometry(level2A_forest_qf_1, NULL) %>% 
  dplyr::filter(orbit_number == 4958 & beam == "BEAM0000") %>% 
  dplyr::sample_n(1) %>% 
  dplyr::select(shot_number))$shot_number 
# In order to plot the RH metrics, you also need to import the elevation recorded 
# at elev_lowestmode (the first elevation recorded) and elev_highestreturn 
# Grab the elevation recorded at the start and end of the RH metrics
shot_subset <- level2A_forest_qf_1[level2A_forest_qf_1$shot_number == shot, ]
zElevation <- shot_subset$elev_lowestmode  # Elevation
zTop <- shot_subset$elev_highestreturn # Elevation Highest Return

# below processes are for plotting a line plot showing the elevation at each percentile 
# for the selected shot using ggplot 
# At first, select all RH interval columns from shot_subset dataframe  
rh <- shot_subset[ ,grepl("rh", names(shot_subset))] 
# transpose the dataframe (columns become rows)
rh_t <- as.data.frame(t(rh)) 
# remove the last row (geometry)
rh_t <- head(rh_t, -1)
# rename the column
names(rh_t)[1] <- "y_value" 
# I want to set digits to 9
options(digits=9)
# Convert canopy height to canopy elevation by adding the ground elevation 
# to each RH interval. 
rh_t$y_value <- as.numeric(rh_t$y_value) + zElevation 
# Add a row for percentil
rh_t$x_value <- 0:(nrow(rh_t)-1)

# plot the relative cumulative RH profile line graph.
ggplot(data=rh_t, aes(x=x_value, y=y_value)) +
  geom_line(color = "darkgreen", size = 1.5) + coord_fixed(ratio=4) +
  # add some horizontal lines such as ground elevation, highest reflecting surface height, 
  # the relative height metrics at each quartile
  geom_hline(aes(yintercept=zElevation), color = "brown") + 
  annotate(geom="text", label='Ground return', x=80, y=zElevation, vjust=-0.5, color ="brown", size=3)+
  geom_hline(aes(yintercept=rh_t['rh25','y_value']), color="steelblue3") + 
  annotate(geom="text", label='rh25', x=15, y=rh_t['rh25','y_value'], vjust=-0.5, color ="steelblue3", size=3) +
  geom_hline(aes(yintercept=rh_t['rh50','y_value']), color="mediumblue") + 
  annotate(geom="text", label='rh50', x=15, y=rh_t['rh50','y_value'], vjust=-0.5, color ="mediumblue", size=3) +
  geom_hline(aes(yintercept=rh_t['rh75','y_value']), color="royalblue4") + 
  annotate(geom="text", label='rh75', x=15, y=rh_t['rh75','y_value'], vjust=-0.5, color ="royalblue4", size=3) +
  geom_hline(aes(yintercept=rh_t['rh100','y_value']), color="black") + 
  annotate(geom="text", label='rh100', x=15, y=rh_t['rh100','y_value'], vjust=-0.5, color ="black", size=3) +
  ggtitle('GEDI L2A Relative Height Metrics (0-100)') + # title 
  xlab('Percent Energy Returned') + # x-axis label
  ylab('Elevation (m)') + # y-axis label
  lims(y = c(rh_t$y_value[1] -1, rh_t$y_value[nrow(rh_t)] +1)) + # set y-axis scale 
  theme_light() + # add a theme
  theme(plot.title = element_text(hjust = 0.5))  # center the title

# ----------------------------Plot Beam Transects------------------------------------------------------------------------ # 
# This time a different orbit number and beam are chosen. 
# The reason why we set a certain orbit number and a certain beam number is,
# we want to plot relative height metrics with elevation on the same line of GEDI pulses. 
# Of course, you can change orbit number or beam number.

shot_number_df <- st_set_geometry(level2A_forest_qf_1, NULL) %>%  
  dplyr::filter(orbit_number == 5187 & beam == "BEAM0010") %>% 
  dplyr::filter(row_number()==1 | row_number()==n()) %>% 
  dplyr::select(shot_number) 

# Subset the rows containing the shot numbers we chose
level2A_forest_qf_1_df_subset<- subset(as.data.frame(level2A_forest_qf_1), 
                  shot_number > shot_number_df[1,1] & shot_number < shot_number_df[2,1])

# y-axis will be canopy height. 
# we make a new data frame and add only canopy height data as a column 
rh_df<- level2A_forest_qf_1_df_subset%>% dplyr::select(c(rh0:rh100))
# column and rows are changed (transpose)
rh_df_t <- data.table(rhs = c(t(rh_df)))

# we will plot the x-axis as distance. 
# So, calculate the distance between pulses 
distance <- as.numeric(diff(level2A_forest_qf_1_df_subset$shot_number))
distance <- c(0, distance) 
distance <- cumsum(distance)
# and add the distance as a new column. 
# Because GEDI Shots are spaced 60m apart, we multiply 60. 
rh_df_t <- cbind(rh_df_t, distance_pulses= rep(distance*60, each = ncol(rh_df)))

# Plot canopy height vs. shot index as a scatter plot.
ggplot(rh_df_t , aes(x = distance_pulses, y = rhs)) +
  geom_point(color='darkgreen', alpha=0.3, size =1.5 ) +
  ggtitle('Relative Height Metrics (0-100)') +
  xlab('Distance Along Transect (m)') + ylab('Canopy Height (m)') +
  theme_minimal() +
  theme(
    plot.title = element_text(color="azure4", size=14, face="bold.italic",hjust = 0.5),
    axis.title.x = element_text(color="azure4"),
    axis.title.y = element_text(color="azure4"))
# ----------------------------Plot RH Metrics Transects and Elevation---------------------------------------------------- #
# Now, we plot RH metrics adding elevation to get a better idea of the overall 
# three-dimensional structure of this transect.


# add elevation to rh 0-100 data frame
rh_df_elv <- rh_df + level2A_forest_qf_1_df_subset$elev_lowestmode
# column and rows are changed (transpose)
rh_df_elv_t <- data.table(rhs = c(t(rh_df_elv)))
# add x axis distance values 
rh_df_elv_t <- cbind(rh_df_elv_t, distance_pulses= rep(distance*60, each = ncol(rh_df_elv)))

# create a data frame for elevation line plot 
elv_df<- level2A_forest_qf_1_df_subset%>% dplyr::select(c(elev_lowestmode))
# add distance
elv_df$dist <- distance*60

# the main plot with all rh0-100 metrics with elevation
p_big <- ggplot(rh_df_elv_t , aes(x = distance_pulses, y = rhs)) +
            geom_point(aes(color='RH 0-100'), alpha=0.2, size =1.5 ) + #scatter plot
            geom_line(data=elv_df, aes(x=dist, y=elev_lowestmode, color="Ground Elevation" )) + #line plot
            scale_color_manual(values = c("Ground Elevation" = "goldenrod2", "RH 0-100" = "darkgreen")) + #add legend manually
            labs(title = "Relative Height Metrics (0-100) and Elevation", #title
                 subtitle ="(orbit: 5187, beam: BEAM0010)") + #subtitle 
            xlab('Distance Along Transect (m)') + ylab('Canopy Height (m)') + #axis labels
            theme_linedraw() + #theme
            geom_rect(aes(xmin = 2600, xmax = 5100, ymin =365, ymax = 425), color = "lightblue4", alpha = 0) + #where we zoom in
            theme( # additional visualization elements like color, size of font...  
              plot.title = element_text(color="lightsteelblue4", size=18, face="bold"),
              plot.subtitle =  element_text(color="lightsteelblue4", size=11),
              axis.title.x = element_text(color="lightsteelblue4", face="bold", size =13),
              axis.title.y = element_text(color="lightsteelblue4", face="bold", size = 13),
              legend.position="bottom",legend.box = "horizontal", 
              legend.title = element_blank(),
              legend.background = element_rect(fill = "NA"))

# the zoomed small plot to analyze more exactly
p_small <- ggplot(rh_df_elv_t , aes(x = distance_pulses, y = rhs)) +
  geom_point(color='darkgreen', alpha=0.2, size =1.5 ) + 
  geom_line(data=elv_df, aes(x=dist, y=elev_lowestmode), color="goldenrod2" ) + 
  theme_minimal() +
  xlim (2600, 5100) +
  ylim (365, 425) +
  theme_void()

# please zoom the plot! 
# now we visualize them   
p_big + 
  annotation_custom(ggplotGrob(p_small), xmin = 1300, xmax = 6400, ymin = 230, ymax = 325) +
  geom_rect(aes(xmin = 1300, xmax = 6400, ymin = 230, ymax = 325), 
            color='lightblue4', linetype='solid', alpha=0, lwd = 0.1) +
  geom_path(aes(x,y,group=grp), color ="lightblue4",
            data=data.frame(x = c(1300,2600,6400,5100), y=c(325,365,325,365),grp=c(1,1,2,2)),
            linetype='solid') +
  geom_curve(
    aes(x = 4000, y = 238, xend = 3000, yend = 255),
    color = "lightblue4",
    curvature = -0.2,
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  annotate("text", x =5050, y=238, label = " forest disturbance!", color = "lightblue4")

# ------------------statistics of GEDI-derived Elevation and Height Metrics (Level2A)----------------------- #

## We will create a data frame consisting of NDVI and rh100. 

# (1) create NDVI column 
# Extract raster value (NDVI) by points (GEDI shots) 
disturbance_ndvi_df <- as.data.frame(raster::extract(ndvi_19, level2A_disturbance_qf_1))
# Rename the column name
colnames(disturbance_ndvi_df) <- "NDVI" 
# change the NDVI value to float (To minimize the size of image, I downloaded the Sentinel-2
# Images as integer) 
disturbance_ndvi_df$NDVI <-(disturbance_ndvi_df$NDVI)/10000

# (2) create rh100 column
# add rh100 for statistical analysis 
disturbance_ndvi_df$rh100 <- level2A_disturbance_qf_1$rh100


# (3) create shot_number column which will be removed later 
# we need this column only to delete outlier values. 
disturbance_ndvi_df$shot_number <- level2A_disturbance_qf_1$shot_number

# (*) remove the values that matches with outliers from the original dataframe. 
# This will be used later for the statistical analysis. 
level2A_forest_qf_1_no_disturbance <- level2A_forest_qf_1[!(level2A_forest_qf_1$shot_number %in% disturbance_ndvi_df$shot_number),]

# (4) save outliers as dataframe 
# (why outliers here? some of pulses were edge of disturbance areas and show very high values.)
outlier_disturbance_ndvi_df <- disturbance_ndvi_df[disturbance_ndvi_df$rh100 %in% boxplot.stats(disturbance_ndvi_df$rh100)$out,]

# (5) destroy outliers from my precious dataframe 
# = remove outliers and save it as dataframe
disturbance_ndvi_df <- disturbance_ndvi_df[!disturbance_ndvi_df$rh100 %in% boxplot.stats(disturbance_ndvi_df$rh100)$out,]

# (6) now remove the shot_number column from disturbance dataframe 
disturbance_ndvi_df <- dplyr::select(disturbance_ndvi_df, -shot_number)



# Let's just check which dataframe we created until now to get an overview. 
# (a) disturbance_ndvi_df : 2 columns (NDVI value and rh100) almost bare area because of forest disturbance  
# (b) level2A_forest_qf_1_no_disturbance : original LEVEL2A data without disturbance 
# (c) outlier_disturbance_ndvi_df :  2 columns (NDVI value and rh100) of outlier values 


# Compute summary statistics (min, max, mean, and quartiles)
summary(level2A_forest_qf_1_no_disturbance$rh100)
# Even though we already deleted disturbance values, we see still very low values 
# what is the lowest 10 % value? 6.278
quantile(level2A_forest_qf_1_no_disturbance$rh100, 0.1) 
# here you can see all the values that are all low 10% of them. 
sort(level2A_forest_qf_1_no_disturbance$rh100)[1:(length(level2A_forest_qf_1_no_disturbance$rh100)/10)]  

#why?
# - some of forest area data from copernicus land are misclassifed. Some areas are not forest. 
# - GEDI shots are on the edge of the forest data. 
# - forest disturbance data was not highly accurate 
# - other forest disturbances were not detected.
# - disturbance data is based on medium of sentinel 2 images from May to October. Maybe some disturbance areas created in November. our GEDI data are from October to November. 

# Here you can see low rh100 GEDI data on the leaflet map
level2A_forest_qf_1_no_disturbance$rh100_round <- round(level2A_forest_qf_1_no_disturbance$rh100, 1)
leaflet(data = (level2A_forest_qf_1_no_disturbance %>% filter(rh100 < 10))) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% #OpenStreetMap
  addMarkers(~lon_lowestmode, ~lat_lowestmode,  
             label =  ~as.character(rh100_round), 
             labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T, textsize = "15px")) 
#names(leaflet.providers::providers_loaded()$providers)


## We will see the correlation between NDVI value and rh100 

# Create function of plotting correlation graphs and compare the results produced by selecting different sample size. 
# Depending on the size of samples from forest disturbance GEDI data and no-disturbance GEDI data, 
# the correlation is different. 
show_cor <- function(sample_n_forest, sample_n_disturbance) {
  forest_sample <- level2A_forest_qf_1_no_disturbance[sample(which(level2A_forest_qf_1_no_disturbance$rh100 >17), sample_n_forest), ]
  extract_df2 <- as.data.frame(raster::extract(ndvi_19, forest_sample)) 
  colnames(extract_df2) <- "NDVI"
  extract_df2$NDVI <-(extract_df2$NDVI)/10000
  extract_df2$rh100 <- forest_sample$rh100
  extract_df2 <- extract_df2[!extract_df2$rh100 %in% boxplot.stats(extract_df2$rh100)$out,]
  disturbance_ndvi_sample <- sample_n(disturbance_ndvi_df, sample_n_disturbance)
  final_extract_df <- rbind(disturbance_ndvi_sample,extract_df2)
  p_title = paste0("(", sample_n_forest, ",",sample_n_disturbance,")")
  p <- ggscatter(final_extract_df, x = "NDVI", y = "rh100", 
                 color = "darkslategrey",
                 fill = "darkslategrey",
                 repel = TRUE,
                 add = "reg.line", # Add regression line
                 conf.int = TRUE,  # Add confidence interval
                 add.params = list(color = "blue",
                                   fill = "cadetblue4"),
                 size = 0.8,
                 cor.coef = TRUE,
                 cor.coef.size = 2.5, 
                 cor.coeff.args = list(method = "pearson"),
                 xlab = "NDVI", ylab = "rh100", title =  p_title,
                 ggtheme = theme(plot.title = element_text(hjust = 0.5, size=8),
                                 axis.title.x  = element_text(size = 6),
                                 axis.title.y  = element_text(size = 6),
                 ))
  
  return(p)
}
library(patchwork)
p1 <- show_cor(30,30)
p2 <- show_cor(60,30)
p3 <- show_cor(200,30)
p4 <- show_cor(90,10)
p5 <- show_cor(300,10)
p6 <- show_cor(400,0)

(p1 + p2 +p3+ p4+ p5+p6) + plot_layout(ncol = 3, nrow = 2,)+ plot_annotation(
  title = 'Correlation comparison by sample size',
  subtitle = "(sample size of rh100 > 17, sample size of forest disturbance)",
  caption = paste0("Whole sample size: (" , nrow(level2A_forest_qf_1_no_disturbance), ",",nrow(disturbance_ndvi_df),")")
)
# Shapiro-Wilk normality test for mpg
#shapiro.test(final_extract_df$NDVI) # => p = 0.1229
# Shapiro-Wilk normality test for wt
##shapiro.test(final_extract_df$rh100) # => p = 0.09
#cor(final_extract_df$NDVI, final_extract_df$rh100)
#ggqqplot(final_extract_df$rh100)
#cor.test(final_extract_df$NDVI, final_extract_df$rh100, method = "pearson")
# plot

#ggscatterstats(data = final_extract_df, x = NDVI, y = rh100)
