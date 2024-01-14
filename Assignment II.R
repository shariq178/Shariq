setwd("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2")

getwd()

library(sf)
library(tmap) 
library(leaflet) 
library(dplyr)
library(ggplot2)
install.packages("rgdal")
install.packages("terra")
library(terra)
library(raster)
install.packages("readxl")
library(readxl)






# Read the shape file

Indonesia_map<- st_read("idn_admbnda_adm0_bps_20200401.shp")


# Read the XL files

power_plants <- read_excel("power plant Indonesia2.xlsx")


gg <- ggplot() +
  geom_sf(data = Indonesia_map, fill = "lightblue", color = "grey") +
  theme_minimal()



#gg + geom_point(data = power_plants, aes(x = longitude, y = latitude), color = "red", size = 2) +
labs(title = "Renewable Power Plants in Indonesia",
     x = "Longitude",
     y = "Latitude")

#gg <-gg + geom_point(data = power_plants, aes(x = longitude, y = latitude, size = capacity_mw), color = "red") +
labs(title = "Renewable Power Plants in Indonesia",
     x = "Longitude",
     y = "Latitude") +
  scale_size_continuous(range = c(1, 12)) +  # You may need to adjust the range based on your data
  theme(legend.position = "right")

# Print the plot
#print(gg)


shapes <- c("solar" = 15, "Geothermal" = 17, "Hydro" = 16, "wind" = 18)


# Plot
gg <- ggplot() +
  geom_sf(data = Indonesia_map, fill = "lightblue", color = "grey") +
  geom_point(data = power_plants, aes(x = longitude, y = latitude, size = capacity_mw, shape = type, color = status)) +
  scale_shape_manual(values = shapes) +
  scale_size_continuous(range = c(1, 12)) +  # Adjust the range based on your data
  labs(title = "Renewable Power Plants in Indonesia",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

# Print the plot
print(gg)


quick_mask_raster <- function(raster_data, masking_vector){
masking_vector <- st_transform(masking_vector, st_crs(raster_data))
masked_raster_data <- mask(raster_data, masking_vector)
return(masked_raster_data)}

Indonesia_map<- st_read("idn_admbnda_adm0_bps_20200401.shp")

1.#LAND COVER
landcover<-rast("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/IDN_msk_cov.vrt")
quick_mask_raster <- function(raster_data, masking_vector)
{
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
  masked_raster_data <- mask(raster_data, masking_vector)
  return(masked_raster_data)}

raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)

land_crop<-crop(landcover, raster_template)
land_crop<-resample(land_crop,raster_template)
r_matrix<-matrix (c(0 ,10, 0,
                    10,15, 1,
                    15, 16, 2,
                    16, 20, 3,
                    20, 25, 4), ncol=3, byrow= TRUE)
land_potential<-classify(land_crop, r_matrix,include.lowest =TRUE)
land_potential <- quick_mask_raster(land_potential,Indonesia_map)
plot(land_potential)

tmap_mode('plot')
tm_shape(land_potential)+tm_raster(style='cat',alpha = 0.7)+
  tm_shape(Indonesia_map)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size =0.8)+
  tm_scale_bar(position=c('0.','0.05'), width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size=1)
tm_layout(main.title="Landcover_potential")





2.#Peat land
peatland <-st_read("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/Indonesia_peat_lands.shp")
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)
peatland_raster <- rasterize(peatland, raster_template)
plot(peatland_raster)

tmap_mode('plot')
tm_shape(peatland_raster)+tm_raster(palette= 'darkgreen')+
  tm_shape(Indonesia_map)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size =0.8)+
  tm_scale_bar(position=c('0.','0.05'), width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size=1)
tm_layout(main.title="Peatland")







#Peatland method2
peatland <-st_read("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/Indonesia_peat_lands.shp")
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)
peatland_raster <- rasterize(peatland, raster_template)
quick_mask_raster <- function(raster_data, masking_vector)
{
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
  masked_raster_data <- mask(raster_data, masking_vector)
  return(masked_raster_data)}

peatland_crop<-crop(peatland_raster, raster_template)
peatland_crop<-resample(peatland_crop,raster_template)
plot(peatland_crop)
r_matrix<-matrix (c(0 ,2000000000, 0,
                    2000000000,4000000000, 1,
                    4000000000,6000000000, 2,
                    6000000000, 8000000000, 3,
                    8000000000, 10000000000, 4,
                    10000000000,15000000000, 5), ncol=3, byrow= TRUE)
peatland_potential<-classify(peatland_crop, r_matrix,include.lowest =TRUE)
peatland_potential <- quick_mask_raster(peatland_potential,Indonesia_map)
plot(peatland_potential)

3.#Population

population<-rast("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/IDN_msk_pop.vrt")
quick_mask_raster <- function(raster_data, masking_vector)
{
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
  masked_raster_data <- mask(raster_data, masking_vector)
  return(masked_raster_data)}

raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)

pop_crop<-crop(population, raster_template)
pop_crop<-resample(pop_crop,raster_template)
pop_crop
plot(pop_crop)
r_matrix<-matrix (c(0 ,10, 0,
                    10,15, 1,
                    15, 20, 2,
                    20, 25, 3,
                    25, 30, 4), ncol=3, byrow= TRUE)
pop_raster<-classify(pop_crop, r_matrix,include.lowest =TRUE)
pop_raster <- quick_mask_raster(pop_raster,Indonesia_map)
plot(pop_raster)

tmap_mode('plot')
tm_shape(pop_raster)+tm_raster(style='pretty',alpha = 0.7)+
  tm_shape(Indonesia_map)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size =0.8)+
  tm_scale_bar(position=c('0.7','0.05'), width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size=1)
tm_layout(main.title='Settlement')


4.#Protected
protected <-st_read("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)
protected_raster <- rasterize(protected, raster_template)
plot(protected_raster)

tmap_mode('plot')
tm_shape(pop_raster)+tm_raster(palette='darkgreen')+
  tm_shape(Indonesia_map)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size =0.8)+
  tm_scale_bar(position=c('0.','0.05'), width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size=1)
tm_layout(main.title='Protected Land')



#Constrained plot

constrained_map <- c(protected_raster, pop_raster, land_potential, peatland_potential)

names(constrained_map)=c("protected_raster","pop_raster","land_potential","peatland_potential")


constrained = raster_template = rast( resolution = 0.05,
                                      xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)


constrained_map_df=as.data.frame(constrained_map, XY=TRUE)
id= which(constrained_map_df$ 'protected_raster'==1|
            constrained_map_df$ 'pop_raster'==0|
            constrained_map_df$ 'land_potential'==0 |
            constrained_map_df$ 'land_potential'==2 |
            constrained_map_df$ 'peatland_potential'==1)
constrained[id]=1
constrained_mask<-quick_mask_raster(constrained, Indonesia_map)

# Replace NA with 0 to indicate non-constrained areas
constrained[is.na(constrained)] <- 0

# Plot the constrained raster
plot(constrained_mask)

tmap_mode('plot')
tm_shape(constrained_mask)+tm_raster(style='cat',alpha = 0.7)+
  tm_shape(Indonesia_map)+tm_borders()+
  tm_compass(position=c('0.9','0.8'),size =0.8)+
  tm_scale_bar(position=c('0.','0.05'), width =0.2)+
  tm_layout(legend.position=c('0.003','0.03'),legend.text.size=0.6,legend.title.size=1)
tm_layout(main.title='Constrained Land Plot')



#ELEVATION INDONESIA
library(terra)
elevation<-rast("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/IDN_msk_alt.vrt")
crs(Indonesia_map)==crs(elevation)
Indonesia_map <-st_transform(Indonesia_map, st_crs(elevation))
elevation_mask <- mask(elevation,Indonesia_map) #note the ordering, the second argument is extent we want to crop raster to and the first argument is one you want to crop a bit.
plot(elevation_mask)
elevation_rc<-resample(elevation_mask,raster_template)

slope = terrain(elevation, v='slope', unit='degrees') 
#  reclassify slope data, assign suitable area as 1, and unsuitable as 0.
slope_m<- matrix(c(0, 10, 2,
                   10, 55, 1), ncol=3, byrow=TRUE) 
elevation_rc <- classify(slope, slope_m, include.lowest=TRUE )

plot(elevation_rc)
elevation_rc







#quick_mask_raster <- function(raster_data, masking_vector){
# masking_vector <- st_transform(masking_vector, st_crs(raster_data))
# masked_raster_data <- mask(raster_data, masking_vector)
# return(masked_raster_data)}




#TEMPERATURE

Temperature<-rast("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/TEMP.tif")
Temperature_mask <- mask(Temperature,Indonesia_map) #note the ordering, the second argument is extent we want to crop raster to and the first argument is one you want to crop a bit.
plot(Temperature_mask)

Temperature_rc<-resample(Temperature_mask,raster_template)

r_matrix<-matrix (c( 0,  10, 0,
                     10, 20, 1,
                     20, 25, 2,
                     25, 40, 3), ncol=3, byrow= TRUE)   #This code chunk indicates that we want to make cell with value between 10-25 into 1, ans cell with value 25-50 into 2, and so on. 

r_matrix

Temperature_rc <- classify(Temperature_rc, r_matrix, include.lowest=TRUE ) #use predefined matrix to guide the reclassification.
plot(Temperature_rc)





#GRID INDONESIA

Indonesia_grid  <- read_sf('grid.geojson')
Indonesia_grid <- st_transform(Indonesia_grid, st_crs(Indonesia_map))
grid_Indonesia <- st_intersection ( Indonesia_grid, Indonesia_map )



library(ggplot2)
ggplot() +
  geom_sf(data=Indonesia_map, fill='antiquewhite') +
  
  
  geom_sf(data=grid_Indonesia, color='blue') + 
  theme_classic()

tm_shape(Indonesia_map)+
  tm_fill( col="antiquewhite", alpha=.75)+
  tm_shape(Indonesia_map)+
  tm_borders(lwd=2)+
  tm_shape(grid_Indonesia)+
  tm_lines(col="blue", lwd=3, lty="dashed")


raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)   

grid_Indonesia_1km <-st_buffer(grid_Indonesia, 1000) 
grid_Indonesia_1km_raster <-rasterize(grid_Indonesia_1km , raster_template)  

grid_Indonesia_5km <-st_buffer(grid_Indonesia, 5000) 
grid_Indonesia_5km_raster <-rasterize(grid_Indonesia_5km , raster_template)

grid_Indonesia_10km <-st_buffer(grid_Indonesia, 10000) 
grid_Indonesia_10km_raster <-rasterize(grid_Indonesia_10km , raster_template)


ind_grid_combined=c(grid_Indonesia_1km_raster,grid_Indonesia_5km_raster,grid_Indonesia_10km_raster)
names(ind_grid_combined)=c("three","two","one")
plot(ind_grid_combined)

ind_grid_combined_df<-as.data.frame(ind_grid_combined , XY= TRUE)
one_grid<-which(ind_grid_combined_df$one==1)
two_grid<-which(ind_grid_combined_df$two==1)
three_grid<-which(ind_grid_combined_df$three==1)

ind_grid_combine_fix=raster_template
values(ind_grid_combine_fix)= NA
ind_grid_combine_fix[one_grid]=1
ind_grid_combine_fix[two_grid]=2
ind_grid_combine_fix[three_grid]=3


ind_grid_combine_fix<-quick_mask_raster(ind_grid_combine_fix,Indonesia_map)
plot(ind_grid_combine_fix)


#Roads Indonesia
Indonesia_roads <-st_read('IDN_roads.shp')
Indonesia_map<- st_read("idn_admbnda_adm0_bps_20200401.shp")
Indonesia_roads <- st_transform(Indonesia_roads, st_crs(Indonesia_map))
road_Indonesia <- st_intersection ( Indonesia_roads, Indonesia_map )

library(ggplot2)
ggplot() +
  geom_sf(data=Indonesia_map, fill='grey') +
  geom_sf(data=road_Indonesia, color='orange') + 
  theme_classic()


road_Indonesia_5km <-st_buffer(road_Indonesia, 5000) 
road_Indonesia_5km_raster <-rasterize(road_Indonesia_5km , raster_template)


#road_Indonesia_25km <-st_buffer(road_Indonesia, 25000) 
#road_Indonesia_25km_raster <-rasterize(road_Indonesia_25km , raster_template) 

road_Indonesia_50km <-st_buffer(road_Indonesia, 50000) 
road_Indonesia_50km_raster <-rasterize(road_Indonesia_50km , raster_template) 


Ind_road_combined<-c(road_Indonesia_5km_raster,road_Indonesia_50km_raster)
names(Ind_road_combined)=c("two","one")
# Define the color palette
colors <- c("blue", "orange")

# Use the color palette when plotting
plot(Ind_road_combined, col = colors)

# Rest of your code for plotting ind_road_combine_fix



plot(Ind_road_combined)

Ind_road_combined_df<-as.data.frame(Ind_road_combined,XY =TRUE)
one_road<-which(Ind_road_combined_df$one==1)
two_road<-which(Ind_road_combined_df$two==1)

ind_road_combine_fix=raster_template
values(ind_road_combine_fix)= NA
ind_road_combine_fix[one_road]=1
ind_road_combine_fix[two_road]=2


ind_road_combine_fix<-quick_mask_raster(ind_road_combine_fix,Indonesia_map)
plot(ind_road_combine_fix)



#2 SSRD calculations

library(ncdf4) #library to read and process netcdf data
era <- nc_open("SSRD data.nc" )
era

lon <- ncvar_get(era, "longitude")
lat <- ncvar_get(era, "latitude")
time <- ncvar_get(era, "time")
time

dim(time)

tunits <- ncatt_get(era,"time","units")
tunits

library(chron)


#convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ") #strsplit: split the element of character vector. we can convert  "hours since 1900-01-01" to "hours"      "since"      "1900-01-01"
tdstr <- strsplit(unlist(tustr)[3], "-") #convert "1900-01-01" to "1900" "01"   "01"
tyear <- as.integer(unlist(tdstr)[1]) 
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])

chron(time/24, origin=c(tmonth, tday, tyear) ) #this function is of great help. It can convert the hours since format to the format we are quite familiar with.


ssrd_array <- ncvar_get(era,"ssrd") #get the Surface solar radiation downwards
ssrd_array
dim(ssrd_array)


dlname <- ncatt_get(era,"ssrd","long_name")
dunits <- ncatt_get(era,"ssrd","units")
fillvalue <- ncatt_get(era,"ssrd","_FillValue")

library(lattice)

library(RColorBrewer)


ssrd_slice <- ssrd_array[,,2] 

ssrd_slice
#The ssrd_slice is actually a matrix. class(ssrd_slice)
# What does 1 in ssrd_array[,,1] indicate?  What if I want to slice all four time slice for "07/01/19"? 

length(na.omit(as.vector(ssrd_slice))) /length(as.vector(ssrd_slice)) 
dim(ssrd_slice)

image(ssrd_slice, col=rev(brewer.pal(10,"RdBu")) )

max_rad <- max(ssrd_slice, na.rm=TRUE)
max_rad

lonlat <- as.matrix( (expand.grid(lon, lat))) #lon and lat are what we extracted in step 2.
dim(lonlat)

ssrd_vec <- as.vector( ssrd_slice) 
length(ssrd_vec)


ssrd_df <- data.frame( cbind( lonlat,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 


library(sf)
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

library(tmap)
tmap_mode("view")

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "YlOrRd")



#Average SSRD FOR Jan21,july21 & jan 22

#JAN 
ssrd_slices_for_date <- ssrd_array[,,1:4]
average_slice <- apply(ssrd_slices_for_date, c(1, 2), mean)
image(average_slice, main = "Average for 01/01/19")

lonlat <- as.matrix( (expand.grid(lon, lat))) #lon and lat are what we extracted in step 2.
dim(lonlat)

ssrd_vec <- as.vector(average_slice) 
length(ssrd_vec)


ssrd_df <- data.frame( cbind( lonlat,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 


library(sf)
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

library(tmap)
tmap_mode("view")

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "YlOrRd")

#FEB
ssrd_slices_for_date <- ssrd_array[,,25:28]
average_slice <- apply(ssrd_slices_for_date, c(1, 2), mean)
image(average_slice, main = "Average for 01/01/19")

lonlat <- as.matrix( (expand.grid(lon, lat))) #lon and lat are what we extracted in step 2.
dim(lonlat)

ssrd_vec <- as.vector(average_slice) 
length(ssrd_vec)


ssrd_df <- data.frame( cbind( lonlat,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 


library(sf)
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

library(tmap)
tmap_mode("view")

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "YlOrRd")



#JULY SSRD

ssrd_slices_for_date <- ssrd_array[,, 93:96]
average_slice <- apply(ssrd_slices_for_date, c(1, 2), mean)
image(average_slice, main = "Average for 07/01/19")

lonlat <- as.matrix( (expand.grid(lon, lat))) #lon and lat are what we extracted in step 2.
dim(lonlat)

ssrd_vec <- as.vector( average_slice) 
length(ssrd_vec)


ssrd_df <- data.frame( cbind( lonlat,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 


library(sf)
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

library(tmap)
tmap_mode("view")

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "YlOrRd")






install.packages("gstat")
library(gstat)

#AVERAGE SSRD for 2 years

ssrd_slices <- ssrd_array[,,1:96]

dim(ssrd_slices )
average_slices <- apply(ssrd_slices, c(1, 2), mean, na.rm = TRUE)

overall_average_slice <- apply(average_slices, c(1, 2), mean, na.rm = TRUE)

ssrd_vec <- as.vector(overall_average_slice)
ssrd_df <- data.frame(cbind(lonlat, ssrd_vec))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df <- na.omit(ssrd_df)

library(sf)
ssrd_sf <- st_as_sf(ssrd_df, coords = c("lon", "lat"), crs = 4326)
ssrd_sf <- st_transform(ssrd_sf, 4326)

library(tmap)
tmap_mode("view")

tm_shape(ssrd_sf) +
  tm_dots(col = "ssrd", style = "quantile", size = .001, palette = "YlOrRd") +
  tm_layout(title = "Average SSRD for 2 years")

ncatt_get(era,"ssrd","units")

radiation_to_power <- function(G, A=1, r=0.175, p=0.6, hours=1){
  kWh <- G * A * r * p * (hours/3600) / 1000
  return(kWh)
}
ssrd_kwh <- as.data.frame (radiation_to_power (ssrd_df_value))
ssrd_df_value <- cbind(ssrd_df_value,ssrd_kwh$ssrd)
colnames(ssrd_df_value) [4] <- 'ssrd_kwh'
ssrd_sf$ssrd_kwh = ssrd_kwh$ssrd
Indonesia_map = st_read("idn_admbnda_adm0_bps_20200401.shp")
solar_sf<-st_transform(ssrd_sf,4326)
indonesia = st_transform(Indonesia_map, st_crs(solar_sf))
coor=as.data.frame(st_coordinates(solar_sf))
solar_sf$x=coor$X
solar_sf$y=coor$Y
solar_nogeom=st_drop_geometry(solar_sf)
solar_nogeom=na.omit(solar_nogeom)
gs <-gstat(formula=ssrd_kwh~1, locations=~x+y, data=solar_nogeom, nmax=Inf,set=list(idp=5))
Indonesia_map = st_read("idn_admbnda_adm0_bps_20200401.shp")

raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)

#Interpolate
idw <- interpolate(raster_template, gs, debug.level=0)
plot(idw$var1.pred)
idw_mask <- mask(idw, indonesia)
plot(idw_mask$var1.pred)

names(idw_mask) = c( "predicted","observed" )

tmap_mode("view")

tm_shape(idw_mask$predicted) + 
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "YlOrRd", legend.show = TRUE) +
  tm_layout(title = "IDW SSRD with idp 5")

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

null <- RMSE(mean(solar_sf$ssrd_kwh), solar_sf$ssrd_kwh)
null

n_idp = 20 #examine power ranging from 1 to 20
n_fold =10

rmse <- rep(NA, n_fold) #generate 10 NA
set.seed(7713)
kf <- sample(1:n_fold, nrow(solar_nogeom), replace=TRUE)
va = data.frame( c(1:n_idp), NA)
colnames(va) =c("idp","rmse") 



for (j in 1:n_idp) 
{
  for (i in 1:n_fold) {
    test <- solar_nogeom[kf == 1, ]
    train <- solar_nogeom[kf != 1, ]
    gs <- gstat(formula=ssrd_kwh~1, locations=~x+y, data=train, nmax=Inf, set=list(idp=j))
    pre = predict(gs, test, debug.level=0 )
    rmse[i] <- RMSE(test$ssrd_kwh, pre$var1.pred)
  }
  va[j,2] = (mean(rmse) )
}
va[which(va$rmse==min(va)),]

library(ggplot2)
ggplot(va) +
  geom_point(aes(x = idp, y= rmse))+
  geom_hline(yintercept=min(va), linetype="dashed", color = "red")+
  theme_classic()


tm_shape(ssrd_sf[ssrd_sf$ssrd>17000000, ])+
  tm_dots(col="ssrd", style = "quantile", size=.01, palette = "YlOrRd")





constrained_mask
elevation_rc
Temperature_rc
ind_grid_combine_fix
ssrd_raster
ind_road_combine_fix




#SUITABILITY

writeRaster(ssrd_raster, filename="solar.tif",overwrite=TRUE)
writeRaster(elevation_rc, filename="slope.tif",overwrite=TRUE)
writeRaster(Temperature_rc, filename="Temperature.tif")
writeRaster(ind_grid_combine_fix, filename="Grid.tif")
writeRaster(ind_road_combine_fix, filename="road.tif")
writeRaster(constrained_mask, filename="constrained.tif")

solar <-rast( "solar.tif")
plot(solar)
r_matrix2<-matrix (c(100000, 130000, 1,
                     130000, 160000, 2,
                     160000, 200000, 3), ncol=3, byrow= TRUE)   
solar_rc <- classify(solar, r_matrix2, include.lowest=TRUE ) 
plot(solar_rc)




 

class(solar)
crs(Indonesia_map)==crs(solar)
Indonesia_map <-st_transform(Indonesia_map, st_crs(solar))
crs(Indonesia_map)==crs(solar)
solar_mask <- mask(solar, Indonesia_map)
plot(solar_mask)

quick_mask_raster <- function(raster_data, masking_vector){
  masking_vector <- st_transform(masking_vector, st_crs(raster_data))
  masked_raster_data <- mask(raster_data, masking_vector)
  return(masked_raster_data)
}


elevation_mask <- quick_mask_raster(elevation, Indonesia_map)
temperature_mask <-  quick_mask_raster(temperature, Indonesia_map)
Road_mask<-quick_mask_raster(Road, Indonesia_map)
Grid_mask<-quick_mask_raster(Grid, Indonesia_map)
Constrained_mask<-quick_mask_raster(Constrained, Indonesia_map)

plot(elevation_mask)
plot(temperature_mask)
plot(Road_mask)
plot(Grid_mask)
plot(Constrained_mask)

#elevation_rc
library(terra)
elevation<-rast("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/IDN_msk_alt.vrt")
crs(Indonesia_map)==crs(elevation)
Indonesia_map <-st_transform(Indonesia_map, st_crs(elevation))
elevation_mask <- mask(elevation,Indonesia_map) #note the ordering, the second argument is extent we want to crop raster to and the first argument is one you want to crop a bit.
plot(elevation_mask)
elevation_rc<-resample(elevation_mask,raster_template)

slope = terrain(elevation, v='slope', unit='degrees') 
#  reclassify slope data, assign suitable area as 1, and unsuitable as 0.
slope_m<- matrix(c(0, 10, 2,
                   10, 55, 1), ncol=3, byrow=TRUE) 
elevation_rc <- classify(slope, slope_m, include.lowest=TRUE )

plot(elevation_rc)

#TEMPERATURE

Temperature<-rast("D:/SPATIAL DATA ANALYSIS/ASSIGNMENT2/TEMP.tif")
Temperature_mask <- mask(Temperature,Indonesia_map) #note the ordering, the second argument is extent we want to crop raster to and the first argument is one you want to crop a bit.
plot(Temperature_mask)

Temperature_rc<-resample(Temperature_mask,raster_template)

r_matrix<-matrix (c( 0,  10, 0,
                     10, 20, 1,
                     20, 25, 2,
                     25, 40, 3), ncol=3, byrow= TRUE)   #This code chunk indicates that we want to make cell with value between 10-25 into 1, ans cell with value 25-50 into 2, and so on. 

r_matrix

Temperature_rc <- classify(Temperature_rc, r_matrix, include.lowest=TRUE ) #use predefined matrix to guide the reclassification.
plot(Temperature_rc)

#CONSTRAINED MASK

constrained_map <- c(protected_raster, pop_raster, land_potential, peatland_potential)

names(constrained_map)=c("protected_raster","pop_raster","land_potential","peatland_potential")


constrained = raster_template = rast( resolution = 0.05,
                                      xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(Indonesia_map)$wkt)


constrained_map_df=as.data.frame(constrained_map, XY=TRUE)
id= which(constrained_map_df$ 'protected_raster'==1|
            constrained_map_df$ 'pop_raster'==0|
            constrained_map_df$ 'land_potential'==0 |
            constrained_map_df$ 'land_potential'==2 |
            constrained_map_df$ 'peatland_potential'==1)
constrained[id]=1
constrained_mask<-quick_mask_raster(constrained, Indonesia_map)

# Replace NA with 0 to indicate non-constrained areas
constrained[is.na(constrained)] <- 0

# Plot the constrained raster
plot(constrained_mask)


slope2= resample(elevation_rc, solar)
temperature2= resample(Temperature_rc, solar)
ind_road_combine=resample(ind_road_combine_fix, solar)
ind_grid_combine= resample(ind_grid_combine_fix, solar)
constrained_rc= resample(constrained_mask, solar)


newdat= c(solar, temperature2, slope2,ind_road_combine,ind_grid_combine,constrained_rc) #combine all three spatial raster datas into one dataset
names(newdat) <- c("solar", "temperature", "slope","road","grid","constrained") 

newdat_df= as.data.frame(newdat, xy=TRUE) #xy=TRUE, will help to export the coordinates of cells
id = which ( newdat_df$slope <= 10 
            & newdat_df$solar> 17000000
            & newdat_df$temperature == 3 
            & newdat_df$road == 2
            & newdat_df$grid == 2
            & constrained== 1)
length (id)/ ncell(solar)

suitability = solar
values(suitability)= NA

#add the suitability layer onto newdata
newdat=c(newdat, suitability)
names(newdat)[7] = "suitability"


#assign matched cells with 1 based on id generated in step 4
newdat$suitability[id] = 1
newdat$suitability[newdat$suitability == 0] <- NA
plot(newdat$suitability, col="lightblue")
newdat$suitability1=round((((.14*newdat_df$slope+.05*newdat_df$temperature+.28*newdat_df$grid+0.08*newdat_df$road+0.45*newdat_df$solar)/(2.78))*100),0)
plot(newdat$suitability1, col="blue")


install.packages("writexl")
library(writexl)

write_xlsx(newdat_df, "output.xlsx")


install.packages("readxl")
Indonesia_map<- st_read("idn_admbnda_adm0_bps_20200401.shp")
# Read the XL files

library(sf)
library(readxl)
library(ggplot2)



# Read the Excel file with region names and coordinates
region_data <- read_excel("Location site 2.xlsx")

region_data_sf <- st_as_sf(region_data, coords = c("x", "y"), crs = 4326)



# Set the tmap mode to plotting
tmap_mode("plot")

# Create the map with yellow fill for the provinces
tm <- tm_shape(Indonesia_map) +
  tm_polygons(col = "yellow", border.col = "grey") + # Fill regions with yellow, borders in grey
  tm_layout(main.title = " 14 Sites selected for SOLAR FARMS in Indonesia", 
            frame = FALSE, 
            bg.color = "lightblue", # Set the background color
            inner.margins = c(0.05, 0.05, 0.05, 0.05))


# Add the region names and symbols to the map
tm <- tm + tm_shape(region_data_sf) +
  tm_symbols(col = "black", size = 0.2) + # Add small black circles for regions
  tm_text(text = "Region Name", size = 0.8, col = "red") # Replace 'name' with the actual column name

# Print the map
print(tm)


# Add the region names and symbols to the map
tm <- tm + tm_shape(region_data_sf) +
  tm_symbols(col = "black", size = 0.2) + # Add small black circles for regions
  tm_text(text ="solar", size = 0.8, col = "red") # Replace 'name' with the actual column name

# Print the map
print(tm)



#FINANCIAL ANALYSIS

#CALCULATIONS:

#CapEx capacity installation:1.16M USD/MW installed power; CapEx Network connection inland: 590 USD/MW.km

calc_NPV <- function(annual_revenue, i=0.05, lifetime_yrs, CAPEX, OPEX=0){
  costs_op <- rep(OPEX, lifetime_yrs) #operating cost
  revenue <- rep(annual_revenue, lifetime_yrs) 
  t <- seq(1, lifetime_yrs, 1) #output: 1, 2, 3, ...25
  
  NPV <- sum( (revenue - costs_op)/(1 + i)**t ) - CAPEX
  return(round(NPV, 0))
}

#ACEH
npv= calc_NPV(annual_revenue = 902000000,lifetime_yrs=25, CAPEX=1162000000  )
npv
ifelse(npv>0, "Support","object" )

#Riau-sumatra
npv= calc_NPV(annual_revenue = 902.2,lifetime_yrs=25, CAPEX=1162  )
ifelse(npv>0, "Support","object" )


#Sumatera Barat
npv= calc_NPV(annual_revenue = 2706.84,lifetime_yrs=25, CAPEX=3488.85  )
ifelse(npv>0, "Support","obeject" )


#Jambi
npv= calc_NPV(annual_revenue = 14000000,lifetime_yrs=25, CAPEX=150000000  )
ifelse(npv>0, "Support","obeject" )

#Lampung
npv= calc_NPV(annual_revenue = 902.2,lifetime_yrs=25, CAPEX=1162  )
ifelse(npv>0, "Support","obeject" )


#Kalimantan Utara
npv= calc_NPV(annual_revenue = 902.2,lifetime_yrs=25, CAPEX=1162  )
ifelse(npv>0, "Support","obeject" )

#North Kalimantan
npv= calc_NPV(annual_revenue = 902.2,lifetime_yrs=25, CAPEX=1162  )
ifelse(npv>0, "Support","obeject" )

#Kalimantan Selatan
npv= calc_NPV(annual_revenue = 902.2,lifetime_yrs=25, CAPEX=1162  )
ifelse(npv>0, "Support","obeject" )

#Papua
npv= calc_NPV(annual_revenue = 902.2,lifetime_yrs=25, CAPEX=1162  )
ifelse(npv>0, "Support","obeject" )

#West Java
npv= calc_NPV(annual_revenue = 902.2,lifetime_yrs=25, CAPEX=1162  )
ifelse(npv>0, "Support","obeject" )

#Sulawesi Selatan
npv= calc_NPV(annual_revenue = 902.2,lifetime_yrs=25, CAPEX=1162  )
ifelse(npv>0, "Support","obeject" )

#Nusa T Barat
npv= calc_NPV(annual_revenue = 902.2,lifetime_yrs=25, CAPEX=1162  )
ifelse(npv>0, "Support","obeject" )

#West Sulawesi
npv= calc_NPV(annual_revenue = 902.2,lifetime_yrs=25, CAPEX=1162  )
ifelse(npv>0, "Support","obeject" )


#Sulawesi Selatan
npv= calc_NPV(annual_revenue = 902.2,lifetime_yrs=25, CAPEX=1162  )
ifelse(npv>0, "Support","obeject" )


#LCOE

#ACEH

Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

Life_span_generation_kWH
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

annual= 8760000000 #kwh
lsg = Life_span_generation_kWH(yearly_generation_kWH=annual)
lsg

lcoe = LCOE(NPV=npv, lsg)
lcoe 
