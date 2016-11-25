library(tidyverse)
library(ggplot2)
library(scales)
library(ggmap)
library(gridExtra)
library(grid)
library(cowplot)
library(stringr)
library(rgdal)
library(maptools)
library(Cairo)
library(rgeos)

degree_format <- unit_format(unit = "°", sep = "")
axis_x_format <- scale_x_continuous("Longitude", labels = degree_format) 
axis_y_format <- scale_y_continuous("Latitude", labels = degree_format)


rdsFiles <- list.files("data/", pattern = "adm0\\.rds", full.names = TRUE)
tdf_shp <- readRDS("data/ARG_adm1.rds")
tdf_shp <- tdf_shp[tdf_shp$NAME_1 == "Tierra del Fuego",]

#regiones_ecologicas <- readOGR("C:\\Users\\lucia\\Trabajo\\GIS DataBase\\shapeFiles/Regiones Ecologicas",
#                               "regiones ecologicas")

#regiones_ecologicas <- spTransform(regiones_ecologicas, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

tdf_box <- as(raster::extent(bbox(tdf_shp)), "SpatialPolygons")
proj4string(tdf_box) <-  proj4string(tdf_shp)
tdf_box <- fortify(tdf_box)

rdsObjects <- lapply(rdsFiles, function(x){
  shp <- readRDS(x)
  shp <- spChFIDs(shp, paste0(x, '_', sapply(slot(shp, "polygons"), slot, "ID")))
  shp
}
)

south_region <- do.call(rbind, rdsObjects)
south_region_simple <- gSimplify(south_region, 0.1, TRUE)
south_region_simple <- fortify(south_region_simple, region = "NAME_ENGLISH")

in_theme <- theme(panel.background = element_rect(fill = "white"))

CP <- as(raster::extent(c(-68.6093321157877, -63.7411235166103, -55.057328903778, 
                          -52.6590023588504)), "SpatialPolygons")
proj4string(CP) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
tdf_region <- gIntersection(south_region, CP, byid = TRUE)

p <-  ggplot() +
  geom_polygon(data = fortify(tdf_region),
               aes(x = long, y = lat, group = group), 
               color = "black", size = 0.25, fill = "grey80")
p <- p + coord_map() 
p <- p +
  axis_y_format + axis_x_format + in_theme 

p <- p + theme(panel.grid = element_blank(), panel.border = element_blank())

p3 <-  ggplot() +
  geom_polygon(data = south_region_simple,
               aes(x = long, y = lat, group = group), 
               color = "black", size = 0.25, fill = "grey80")
p3 <- p3 + coord_map(ylim = c(-20, -56), xlim = c(-50, -77))
p3 <- p3 + theme_inset() + in_theme +
  geom_polygon(data = tdf_box, aes(x = long, y = lat),
               colour = "black", fill = NA, size = 1)

cuerpos_de_agua <- readOGR("data/Mis_lugares.kml", "Mis lugares")
cuerpos_de_agua@data <- cuerpos_de_agua@data %>%  mutate(location = str_sub(Name, 1L, 1L))
cnt <- lapply(unique(cuerpos_de_agua$location), 
function(x) gCentroid(cuerpos_de_agua[cuerpos_de_agua$location == x,])) %>% 
  do.call(rbind, .)

cnt <- as.data.frame(coordinates(cnt))

p + geom_point(data = cnt, aes(x, y), size = 4)

