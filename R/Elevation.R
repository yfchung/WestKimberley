
library(sf)
library(readxl)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggspatial)
library(ggpubr)
library(shadowtext)
library(RColorBrewer)
library(ggrepel)
library(patchwork)
library(xml2)
library(lwgeom)

INPUT_DIR <- file.path("input")
INPUT_SPA_DIR <- file.path("input/Spatial_data")
OUTPUT_DIR <- file.path("output")


# General base layers ----
## State boundary
STE <- st_read(file.path(INPUT_SPA_DIR, "STE_2021_AUST_SHP_GDA2020", "STE_2021_AUST_GDA2020.shp"))

STE_WA <- STE %>% filter(STE_NAME21 == "Western Australia") %>% st_make_valid() %>% st_as_sf()

# Define BBOX
BBOX <- st_bbox(c(xmin = 119.2661, xmax = 129.1015, ymin = -21.33202, ymax = -13.58869), crs = st_crs(STE)) %>% 
  st_as_sfc() %>% st_make_valid()

# Study area boundary
# 
StudyArea_SF <- st_read(file.path(OUTPUT_DIR, "data", "Fitzroy2Coast_Aus_SEA_dsvl.gpkg"))
StudyArea_SF_10kBuff <- StudyArea_SF %>%
    st_transform(crs = st_crs("EPSG:7851")) %>%
    st_buffer(10000) %>% st_make_valid() %>% st_transform(crs = st_crs(StudyArea_SF))


# This part is to crop the raw 1 arc second DEM to study area box. (only run once)
# c:\Users\uqychun7\Downloads\69816\srtm-1sec-dem-v1-COG.tif
# DEM_RAW <- rast("c:/Users/uqychun7/Downloads/69816/srtm-1sec-dem-v1-COG.tif")

# StudyArea_SF_5kBuff_WGS84 <- StudyArea_SF_5kBuff %>% st_transform(crs = st_crs(DEM_RAW))

DEM_SA_5kBuff_WGS84 <- crop(DEM_RAW, vect(StudyArea_SF_5kBuff_WGS84), snap = "out", mask = TRUE, 
    filename = file.path(INPUT_SPA_DIR, "DEM_SA_5kBuff_WGS84.tif"), overwrite = TRUE)

DEM_SA <- project(DEM_SA_5kBuff_WGS84, vect(StudyArea_SF), threads=TRUE, method = "bilinear") %>% 
    crop(vect(StudyArea_SF), snap = "out", mask = TRUE, filename = file.path(INPUT_SPA_DIR, "DEM_SA.tif"), overwrite = TRUE)
plot(DEM_SA)
ggplot()+ 
    geom_spatraster(data = DEM_SA) + 
    scale_fill_viridis_c(option = "magma", na.value = NA) + 
    geom_sf(data = StudyArea_SF, fill = NA, color = "black")

# Calculate TRI
TRI <- terrain(DEM_SA_5kBuff_WGS84, v = "TRI", neighbors = 8) %>% 
    project(vect(StudyArea_SF), threads=TRUE, method = "bilinear") %>% 
    crop(vect(StudyArea_SF), snap = "out", mask = TRUE, 
         filename = file.path(INPUT_SPA_DIR, "TRI.tif"), overwrite = TRUE)
TRI <- rast(file.path(INPUT_SPA_DIR, "TRI.tif"))
summary(TRI)
TRI_cat <- classify(TRI, rcl = cbind(c(0, 31, 110), c(31, 110, Inf), c(1, 2, 3)), 
                    include.lowest = TRUE, right = FALSE, 
                    filename = file.path(INPUT_SPA_DIR, "TRI_cat.tif"), overwrite = TRUE)

