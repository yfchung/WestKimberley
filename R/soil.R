
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

library(terra)
library(sf)
library(tidyverse)
library(furrr)
library(qs2)
library(tidyterra)

# Load study area
StudyArea_box <- st_read(file.path("output", "data", "StudyArea_box.gpkg"))  %>% 
    st_transform(crs = st_crs(4326))
StudyArea_box_vect <- vect(StudyArea_box) %>% terra::wrap()



# Get SLGA Soil Data
# SLGA APIKEY
# usethis::edit_r_environ()
TERN_APIkey <- Sys.getenv("TERN_APIkey") # assign your API key to TERN_APIkey or set TERN_APIkey in .Renviron
if (TERN_APIkey == "") {
  stop("API key not found. Please assign TERN_APIkey or set TERN_APIkey in .Renviron")
}
# https://data.tern.org.au/model-derived/slga/NationalMaps/SoilAndLandscapeGrid/AVP/
apikey <- paste0('apikey:', TERN_APIkey)

SLGA_Depth <- c("000_005", "005_015", "015_030" #, # Top soil layers only 
                # "030_060", "060_100", "100_200"
                )
SLGA_Fname <- c(paste0("AVP/v1/AVP_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20220826.tif"),
                paste0("AWC/V2/AWC_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20220826.tif"),
                paste0("BDW/BDW_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20230607.tif"),
                paste0("CEC/CEC_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20220826.tif"),
                paste0("CEC/CEC_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20220826.tif"),
                paste0("CEC/CEC_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20220826.tif"),
                paste0("CEC/CEC_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20220826.tif"),
                paste0("CEC/CEC_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20220826.tif"),
                paste0("NTO/NTO_", SLGA_Depth, "_EV_N_P_AU_NAT_C_20231101.tif"),
                paste0("PHC/PHC_", SLGA_Depth, "_EV_N_P_AU_NAT_C_20210913.tif"),
                paste0("PHW/PHW_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20220520.tif"),
                paste0("SOC/SOC_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20220727_30m.tif"),
                paste0("PHW/PHW_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20220520.tif"),
                paste0("PHW/PHW_", SLGA_Depth, "_EV_N_P_AU_TRN_N_20220520.tif"),

                "DES/DES_000_200_EV_N_P_AU_TRN_C_20190901.tif")

plan(multisession, workers = 4)

x <- SLGA_Fname[1]

SOIL_DIR <- file.path("input", "SoilData", "Raw_download", "SLGA")
if(!dir.exists(SOIL_DIR)){dir.create(SOIL_DIR, recursive = TRUE)}

future_map(SLGA_Fname, function (x){
  
 R <-  rast(paste0('/vsicurl/https://',apikey,'@data.tern.org.au/model-derived/slga/NationalMaps/SoilAndLandscapeGrid/', x))
 plot(R)
 R2 <- terra::crop(R, y = terra::unwrap(StudyArea_box_vect), snap = "out") %>%  
                writeRaster(filename = file.path(SOIL_DIR, substr(x, start = 8, stop = nchar(x))))
                plot(R2)
})
plan(sequential)
proc.time() - ptm
