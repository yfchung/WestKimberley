library(sf)
library(geojsonsf)
library(httr2)
library(jsonlite)
library(tidyverse)

INPUT_DIR <- file.path("input")
OUTPUT_DIR <- file.path("output")

AUS <- st_read(file.path(INPUT_DIR, "STE_2021_AUST_SHP_GDA2020", "STE_2021_AUST_GDA2020.shp"))
AUS_box <- (st_bbox(AUS)+c(0, -5, 0, 5)) %>% st_as_sfc() %>% st_make_valid() %>% st_as_sf()

# Native language ----
Native_Land_Digital_API <- Sys.getenv("Native_Land_Digital_API")

# Convert to GeoJSON list object for the POST body
AUS_GeoJSON_txt <- sf_geojson(AUS_box)
AUS_GeoJSON <- fromJSON(AUS_GeoJSON_txt, simplifyVector = FALSE)
str(AUS_GeoJSON)
??geojson 

AUS_GeoJSON <- list(
  type = "FeatureCollection",
  features = list(
    list(
      type = "Feature",
      properties = list(),
      geometry = AUS_GeoJSON
    )
  )
)

# Function to query Native Land
GET_NATIVE_LAND <- function(MAPS, GeoJSON_Poly, APIkey, 
                            Output_TYPE = "geojson", 
                            Output_FPATH){
    BODY <- list(
        key = APIkey,
        maps = MAPS,
        polygon_geojson = GeoJSON_Poly
    )

    RESP <- request("https://native-land.ca/api/index.php") |>
        req_method("POST") |>
        req_body_json(BODY, auto_unbox = TRUE) |>
        req_perform()

    TXT <- resp_body_string(RESP)
    OBJ <- jsonlite::fromJSON(TXT, simplifyVector = FALSE)
    OBJ_FC <- list(type = "FeatureCollection", features = OBJ)
    TXT_FC <- jsonlite::toJSON(OBJ_FC, auto_unbox = TRUE)

    if(Output_TYPE == "geojson"){
        writeLines(TXT_FC, Output_FPATH)
        return(cat("GeoJSON saved to:", Output_FPATH, "\n"))
    } else if(Output_TYPE == "gpkg"){
        TEMP_OUTPUT <- tempfile(fileext = ".geojson")
        writeLines(TXT_FC, TEMP_OUTPUT)
        st_read(TEMP_OUTPUT) %>% st_write(Output_FPATH)
        unlink(TEMP_OUTPUT)
        return(cat("GeoPackage saved to:", Output_FPATH, "\n"))
    } else {
        stop("Unsupported Output_TYPE. Use 'geojson' or 'gpkg'. \n")
    }
}

GET_NATIVE_LAND(
    MAPS = "languages", 
    GeoJSON_Poly = AUS_GeoJSON, 
    APIkey = Native_Land_Digital_API,
    Output_TYPE = "geojson",
    Output_FPATH = file.path(OUTPUT_DIR, "data", "Native_Language.geojson")
)

GET_NATIVE_LAND(
    MAPS = "territories", 
    GeoJSON_Poly = AUS_GeoJSON,
    APIkey = Native_Land_Digital_API,
    Output_TYPE = "geojson",
    Output_FPATH = file.path(OUTPUT_DIR, "data", "Native_Territories.geojson")
)

GET_NATIVE_LAND(
    MAPS = "treaties", 
    GeoJSON_Poly = AUS_GeoJSON,
    APIkey = Native_Land_Digital_API,
    Output_TYPE = "geojson",
    Output_FPATH = file.path(OUTPUT_DIR, "data", "Native_Treaties.geojson")
)

AUS_LANGUAGES <- st_read(file.path(OUTPUT_DIR, "data", "Native_Language.geojson"))

AUS_TERRITORIES <- st_read(file.path(OUTPUT_DIR, "data", "Native_Territories.geojson"))

AUS_TREATIES <- st_read(file.path(OUTPUT_DIR, "data", "Native_Treaties.geojson"))

ggplot() +
    geom_sf(data = AUS_LANGUAGES, aes(fill = color), alpha = 0.5, colour = NA) +
    scale_fill_identity() +
    # geom_sf(data = AUS_box, fill = NA, color = "black", lwd = 0.5) +
    theme_minimal() +
    labs(title = "Native Languages in Australia",
         fill = "Language Name") +
    theme(legend.position = "none")

ggplot() +
    geom_sf(data = AUS_TERRITORIES, aes(fill = color), alpha = 0.5, colour = NA) +
    scale_fill_identity() +
    # geom_sf(data = AUS_box, fill = NA, color = "black", lwd = 0.5) +
    theme_minimal() +
    labs(title = "Native Territories in Australia",
         fill = "Territory Name") +
    theme(legend.position = "none")

ggplot() +
    geom_sf(data = AUS_TREATIES, aes(fill = color), alpha = 0.5, colour = NA) +
    scale_fill_identity() +
    # geom_sf(data = AUS_box, fill = NA, color = "black", lwd = 0.5) +
    theme_minimal() +
    labs(title = "Native Treaties in Australia",
         fill = "Treaty Name") +
    theme(legend.position = "none")
