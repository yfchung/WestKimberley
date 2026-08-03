library(terra)
library(sf)
library(tidyverse)

# Create a study site boundary

Catchment_Fitzroy <- st_read(file.path("input", "Hydrographic_Catchments_Catchments_DWER_028_WA_GDA2020_Public_Geopackage", "Hydrographic_Catchments_Catchments_DWER_028_WA_GDA2020_Public.gpkg"), 
    layer = "Hydrographic_Catchments_Catchments_DWER_028") %>% 
    filter(basin_name == "Fitzroy River")


National_Heritage_Aus <- st_read(file.path("input", "National_Heritage_List_Australia.gdb"), layer = "national_list") %>% filter(STATE == "WA")
National_Heritage_WK <- st_read(file.path("input", "National_Heritage_List_Australia.gdb"), layer = "national_list") %>% filter(NAME == "The West Kimberley") %>% 
    st_transform(crs = st_crs(Catchment_Fitzroy))


st_bbox(Catchment_Fitzroy)
st_bbox(National_Heritage_WK)
StudyArea_box <- st_bbox(c(xmin = min(st_bbox(Catchment_Fitzroy)[1], st_bbox(National_Heritage_WK)[1]),
                           xmax = max(st_bbox(Catchment_Fitzroy)[3], st_bbox(National_Heritage_WK)[3]),
                           ymin = min(st_bbox(Catchment_Fitzroy)[2], st_bbox(National_Heritage_WK)[2]),
                           ymax = max(st_bbox(Catchment_Fitzroy)[4], st_bbox(National_Heritage_WK)[4])),
                    crs = st_crs(Catchment_Fitzroy)) %>% 
    st_as_sfc()
plot(StudyArea_box)
st_write(StudyArea_box, file.path("output", "data", "StudyArea_box.gpkg"), append = TRUE)



