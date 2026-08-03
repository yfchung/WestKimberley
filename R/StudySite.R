library(sf)
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
OUTPUT_DIR <- file.path("output")

# General base layers ----
## State boundary
STE <- st_read(file.path(INPUT_DIR, "STE_2021_AUST_SHP_GDA2020", "STE_2021_AUST_GDA2020.shp"))

STE_WA <- STE %>% filter(STE_NAME21 == "Western Australia") %>% st_make_valid() %>% st_as_sf()

# Define BBOX
BBOX <- st_bbox(c(xmin = 119.2661, xmax = 129.1015, ymin = -21.33202, ymax = -13.58869), crs = st_crs(STE)) %>% 
  st_as_sfc() %>% st_make_valid()

## national heritage ----
National_Heritage_WK <- st_read(file.path(INPUT_DIR, "National_Heritage_List_Australia.gdb"), layer = "national_list") %>% 
  filter(NAME == "The West Kimberley") %>% 
  st_transform(crs = st_crs(STE)) %>% select(NAME, SHAPE) %>% st_make_valid() %>% st_as_sf()

### Clipped to Terrestrial WA boundary
National_Heritage_WK_WA <- st_intersection(National_Heritage_WK, STE_WA) %>% st_make_valid() %>% st_as_sf()

# National_Heritage_ConvexH <- st_convex_hull(National_Heritage_WK) %>% st_make_valid() %>% st_as_sf()
# Basin_Fitzroy_uNational_Heritage_WK <- st_union(Basin_Fitzroy_grt_dsvl, National_Heritage_WK) %>% st_make_valid() %>% st_union()
# Basin_Fitzroy_uNational_Heritage_WK_WA <- st_intersection(Basin_Fitzroy_uNational_Heritage_WK, STE_WA %>% st_make_valid() %>% st_union()) %>% st_as_sf()

## Legislated Land ----
PAs <- st_read(file.path(INPUT_DIR, "Legislated_Lands_and_Waters_DBCA_011_WA_GDA2020_Public_Geopackage", "Legislated_Lands_and_Waters_DBCA_011_WA_GDA2020_Public.gpkg"), 
    layer = "Legislated_Lands_and_Waters_DBCA_011") %>% mutate(IDX = 1:n())

PAs_WK_IDX <- st_intersection(PAs, st_bbox(National_Heritage_WK_WA) %>% st_as_sfc() %>% st_make_valid()) %>% 
    st_drop_geometry() %>% pull(IDX)

PAs_WK_WA <- PAs %>% filter(IDX %in% PAs_WK_IDX) %>% 
    st_intersection(STE_WA %>% select(STE_NAME21)) %>% st_make_valid() %>% st_as_sf() %>% 
    filter(leg_name_status %in% c("Gazetted", "State Approved", "Official")) %>% 
    group_by(leg_name) %>% 
    summarise(SHAPE = st_union(SHAPE)) %>% ungroup() %>% 
    mutate(x = st_coordinates(st_centroid(SHAPE))[,1], y = st_coordinates(st_centroid(SHAPE))[,2],
           leg_name = str_wrap(leg_name, width = 20))

CAPAD_Land <- st_read(file.path(INPUT_DIR, "Protected_Areas_(CAPAD)", "Protected_Areas_(CAPAD).shp")) %>% 
    filter(STATE == "WA") %>% st_make_valid() %>% st_as_sf() %>% filter(st_is_valid(.) == TRUE)
sum(!st_is_valid(CAPAD_Land))
CAPAD_Land_WK_IDX <- st_intersection(CAPAD_Land, BBOX %>% st_transform(st_crs(CAPAD_Land))) %>% 
    st_drop_geometry() %>% pull(OBJECTID)

CAPAD_Land_WK <- CAPAD_Land %>% filter(OBJECTID %in% CAPAD_Land_WK_IDX)
CAPAD_Land_WK %>% st_drop_geometry() %>% head()
CAPAD_SEA <- st_read(file.path(INPUT_DIR, "Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine",
                      "Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp")) %>% 
                st_make_valid() %>% st_as_sf() %>% filter(st_is_valid(.))

CAPAD_SEA_WK_IDX <- st_intersection(CAPAD_SEA, BBOX %>% st_transform(st_crs(CAPAD_SEA))) %>% 
    st_drop_geometry() %>% pull(OBJECTID)

CAPAD_SEA_WK <- CAPAD_SEA %>% filter(OBJECTID %in% CAPAD_SEA_WK_IDX)

CAPAD_WK <- bind_rows(
  CAPAD_Land_WK %>% select(OBJECTID, NAME, IUCN, EPBC) %>% mutate(medium = "Terrestrial"),
  CAPAD_SEA_WK %>% select(OBJECTID, NAME, IUCN, EPBC) %>% mutate(medium = "Marine")
) %>% mutate(Type = paste0(EPBC, " PA (", medium, ")")) %>% st_make_valid() %>% st_as_sf() %>% 
  mutate(Type = factor(Type, levels = c("State PA (Terrestrial)", "Indigenous PA (Terrestrial)", "Private PA (Terrestrial)",
                                        "Commonwealth PA (Marine)", "State PA (Marine)", "Indigenous PA (Marine)" )))



# Basins ----
Basin <- st_read(file.path(INPUT_DIR, "Hydrographic_Catchments_Basins_DWER_027_WA_GDA2020_Public_Geopackage", "Hydrographic_Catchments_Basins_DWER_027_WA_GDA2020_Public.gpkg"), 
    layer = "Hydrographic_Catchments_Basins_DWER_027")

Basin_Fitzroy <- Basin %>% filter(basin_name == "Fitzroy River")

# Neighbour_basin <- Basin %>% filter(lengths(st_touches(., Basin_Fitzroy))>0) 

# Basin_Fitzroy_grt <- Basin %>% dplyr::filter(basin_name %in% c("Fitzroy River", "Isdell River", "Cape Leveque Coast", "Lennard River")) %>% 
#     mutate(Area_Ha = as.numeric(st_area(.))/10000)

Basin_Fitzroy_WK_Bno <- st_intersection(Basin, National_Heritage_WK) %>% pull(basin_no)  %>% unique()

Basin_Fitzroy_WK <- Basin %>% filter(basin_no %in% c(Basin_Fitzroy_WK_Bno, 810)) %>% 
    mutate(Fitzroy_Neighbour = ifelse(lengths(st_touches(., Basin_Fitzroy))>0, 1, 0),
           StudySite = case_when(basin_name == "Fitzroy River" ~ "Fitzroy River basin",
                                 Fitzroy_Neighbour == 1 ~ "Fitzroy neighbouring basins", 
                                 .default = "Other West Kimberley basins"))
unique(Basin_Fitzroy_WK$basin_name)
Basin_Fitzroy_neighbour_dsvl <- Basin_Fitzroy_WK %>% 
    filter(StudySite %in% c("Fitzroy River basin", "Fitzroy neighbouring basins")) %>%
    st_intersection(STE_WA %>% select(STE_NAME21)) %>%
    st_union() %>% st_as_sf() %>% st_make_valid() %>% mutate(SHAPE = x) %>% 
    st_drop_geometry() %>% select(SHAPE) %>% st_as_sf() %>% st_make_valid() 

Basin_Fitzroy_WK_dsvl <-  st_intersection(Basin_Fitzroy_WK, STE_WA %>% select(STE_NAME21)) %>% 
    st_union() %>% st_as_sf() %>% st_make_valid() %>% mutate(SHAPE = x) %>% 
    st_drop_geometry() %>% select(SHAPE) %>% st_as_sf() %>% st_make_valid()

Basin_Fitzroy_boundaries <- bind_rows(
  Basin_Fitzroy %>% select() %>% mutate(Boundary = "Fitzroy River basin"),
  Basin_Fitzroy_neighbour_dsvl %>% select() %>% mutate(Boundary = "Fitzroy neighbouring basins"),
  Basin_Fitzroy_WK_dsvl %>% select() %>% mutate(Boundary = "West Kimberley basins")
) %>% st_boundary()

Basin_Fitzroy2Coast <- Basin %>% 
  filter(basin_name %in% c("Cape Leveque Coast", "Fitzroy River", 
                           "Isdell River", "Lennard River", 
                           "Prince Regent River"))

Basin_Fitzroy2Coast2 <- Basin %>% 
  filter(basin_name %in% c("Cape Leveque Coast", "Fitzroy River", 
                           "Isdell River", "Lennard River")) %>% 
  mutate(Name = str_replace(basin_name, " Coast| River", ""),
         Name = paste0("Basin: ", Name))

Basin_Fitzroy2Coast2_dsvl <- Basin_Fitzroy2Coast2 %>% st_union() %>% st_as_sf() %>% st_make_valid()

## Bioregions----
IBRAsub <- st_read(file.path(INPUT_DIR, "Interim_Biogeographic_Regionalisation_for_Australia_(IBRA)_Version_7.1_(Subregions).gdb"), layer = "IBRA71_subregions") %>% 
    st_transform(crs = st_crs(STE)) %>% st_make_valid() 

IBRAsub %>% st_drop_geometry() %>% pull(REG_NAME_7) %>% unique() %>% sort()
IBRA_WK_vec <- st_intersection(IBRAsub, National_Heritage_WK_WA) %>% st_drop_geometry() %>% pull(REG_NAME_7) %>% unique()

IBRA_WK <- IBRAsub %>% 
  filter(REG_NAME_7 %in% IBRA_WK_vec) %>% 
  mutate(Area_Ha = as.numeric(st_area(.))/10000) %>% 
  select(REG_NAME_7, REG_CODE_7, SUB_NAME_7, SUB_CODE_7, Area_Ha)

IBRA_WK_WA <- st_intersection(IBRA_WK, STE_WA) %>% 
  st_make_valid()%>% select(REG_NAME_7, REG_CODE_7, SUB_NAME_7, SUB_CODE_7, Area_Ha)

IBRA_Fitzroy_WA_vec <- IBRA_WK_WA %>% 
  group_by(REG_CODE_7) %>%
  summarise(SHAPE = st_union(SHAPE)) %>% 
  st_intersection(Basin_Fitzroy) %>% 
  mutate(Area_Ha = as.numeric(st_area(.))/10000) %>%
  st_drop_geometry() %>%
  arrange(desc(Area_Ha)) %>%
  slice_max(Area_Ha, n=3) %>% 
  pull(REG_CODE_7) %>% unique() 

IBRA_Fitzroy_WA <- IBRA_WK_WA %>% 
  filter(REG_CODE_7 %in% IBRA_Fitzroy_WA_vec) %>% 
  st_make_valid() %>% st_as_sf()

IBRA_WK_WA_dsvl <- st_union(IBRA_WK_WA) %>% st_as_sf() %>% st_make_valid()
IBRA_Fitzroy_WA_dsvl <- st_union(IBRA_Fitzroy_WA) %>% st_as_sf() %>% st_make_valid()

IBRA_WK_WA_box <- st_bbox(IBRA_WK_WA) %>% st_as_sfc() %>% st_make_valid()
IBRA_WK_WA_box_buff <- st_bbox(c(xmin = as.numeric(st_bbox(IBRA_WK_WA)[1]) - 0.2,
                                xmax = as.numeric(st_bbox(IBRA_WK_WA)[3]) + 0.2,
                                ymin = as.numeric(st_bbox(IBRA_WK_WA)[2]) - 0.2,
                                ymax = as.numeric(st_bbox(IBRA_WK_WA)[4]) + 0.2),
                         crs = st_crs(IBRA_WK_WA)) %>% 
  st_as_sfc() %>% st_make_valid()


# ABS Indigenous areas ----
## ABS indigenous locations align quite well with LGA, and SA1, SA2 regions
ABS_ILOC <- st_read(file.path(INPUT_DIR, "ASGS_Ed3_2021_Indigenous_Structure_GDA2020_GPKG", "ASGS_Ed3_2021_Indigenous_Structure_GDA2020.gpkg"), 
                    layer = "ILOC_2021_AUST_GDA2020")

# ABS_ILOC_WK_ILOCcode <- st_intersection(ABS_ILOC, IBRA_WK_WA_box) %>% st_drop_geometry() %>% 
#                           mutate(ILOC_CODE_2021 = as.integer(ILOC_CODE_2021)) %>% 
#                           # filter for 6digit ILOC code, starts with 5
#                           filter(STATE_CODE_2021 == 5) %>%
#                           pull(ILOC_CODE_2021) %>% unique() %>% as.character()
# ABS_ILOC_WK <- ABS_ILOC %>% filter(ILOC_CODE_2021 %in% ABS_ILOC_WK_ILOCcode)

ABS_ILOC_WK_IAREcode <- ABS_ILOC %>% 
                          group_by(IARE_CODE_2021)  %>% 
                          summarise(geom = st_union(geom)) %>% 
                          st_make_valid() %>% st_as_sf()  %>% 
                          st_intersection(Basin_Fitzroy) %>% 
                          mutate(AREA_Ha = as.numeric(st_area(.))/10000) %>%
                          st_drop_geometry() %>%
                          arrange(desc(AREA_Ha)) %>%
                          slice_max(AREA_Ha, n=4) %>%
                          pull(IARE_CODE_2021) %>% unique() %>% as.character()
ABS_IARE_WK <- ABS_ILOC %>% filter(IARE_CODE_2021 %in% ABS_ILOC_WK_IAREcode)

ABS_ILOC_WK_IREGcode <- st_intersection(ABS_ILOC, Basin_Fitzroy) %>% st_drop_geometry() %>% 
                          mutate(IREG_CODE_2021 = as.integer(IREG_CODE_2021)) %>% 
                          # filter for 6digit IREG code, starts with 5
                          filter(STATE_CODE_2021 == 5) %>%
                          pull(IREG_CODE_2021) %>% unique() %>% as.character()
ABS_IREG_WK <- ABS_ILOC %>% filter(IREG_CODE_2021 %in% ABS_ILOC_WK_IREGcode)

ABS_IARE_WK_dsvl <- st_union(ABS_IARE_WK) %>% st_as_sf() %>% st_make_valid()
ABS_IREG_WK_dsvl <- st_union(ABS_IREG_WK) %>% st_as_sf() %>% st_make_valid()

ABS_IREG_WK %>% st_drop_geometry() %>% pull(IREG_NAME_2021) %>% unique()

ABS_IREG_WK_dsvl <- st_union(ABS_IREG_WK %>% filter(IREG_NAME_2021 != "South Hedland")) %>% st_as_sf() %>% st_make_valid()


BBOX <- st_bbox(c(xmin = min(as.numeric(st_bbox(IBRA_WK_WA_dsvl)[1]), as.numeric(st_bbox(ABS_IREG_WK_dsvl)[1])) - 0.1,
                  xmax = max(as.numeric(st_bbox(IBRA_WK_WA_dsvl)[3]), as.numeric(st_bbox(ABS_IREG_WK_dsvl)[3])) + 0.1,
                  ymin = min(as.numeric(st_bbox(IBRA_WK_WA_dsvl)[2]), as.numeric(st_bbox(ABS_IREG_WK_dsvl)[2])) - 0.1,
                  ymax = max(as.numeric(st_bbox(IBRA_WK_WA_dsvl)[4]), as.numeric(st_bbox(ABS_IREG_WK_dsvl)[4])) + 0.1),
            crs = st_crs(IBRA_WK_WA)) %>% 
  st_as_sfc() %>% st_make_valid()


# Native title ----
#"C:\Users\uqychun7\Documents\Data\WestKimberley\input\Native_Title_Determination_LGATE_066_WA_GDA2020_Public_Geopackage\Native_Title_Determination_LGATE_066_WA_GDA2020_Public.gpkg"
st_layers(file.path(INPUT_DIR, "Native_Title_Determination_LGATE_066_WA_GDA2020_Public_Geopackage", "Native_Title_Determination_LGATE_066_WA_GDA2020_Public.gpkg"))
# "C:\Users\uqychun7\Documents\Data\WestKimberley\input\Native_Title_ILUA_LGATE_067_WA_GDA2020_Public_Geopackage\Native_Title_ILUA_LGATE_067_WA_GDA2020_Public.gpkg"
st_layers(file.path(INPUT_DIR, "Native_Title_ILUA_LGATE_067_WA_GDA2020_Public_Geopackage", "Native_Title_ILUA_LGATE_067_WA_GDA2020_Public.gpkg"))

NT_Detm <- st_read(file.path(INPUT_DIR, "Native_Title_Determination_LGATE_066_WA_GDA2020_Public_Geopackage", "Native_Title_Determination_LGATE_066_WA_GDA2020_Public.gpkg"), 
    layer = "Native_Title_Determination_LGATE_066") %>% st_make_valid() %>% st_as_sf() %>% mutate(IDX = 1:n())
NT_ILUA <- st_read(file.path(INPUT_DIR, "Native_Title_ILUA_LGATE_067_WA_GDA2020_Public_Geopackage", "Native_Title_ILUA_LGATE_067_WA_GDA2020_Public.gpkg"), 
    layer = "Native_Title_ILUA_LGATE_067") %>% st_make_valid() %>% st_as_sf() %>% mutate(IDX = 1:n())

NT_Detm_WK_IDX <- st_intersection(NT_Detm, BBOX) %>% st_drop_geometry() %>% pull(IDX) %>% unique()
NT_ILUA_WK_IDX <- st_intersection(NT_ILUA, BBOX) %>% st_drop_geometry() %>% pull(IDX) %>% unique()

NT_Detm_WK <- NT_Detm %>% filter(IDX %in% NT_Detm_WK_IDX) %>% st_make_valid() %>% st_as_sf()
NT_ILUA_WK <- NT_ILUA %>% filter(IDX %in% NT_ILUA_WK_IDX) %>% st_make_valid() %>% st_as_sf()

NT_Detm_Fitzroy_IDX <- st_intersection(NT_Detm, Basin_Fitzroy) %>% st_drop_geometry() %>% pull(IDX) %>% unique()
NT_ILUA_Fitzroy_IDX <- st_intersection(NT_ILUA, Basin_Fitzroy) %>% st_drop_geometry() %>% pull(IDX) %>% unique()

NT_Detm_Fitzroy <- NT_Detm %>% filter(IDX %in% NT_Detm_Fitzroy_IDX) %>% st_make_valid() %>% st_as_sf() %>% 
    mutate(x = st_coordinates(st_centroid(SHAPE))[,1], y = st_coordinates(st_centroid(SHAPE))[,2])
NT_ILUA_Fitzroy <- NT_ILUA %>% filter(IDX %in% NT_ILUA_Fitzroy_IDX) %>% st_make_valid() %>% st_as_sf()
nrow(NT_Detm_WK); nrow(NT_ILUA_WK); nrow(NT_Detm_Fitzroy); nrow(NT_ILUA_Fitzroy)

NT_Detm_Fitzroy_dsvl2 <- NT_Detm_Fitzroy %>% 
    filter(!IDX %in% c(150, 9, 68, 46, 94)) %>% 
    st_union() %>%
    st_intersection(STE_WA) %>% st_make_valid() %>% st_as_sf() %>% 
    nngeo::st_remove_holes() %>% st_make_valid() %>% st_as_sf()  %>% 
    st_cast("POLYGON") %>% st_make_valid() %>% st_as_sf() %>%
    mutate(AREA_HA = as.numeric(st_area(.))/10000) %>% filter(AREA_HA > 10000) %>%
    st_union() %>% st_make_valid() %>% st_as_sf()

NT_Detm_Fitzroy_dsvl1 <- NT_Detm_Fitzroy %>%
    st_union() %>%
    st_intersection(STE_WA) %>% st_make_valid() %>% st_as_sf() %>% 
    nngeo::st_remove_holes() %>% st_make_valid() %>% st_as_sf()

# ggplot()+
#   geom_sf(data = NT_Detm_WK, fill = "red3", alpha = 0.5, colour = "grey50", lwd = 0.3)+
#   # geom_sf(data = NT_ILUA_WK, fill = "blue3", alpha = 0.5, colour = "grey50", lwd = 0.3)+
#   geom_sf(data = Basin_Fitzroy, fill = NA, color = "grey30", lwd = 0.5)+
#   theme_pubr()+
#   theme(legend.position = "bottom")

# ggplot()+
#   geom_sf(data = NT_Detm_Fitzroy, fill = "red3", alpha = 0.5, colour = "grey50", lwd = 0.3)+
#   geom_text(data = NT_Detm_Fitzroy, aes(x = x, y = y, label = IDX), size = 5, color = "red3", fontface = "bold")+
#   # geom_sf(data = NT_Detm_Fitzroy_dsvl2, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
#   geom_sf(data = NT_Detm_Fitzroy_dsvl1, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
#   geom_sf(data = Basin_Fitzroy, fill = NA, color = "grey30", lwd = 0.5)+
#   theme_pubr()+
#   theme(legend.position = "bottom")


## Groundwater ----
# st_layers(file.path(INPUT_DIR, "WRIMS_Groundwater_Resources_DWER_084_WA_GDA2020_Public_Geopackage", "WRIMS_Groundwater_Resources_DWER_084_WA_GDA2020_Public.gpkg"))

GrdWtr <- st_read(file.path(INPUT_DIR, "WRIMS_Groundwater_Resources_DWER_084_WA_GDA2020_Public_Geopackage", "WRIMS_Groundwater_Resources_DWER_084_WA_GDA2020_Public.gpkg"), 
    layer = "WRIMS_Groundwater_Resources_DWER_084") %>% st_cast("POLYGON") %>% 
    st_make_valid() %>% st_as_sf() %>%
    mutate(x = st_coordinates(st_centroid(SHAPE))[,1], y = st_coordinates(st_centroid(SHAPE))[,2],
           IDX = 1:n()) 
# nrow(GrdWtr)

# ggplot()+
#   geom_sf(data = GrdWtr, aes(fill = aquifer), alpha = 0.5, colour = NA)+
#   geom_text(data = GrdWtr, aes(x = x, y = y, label = IDX), size = 5, color = "grey10", fontface = "bold")+
#   theme_pubr()+
#   theme(legend.position = "none")

GrdWtr_WK_IDX <- st_intersection(GrdWtr, BBOX) %>% 
    st_drop_geometry() %>% pull(IDX) 

GrdWtr_WK <- GrdWtr %>% 
    filter(IDX %in% GrdWtr_WK_IDX) %>% 
    mutate(aquifer = str_replace(aquifer, "\\.", ""))

ggplot()+
  geom_sf(data = GrdWtr_WK, aes(fill = aquifer), alpha = 0.5, colour = NA)+
  geom_text(data = GrdWtr_WK, aes(x = x, y = y, label = IDX), size = 5, color = "grey10", fontface = "bold")+
  theme_pubr()+
  theme(legend.position = "none")


GrdWtr_Fitzroy_IDX <- st_intersection(GrdWtr , Basin_Fitzroy) %>% 
    st_drop_geometry() %>% pull(IDX)

GrdWtr_Fitzroy <- GrdWtr %>%  
    mutate(aquifer = str_replace(aquifer, "\\.", "")) %>% 
    filter(
      IDX %in% GrdWtr_Fitzroy_IDX | 
      aquifer %in% c("Canning - Broome", "Canning - Grant", "Canning - Wallal", "Canning Broome Saline")
    ) %>% filter(!IDX %in% c(275, 272))  %>% 
    mutate(x = st_coordinates(st_centroid(SHAPE))[,1], y = st_coordinates(st_centroid(SHAPE))[,2], IDX = 1:n())

GrdWtr_subarea_LaGr <- GrdWtr_Fitzroy %>%
  filter(str_detect(subarea, "La Grange"))

GrdWtr_Fitzroy %>% st_drop_geometry() %>% pull(subarea) %>% unique()

Basin_Fitzroy2Coast_GrdWtr_LaGr <- bind_rows(
  Basin_Fitzroy2Coast  %>% mutate(Type = "Basin", Name = basin_name) %>% select(Type, Name),
  GrdWtr_Fitzroy %>% filter(subarea == "Canning - La Grange") %>% mutate(Type = "G.water", Name = subarea) %>% select(Type, Name)
) %>% st_intersection(STE_WA %>% select()) %>% st_make_valid() %>% st_as_sf() %>% 
  mutate(Name2 = str_replace(Name, " Coast| River|Canning - ", ""),
         Name2 = paste(Type, Name2, sep = ": "),
         x = st_coordinates(st_centroid(SHAPE))[,1], y = st_coordinates(st_centroid(SHAPE))[,2])

Basin_Fitzroy2Coast_GrdWtr_LaGr_dsvl <- st_union(Basin_Fitzroy2Coast_GrdWtr_LaGr) %>% 
    st_intersection(STE_WA) %>% st_make_valid() %>% st_as_sf() %>% filter(!st_is_empty(.)) %>% 
    nngeo::st_remove_holes()

st_write(Basin_Fitzroy2Coast_GrdWtr_LaGr_dsvl, file.path(OUTPUT_DIR, "data", "Basin_Fitzroy2Coast_GrdWtr_LaGr_dsvl.gpkg"), append = FALSE)

Basin_Fitzroy2Coast_GrdWtr_LaGr_dsvl_50k <- Basin_Fitzroy2Coast_GrdWtr_LaGr_dsvl %>%
  st_transform(crs = "EPSG:8015") %>% 
  st_buffer(dist = 50000) %>% 
  st_make_valid() %>% st_as_sf() %>% st_transform(crs = st_crs(STE))

st_write(Basin_Fitzroy2Coast_GrdWtr_LaGr_dsvl_50k, file.path(OUTPUT_DIR, "data", "Basin_Fitzroy2Coast_GrdWtr_LaGr_dsvl_50k.gpkg"), append = FALSE)

Basin_Fitzroy_Broome_Aqui <- bind_rows(
  Basin_Fitzroy_WK %>% mutate(Type = "Basin", Name = basin_name) %>% select(Type, Name),
  GrdWtr_Fitzroy %>% filter(aquifer %in% c("Canning - Broome", "Canning - Grant", "Canning Broome Saline")) %>% mutate(Type = "Aquifer", Name = aquifer) %>% select(Type, Name)
)

Basin_Fitzroy_Broome_Aqui_dsvl <- st_union(Basin_Fitzroy_Broome_Aqui) %>% 
    st_intersection(STE_WA) %>% st_make_valid() %>% st_as_sf() %>% 
    nngeo::st_remove_holes()

Basin_Fitzroy_Wallal_Aqui <- bind_rows(
  Basin_Fitzroy_WK %>% mutate(Type = "Basin", Name = basin_name) %>% select(Type, Name),
  GrdWtr_Fitzroy %>% filter(aquifer %in% c("Canning - Wallal", "Canning - Broome", "Canning - Grant", "Canning Broome Saline")) %>% mutate(Type = "Aquifer", Name = aquifer) %>% select(Type, Name)
)
Basin_Fitzroy_Wallal_Aqui_dsvl <- st_union(Basin_Fitzroy_Wallal_Aqui) %>% 
    st_intersection(STE_WA) %>% st_make_valid() %>% st_as_sf() %>% 
    st_cast("POLYGON") %>% st_make_valid() %>% st_as_sf() %>%
    nngeo::st_remove_holes() %>% 
    mutate(x = st_coordinates(st_centroid(geometry))[,1], y = st_coordinates(st_centroid(geometry))[,2], IDX = 1:n()) %>% 
    filter(IDX != 1) %>% 
    st_union() %>% st_make_valid() %>% st_as_sf()


# ggplot()+
#   geom_sf(data = Basin_Fitzroy_Wallal_Aqui_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
#   # geom_sf(data = Basin_Fitzroy_Broome_Aqui_dsvl, fill = NA, color = "blue3", lwd = 1, linetype = "solid")+
#   # geom_text(data = Basin_Fitzroy_Wallal_Aqui_dsvl, aes(x = x, y = y, label = IDX), size = 5, color = "red3", fontface = "bold")+
#   geom_sf(data = Basin_Fitzroy, fill = NA, color = "grey30", lwd = 0.5)+
#   theme_pubr()+
#   theme(legend.position = "bottom")

Basin_Fitzroy_Liveringa_Aqui <- bind_rows(
  Basin_Fitzroy_WK %>% mutate(Type = "Basin", Name = basin_name) %>% select(Type, Name),
  GrdWtr_Fitzroy %>% filter(aquifer %in% c("Canning - Wallal", "Canning - Broome", "Canning - Grant", "Canning Broome Saline", "Canning - Liveringa")) %>% 
  mutate(Type = "Aquifer", Name = aquifer) %>% select(Type, Name)
)
Basin_Fitzroy_Liveringa_Aqui$Name

Basin_Fitzroy_Liveringa_Aqui_dsvl <- st_union(Basin_Fitzroy_Liveringa_Aqui) %>% 
    st_intersection(STE_WA) %>% st_make_valid() %>% st_as_sf() %>% 
    st_cast("POLYGON") %>% st_make_valid() %>% st_as_sf() %>%
    nngeo::st_remove_holes() %>% 
    mutate(x = st_coordinates(st_centroid(geometry))[,1], y = st_coordinates(st_centroid(geometry))[,2], IDX = 1:n()) %>% 
    filter(IDX != 28) %>% 
    st_union() %>% st_make_valid() %>% st_as_sf()

ggplot()+
  geom_sf(data = Basin_Fitzroy_Liveringa_Aqui_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  # geom_sf(data = Basin_Fitzroy_Broome_Aqui_dsvl, fill = NA, color = "blue3", lwd = 1, linetype = "solid")+
  # geom_text(data = Basin_Fitzroy_Liveringa_Aqui_dsvl, aes(x = x, y = y, label = IDX), size = 5, color = "red3", fontface = "bold")+
  geom_sf(data = Basin_Fitzroy, fill = NA, color = "grey30", lwd = 0.5)+
  theme_pubr()+
  theme(legend.position = "bottom")

# ggplot()+
#   geom_sf(data = GrdWtr_WK_Fitzroy_antijoin, aes(fill = aquifer), alpha = 0.5, colour = NA)+
#   geom_sf(data = Basin_Fitzroy, fill = NA, color = "grey30", lwd = 0.5)+
#   theme_pubr()+
#   theme(legend.position = "bottom")

# View(GrdWtr_WK %>% st_drop_geometry())
# GrdWtr_WK$aquifer %>% unique()
# GrdWtr_WK$subarea %>% unique()
# GrdWtr_WK$area_name %>% unique()
# GrdWtr_WK$resourcetype %>% unique()
# GrdWtr_WK$status %>% unique()
# GrdWtr_WK$allocationplan %>% unique()


# ggplot()+
#   geom_sf(data = GrdWtr_WK %>% group_by(aquifer) %>% summarise(SHAPE = st_union(SHAPE)), aes(fill = aquifer), alpha = 0.2, colour = "grey30")+
#   geom_sf(data = Basin_Fitzroy, fill = "grey50", color = NA, alpha = 0.5)+
#   theme_pubr()+
#   theme(legend.position = "bottom")
# ggsave(file.path(OUTPUT_DIR, "figures", "GrdWtr_WK_aquifer.png"), width = 20, height = 15, dpi = 300)

# ggplot()+
#   geom_sf(data = GrdWtr_Fitzroy, aes(fill = aquifer), alpha = 0.5, colour = NA)+
#   geom_sf(data = Basin_Fitzroy, fill = "grey50", color = NA, alpha = 0.2)+
#   theme_pubr()+
#   theme(legend.position = "bottom")

# ggplot()+
#   geom_sf(data = GrdWtr_WK, aes(fill = subarea), alpha = 0.5, colour = NA)+
#   theme_pubr()+
#   theme(legend.position = "bottom")

# ggplot()+
#   geom_sf(data = GrdWtr_WK, aes(fill = area_name), alpha = 0.5, colour = NA)+
#   theme_pubr()+
#   theme(legend.position = "bottom")

# Marine ----

XY_Isdell_Basin_N <- st_point(c(124.3951034, -16.3390494)) %>% st_sfc(crs = st_crs(STE)) %>% st_as_sf() %>% st_make_valid() %>%  st_transform(crs = st_crs("EPSG:8015"))
XY_Cape_Leveque_S <- st_point(c(122.0249171, -18.3801309)) %>% st_sfc(crs = st_crs(STE)) %>% st_as_sf() %>% st_make_valid() %>% st_transform(crs = st_crs("EPSG:8015"))
XYs_Cape_Leveque <- rbind(
  c(121.9957423, -18.4042833),
  c(121.9975225, -18.4032632),
  c(122.0003622, -18.4027533),
  c(122.0020225, -18.4013432),
  c(122.0051034, -18.3972833),
  c(122.0072335, -18.3953633),
  c(122.0118544, -18.3923133),
  c(122.0151743, -18.3916432),
  c(122.0174244, -18.3905133),
  c(122.0194343, -18.3884832),
  c(122.0212144, -18.3859933),
  c(122.0263135, -18.3777533),
  c(122.0306922, -18.3744833),
  c(122.0363834, -18.3644333),
  c(122.0382835, -18.3629623),
  c(122.0411231, -18.3622823),
  c(122.0459825, -18.3573123),
  c(122.0538024, -18.3438723),
  c(122.0581922, -18.3332523),
  c(122.0602024, -18.3296422),
  c(122.0635223, -18.3258023)) %>% 
  as.data.frame %>% st_as_sf(coords = c(1,2), crs = st_crs(STE)) %>% 
  st_transform(crs = st_crs("EPSG:8015"))

Territorial_Sea_Baseline <- st_read(file.path(INPUT_DIR, "Territorial_Sea_Baseline_(straight_baseline_limits)", "Territorial_Sea_Baseline_(straight_baseline_limits).shp")) %>% 
    st_transform(crs = st_crs("EPSG:8015")) %>% filter(objnam == "AMB275768") 

Territorial_Sea_Baseline_XY <- st_coordinates(Territorial_Sea_Baseline)[, c("X", "Y")]
dX <- Territorial_Sea_Baseline_XY[2, "X"] - Territorial_Sea_Baseline_XY[1, "X"]
dY <- Territorial_Sea_Baseline_XY[2, "Y"] - Territorial_Sea_Baseline_XY[1, "Y"]

dX_n <- dX / sqrt(dX^2 + dY^2)
dY_n <- dY / sqrt(dX^2 + dY^2)
dX_n_perp <- -dY_n
dY_n_perp <- dX_n

XY_N <- st_coordinates(st_geometry(XY_Isdell_Basin_N))[1,]
# Create a long line passing through the point
Line_Perp_Isdell_Basin_N <- matrix(c(XY_N["X"], XY_N["Y"],
                                     XY_N["X"] + 1852*150 * dX_n_perp, XY_N["Y"] + 1852*150 * dY_n_perp), ncol = 2, byrow = TRUE) %>% 
  st_linestring() %>% st_sfc(crs = st_crs("EPSG:8015")) %>% st_transform(crs = st_crs(STE)) 

XYs_Cape_Leveque_P <- st_coordinates(XYs_Cape_Leveque)[, c("X", "Y")]
LM_Cape_Leveque <- lm(Y ~ X, data = as.data.frame(XYs_Cape_Leveque_P))
SLOPE_Cape_Leveque <- coef(LM_Cape_Leveque)[2]
dX_n <- 1 / sqrt(1 + SLOPE_Cape_Leveque^2)
dY_n <- SLOPE_Cape_Leveque / sqrt(1 + SLOPE_Cape_Leveque^2)
dX_n_perp <- -dY_n
dY_n_perp <- dX_n
XY_S <- st_coordinates(XY_Cape_Leveque_S)[1, c("X", "Y")]
Line_Perp_Cape_Leveque_S <- matrix(c(XY_S["X"], XY_S["Y"],
                                     XY_S["X"] + 1852*150 * dX_n_perp, XY_S["Y"] + 1852*150 * dY_n_perp), ncol = 2, byrow = TRUE) %>% 
  st_linestring() %>% st_sfc(crs = st_crs("EPSG:8015")) %>% st_transform(crs = st_crs(STE))

Basin_Fitzroy2Coast2_extended_lines <- bind_rows(
  st_as_sf(Line_Perp_Isdell_Basin_N) %>% mutate(ID = "Isdell Basin"),
  st_as_sf(Line_Perp_Cape_Leveque_S) %>% mutate(ID = "Cape Leveque Basin")
)

Aus_SEA_12NM <- st_read(file.path(INPUT_DIR, "Territorial_Sea_areas", "Territorial_Sea_areas.shp")) %>% 
    st_transform(crs = st_crs(STE)) %>% st_union()

Aus_SEA_12NM_split <- lwgeom::st_split(Aus_SEA_12NM, Basin_Fitzroy2Coast2_extended_lines) %>% 
    st_collection_extract("POLYGON") %>% st_make_valid() %>% st_as_sf() %>%
    rename(SHAPE = x) %>% 
    mutate(IDX = 1:n(), 
           x = st_coordinates(st_centroid(SHAPE))[,1], y = st_coordinates(st_centroid(SHAPE))[,2])

ggplot()+
  geom_sf(data = Aus_SEA_12NM_split, color = "red3", fill = NA, lwd = 1)+
  geom_text(data = Aus_SEA_12NM_split, aes(x = x, y = y, label = IDX), size = 5, color = "red3", fontface = "bold")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)])+
  theme_pubr()+
  theme(legend.position = "bottom")


Aus_SEA_Fitzroy2Coast2_band <- Aus_SEA_12NM_split %>% filter(IDX == 42) %>% 
    nngeo::st_remove_holes() %>% st_make_valid() %>% st_as_sf() %>% rename(SHAPE = geometry)

ggplot()+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_band, fill = "red3", alpha = 0.5, color = NA)+
  # geom_sf(data = Basin_Fitzroy, fill = NA, color = "grey30", lwd = 0.5)+
  theme_pubr()+
  theme(legend.position = "bottom")

Aus_SEA_Fitzroy2Coast2_boundary <- Aus_SEA_Fitzroy2Coast2_band %>% st_boundary() %>% st_make_valid() %>% st_as_sf()
ggplot()+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_boundary, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  # geom_sf(data = Basin_Fitzroy, fill = NA, color = "grey30", lwd = 0.5)+
  theme_pubr()+
  theme(legend.position = "bottom")

Aus_SEA_Fitzroy2Coast2_boundary_N <- st_intersection(Aus_SEA_Fitzroy2Coast2_boundary, Line_Perp_Isdell_Basin_N) %>% 
  st_cast("POINT")
ggplot()+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_boundary_N, fill = "red3", color = "red3", size = 3)+
  # geom_sf(data = Basin_Fitzroy, fill = NA, color = "grey30", lwd = 0.5)+
  theme_pubr()+
  theme(legend.position = "bottom")

Dummy_polygon <- st_polygon(list(rbind(st_coordinates(Aus_SEA_Fitzroy2Coast2_boundary_N)[2, c("X", "Y")],
                                  st_coordinates(XY_Cape_Leveque_S %>% st_transform(crs = st_crs(STE)))[1, c("X", "Y")],
                                  st_coordinates(XY_Isdell_Basin_N %>% st_transform(crs = st_crs(STE)))[1,c("X", "Y")],
                                  st_coordinates(Aus_SEA_Fitzroy2Coast2_boundary_N)[2, c("X", "Y")]))) %>% 
  st_sfc(crs = st_crs(STE)) %>% st_as_sf() %>% st_make_valid() %>% rename(SHAPE = x)

Aus_SEA_Fitzroy2Coast2_dsvl <- bind_rows(
  Aus_SEA_Fitzroy2Coast2_band %>% select(),
  Dummy_polygon %>% select(),
  Basin_Fitzroy2Coast2 %>% select()
) %>% st_make_valid() %>% st_as_sf() %>% st_union() %>% 
  nngeo::st_remove_holes() %>% st_make_valid() %>% st_as_sf()

ggplot()+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  # geom_sf(data = Dummy_polygon, fill = "grey", alpha = 0.5)+
  # geom_sf(data = Aus_SEA_Fitzroy2Coast2, fill = "blue3", alpha = 0.5, colour = NA)+
  # geom_sf(data = Basin_Fitzroy2Coast2_extended_lines, color = "red3", lwd = 1)+
  # geom_sf(data = Basin_Fitzroy2Coast2, fill = NA, color = "grey30", lwd = 0.5)+
  # # geom_text(data = Basin_Fitzroy2Coast2_extended_polygon, aes(x = st_coordinates(st_centroid(x))[,1], y = st_coordinates(st_centroid(x))[,2], label = IDX), size = 5, color = "blue3", fontface = "bold")+
  theme_pubr()+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)])+
  theme(legend.position = "bottom")

st_write(Aus_SEA_Fitzroy2Coast2_dsvl, file.path(OUTPUT_DIR, "data", "Fitzroy2Coast_Aus_SEA_dsvl.gpkg"), append = FALSE)
st_write(Aus_SEA_Fitzroy2Coast2_dsvl, file.path(OUTPUT_DIR, "data", "Fitzroy2Coast_Aus_SEA_dsvl.shp"), append = FALSE)

# Language ----
AUS_LANGUAGES <- st_read(file.path(OUTPUT_DIR, "data", "Native_Language.geojson")) %>% 
    st_transform(crs = st_crs(STE)) %>% st_make_valid() %>% st_as_sf() %>%
    mutate(ST_VALID = st_is_valid(.))
AUS_TERRITORIES <- st_read(file.path(OUTPUT_DIR, "data", "Native_Territories.geojson")) %>% 
    st_transform(crs = st_crs(STE)) %>% st_make_valid() %>% st_as_sf() %>% 
    mutate(ST_VALID = st_is_valid(.))

# ggplot()+
#   geom_sf(data = AUS_LANGUAGES, aes(fill = ST_VALID), alpha = 0.5, colour = NA)
# ggplot()+
#   geom_sf(data = AUS_TERRITORIES, aes(fill = ST_VALID), alpha = 0.5, colour = NA)

LANGUAGES_WK_ID <- st_intersection(AUS_LANGUAGES %>% filter(ST_VALID == TRUE), ABS_IREG_WK_dsvl) %>% 
    st_drop_geometry() %>% pull(ID) %>% unique()
LANGUAGES_FITZROY_ID <- st_intersection(AUS_LANGUAGES %>% filter(ST_VALID == TRUE), Basin_Fitzroy) %>% 
    mutate(Area_Ha = as.numeric(st_area(.))/10000) %>% filter(Area_Ha > 24950) %>%
    st_drop_geometry() %>% pull(ID) %>% unique()
TERRITORIES_WK_ID <- st_intersection(AUS_TERRITORIES %>% filter(ST_VALID == TRUE), ABS_IREG_WK_dsvl) %>% 
    st_drop_geometry() %>% pull(ID) %>% unique()
TERRITORIES_FITZROY_ID <- st_intersection(AUS_TERRITORIES %>% filter(ST_VALID == TRUE), Basin_Fitzroy) %>% 
    st_drop_geometry() %>% pull(ID) %>% unique()

LANGUAGES_WK <- AUS_LANGUAGES %>% filter(ID %in% LANGUAGES_WK_ID) %>% 
    st_intersection(STE_WA %>% select(STE_NAME21)) %>% st_make_valid() %>% st_as_sf() %>% 
    mutate(Name2 = ifelse(Name == "Doolboong/Miriwoong", "Doolboong/\nMiriwoong", Name),
           x = st_coordinates(st_centroid(geometry))[,1], y = st_coordinates(st_centroid(geometry))[,2])

TERRITORIES_WK <- AUS_TERRITORIES %>% filter(ID %in% TERRITORIES_WK_ID) %>% 
    st_intersection(STE_WA %>% select(STE_NAME21)) %>% st_make_valid() %>% st_as_sf()

LANGUAGES_WK_dsvl <- st_union(LANGUAGES_WK) %>% st_as_sf() %>% st_make_valid()
TERRITORIES_WK_dsvl <- st_union(TERRITORIES_WK) %>% st_as_sf() %>% st_make_valid()

LANGUAGES_FITZROY_dsvl <- st_union(AUS_LANGUAGES %>% filter(ID %in% LANGUAGES_FITZROY_ID)) %>% 
    st_intersection(STE_WA %>% select(STE_NAME21)) %>% st_make_valid() %>% st_as_sf() %>% 
    st_boundary() %>% st_make_valid() %>% st_as_sf()
TERRITORIES_FITZROY_dsvl <- st_union(AUS_TERRITORIES %>% filter(ID %in% TERRITORIES_FITZROY_ID)) %>% 
    st_intersection(STE_WA %>% select(STE_NAME21)) %>% st_make_valid() %>% st_as_sf()

LANGUAGES_pal <- setNames(LANGUAGES_WK$color, LANGUAGES_WK$Name2)

ggplot()+
  geom_sf(data = LANGUAGES_WK, aes(fill = color), alpha = 0.5, colour = NA) +
  geom_sf(data = LANGUAGES_WK_dsvl, fill = NA, color = "grey10", lwd = 0.5) +
  geom_sf(data = LANGUAGES_FITZROY_dsvl, fill = NA, color = "red3", lwd = 0.5)+
  geom_sf(data = ABS_IREG_WK_dsvl, fill = NA, color = "black", lwd = 0.5)



# Tenure ----

AUSTEN <- rast(paste0("/vsizip/", normalizePath(file.path(INPUT_DIR, "land_tenure_of_Australia_v2_2010_11_to_2020_21_20241031", "AUSTEN_v2_250m_2020_21_alb.zip"), winslash = "/"), "/", "AUSTEN_v2_250m_2020_21_alb.tif")) 
cats(AUSTEN)
levels(AUSTEN)
activeCat(AUSTEN) <- "L3N"

L3_sym_qml <- read_xml(unz(
  file.path(INPUT_DIR, "land_tenure_of_Australia_v2_2010_11_to_2020_21_20241031", "Symbology.zip"), 
  "AUSTEN_v2_Level3.qml"
))
xml_name(L3_sym_qml)
pal <- xml_find_all(L3_sym_qml, ".//paletteEntry")
L3_sym_pal <- tibble(
  value = xml_attr(pal, "value"),
  label = xml_attr(pal, "label"),
  color = xml_attr(pal, "color")
) %>% 
  mutate(value = as.integer(substr(value, 1, 3))) %>% 
  distinct()

L3_sym_pal_vec <- setNames(L3_sym_pal$color, L3_sym_pal$label)
L3_sym_pal_vec["Freehold"] <- "#a77f1a"
L3_sym_pal_vec["No data/ unresolved"] <- "grey50"


AUSTEN_WK <- AUSTEN %>% 
    crop(vect(BBOX) %>% project(crs(AUSTEN))) %>% 
    crop(vect(STE_WA) %>% project(crs(AUSTEN)), mask = TRUE)
AUSTEN_WK_vt <- as.polygons(AUSTEN_WK) 

AUSTEN_WK_sf <- st_as_sf(AUSTEN_WK_vt) %>% st_make_valid() %>% st_as_sf() %>% st_transform(crs = st_crs(STE)) %>% 
    st_cast("POLYGON") %>% mutate(IDX = 1:n())

# ggplot()+
#     geom_sf(data = AUSTEN_WK_sf, aes(fill = L3_DESC))+
#     geom_sf(data = ABS_IREG_WK_dsvl, fill = NA, color = "grey10", lwd = 0.5) +
#     geom_sf(data = IBRA_WK_WA, fill = NA, color = "grey50", lwd = 0.8)+
#     geom_sf(data = IBRA_WK_WA_dsvl, fill = NA, color = "blue3", lwd = 0.8)

AUSTEN_WK_ABS_IREG_IDX <- st_intersection(AUSTEN_WK_sf, 
                                      ABS_IREG_WK %>% 
                                        group_by(IREG_NAME_2021) %>% summarise(geometry = st_union(geom))) %>% 
                          filter(!st_is_empty(geometry), IREG_NAME_2021 != "South Hedland") %>% 
                          st_drop_geometry() %>% pull(IDX) %>% unique()

AUSTEN_WK_ABS_IREG <- AUSTEN_WK_sf %>% filter(IDX %in% AUSTEN_WK_ABS_IREG_IDX) %>% 
    mutate(x = st_coordinates(st_centroid(geometry))[,1], y = st_coordinates(st_centroid(geometry))[,2]) %>% 
    left_join(L3_sym_pal, by = c("L3N" = "value")) %>% 
    rename(L3_DESC = label)


ggplot()+
    geom_sf(data = AUSTEN_WK_ABS_IREG, aes(fill = L3_DESC))+
    geom_sf(data = ABS_IREG_WK_dsvl, fill = NA, color = "grey10", lwd = 0.5)+
    geom_text(data = AUSTEN_WK_ABS_IREG, aes(x = x, y = y, label = IDX), size = 3, color = "grey10", fontface = "bold")

## Removing other crown land and other crown purpose at the southern part of the map
IDX_RM <- c(13045, 11713, 11716, 1246, 9125, 11714, 1239)

AUSTEN_WK_ABS_IREG_SEL <- AUSTEN_WK_sf %>% filter(IDX %in% AUSTEN_WK_ABS_IREG_IDX) %>% 
    filter(!(IDX %in% IDX_RM)) %>% mutate(AREA = as.numeric(st_area(.))/10000) %>%
    filter(AREA > 1000)
AUSTEN_WK_ABS_IREG_SEL_U <- st_union(AUSTEN_WK_ABS_IREG_SEL) %>% st_make_valid() %>% st_as_sf() 
AUSTEN_WK_ABS_IREG_ConvexH <- st_union(AUSTEN_WK_ABS_IREG_SEL) %>% st_make_valid() %>% st_convex_hull() %>% st_as_sf() %>% st_transform(crs = st_crs(STE))

AUSTEN_WK_ABS_IREG_ConcaveH <- st_union(AUSTEN_WK_ABS_IREG_SEL) %>% st_make_valid() %>% st_concave_hull(0.05) %>% st_as_sf()



# Property ----
## Client Property Event System - Properties (DPIRD-018)
### The client property event (CPE) system is a spatially linked database of agricultural and other properties maintained by the Department of Primary Industries and Regional Development (DPIRD).
### Property boundaries consist of one or more contiguous cadastral parcels that are believed to be managed as one unit. Property information is collected by DPIRD staff through direct contact with landholders through mail-outs and attendance at field days.
### Properties may also contain information autogenerated from Landgate parcel data records.
### The underlying parcel data is updated on a regular basis using Cadastral boundaries supplied by Landgate. Property boundary and ownership information are updated on a daily basis.
### In accordance with Departmental Policy, contact information such as names and addresses of landholders linked to properties is restricted use only.
# "C:\Users\uqychun7\Documents\Data\WestKimberley\input\Client_Property_Event_System_Properties_DPIRD_018_WA_GDA2020_Public_Geopackage\Client_Property_Event_System_Properties_DPIRD_018_WA_GDA2020_Public.gpkg"
CPE_Prop <- st_read(file.path(INPUT_DIR, "Client_Property_Event_System_Properties_DPIRD_018_WA_GDA2020_Public_Geopackage", "Client_Property_Event_System_Properties_DPIRD_018_WA_GDA2020_Public.gpkg"), 
    layer = "Client_Property_Event_System_Properties_DPIRD_018")  %>% 
    mutate(IDX = 1:n())

CPE_Prop_ABS_IREG_WK_IDX <- st_intersection(CPE_Prop, ABS_IREG_WK_dsvl) %>%
                          filter(lga!="EAST PILBARA") %>% 
                          st_drop_geometry() %>% pull(IDX) %>% unique()

CPE_Prop_ABS_IREG_WK <- CPE_Prop %>% filter(IDX %in% CPE_Prop_ABS_IREG_WK_IDX)

CPE_Prop_ABS_IREG_WK_dsvl <- st_union(
  bind_rows(st_union(CPE_Prop_ABS_IREG_WK) %>% st_as_sf(), ABS_IREG_WK_dsvl)
) %>% st_make_valid() %>% st_as_sf()

CPE_Prop_Fitzroy_IDX <- st_intersection(CPE_Prop, Basin_Fitzroy) %>% 
  filter(lga!="EAST PILBARA") %>%
  st_drop_geometry() %>% pull(IDX) %>% unique()

CPE_Prop_Fitzroy <- CPE_Prop %>% filter(IDX %in% CPE_Prop_Fitzroy_IDX)
CPE_Prop_ABS_IREG_WK$property_type %>% unique()

CPE_Prop_Fitzroy_dsvl <- st_union(CPE_Prop_Fitzroy) %>% st_make_valid() %>% st_as_sf() %>% 
  st_concave_hull(ratio = 0.025, allow_holes = FALSE)


# River ----

# Catchment_Fitzroy <- st_read(file.path(INPUT_DIR, "Hydrographic_Catchments_Catchments_DWER_028_WA_GDA2020_Public_Geopackage", "Hydrographic_Catchments_Catchments_DWER_028_WA_GDA2020_Public.gpkg"), 
#     layer = "Hydrographic_Catchments_Catchments_DWER_028") %>% 
#     filter(basin_name == "Fitzroy River")

# Basin <- st_read(file.path(INPUT_DIR, "Hydrographic_Catchments_Basins_DWER_027_WA_GDA2020_Public_Geopackage", "Hydrographic_Catchments_Basins_DWER_027_WA_GDA2020_Public.gpkg"), 
#     layer = "Hydrographic_Catchments_Basins_DWER_027")

# Basin_Fitzroy <- Basin %>% filter(basin_name == "Fitzroy River")

# Neighbour_basin <- Basin %>% filter(lengths(st_touches(., Basin_Fitzroy))>0) 

# Basin_Fitzroy_grt <- Basin %>% dplyr::filter(basin_name %in% c("Fitzroy River", "Isdell River", "Cape Leveque Coast", "Lennard River")) %>% 
#     mutate(Area_Ha = as.numeric(st_area(.))/10000)

# Basin_Fitzroy_WK_Bno <- st_intersection(Basin, IBRA_WK_WA_dsvl) %>% pull(basin_no)  %>% unique() %>% subset(! . %in% c(1205, 1206, 710))

# Basin_Fitzroy_WK <- Basin %>% filter(lengths(st_touches(Basin, Basin_Fitzroy))>0) %>% 
#     bind_rows(Basin_Fitzroy) %>% 
#     st_crop(BBOX) %>% 
#     mutate(StudySite = case_when(basin_name == "Fitzroy River" ~ "Fitzroy River basin",
#                              basin_name %in% c("Isdell River", "Cape Leveque Coast", "Lennard River") ~ "Basin near Fitzroy",
#                              TRUE ~ "Other West Kimberley basins"))

# # Basin_Fitzroy_WK <- Basin %>% filter(basin_no %in% Basin_Fitzroy_WK_Bno) %>% st_intersection(STE_WA) %>% 
# #     mutate(StudySite = case_when(basin_name == "Fitzroy River" ~ "Fitzroy River basin",
# #                              basin_name %in% c("Isdell River", "Cape Leveque Coast", "Lennard River") ~ "Basin near Fitzroy",
# #                              TRUE ~ "Other West Kimberley basins"))

# Basin_Fitzroy_WK_dsvl <- st_union(Basin_Fitzroy_WK) %>% st_as_sf() %>% st_make_valid()

# Basin_Fitzroy_grt_dsvl <- st_union(Basin_Fitzroy_grt) %>% st_as_sf() 

# Basin_Fitzroy_grt_label <- st_centroid(Basin_Fitzroy_grt) %>% 
#     dplyr::mutate(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2],
#            basin_name2 = case_when(basin_name == "Cape Leveque Coast" ~ "Cape\nLeveque\nCoast",
#                                    TRUE ~ basin_name))

# Basin_Label <- st_centroid(Basin) %>% 
#     mutate(basin_name2 = str_wrap(basin_name, width = 10),
#            x = st_coordinates(.)[,1], y = st_coordinates(.)[,2])


Hydrography <- st_read(file.path(INPUT_DIR, "Hydrography_Linear_Hierarchy_DWER_031_WA_GDA2020_Public_Geopackage", "Hydrography_Linear_Hierarchy_DWER_031_WA_GDA2020_Public.gpkg"), 
    layer = "Hydrography_Linear_Hierarchy_DWER_031") %>% 
    st_crop(BBOX)

Rivers <- Hydrography %>% 
    filter(level_name %in% c("Mainstream", "Major River", 
                              "Minor River", "Significant Stream", 
                              "Major Trib", "Minor Trib", 
                              "Insignificant Trib")) %>% 
    mutate(RiverType = case_when(level_name %in% c("Mainstream", "Major River") ~ "Major river",
                                level_name %in% c("Minor River", "Significant Stream") ~ "Minor river",
                                TRUE ~ "Tributary"))

# Rivers_Fitzroy_grt <- Rivers %>%
#     filter(basin %in% unique(Basin_Fitzroy_grt$basin_no))

# Rivers_Fitzroy <- st_intersection(Hydrography, Basin_Fitzroy) %>% 
#     filter(basin_name == "Fitzroy River")

st_area(Basin_Fitzroy) %>% units::set_units("km^2")
st_area(Basin_Fitzroy_grt) %>% units::set_units("km^2")  %>% sum()
st_area(Basin_Fitzroy_WK) %>% units::set_units("km^2")  %>% sum()

# national heritage ----
# National_Heritage_WK <- st_read(file.path(INPUT_DIR, "National_Heritage_List_Australia.gdb"), layer = "national_list") %>% 
#   filter(NAME == "The West Kimberley") %>% 
#   st_transform(crs = st_crs(STE)) %>% select(NAME, SHAPE) %>% st_make_valid() %>% st_as_sf()

# National_Heritage_WK_WA <- st_intersection(National_Heritage_WK, STE_WA) %>% st_make_valid() %>% st_as_sf()

# National_Heritage_ConvexH <- st_convex_hull(National_Heritage_WK) %>% st_make_valid() %>% st_as_sf()

# Basin_Fitzroy_uNational_Heritage_WK <- st_union(Basin_Fitzroy_grt_dsvl, National_Heritage_WK) %>% st_make_valid() %>% st_union()

# Basin_Fitzroy_uNational_Heritage_WK_WA <- st_intersection(Basin_Fitzroy_uNational_Heritage_WK, STE_WA %>% st_make_valid() %>% st_union()) %>% st_as_sf()

# st_area(National_Heritage_WK_WA) %>% units::set_units("km^2")
# st_area(Basin_Fitzroy_uNational_Heritage_WK_WA) %>% units::set_units("km^2")

# Other general layers ----
Roads <- st_read(file.path(INPUT_DIR, "Road_Network", "Road_Network.shp")) %>% 
    st_transform(crs = st_crs(STE)) %>% 
    st_crop(BBOX) %>% 
    filter(NETWORK_TY != "Proposed Road") %>%
    mutate(RoadType = case_when(NETWORK_TY %in% c("State Road") ~ "State road",
                                TRUE ~ "Other road"))

Roads_State <- Roads %>%
    filter(NETWORK_TY %in% c("State Road"))

ABS_urb <- st_read(file.path(INPUT_DIR, "UCL_2021_AUST_GDA94_SHP", "UCL_2021_AUST_GDA94.shp")) %>% 
    st_transform(st_crs(STE))

STE1 <- STE %>%
  filter(!STE_NAME21 %in% c("Western Australia", "Other Territories", "Outside Australia"))

URB_SA <- st_intersection(st_centroid(ABS_urb), BBOX) %>%
  st_drop_geometry()


# Select urban areas for labelling
WK_urb_sel_pt <- st_intersection(st_centroid(ABS_urb), BBOX) %>% 
  dplyr::select(UCL_NAME21, geometry) %>%
  distinct(UCL_NAME21, .keep_all = TRUE) %>%
  st_centroid(.) %>%
  mutate(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2]-0.05,
         UCL_NAME21 = case_when(UCL_NAME21 == "Broome" ~ "Broome",
                                UCL_NAME21 == "Fitzroy Crossing" ~ "Fitzroy\nCrossing",
                                UCL_NAME21 == "Bidyadanga (La Grange) (L)" ~ "Bidyadanga\n(La Grange)",
                                UCL_NAME21 == "Derby" ~ "Derby",
                                UCL_NAME21 == "Halls Creek (L)" ~ "Halls Creek",
                                UCL_NAME21 == "Wyndham (L)" ~ "Wyndham",
                                UCL_NAME21 == "Bardi (One Arm Point) (L)" ~ "Bardi\n(One Arm Point)",
                                UCL_NAME21 == "Beagle Bay (L)" ~ "Beagle Bay",
                                UCL_NAME21 == "Kalumburu (L)" ~ "Kalumburu",
                                UCL_NAME21 == "Mindibungu (L)" ~ "Mindibungu",
                                UCL_NAME21 == "Warmun (L)" ~ "Warmun",
                                UCL_NAME21 == "Looma (L)" ~ "Looma",
                                UCL_NAME21 == "Wangkatjungka (L)" ~ "Wangkatjungka",
                                UCL_NAME21 == "Yungngora (L)" ~ "Yungngora"),
         y = ifelse(str_detect(UCL_NAME21, "\n"), y, y - 0.08)) %>% 
  drop_na()




# Plotting ----

# State inset map

STE_SA_plot <- ggplot()+
  geom_sf(data = STE, fill = "grey50", color = NA)+
  geom_sf(data = STE_WA, fill = "grey70", color = NA)+
  geom_sf(data = STE, fill = NA, color = "black", linewidth = 0.2)+
  geom_sf(data = BBOX, fill = NA, color = "red3", linewidth = 1)+
  coord_sf(xlim = c(111, 156), ylim = c(-44.5, -9.5), expand = FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme_void()+theme(legend.position = "none")+
  theme(plot.background  = element_rect(fill = "#91daff", color = "black", linewidth = 0.7))

STE_SA_plot + 
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)


# General map

WK_map <- ggplot()+

  # State boundary grey background
  geom_sf(data = STE1, fill = "grey60", color = NA)+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+

  # Road
  geom_sf(data = Roads, aes(linewidth = RoadType, color = RoadType), alpha = 0.9,key_glyph = "path")+
  
  # Hydrography
  geom_sf(data = Rivers, aes(linewidth = RiverType, color = RiverType), alpha = 0.7,key_glyph = "path")+

  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.5)+

  # Labels for urban areas
  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, aes(color = "Urban", linewidth = "Urban"), size = 1.5)+
  # geom_shadowtext(data = WK_urb_sel_pt, aes(x = x, y = y , label = UCL_NAME21), size = 5.5, color = "grey10", bg.color = "white",  bg.r = 0.1)+ 
  geom_text_repel(data = WK_urb_sel_pt, aes(x = x, y = y , label = UCL_NAME21), size = 5.5, color = "grey10", bg.color = "white",  bg.r = 0.2)+ 


  scale_linewidth_manual(values = c("Urban" = 0,
                                    "State road" = 0.8, "Other road" = 0.4, 
                                    "Major river" = 0.6, "Minor river" = 0.4, "Tributary" = 0.2),
                         breaks = c("Urban" = "Urban", "State road" = "State road", "Other road" = "Other road", 
                                    "Major river" = "Major river", "Minor river" = "Minor river", "Tributary" = "Tributary"), name = NULL)+
  scale_color_manual(values = c("Urban" = "red3",
                                "State road" = "grey40", "Other road" = "grey40", 
                                "Major river" = "steelblue3", "Minor river" = "steelblue3", "Tributary" = "steelblue3"),
                     breaks = c("Urban" = "Urban",
                                "State road" = "State road", "Other road" = "Other road", 
                                "Major river" = "Major river", "Minor river" = "Minor river", "Tributary" = "Tributary"), name = NULL)+
  
  # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(0.5, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.4, line_width = 2, pad_x = unit(4, "cm"), text_cex = 1.2)+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = c(0.1, 0.5), legend.text = element_text(size = 16))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)

ggsave(file.path("output", "figures", "WK_map_base.png"), WK_map, width = 11, height = 9, dpi = 300)

Gen_Map <- WK_map + inset_element(STE_SA_plot, left = 0.01, bottom = 0.65, right = 0.35, top = 0.95)
ggsave(file.path("output", "figures", "WK_map.png"), Gen_Map, width = 11, height = 9, dpi = 300, bg = "transparent")


# National Heritage map
PA_COL <- setNames(c("olivedrab4", "olivedrab3", "olivedrab2", "steelblue4", "steelblue3", "steelblue2"), 
                    levels(CAPAD_WK$Type))

National_Heritage_WK_map <- ggplot()+
 # State boundary grey background
  geom_sf(data = STE1, fill = "grey60", color = NA)+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+
  
  # background
  geom_sf(data = Basin_Fitzroy, fill = "lightgoldenrod2", color = "grey20", lwd = 0.2, linetype = "dashed", alpha = 0.7)+
  geom_sf(data = National_Heritage_WK, color = NA, fill = "lightpink", lwd = 0.2, linetype = "dashed", alpha = 0.5)+
  geom_sf(data = CAPAD_WK, aes(fill = Type), color = "grey20", lwd = 0.2, linetype = "dashed", alpha = 0.5)+
  geom_sf(data = National_Heritage_WK, aes(color = "WK National Heritage"), fill = NA, lwd = 0.2, linetype = "dashed", alpha = .8)+
  
  # Rivers
  geom_sf(data = Rivers, aes(color = "Rivers"), lwd = 0.3, alpha = 0.4, key_glyph = "path")+
  
  # boundaries
  # geom_sf(data = Basin_Fitzroy_uNational_Heritage_WK_WA, aes(linetype = "National heritage &\nFitzroy basin"), fill = NA, color = "grey10", lwd = 0.8)+
  
  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, aes(color = "Urban"), size = 1.5)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.5)+

  # # Basin labels
  # geom_shadowtext(data = data.frame(x = 127, y = -16, label = "The West Kimberley\nnational heritage"), aes(x = x, y = y , label = label), size = 5.5, color = "grey10", bg.color = "white",  bg.r = 0.05)+
  # geom_shadowtext(data = data.frame(x = 125.5, y = -19, label = "Fitzroy Basin"), aes(x = x, y = y , label = label), size = 5.5, color = "grey10", bg.color = "white",  bg.r = 0.05)+
  # geom_text_repel(data = PAs_WK_WA %>% st_drop_geometry(), aes(x = x, y = y, label = leg_name), size = 3, color = "grey10", bg.color = "white", bg.r = 0.02, alpha = 0.9)+

  scale_fill_manual(values = c(PA_COL), name = NULL)+
  scale_color_manual(values = c("Rivers" = "steelblue3", "Urban" = "red3", "WK National Heritage" = "red4"), name = NULL)+
  # scale_linetype_manual(values = c("National heritage &\nFitzroy basin" = "dotdash"), name = NULL)+
  # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "tl", which_north = "true", height = unit(3, "cm"), width = unit(3, "cm"), pad_y = unit(1, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "tl", width_hint = 0.4, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = c(0.12, 0.15), legend.text = element_text(size = 12))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
# ggsave(file.path("output", "figures", "National_Heritage_WK_map.png"), National_Heritage_WK_map, width = 11, height = 9, dpi = 300, bg = "transparent")
ggsave(file.path("output", "figures", "National_Heritage_WK_xlab_map.png"), National_Heritage_WK_map, width = 11, height = 9, dpi = 300, bg = "transparent")



# Basin map
Basin_map <- ggplot()+
  # State boundary grey background
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+
  geom_sf(data = STE1, fill = "grey50", color = NA)+
  
  # Basin background
  geom_sf(data = Basin_Fitzroy_WK, aes(fill = StudySite), alpha = 0.5)+
    
  # River
  geom_sf(data = Rivers, aes(linewidth = RiverType, color = RiverType), alpha = 0.6, key_glyph = "path")+
  
  # Basin boundaries
  # geom_sf(data = Basin_Fitzroy_WK, aes(linetype = "Basin boundary"), fill = NA, color = "black",  lwd = 0.5)+
  # geom_sf(data = Basin_Fitzroy_neighbour_dsvl, fill = NA, color = "grey10", lwd = 0.7, linetype = "dotdash")+
  # geom_sf(data = Basin_Fitzroy_WK_dsvl, fill = NA, color = "grey10", lwd = 0.7, linetype = "dotdash")+
  # geom_sf(data = Basin_Fitzroy, fill = NA, color = "grey10", lwd = 1, linetype = "dotdash")+

  # geom_sf(data = Basin_Fitzroy_boundaries, aes(linewidth = Boundary, color = Boundary), linetype = "solid", key_glyph = "path")+
  
  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, aes(color = "Urban", linewidth = "Urban"), size = 1.5)+

  geom_sf(data = STE1, fill = "grey50", color = NA, alpha = 0.2)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.2)+

  scale_fill_manual(values = c("Fitzroy River basin" = hcl.colors(15, palette = "BrwnYl", rev = TRUE)[6], 
                               "Fitzroy neighbouring basins" = hcl.colors(15, palette = "BrwnYl", rev = TRUE)[4], 
                               "Other West Kimberley basins" = hcl.colors(15, palette = "BrwnYl", rev = TRUE)[2]), 
                    breaks = c("Fitzroy River basin" = "Fitzroy River basin", 
                               "Fitzroy neighbouring basins" = "Fitzroy neighbouring basins",
                               "Other West Kimberley basins" = "Other West Kimberley basins"), name = NULL)+
  scale_linewidth_manual(values = c("Urban" = 0,
                                    "Major river" = 0.3, "Minor river" = 0.2, "Tributary" = 0.1,
                                    "Fitzroy River basin" = 1.2, "Fitzroy neighbouring basins" = 0.9, "West Kimberley basins" = 0.5),
                         breaks = c("Urban" = "Urban", 
                                    "Major river" = "Major river", "Minor river" = "Minor river", "Tributary" = "Tributary",
                                    "Fitzroy River basin" = "Fitzroy River basin", "Fitzroy neighbouring basins" = "Fitzroy neighbouring basins", "West Kimberley basins" = "West Kimberley basins"), name = NULL)+
  scale_color_manual(values = c("Urban" = "red3",
                                "Major river" = "steelblue3", "Minor river" = "steelblue3", "Tributary" = "steelblue3",
                                "Fitzroy River basin" = "goldenrod1", "Fitzroy neighbouring basins" = "goldenrod3", "West Kimberley basins" = "goldenrod4"),
                     breaks = c("Urban" = "Urban",
                                "Major river" = "Major river", "Minor river" = "Minor river", "Tributary" = "Tributary",
                                "Fitzroy River basin" = "Fitzroy River basin", "Fitzroy neighbouring basins" = "Fitzroy neighbouring basins", "West Kimberley basins" = "West Kimberley basins"), name = NULL)+
  # scale_linetype_manual(values = c("Basin boundary" = "dotdash"), name = NULL)+
  # Basin labels
  # geom_shadowtext(data = Basin_Fitzroy_grt_label, aes(x = x, y = y , label = basin_name2), size = 5.5, color = "grey10", bg.color = "white",  bg.r = 0.1)+
  # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "tl", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(1, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "tl", width_hint = 0.3, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = c(0.16, 0.6), legend.text = element_text(size = 16))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
   coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Basin_map.png"), Basin_map, width = 11, height = 9, dpi = 300, bg = "transparent")

Basin_map_option1 <- Basin_map +
  geom_sf(data = Basin_Fitzroy, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Basin_map_option1.png"), Basin_map_option1, width = 11, height = 9, dpi = 300, bg = "transparent")

Basin_map_option2 <- Basin_map +
  geom_sf(data = Basin_Fitzroy, fill = NA, color = "red4", lwd = 0.5, linetype = "dotted")+
  geom_sf(data = Basin_Fitzroy_neighbour_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Basin_map_option2.png"), Basin_map_option2, width = 11, height = 9, dpi = 300, bg = "transparent")

Basin_map_option3 <- Basin_map +
  geom_sf(data = Basin_Fitzroy, fill = NA, color = "red4", lwd = 0.5, linetype = "dotted")+
  geom_sf(data = Basin_Fitzroy_neighbour_dsvl, fill = NA, color = "red4", lwd = 0.5, linetype = "dotted")+
  geom_sf(data = Basin_Fitzroy_WK_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Basin_map_option3.png"), Basin_map_option3, width = 11, height = 9, dpi = 300, bg = "transparent")

Basin_map2 <- ggplot()+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+
  geom_sf(data = STE1, fill = "grey50", color = NA)+
  
  # Basin background
  geom_sf(data = Basin_Fitzroy2Coast2, aes(fill = Name2), color = "grey30", lwd = 0.2, alpha = 0.5)+
  # River
  geom_sf(data = Rivers, aes(linewidth = RiverType), color = "steelblue3", alpha = 0.4, key_glyph = "path", show.legend = FALSE)+

  geom_sf(data = Basin_Fitzroy2Coast2_dsvl, aes(color = "Study Area"), fill = NA, alpha = 0.9, lwd = 0.8)+
  
  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, aes(color = "Urban"), size = 1.5)+
  
  geom_sf(data = STE1, fill = "grey50", color = NA, alpha = 0.2)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.2)+

  scale_fill_manual(values = hcl.colors(6, palette = "Set2"), name = NULL)+
  scale_linewidth_manual(values = c("Major river" = 0.3, "Minor river" = 0.2, "Tributary" = 0.1), name = NULL)+
  scale_color_manual(values = c("Urban" = "black", "Study Area" = "red3"), name = NULL)+
  # # scale_linetype_manual(values = c("Basin boundary" = "dotdash"), name = NULL)+
  # # Basin labels
  # # geom_shadowtext(data = Basin_Fitzroy_grt_label, aes(x = x, y = y , label = basin_name2), size = 5.5, color = "grey10", bg.color = "white",  bg.r = 0.1)+
  # # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(1, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "br", width_hint = 0.3, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = c(0.15, 0.45), legend.text = element_text(size = 16))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
   coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
Basin_wInset_map2  <- Basin_map2 + inset_element(STE_SA_plot, left = 0.01, bottom = 0.65, right = 0.35, top = 0.95)
ggsave(file.path("output", "figures", "Basin_map2.png"), Basin_map2, width = 11, height = 9, dpi = 300, bg = "transparent")
ggsave(file.path("output", "figures", "Basin_wInset_map2.png"), Basin_wInset_map2, width = 11, height = 9, dpi = 300, bg = "transparent")

Basin_Aquifer_map_Urb_Map <- Basin_Aquifer_map +
  geom_text_repel(data = WK_urb_sel_pt %>% st_drop_geometry(), aes(x = x, y = y, label = UCL_NAME21), size = 5, color = "grey10", bg.color = "white", bg.r = 0.1, alpha = 0.9)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Basin_Aquifer_Urb_Map.png"), Basin_Aquifer_map_Urb_Map, width = 11, height = 9, dpi = 300, bg = "transparent")

Basin_Marine <- ggplot()+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+
  geom_sf(data = STE1, fill = "grey50", color = NA)+
  
  # Basin background
  geom_sf(data = Basin_Fitzroy2Coast2, aes(fill = Name), color = "grey30", lwd = 0.2, alpha = 0.5)+
  geom_sf(data = Aus_SEA_12NM, aes(fill = "Territorial Sea"), color = NA, alpha = 0.5)+
  # River
  geom_sf(data = Rivers, aes(linewidth = RiverType), color = "steelblue3", alpha = 0.4, key_glyph = "path", show.legend = FALSE)+
  geom_sf(data = Basin_Fitzroy2Coast2_extended_lines, aes(color = "Basin extended lines"), fill = NA, linetype = "dashed", lwd = 0.5, alpha = 0.7)+
  geom_sf(data = Basin_Fitzroy_WK, aes(color = "Other basins"), fill = NA, linetype = "dotdash", lwd = 0.2, alpha = 0.5)+

  geom_sf(data =Aus_SEA_Fitzroy2Coast2_dsvl , aes(color = "Study Area"), fill = NA, alpha = 0.9, lwd = 0.8)+
  
  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, aes(color = "Urban"), size = 1.5)+
  
  geom_sf(data = STE1, fill = "grey50", color = NA, alpha = 0.2)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.2)+

  scale_fill_manual(values = c(setNames(hcl.colors(4, palette = "Set2"), Basin_Fitzroy2Coast2$Name), "Territorial Sea" = "steelblue2"), name = NULL)+
  scale_linewidth_manual(values = c("Major river" = 0.3, "Minor river" = 0.2, "Tributary" = 0.1), name = NULL)+
  scale_color_manual(values = c("Urban" = "black", "Study Area" = "red3", "Other basins" = "grey10", "Basin extended lines" = "grey50"), name = NULL)+
  # # scale_linetype_manual(values = c("Basin boundary" = "dotdash"), name = NULL)+
  # # Basin labels
  # # geom_shadowtext(data = Basin_Fitzroy_grt_label, aes(x = x, y = y , label = basin_name2), size = 5.5, color = "grey10", bg.color = "white",  bg.r = 0.1)+
  # # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(1, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "br", width_hint = 0.3, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = c(0.125, 0.785), legend.text = element_text(size = 16))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
   coord_sf(xlim = st_bbox(BBOX)[c(1,3)] + c(-1, 0), ylim = st_bbox(BBOX)[c(2,4)] + c(1.5, 0), expand = FALSE)
ggsave(file.path("output", "figures", "Basin_Marine.png"), Basin_Marine, width = 11, height = 9, dpi = 300, bg = "transparent")

#Aquifer map
Aquifer_map <- ggplot()+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+
  geom_sf(data = STE1, fill = "grey50", color = NA)+
  
  # Basin background
  geom_sf(data = GrdWtr_WK, aes(fill = aquifer), color = NA, alpha = 0.3)+
  geom_sf(data = Basin_Fitzroy_WK, fill = NA, color = "grey10", lwd = 0.2)+
  # River
  geom_sf(data = Rivers, aes(linewidth = RiverType, color = RiverType), alpha = 0.4, key_glyph = "path", show.legend = FALSE)+
  
  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, aes(color = "Urban", linewidth = "Urban"), size = 1.5)+
  
  geom_sf(data = STE1, fill = "grey50", color = NA, alpha = 0.2)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.2)+

  scale_fill_manual(values = palette.colors(n = 24, palette = "Alphabet", alpha = 0.5), name = NULL)+
  scale_linewidth_manual(values = c("Urban" = 0,
                                    "Major river" = 0.3, "Minor river" = 0.2, "Tributary" = 0.1),
                         breaks = c("Urban" = "Urban", 
                                    "Major river" = "Major river", "Minor river" = "Minor river", "Tributary" = "Tributary"), name = NULL)+
  scale_color_manual(values = c("Urban" = "red3",
                                "Major river" = "steelblue3", "Minor river" = "steelblue3", "Tributary" = "steelblue3"),
                     breaks = c("Urban" = "Urban",
                                "Major river" = "Major river", "Minor river" = "Minor river", "Tributary" = "Tributary"), name = NULL)+
  # scale_linetype_manual(values = c("Basin boundary" = "dotdash"), name = NULL)+
  # Basin labels
  # geom_shadowtext(data = Basin_Fitzroy_grt_label, aes(x = x, y = y , label = basin_name2), size = 5.5, color = "grey10", bg.color = "white",  bg.r = 0.1)+
  # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "tl", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(1, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "tl", width_hint = 0.3, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = "none", legend.text = element_text(size = 16))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
   coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
Aquifer_legend <- cowplot::get_legend(Aquifer_map + guides(
    fill = guide_legend(ncol = 1),
    color = guide_legend(ncol = 1),
    linewidth = guide_legend(ncol = 1)
  ))
ggsave(file.path("output", "figures", "Aquifer_legend.png"), Aquifer_legend, width = 8, height = 10, dpi = 300, bg = "transparent")
ggsave(file.path("output", "figures", "Aquifer_map.png"), Aquifer_map + theme(legend.position = "none") , width = 11, height = 9, dpi = 300, bg = "transparent")

Aquifer_map_option1 <- Aquifer_map +
  geom_sf(data = Basin_Fitzroy_Broome_Aqui_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)+
  theme(legend.position = "none")
ggsave(file.path("output", "figures", "Aquifer_map_option1.png"), Aquifer_map_option1, width = 11, height = 9, dpi = 300, bg = "transparent")


#Aquifer map
Aquifer_map2 <- ggplot()+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+
  geom_sf(data = STE1, fill = "grey50", color = NA)+
  
  # Basin background
  geom_sf(data = GrdWtr_WK, aes(fill = aquifer), color = NA, alpha = 0.3)+
  geom_sf(data = Basin_Fitzroy_WK, fill = NA, color = "grey10", lwd = 0.2)+
  # River
  geom_sf(data = Rivers, aes(linewidth = RiverType, color = RiverType), alpha = 0.4, key_glyph = "path", show.legend = FALSE)+
  
  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, aes(color = "Urban", linewidth = "Urban"), size = 1.5)+
  
  geom_sf(data = STE1, fill = "grey50", color = NA, alpha = 0.2)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.2)+

  scale_fill_manual(values = palette.colors(n = 24, palette = "Alphabet", alpha = 0.5), name = NULL)+
  scale_linewidth_manual(values = c("Urban" = 0,
                                    "Major river" = 0.3, "Minor river" = 0.2, "Tributary" = 0.1),
                         breaks = c("Urban" = "Urban", 
                                    "Major river" = "Major river", "Minor river" = "Minor river", "Tributary" = "Tributary"), name = NULL)+
  scale_color_manual(values = c("Urban" = "red3",
                                "Major river" = "steelblue3", "Minor river" = "steelblue3", "Tributary" = "steelblue3"),
                     breaks = c("Urban" = "Urban",
                                "Major river" = "Major river", "Minor river" = "Minor river", "Tributary" = "Tributary"), name = NULL)+
  # scale_linetype_manual(values = c("Basin boundary" = "dotdash"), name = NULL)+
  # Basin labels
  # geom_shadowtext(data = Basin_Fitzroy_grt_label, aes(x = x, y = y , label = basin_name2), size = 5.5, color = "grey10", bg.color = "white",  bg.r = 0.1)+
  # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "tl", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(1, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "tl", width_hint = 0.3, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = "none", legend.text = element_text(size = 16))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
   coord_sf(xlim = st_bbox(Basin_Fitzroy_Wallal_Aqui_dsvl)[c(1,3)], ylim = st_bbox(Basin_Fitzroy_Wallal_Aqui_dsvl)[c(2,4)], expand = FALSE)

ggsave(file.path("output", "figures", "Aquifer_map2.png"), Aquifer_map2 + theme(legend.position = "none") , width = 11, height = 9, dpi = 300, bg = "transparent")

Aquifer_map_option2 <- Aquifer_map2 +
  geom_sf(data = Basin_Fitzroy_Wallal_Aqui_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(Basin_Fitzroy_Wallal_Aqui_dsvl)[c(1,3)], ylim = st_bbox(Basin_Fitzroy_Wallal_Aqui_dsvl)[c(2,4)], expand = FALSE)+
  theme(legend.position = "none")
ggsave(file.path("output", "figures", "Aquifer_map_option2.png"), Aquifer_map_option2, width = 11, height = 9, dpi = 300, bg = "transparent")

Aquifer_map_option3 <- Aquifer_map2 +
  geom_sf(data = Basin_Fitzroy_Wallal_Aqui_dsvl, fill = NA, color = "red4", lwd = 0.5, linetype = "dotted")+
  geom_sf(data = Basin_Fitzroy_Liveringa_Aqui_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(Basin_Fitzroy_Wallal_Aqui_dsvl)[c(1,3)], ylim = st_bbox(Basin_Fitzroy_Wallal_Aqui_dsvl)[c(2,4)], expand = FALSE)+
  theme(legend.position = "none")
ggsave(file.path("output", "figures", "Aquifer_map_option3.png"), Aquifer_map_option3, width = 11, height = 9, dpi = 300, bg = "transparent")

## Basin & aquifer map ----
Basin_Aquifer_map <- ggplot()+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+
  geom_sf(data = STE1, fill = "grey50", color = NA)+
  
  # Basin background
  geom_sf(data = Basin_Fitzroy2Coast_GrdWtr_LaGr, aes(fill = Name2), color = "grey30", lwd = 0.2, alpha = 0.5)+
  # River
  geom_sf(data = Rivers, aes(linewidth = RiverType), color = "steelblue3", alpha = 0.4, key_glyph = "path", show.legend = FALSE)+

  geom_sf(data = Basin_Fitzroy2Coast_GrdWtr_LaGr_dsvl, aes(color = "Study Area"), fill = NA, alpha = 0.9, lwd = 0.8)+
  # geom_sf(data = Basin_Fitzroy2Coast_GrdWtr_LaGr_dsvl_50k, color = "red4", linetype = "dashed", fill = NA, alpha = 1, lwd = 0.8)+
  
  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, aes(color = "Urban"), size = 1.5)+
  
  geom_sf(data = STE1, fill = "grey50", color = NA, alpha = 0.2)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.2)+

  scale_fill_manual(values = hcl.colors(6, palette = "Set2"), name = NULL)+
  scale_linewidth_manual(values = c("Major river" = 0.3, "Minor river" = 0.2, "Tributary" = 0.1), name = NULL)+
  scale_color_manual(values = c("Urban" = "black", "Study Area" = "red3"), name = NULL)+
  # # scale_linetype_manual(values = c("Basin boundary" = "dotdash"), name = NULL)+
  # # Basin labels
  # # geom_shadowtext(data = Basin_Fitzroy_grt_label, aes(x = x, y = y , label = basin_name2), size = 5.5, color = "grey10", bg.color = "white",  bg.r = 0.1)+
  # # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(1, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "br", width_hint = 0.3, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = c(0.15, 0.45), legend.text = element_text(size = 16))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
   coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
Basin_Aquifer_wInset_map  <- Basin_Aquifer_map + inset_element(STE_SA_plot, left = 0.01, bottom = 0.65, right = 0.35, top = 0.95)
ggsave(file.path("output", "figures", "Basin_Aquifer_map.png"), Basin_Aquifer_map, width = 11, height = 9, dpi = 300, bg = "transparent")
ggsave(file.path("output", "figures", "Basin_Aquifer_wInset_map.png"), Basin_Aquifer_wInset_map, width = 11, height = 9, dpi = 300, bg = "transparent")

Basin_Aquifer_map_Urb_Map <- Basin_Aquifer_map +
  geom_text_repel(data = WK_urb_sel_pt %>% st_drop_geometry(), aes(x = x, y = y, label = UCL_NAME21), size = 5, color = "grey10", bg.color = "white", bg.r = 0.1, alpha = 0.9)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Basin_Aquifer_Urb_Map.png"), Basin_Aquifer_map_Urb_Map, width = 11, height = 9, dpi = 300, bg = "transparent")

Basin_Aquifer_50k_map <- Basin_Aquifer_map +
  geom_sf(data = Basin_Fitzroy2Coast_GrdWtr_LaGr_dsvl_50k, color = "red4", linetype = "dashed", fill = NA, alpha = 1, lwd = 0.8)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Basin_Aquifer_50k_map.png"), Basin_Aquifer_50k_map, width = 11, height = 9, dpi = 300, bg = "transparent")


## Bioregions ----
IBRA_map <- ggplot()+
  # State boundary grey background
  geom_sf(data = STE1, fill = "grey60", color = NA)+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+
  
  # IBRA background
  geom_sf(data = IBRA_WK_WA, aes(fill = REG_NAME_7), color = NA, alpha = 0.5)+
  
  # IBRA boundaries
  geom_sf(data = IBRA_WK_WA, aes(linetype = "Subregion boundary"),  fill = NA, color = "grey10", lwd = 0.2)+

  scale_linetype_manual(values = c("Subregion boundary" = "solid"), name = NULL)+

  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, colour = "red3", size = 1.5)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.5)+
  
  geom_sf(data = Basin_Fitzroy, fill = "grey30", color = NA, alpha = 0.2)+
  
  scale_fill_manual(values = brewer.pal(10, name = "Set3")[c(1,2,4,5,6)], name = "Bioregions")+
  # scale_fill_manual
  # IBRA labels
  # geom_shadowtext(data = IBRA_WK_label, aes(x = x, y = y , label = REG_NAME_7), size = 5.5, color = "grey10", bg.color = "white",  bg.r = 0.05)+
  
  # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", height = unit(3, "cm"), width = unit(3, "cm"), pad_y = unit(1, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "br", width_hint = 0.4, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = c(0.15, 0.8), legend.text = element_text(size = 16), legend.title = element_text(size = 16))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "IBRA_map.png"),  IBRA_map, width = 11, height = 9, dpi = 300, bg = "transparent")

IBRA_map_option1 <- IBRA_map +
  geom_sf(data = IBRA_Fitzroy_WA_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "IBRA_map_option1.png"), IBRA_map_option1, width = 11, height = 9, dpi = 300, bg = "transparent")

IBRA_map_option2 <- IBRA_map +
  geom_sf(data = IBRA_Fitzroy_WA_dsvl, fill = NA, color = "red4", lwd = 0.5, linetype = "dotted")+
  geom_sf(data = IBRA_WK_WA_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "IBRA_map_option2.png"), IBRA_map_option2, width = 11, height = 9, dpi = 300, bg = "transparent")

# ABS Indegenous areas map ----
ABS_IARE_map <- ggplot()+
  # State boundary grey background
  geom_sf(data = STE1, fill = "grey60", color = NA)+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+

  # ABS IARE background
  geom_sf(data = ABS_IREG_WK, aes(fill = IREG_NAME_2021), color = NA, alpha = 0.5)+
  scale_fill_manual(values = c(brewer.pal(12, name = "Set3"), brewer.pal(6, name = "Set2")), name = "Indigenous regions")+
  
  # ABS ILOC boundaries
  geom_sf(data = ABS_ILOC, aes(linetype = "Indigenous locations"), fill = NA, color = "grey10", lwd = 0.2)+
  scale_linetype_manual(values = c("Indigenous locations" = "solid"), name = NULL)+

  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, colour = "red3", size = 1.5)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.5)+
  geom_sf(data = Basin_Fitzroy, fill = "grey30", color = NA, alpha = 0.2)+

  # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(1, "cm"), pad_x = unit(2, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.4, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = c(0.15, 0.7), legend.text = element_text(size = 16), legend.title = element_text(size = 16))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "ABS_IARE_map.png"),  ABS_IARE_map, width = 11, height = 9, dpi = 300, bg = "transparent")

ABS_IARE_map_option1 <- ABS_IARE_map +
  geom_sf(data = ABS_IARE_WK_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "ABS_IARE_map_option1.png"), ABS_IARE_map_option1, width = 11, height = 9, dpi = 300, bg = "transparent")

ABS_IARE_map_option2 <- ABS_IARE_map +
  geom_sf(data = ABS_IARE_WK_dsvl, fill = NA, color = "red4", lwd = 0.5, linetype = "dotted")+
  geom_sf(data = ABS_IREG_WK_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "ABS_IARE_map_option2.png"), ABS_IARE_map_option2, width = 11, height = 9, dpi = 300, bg = "transparent")


## Native Title map ----
Native_Title_map <- ggplot()+
  # State boundary grey background
  geom_sf(data = STE1, fill = "grey60", color = NA)+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+

  # Native title background
  geom_sf(data = NT_Detm_WK, aes(fill = "Native title"), color = "grey50", lwd = 0.1, alpha = 0.5)+
  scale_fill_manual(values = c("Native title" = "goldenrod"), name = NULL)+

  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, colour = "red3", size = 1.5)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.5)+
  
  geom_sf(data = Basin_Fitzroy, fill = "grey30", color = NA, alpha = 0.2)+
  
  # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "tl", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(1, "cm"), pad_x = unit(2, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "tl", width_hint = 0.4, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = c(0.15, 0.6), legend.text = element_text(size = 12), legend.title = element_text(size = 12))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Native_Title_map.png"),  Native_Title_map, width = 11, height = 9, dpi = 300, bg = "transparent")

Native_Title_map_option1 <- Native_Title_map +
  geom_sf(data = NT_Detm_Fitzroy_dsvl1, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Native_Title_map_option1.png"), Native_Title_map_option1, width = 11, height = 9, dpi = 300, bg = "transparent")

Native_Title_map_option2 <- Native_Title_map +
  geom_sf(data = NT_Detm_Fitzroy_dsvl2, fill = NA, color = "red4", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Native_Title_map_option2.png"), Native_Title_map_option2, width = 11, height = 9, dpi = 300, bg = "transparent")


## Tenure map ----
Tenure_map <- ggplot()+
  # State boundary grey background
  geom_sf(data = STE1, fill = "grey60", color = NA)+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+

  # Tenure background
  geom_sf(data = AUSTEN_WK_ABS_IREG, aes(fill = L3_DESC), color = NA, alpha = 0.5)+
  scale_fill_manual(values = L3_sym_pal_vec, name = "Tenure class (L3)")+

  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, colour = "red3", size = 1.5)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.5)+
  
  geom_sf(data = Basin_Fitzroy, fill = "grey30", color = NA, alpha = 0.2)+
  # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "tl", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(1, "cm"), pad_x = unit(2, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "tl", width_hint = 0.4, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = c(0.15, 0.6), legend.text = element_text(size = 12), legend.title = element_text(size = 12))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Tenure_map.png"),  Tenure_map, width = 11, height = 9, dpi = 300, bg = "transparent")

Tenure_map_option1 <- Tenure_map+
  geom_sf(data = AUSTEN_WK_ABS_IREG_ConcaveH, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Tenure_map_option1.png"), Tenure_map_option1, width = 11, height = 9, dpi = 300, bg = "transparent")

# Property map ----
Prop_map <- ggplot()+
  # State boundary grey background
  geom_sf(data = STE1, fill = "grey60", color = NA)+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+
  
  # Tenure background
  geom_sf(data = CPE_Prop_ABS_IREG_WK, aes(fill = property_type), color = "grey40", alpha = 0.5, lwd = 0.06)+
  # scale_fill_manual(values = brewer.pal(10, name = "Set3")[c(1,2,4,5,6,7,8,9,10,11)], name = "Property type")+
  scale_fill_manual(values = brewer.pal(12, name = "Set3")[c(5, 3, 11, 6, 7, 9, 1, 2, 4, 8)], name = "Property type")+
  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, colour = "red3", size = 1.5)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.5)+
  
  geom_sf(data = Basin_Fitzroy, fill = "grey30", color = NA, alpha = 0.2)+
  
  # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "tl", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(1, "cm"), pad_x = unit(2, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "tl", width_hint = 0.4, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = c(0.14, 0.6), legend.text = element_text(size = 16), legend.title = element_text(size = 16))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Prop_map.png"),  Prop_map, width = 11, height = 9, dpi = 300, bg = "transparent")

Prop_map_option1 <- Prop_map+
  geom_sf(data = CPE_Prop_Fitzroy_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Prop_map_option1.png"), Prop_map_option1, width = 11, height = 9, dpi = 300, bg = "transparent")

Prop_map_option2 <- Prop_map+
  geom_sf(data = CPE_Prop_Fitzroy_dsvl, fill = NA, color = "red4", lwd = 0.5, linetype = "dotted")+
  geom_sf(data = CPE_Prop_ABS_IREG_WK_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Prop_map_option2.png"), Prop_map_option2, width = 11, height = 9, dpi = 300, bg = "transparent")

# Language
Lang_map <- ggplot()+
  # State boundary grey background
  geom_sf(data = STE1, fill = "grey60", color = NA)+
  geom_sf(data = STE_WA, fill = "grey90", color = NA)+
  
  # Tenure background
  geom_sf(data = LANGUAGES_WK, aes(fill = Name2), color = NA, alpha = 0.3)+
  scale_fill_manual(values = LANGUAGES_pal, name = "Language regions",guide = guide_legend(ncol = 3))+
  # Urban areas points
  geom_sf(data = WK_urb_sel_pt, colour = "red3", size = 1.5)+
  geom_sf(data = STE, fill = NA, color = "grey10", lwd = 0.5)+
  
  geom_sf(data = Basin_Fitzroy, fill = "grey30", color = NA, alpha = 0.2)+
  geom_text_repel(data = LANGUAGES_WK %>% st_drop_geometry(), aes(x = x, y = y, label = Name2), size = 3.5, color = "grey10", bg.color = "white", bg.r = 0.1)+
  
  # North arrow and scale bar
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), pad_y = unit(1, "cm"), pad_x = unit(2, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c("black", "white"), text_size = 16, line_width = 2, line_col = "black", text_col = "black"))+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.4, line_width = 2, pad_x = unit(0.5, "cm"), text_cex = 1.2)+

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(legend.position = "none", legend.text = element_text(size = 9), legend.title = element_text(size = 9))+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Lang_map.png"),  Lang_map, width = 11, height = 9, dpi = 300, bg = "transparent")

Lang_map_option1 <- Lang_map+
  geom_sf(data = LANGUAGES_FITZROY_dsvl, fill = NA, color = "red3", lwd = 1, linetype = "solid")+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "Lang_map_option1.png"), Lang_map_option1, width = 11, height = 9, dpi = 300, bg = "transparent")


# WK_PLOTS <- (Basin_map | National_Heritage_WK_map)/ (IBRA_map| WK_map) + plot_layout(guides = "collect")
# ggsave(file.path("output", "figures", "WK_PLOTS.png"), WK_PLOTS, width = 22, height = 22, dpi = 300, bg = "transparent")


## Study area boundary over other maps ----

SA_WK_map <- WK_map+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_dsvl, fill = NA, color = "red3", lwd = 0.8)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "SA_WK_map.png"), SA_WK_map, width = 11, height = 9, dpi = 300, bg = "transparent")

SA_National_Heritage_WK_map <- National_Heritage_WK_map+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_dsvl, fill = NA, color = "red3", lwd = 0.8)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "SA_National_Heritage_WK_map.png"), SA_National_Heritage_WK_map, width = 11, height = 9, dpi = 300, bg = "transparent")

SA_IBRA_map <- IBRA_map+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_dsvl, fill = NA, color = "red3", lwd = 0.8)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "SA_IBRA_map.png"), SA_IBRA_map, width = 11, height = 9, dpi = 300, bg = "transparent")


SA_Basin_map <- Basin_map+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_dsvl, fill = NA, color = "red3", lwd = 0.8)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE) 
ggsave(file.path("output", "figures", "SA_Basin_map.png"), SA_Basin_map, width = 11, height = 9, dpi = 300, bg = "transparent")


SA_Aquifer_map <- Aquifer_map+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_dsvl, fill = NA, color = "red3", lwd = 0.8)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "SA_Aquifer_map.png"), SA_Aquifer_map, width = 11, height = 9, dpi = 300, bg = "transparent")

SA_Language_map <- Lang_map+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_dsvl, fill = NA, color = "red3", lwd = 0.8)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "SA_Language_map.png"), SA_Language_map, width = 11, height = 9, dpi = 300, bg = "transparent")


SA_Native_Title_map <- Native_Title_map+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_dsvl, fill = NA, color = "red3", lwd = 0.8)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "SA_Native_Title_map.png"), SA_Native_Title_map, width = 11, height = 9, dpi = 300, bg = "transparent")

SA_Tenure_map <- Tenure_map+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_dsvl, fill = NA, color = "red3", lwd = 0.8)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "SA_Tenure_map.png"), SA_Tenure_map, width = 11, height = 9, dpi = 300, bg = "transparent")

SA_Property_map <- Prop_map+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_dsvl, fill = NA, color = "red3", lwd = 0.8)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "SA_Property_map.png"), SA_Property_map, width = 11, height = 9, dpi = 300, bg = "transparent")

SA_ABS_IARE_map <- ABS_IARE_map+
  geom_sf(data = Aus_SEA_Fitzroy2Coast2_dsvl, fill = NA, color = "red3", lwd = 0.8)+
  coord_sf(xlim = st_bbox(BBOX)[c(1,3)], ylim = st_bbox(BBOX)[c(2,4)], expand = FALSE)
ggsave(file.path("output", "figures", "SA_ABS_IARE_map.png"), SA_ABS_IARE_map, width = 11, height = 9, dpi = 300, bg = "transparent")

###############################################

# test colour palette ----

library(scales)
library(viridis)
colors <- hcl.colors(8, palette = "Reds 3", rev = TRUE)

colors <- palette.colors(n = 8)[c(2,4,6,8)]
colors <- hcl.colors(palette = "Grays", n = 9)[2:5]
colors <- c(brewer.pal(12, name = "Set3"), brewer.pal(5, name = "Set2"))
show_col(colors)
show_col(brewer.pal(12, name = "Set3")[c(5, 6, 11, 3, 7, 9, 1, 2, 4, 8)])
colors <- hcl.colors(8, palette = "Purples 3", rev = TRUE)
barplot(rep(1, length(colors)), col = colors, border = NA, space = 0)

df <- expand.grid(x = 1:10, y = 1:10)
df$z <- df$x + df$y  # Example gradient values

# Define colors
colors <- hcl.colors(8, palette = "viridis", rev = FALSE)
# colors <- viridis(8, option = "magma")
# Plot using scale_fill_gradientn
ggplot(df, aes(x, y, fill = z)) +
  geom_tile() +
  scale_fill_gradientn(colors = colors) +
  theme_minimal()