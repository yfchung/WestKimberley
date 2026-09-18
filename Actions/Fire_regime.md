# Fire regime management


- [<span class="toc-section-number">1</span> Overview](#overview)
- [<span class="toc-section-number">2</span> Establish burnable
  area](#establish-burnable-area)
- [<span class="toc-section-number">3</span> Establish area of action by
  ground and aerial fire
  management](#establish-area-of-action-by-ground-and-aerial-fire-management)
- [<span class="toc-section-number">4</span> Estimate pre-action cost of
  fire management
  actions](#estimate-pre-action-cost-of-fire-management-actions)
- [<span class="toc-section-number">5</span> Management action 1 -
  Fuel-reduced buffer
  establishment](#management-action-1---fuel-reduced-buffer-establishment)
  - [<span class="toc-section-number">5.1</span> Labour
    requirements](#labour-requirements)
  - [<span class="toc-section-number">5.2</span> Vehicle
    requirements](#vehicle-requirements)
  - [<span class="toc-section-number">5.3</span> Consumables and ground
    support](#consumables-and-ground-support)
  - [<span class="toc-section-number">5.4</span> Equipment](#equipment)
- [<span class="toc-section-number">6</span> Management action 2 -
  Aerial burning](#management-action-2---aerial-burning)
- [<span class="toc-section-number">7</span> Management action 3 -
  On-ground burning](#management-action-3---on-ground-burning)
- [<span class="toc-section-number">8</span> Post-action monitoring
  (Aerial and on-ground) and
  evaluation](#post-action-monitoring-aerial-and-on-ground-and-evaluation)
- [<span class="toc-section-number">9</span> Travel to management
  sites](#travel-to-management-sites)
- [<span class="toc-section-number">10</span> Annualise and total
  combine costs](#annualise-and-total-combine-costs)

``` r
library(sf)
library(gt)
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
library(tictoc)
terraOptions(progress = 0)

INPUT_DIR <- file.path("input")
INPUT_SPA_DIR <- file.path("input/Spatial_data")
OUTPUT_DIR <- file.path("output")
OUTPUT_SPA_DIR <- file.path("output/Spatial_data")

source("Actions/functions.R")
# sum(file.info(terra::tmpFiles())$size) / 1e9

# General base layers ----
## State boundary
STE <- st_read(file.path(INPUT_SPA_DIR, "STE_2021_AUST_SHP_GDA2020", "STE_2021_AUST_GDA2020.shp"))
STE_WA <- STE %>% filter(STE_NAME21 == "Western Australia") %>% st_make_valid() %>% st_as_sf()

# Define BBOX
BBOX <- st_bbox(c(xmin = 119.2661, xmax = 129.1015, ymin = -21.33202, ymax = -13.58869), crs = st_crs(STE)) %>% st_as_sfc() %>% st_make_valid()

# Study area boundary
StudyArea_SF <- st_read(file.path(OUTPUT_DIR, "data", "Fitzroy2Coast_Aus_SEA_dsvl.gpkg"))

# Study area Digital elevation model (DEM) for raster template
DEM_SA <- rast(file.path(OUTPUT_SPA_DIR, "DEM_SA.tif"))
DEM_SA_GdaMga <- if(file.exists(file.path(OUTPUT_SPA_DIR, "DEM_SA_GdaMga.tif"))) {
  rast(file.path(OUTPUT_SPA_DIR, "DEM_SA_GdaMga.tif"))
} else {
  terra::project(DEM_SA, "EPSG:7851", method = "bilinear", filename = file.path(OUTPUT_SPA_DIR, "DEM_SA_GdaMga.tif"), overwrite = TRUE)
}

# Create raster template with same resolution and crs as DEM_SA but with BBOX extent
BBOX_GDA_rast <- if(file.exists(file.path(OUTPUT_SPA_DIR, "BBOX_GDA_rast.tif"))){
  rast(file.path(OUTPUT_SPA_DIR, "BBOX_GDA_rast.tif"))
} else {
  extend(DEM_SA, ext(vect(BBOX)))
  values(BBOX_GDA_rast) <- NA
  varnames(BBOX_GDA_rast) <- ""
  terra::writeRaster(BBOX_GDA_rast, file.path(OUTPUT_SPA_DIR, "BBOX_GDA_rast.tif"), overwrite = TRUE)
}

BBOX_GdaMga_rast <- if(file.exists(file.path(OUTPUT_SPA_DIR, "BBOX_GdaMga_rast.tif"))){
  rast(file.path(OUTPUT_SPA_DIR, "BBOX_GdaMga_rast.tif"))
} else {
  terra::project(BBOX_GDA_rast, "EPSG:7851", threads = TRUE, method = "near", filename = file.path(OUTPUT_SPA_DIR, "BBOX_GdaMga_rast.tif"), overwrite = TRUE)
}
```

# Overview

This document is to estimate cost of the fire regime management actions
in the West Kimberley region. The cost estimation is largely based on
methods developed by [Yong et
al. 2023](https://doi.org/10.1111/1365-2664.14377). We adopted the most
of the assumptions and formula while updating the calculations using
pre-European vegetation mapping of Western Australia dataset, 1
arc-second (equivalent to approximately 30 m) digital elevation model
(DEM) and regional road network data. Using these updated datasets, we
estimated the cost of fire management actions in the West Kimberley
region based on ground and aerial fire management. The total cost of
fire management actions in each pixel of the study area was calculated
as the sum of pre- and post- action planning and surveys, management
actions, equipment and travel costs. The cost includes 4% annual
discount rate, 30 years implementation period, 30% overhead cost and 10%
field contingency cost.

$$C_{\text{total, j}} = C_{\text{PrePos, j}} + C_{\text{Treatment, j}} + C_{\text{Equipment, j}} + C_{\text{Travel, j}}$$

where $C_{\text{total, j}}$ is the total cost of fire management actions
in pixel $j$, $C_{\text{PrePos, j}}$ is the cost of pre- and post-
action planning and surveys, $C_{\text{Treatment, j}}$ is the cost of
management actions, $C_{\text{Equipment, j}}$ is the cost of equipment,
and $C_{\text{Travel, j}}$ is the cost of travel.

# Establish burnable area

We used the Pre-European vegetation mapping of Western Australia
[dataset](https://catalogue.data.wa.gov.au/dataset/pre-european-dpird-006)
to identify vegetation types requiring fire management. We matched these
vegetation types to the fire-regime classifications of [Enright and
Thomas (2008)](https://doi.org/10.1111/j.1749-8198.2008.00126.x) to
estimate an appropriate fire interval for each vegetation type. We then
used the assumed fire interval to estimate the annual extent of
prescribed burning, expressed as the proportion of each vegetation type
requiring treatment in a given year. The vegetation-specific
fire-management assumptions are summarised in
<a href="#tbl-veg-fire" class="quarto-xref">Table 1</a>, while land-use
categories (based on [Catchment Scale Land Use of Australia
v2](https://www.agriculture.gov.au/abares/aclump/land-use/catchment-scale-land-use-and-commodities-update-2023))
considered unsuitable for prescribed burning are shown in
<a href="#tbl-clum-fire" class="quarto-xref">Table 2</a>.

``` r
# Read in pre-European vegetation layer and intersect with study area
PreVeg <- st_read(file.path(INPUT_SPA_DIR, "Pre_European_Vegetation_DPIRD_006_WA_GDA2020_Public_FileGeodatabase", "Pre_European_Vegetation_DPIRD_006_WA_GDA2020_Public.gdb"), 
                  layer = "Pre_European_Vegetation_DPIRD_006")
PreVeg_WK <- st_intersection(PreVeg, StudyArea_SF) %>% st_make_valid() %>% st_as_sf() %>% mutate(ID = row_number())

# Read in Catchment scale land use of Australia and commodities – Update December 2023
CLUM_Aus <- rast(file.path(INPUT_SPA_DIR, "clum_50m_2023_v2", "clum_50m_2023_v2.tif"))
#"C:\Users\uqychun7\Documents\Data\WestKimberley\input\Spatial_data\clum_50m_2023_v2\clum_50m_2023_v2.tif.vat.dbf"
CLUM_Aus_cat_DF <- foreign::read.dbf(
  file.path(INPUT_SPA_DIR, "clum_50m_2023_v2", "clum_50m_2023_v2.tif.vat.dbf"))%>% 
  as.data.frame()

CLUM_WK <- terra::crop(CLUM_Aus, vect(StudyArea_SF %>% st_transform(crs = st_crs(CLUM_Aus))), snap = "out", mask = TRUE) %>% 
  terra::project(crs(DEM_SA), method = "near")
CLUM_WK_FREQ <- freq(CLUM_WK) %>% as.data.frame() 
CLUM_WK_Burnable <- left_join(CLUM_WK_FREQ, CLUM_Aus_cat_DF, by = c("value" = "TERTV8")) %>% 
  # define land use that is not burnable
  # Not burnable: 4 = Irrigated ag, 5 = Intensive, 6 = water
  # Except for 4.6.0 Irrigated land in transition, 5.8.4 Extractive industry not in use
  mutate(Burnable = ifelse(PRIMV8N %in% c(4, 5, 6), 0 , 1), 
         Burnable = ifelse(LUV8N %in% c(460, 584), 1, Burnable)) 
CLUM_WK_Burnable_mt <- CLUM_WK_Burnable %>% select(Value, Burnable) %>% as.matrix()
CLUM_Burnable_rast <- classify(CLUM_WK, CLUM_WK_Burnable_mt) %>% 
  resample(DEM_SA, method = "near", threads = TRUE) %>% 
  crop(DEM_SA, mask = TRUE)


# Read in Yong et al. 2022 fire regime data
YongFireDf <- read_xlsx(file.path(INPUT_DIR, "Data_Tables", "Yong 2022 broad pre-European fire regimes table.xlsx"))

VegFire_df <- data.frame(struct_des = unique(PreVeg_WK$struct_des) %>% sort()) %>% 
  mutate(EnrichYongCat = case_when(
    struct_des %in% c("Grasslands, curly spinifex savanna woodland or low trees",
                      "Curly spinifex low tree savanna / Sparse low tree-steppe",
                      "Curly spinifex or short grass low tree savanna / Grass-steppe") ~ "Semi-arid spinifex grasslands",
    struct_des %in% c("Shrub-steppe", "Thicket", "Grass-steppe", "Low tree-steppe",
                      "Sparse low tree-steppe",
                      "Tree-and-shrub-steppe", "Scrub, open scrub or sparse scrub") ~ "Shrubland",
    struct_des %in% c("Grasslands, tall bunch-grass savanna woodland",
                      "Grasslands, short bunch-grass low-tree savanna",
                      "Grasslands, tall bunch-grass savanna",
                      "Grasslands, tall bunch-grass low-tree savanna",
                      "Grasslands, tall bunch-grass open savanna woodland",
                      "Grasslands, tall bunch- grass open savanna woodland",
                      "Grasslands, short bunch-grass savanna",
                      "Grasslands, high grass savanna woodland on basalt",
                      "Grasslands, high grass savanna woodland on sandstone",
                      "Pindan / Tall bunch-grass savanna with low trees",
                      "Short bunch-grass low tree savanna / Tree-steppe",
                      "High grass savanna woodland / Curly spinifex savanna") ~ "Tropical savanna",
    struct_des %in% c("Tidal mud flat",
                      "Salt lake, lagoon, clay pan",
                      "Freshwater lake",
                      "Mangroves",
                      "Dune sand") ~ "Exclude / no burn",
    struct_des %in% c("Woodland other") ~ "Dry sclerophyll forest (NB2) (low rates of litter and near surface live fuel accumulation)",
    struct_des %in% c("Pindan woodland",
                      "Pindan with low trees") ~ "unmatched",
                      .default = "unmatched")) %>% 
  left_join(YongFireDf, by = c("EnrichYongCat" = "Vegetation type")) %>% 
  mutate("fuel load" = case_when(EnrichYongCat == "Exclude / no burn" ~ NA_character_,
                                 struct_des %in% c("Pindan woodland", "Pindan with low trees") ~ "Moderate to High",
                                 .default = `fuel load`),
          "natural fire interval" = case_when(EnrichYongCat == "Exclude / no burn" ~ ">300 years (very long)",
                                        struct_des %in% c("Pindan woodland", "Pindan with low trees") ~ "4-7 years",
                                        .default = `natural fire interval`),
          "Assumed fire interval" = case_when(EnrichYongCat == "Exclude / no burn" ~ NA_real_,
                                                        struct_des %in% c("Pindan woodland", "Pindan with low trees") ~ 6.5,
                                                        .default = `Assumed fire interval`),
          "Assumed Extent for costings" = case_when(EnrichYongCat == "Exclude / no burn" ~ 0,
                                                    struct_des %in% c("Pindan woodland", "Pindan with low trees") ~ 1/6.5,
                                                    .default = `Assumed Extent for costings`),
          "managed fire interval" = case_when(EnrichYongCat == "Exclude / no burn" ~ "no burn",
                                              struct_des %in% c("Pindan woodland", "Pindan with low trees") ~ "5-8 years",
                                              .default = `managed fire interval`))

# VegFire_df %>% filter(EnrichYongCat == "unmatched")

PreVeg_WK <- PreVeg_WK %>% left_join(VegFire_df, by = c("struct_des" = "struct_des"))

FireExtent_rast <- rasterize(vect(PreVeg_WK), DEM_SA, field = "Assumed Extent for costings", background = NA, touches = TRUE)
names(FireExtent_rast) <- "prop_yr_burn"

# Exclude areas that are not burnable based on CLUM land use data
FireExtent_rast <- FireExtent_rast * CLUM_Burnable_rast
```

``` r
VegFire_df %>% 
  select("Vegetation structure" = struct_des, 
    "Enright & Yong Category" = EnrichYongCat, 
    "Natural fire interval" = `natural fire interval`, 
    "Assumed fire interval" = `Assumed fire interval`, 
    "Annual treatment proportion" = `Assumed Extent for costings`) %>%
  mutate(`Annual treatment proportion` = round(`Annual treatment proportion`, 2)) %>%
  arrange(`Vegetation structure`) %>%
  gt::gt() %>% 
  gt::tab_options(table.font.size = gt::px(14), data_row.padding = gt::px(4))
```

<div id="tbl-veg-fire">

Table 1: Fire-management assumptions based on pre-European vegetation
mapping of Western Australia. The assumed fire interval is used to
estimate the annual treatment proportion for costings.

<div class="cell-output-display">

<div id="mcupdczjsa" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mcupdczjsa table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#mcupdczjsa thead, #mcupdczjsa tbody, #mcupdczjsa tfoot, #mcupdczjsa tr, #mcupdczjsa td, #mcupdczjsa th {
  border-style: none;
}
&#10;#mcupdczjsa p {
  margin: 0;
  padding: 0;
}
&#10;#mcupdczjsa .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#mcupdczjsa .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#mcupdczjsa .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#mcupdczjsa .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#mcupdczjsa .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#mcupdczjsa .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mcupdczjsa .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#mcupdczjsa .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#mcupdczjsa .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#mcupdczjsa .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#mcupdczjsa .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#mcupdczjsa .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#mcupdczjsa .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#mcupdczjsa .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#mcupdczjsa .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#mcupdczjsa .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#mcupdczjsa .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#mcupdczjsa .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#mcupdczjsa .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mcupdczjsa .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#mcupdczjsa .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#mcupdczjsa .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#mcupdczjsa .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mcupdczjsa .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#mcupdczjsa .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#mcupdczjsa .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mcupdczjsa .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mcupdczjsa .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#mcupdczjsa .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#mcupdczjsa .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#mcupdczjsa .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mcupdczjsa .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#mcupdczjsa .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mcupdczjsa .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#mcupdczjsa .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mcupdczjsa .gt_left {
  text-align: left;
}
&#10;#mcupdczjsa .gt_center {
  text-align: center;
}
&#10;#mcupdczjsa .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#mcupdczjsa .gt_font_normal {
  font-weight: normal;
}
&#10;#mcupdczjsa .gt_font_bold {
  font-weight: bold;
}
&#10;#mcupdczjsa .gt_font_italic {
  font-style: italic;
}
&#10;#mcupdczjsa .gt_super {
  font-size: 65%;
}
&#10;#mcupdczjsa .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#mcupdczjsa .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#mcupdczjsa .gt_indent_1 {
  text-indent: 5px;
}
&#10;#mcupdczjsa .gt_indent_2 {
  text-indent: 10px;
}
&#10;#mcupdczjsa .gt_indent_3 {
  text-indent: 15px;
}
&#10;#mcupdczjsa .gt_indent_4 {
  text-indent: 20px;
}
&#10;#mcupdczjsa .gt_indent_5 {
  text-indent: 25px;
}
&#10;#mcupdczjsa .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#mcupdczjsa div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Vegetation structure | Enright & Yong Category | Natural fire interval | Assumed fire interval | Annual treatment proportion |
|----|----|----|----|----|
| Curly spinifex low tree savanna / Sparse low tree-steppe | Semi-arid spinifex grasslands | 10-30 years (intermediate - long) rainfall-spinifex biomass event driven | 20.0 | 0.05 |
| Curly spinifex or short grass low tree savanna / Grass-steppe | Semi-arid spinifex grasslands | 10-30 years (intermediate - long) rainfall-spinifex biomass event driven | 20.0 | 0.05 |
| Dune sand | Exclude / no burn | \>300 years (very long) | NA | 0.00 |
| Freshwater lake | Exclude / no burn | \>300 years (very long) | NA | 0.00 |
| Grass-steppe | Shrubland | 10-30 years (Short - intermediate) | 20.0 | 0.05 |
| Grasslands, curly spinifex savanna woodland or low trees | Semi-arid spinifex grasslands | 10-30 years (intermediate - long) rainfall-spinifex biomass event driven | 20.0 | 0.05 |
| Grasslands, high grass savanna woodland on basalt | Tropical savanna | 2-4 years (very short) | 3.0 | 0.33 |
| Grasslands, high grass savanna woodland on sandstone | Tropical savanna | 2-4 years (very short) | 3.0 | 0.33 |
| Grasslands, short bunch-grass low-tree savanna | Tropical savanna | 2-4 years (very short) | 3.0 | 0.33 |
| Grasslands, short bunch-grass savanna | Tropical savanna | 2-4 years (very short) | 3.0 | 0.33 |
| Grasslands, tall bunch- grass open savanna woodland | Tropical savanna | 2-4 years (very short) | 3.0 | 0.33 |
| Grasslands, tall bunch-grass low-tree savanna | Tropical savanna | 2-4 years (very short) | 3.0 | 0.33 |
| Grasslands, tall bunch-grass savanna | Tropical savanna | 2-4 years (very short) | 3.0 | 0.33 |
| Grasslands, tall bunch-grass savanna woodland | Tropical savanna | 2-4 years (very short) | 3.0 | 0.33 |
| High grass savanna woodland / Curly spinifex savanna | Tropical savanna | 2-4 years (very short) | 3.0 | 0.33 |
| Low tree-steppe | Shrubland | 10-30 years (Short - intermediate) | 20.0 | 0.05 |
| Mangroves | Exclude / no burn | \>300 years (very long) | NA | 0.00 |
| Pindan / Tall bunch-grass savanna with low trees | Tropical savanna | 2-4 years (very short) | 3.0 | 0.33 |
| Pindan with low trees | unmatched | 4-7 years | 6.5 | 0.15 |
| Pindan woodland | unmatched | 4-7 years | 6.5 | 0.15 |
| Salt lake, lagoon, clay pan | Exclude / no burn | \>300 years (very long) | NA | 0.00 |
| Scrub, open scrub or sparse scrub | Shrubland | 10-30 years (Short - intermediate) | 20.0 | 0.05 |
| Short bunch-grass low tree savanna / Tree-steppe | Tropical savanna | 2-4 years (very short) | 3.0 | 0.33 |
| Shrub-steppe | Shrubland | 10-30 years (Short - intermediate) | 20.0 | 0.05 |
| Sparse low tree-steppe | Shrubland | 10-30 years (Short - intermediate) | 20.0 | 0.05 |
| Thicket | Shrubland | 10-30 years (Short - intermediate) | 20.0 | 0.05 |
| Tidal mud flat | Exclude / no burn | \>300 years (very long) | NA | 0.00 |
| Tree-and-shrub-steppe | Shrubland | 10-30 years (Short - intermediate) | 20.0 | 0.05 |
| Woodland other | Dry sclerophyll forest (NB2) (low rates of litter and near surface live fuel accumulation) | 10-20 and 50-80 years (NB4) (Intermediate and Long) (average of 15 and 65) | 40.0 | 0.02 |

</div>

</div>

</div>

``` r
CLUM_WK_Burnable %>% 
  select(PRIMV8, value, Burnable) %>%
  mutate(Burnable = ifelse(Burnable == 1,"Burnable","Not burnable")) %>%
  gt::gt() %>%
  gt::cols_label(PRIMV8 = "CLUM primary land use", 
                 value = "CLUM tertiary land use", 
                 Burnable = "Burnable status") %>%
  gt::cols_align(align = "left") %>%
  gt::tab_options(table.font.size = gt::px(14), data_row.padding = gt::px(4))
```

<div id="tbl-clum-fire">

Table 2: Fire-management assumptions based on the Catchment Scale Land
Use of Australia and Commodities (CLUM) dataset. Land-use categories
assumed to be not burnable are assigned an annual treatment proportion
of 0.

<div class="cell-output-display">

<div id="cbsliiqtsn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#cbsliiqtsn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#cbsliiqtsn thead, #cbsliiqtsn tbody, #cbsliiqtsn tfoot, #cbsliiqtsn tr, #cbsliiqtsn td, #cbsliiqtsn th {
  border-style: none;
}
&#10;#cbsliiqtsn p {
  margin: 0;
  padding: 0;
}
&#10;#cbsliiqtsn .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#cbsliiqtsn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#cbsliiqtsn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#cbsliiqtsn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#cbsliiqtsn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#cbsliiqtsn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cbsliiqtsn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#cbsliiqtsn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#cbsliiqtsn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#cbsliiqtsn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#cbsliiqtsn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#cbsliiqtsn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#cbsliiqtsn .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#cbsliiqtsn .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#cbsliiqtsn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#cbsliiqtsn .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#cbsliiqtsn .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#cbsliiqtsn .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#cbsliiqtsn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cbsliiqtsn .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#cbsliiqtsn .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#cbsliiqtsn .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#cbsliiqtsn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cbsliiqtsn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#cbsliiqtsn .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#cbsliiqtsn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cbsliiqtsn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cbsliiqtsn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#cbsliiqtsn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#cbsliiqtsn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#cbsliiqtsn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cbsliiqtsn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#cbsliiqtsn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cbsliiqtsn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#cbsliiqtsn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cbsliiqtsn .gt_left {
  text-align: left;
}
&#10;#cbsliiqtsn .gt_center {
  text-align: center;
}
&#10;#cbsliiqtsn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#cbsliiqtsn .gt_font_normal {
  font-weight: normal;
}
&#10;#cbsliiqtsn .gt_font_bold {
  font-weight: bold;
}
&#10;#cbsliiqtsn .gt_font_italic {
  font-style: italic;
}
&#10;#cbsliiqtsn .gt_super {
  font-size: 65%;
}
&#10;#cbsliiqtsn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#cbsliiqtsn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#cbsliiqtsn .gt_indent_1 {
  text-indent: 5px;
}
&#10;#cbsliiqtsn .gt_indent_2 {
  text-indent: 10px;
}
&#10;#cbsliiqtsn .gt_indent_3 {
  text-indent: 15px;
}
&#10;#cbsliiqtsn .gt_indent_4 {
  text-indent: 20px;
}
&#10;#cbsliiqtsn .gt_indent_5 {
  text-indent: 25px;
}
&#10;#cbsliiqtsn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#cbsliiqtsn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| CLUM primary land use | CLUM tertiary land use | Burnable status |
|----|----|----|
| 1 Conservation and natural environments | 1.1.1 Strict nature reserves | Burnable |
| 1 Conservation and natural environments | 1.1.3 National park | Burnable |
| 1 Conservation and natural environments | 1.1.5 Habitat/species management area | Burnable |
| 1 Conservation and natural environments | 1.1.6 Protected landscape | Burnable |
| 1 Conservation and natural environments | 1.1.7 Other conserved area | Burnable |
| 1 Conservation and natural environments | 1.2.0 Managed resource protection | Burnable |
| 1 Conservation and natural environments | 1.2.5 Traditional indigenous uses | Burnable |
| 1 Conservation and natural environments | 1.3.0 Other minimal use | Burnable |
| 1 Conservation and natural environments | 1.3.1 Defence land - natural areas | Burnable |
| 1 Conservation and natural environments | 1.3.3 Residual native cover | Burnable |
| 2 Production from relatively natural environments | 2.1.0 Grazing native vegetation | Burnable |
| 3 Production from dryland agriculture and plantations | 3.1.2 Softwood plantation forestry | Burnable |
| 3 Production from dryland agriculture and plantations | 3.6.1 Degraded land | Burnable |
| 4 Production from irrigated agriculture and plantations | 4.2.4 Irrigated sown grasses | Not burnable |
| 4 Production from irrigated agriculture and plantations | 4.3.0 Irrigated cropping | Not burnable |
| 4 Production from irrigated agriculture and plantations | 4.3.9 Irrigated rice | Not burnable |
| 4 Production from irrigated agriculture and plantations | 4.4.1 Irrigated tree fruits | Not burnable |
| 4 Production from irrigated agriculture and plantations | 4.4.8 Irrigated citrus | Not burnable |
| 4 Production from irrigated agriculture and plantations | 4.5.3 Irrigated seasonal vegetables and herbs | Not burnable |
| 4 Production from irrigated agriculture and plantations | 4.6.0 Irrigated land in transition | Burnable |
| 5 Intensive uses | 5.1.1 Production nurseries | Not burnable |
| 5 Intensive uses | 5.2.2 Feedlots | Not burnable |
| 5 Intensive uses | 5.2.5 Aquaculture | Not burnable |
| 5 Intensive uses | 5.2.7 Saleyards/stockyards | Not burnable |
| 5 Intensive uses | 5.3.0 Manufacturing and industrial | Not burnable |
| 5 Intensive uses | 5.3.1 General purpose factory | Not burnable |
| 5 Intensive uses | 5.4.1 Urban residential | Not burnable |
| 5 Intensive uses | 5.4.2 Rural residential with agriculture | Not burnable |
| 5 Intensive uses | 5.5.1 Commercial services | Not burnable |
| 5 Intensive uses | 5.5.2 Public services | Not burnable |
| 5 Intensive uses | 5.5.3 Recreation and culture | Not burnable |
| 5 Intensive uses | 5.6.1 Fuel powered electricity generation | Not burnable |
| 5 Intensive uses | 5.6.7 Water extraction and transmission | Not burnable |
| 5 Intensive uses | 5.7.1 Airports/aerodromes | Not burnable |
| 5 Intensive uses | 5.7.2 Roads | Not burnable |
| 5 Intensive uses | 5.8.0 Mining | Not burnable |
| 5 Intensive uses | 5.8.2 Quarries | Not burnable |
| 5 Intensive uses | 5.8.3 Tailings | Not burnable |
| 5 Intensive uses | 5.8.4 Extractive industry not in use | Burnable |
| 5 Intensive uses | 5.9.0 Waste treatment and disposal | Not burnable |
| 5 Intensive uses | 5.9.1 Effluent pond | Not burnable |
| 5 Intensive uses | 5.9.3 Solid garbage | Not burnable |
| 5 Intensive uses | 5.9.5 Sewage/sewerage | Not burnable |
| 6 Water | 6.1.0 Lake | Not burnable |
| 6 Water | 6.2.1 Reservoir | Not burnable |
| 6 Water | 6.2.2 Water storage - intensive use/farm dams | Not burnable |
| 6 Water | 6.3.0 River | Not burnable |
| 6 Water | 6.5.0 Marsh/wetland | Not burnable |
| 6 Water | 6.6.0 Estuary/coastal waters | Not burnable |

</div>

</div>

</div>

This map shows the annual treatment proportion of the burnable areas in
the West Kimberley region. The areas with higher annual treatment
proportion require more frequent fire management actions, while areas
with lower annual treatment proportion require less frequent actions.

``` r
PreVeg_WK_plot <- PreVeg_WK %>% 
  select(struct_des, `Assumed fire interval`) %>%
  mutate(AssumedFireInt = ifelse(is.na(`Assumed fire interval`), paste0("Not burned"), paste0(`Assumed fire interval`, " years")),
         AssumedFireInt = factor(AssumedFireInt, levels = c("3 years", "6.5 years", "20 years", "40 years", "Not burned")))

ggplot() +
  geom_sf(data = STE_WA, fill = "grey70", color = NA) +
  geom_sf(data = PreVeg_WK_plot, aes(fill = AssumedFireInt), color = NA, alpha = 0.8) +
  scale_fill_manual(values = hcl.colors(5, "Red-Yellow"), name = "Assumed fire interval")+
  geom_sf(data = StudyArea_SF, aes(color = "black"), fill = NA, linewidth = 0.5) +
  scale_color_manual(values = "black", label = "Study area", name = NULL) +
  coord_sf(xlim = st_bbox(StudyArea_SF)[c(1, 3)], ylim = st_bbox(StudyArea_SF)[c(2, 4)]) +
  theme_bw() +
  theme(legend.position = "top")+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))
```

<div id="fig-fire-extent">

![](Fire_regime_files/figure-commonmark/fig-fire-extent-1.png)

Figure 1: Annual treatment proportion of burnable areas in the West
Kimberley region

</div>

# Establish area of action by ground and aerial fire management

For costing purposes, burnable areas within 5 km of the [road
network](https://catalogue.data.wa.gov.au/dataset/mrwa-road-network)
were assigned to ground-based fire management, while burnable areas more
than 5 km from the road network were assigned to aerial fire management.

``` r
Roads <- st_read(file.path(INPUT_SPA_DIR, "Road_Network", "Road_Network.shp")) %>% 
    st_transform(crs = st_crs(STE)) %>% 
    st_crop(StudyArea_SF) %>% 
    filter(NETWORK_TY != "Proposed Road") %>% 
    mutate(GeoType = st_geometry_type(.)) %>%
    filter(GeoType == "LINESTRING")

Road_5kmBuff <- st_buffer(Roads %>% st_transform(crs = 7851), dist = 5000) %>% 
    st_union() %>% 
    st_transform(crs = st_crs(STE)) %>%
    st_make_valid()

Road_5kmBuff_rast <- rasterize(vect(Road_5kmBuff), DEM_SA, field = 1, background = NA, touches = TRUE)

FireMode_rast <- ifel(not.na(Road_5kmBuff_rast), 1, 2) # Assign 1 for ground fire management and 2 for aerial fire management
FireMode_rast[FireExtent_rast == 0] <- NA
FireMode_rast[is.na(FireExtent_rast)] <- NA
FireMode_fac_rast <- as.factor(FireMode_rast)

FireExtent_Ground_rast <- ifel(FireMode_rast == 1, FireExtent_rast, NA)
FireExtent_Aerial_rast <- ifel(FireMode_rast == 2, FireExtent_rast, NA)
```

``` r
ggplot() +
  geom_sf(data = STE_WA, fill = "grey70", color = NA) +
  geom_spatraster(data = FireMode_fac_rast) +
  scale_fill_manual(values = c("1" = "goldenrod4", "2" = "goldenrod2"), 
                    na.value = "transparent", breaks = c("1", "2"),
                    name = "Management mode", labels = c("Ground", "Aerial")) +
  geom_sf(data = StudyArea_SF, aes(color = "black"), fill = NA, linewidth = 0.5) +
  scale_color_manual(values = "black", label = "Study area", name = NULL) +
  coord_sf(xlim = st_bbox(StudyArea_SF)[c(1, 3)], ylim = st_bbox(StudyArea_SF)[c(2, 4)]) +
  theme_bw() +
  theme(legend.position = "top")+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))
```

<div id="fig-fire-mode">

![](Fire_regime_files/figure-commonmark/fig-fire-mode-1.png)

Figure 2: Burnable areas assigned to ground and aerial fire management
in the West Kimberley region

</div>

We calculated the Terrain Ruggedness Index (TRI) (Riley et al., 1999)
using 1 arc second resolution [digital elevation model
(DEM)](https://pid.geoscience.gov.au/dataset/ga/72759) as a proxy for
terrain resistance to ground-based fire management. TRI values were
classified into three ranges (0–31, 31–110, and \>110), corresponding to
high, moderate, and low off-road walking speeds of 4.46, 3.35, and 2.23
km h⁻¹, respectively. These speeds were used to parameterise the ground
travel-cost surface.

``` r
DEM_SA <- rast(file.path(OUTPUT_SPA_DIR, "DEM_SA.tif"))
RES <- c(29.95607, 29.95607)
TRI <- terrain(DEM_SA, v="TRIriley", neighbors=8)
TRI_cat <- classify(TRI, rcl = cbind(c(0, 31, 110), c(31, 110, Inf), c(3, 2, 1)), include.lowest = TRUE, right = FALSE)
```

# Estimate pre-action cost of fire management actions

The pre-action work includes pre-action office planning (e.g. permit
application, organising field work, etc.) and pre-action field survey
(in aerial and on-ground) to determine the area of action. These actions
are estimated to be performed for each 100 km² of management area per
year (before scaling to the actual management area). Rates are:

- Pre-action office planning (PreA_Off): 30 person-days of experienced
  staff time (7.5h working day at \$45 h⁻¹) were assumed per 100 km²
  management area.
- Pre-action field planning (aerial) (PreA_Aer): 500m width flight
  transects with a \$850/hour cost of aircraft hire and pilot.
- Pre-action field planning (on-ground) (PreA_Grd): 2 person walking
  through 30% of the management area by foot at transect width of 500m
  with 10 minutes of activity time spent every km walked.

``` r
# For every 100 km² management area for every year, the pre-action cost
Stnd_Mgnt_A <- 100 # 100 km² is used as the standardised management area for cost estimation

Workday_hr <- 7.5 # Working hours per day
Rate_Wage1_Dhr <- 30 # Rate of wage for entry-level staff ($/h)
Rate_Wage2_Dhr <- 45 # Rate of wage for experienced staff ($/h)
Rate_Wage3_Dhr <- 65 # Rate of wage for highly experienced staff ($/h)

# Aerial pre-action planning
Aer_PreA_Survey_prop <- 1 # Proportion of area to be surveyed in pre-action by flight
Aer_PreA_Transect_km <- 0.5 # Transect width in km for aerial pre-action survey (km)
Aer_workspeed1_kmh <- 130 # Aircraft working speed (km/h)
Rate_Aircraft1_Dhr <- 850 # Aircraft1 hire and pilot cost ($/h)

Dura_PreA_Off_hr <- 30 * Workday_hr # Duration taken for pre-action office planning (hour) for every 100 km² management area
Cost_PreA_Off_D <- Dura_PreA_Off_hr * Rate_Wage2_Dhr # 30 person-days of experienced staff time at $45 h⁻¹

# Duration taken for aerial pre-action survey (hour) for every 100 km² management area
Dura_Aer_PreA_hr <- (Stnd_Mgnt_A * Aer_PreA_Survey_prop) /  
                    Aer_PreA_Transect_km / 
                    Aer_workspeed1_kmh


# Cost of aerial pre-action survey ($) for every 100 km² management area per year
Cost_Aer_PreA_Aircraft_D <- Dura_Aer_PreA_hr * Rate_Aircraft1_Dhr
Cost_Aer_PreA_Lbr_D  <- Dura_Aer_PreA_hr * 1 * Rate_Wage1_Dhr
Cost_Aer_PreA_D <- Cost_Aer_PreA_Aircraft_D + Cost_Aer_PreA_Lbr_D

Grd_PreA_Survey_prop <- 0.30 # Proportion of area to be surveyed in pre-action by ground
Grd_PreA_Transect_km <- 0.5 # Transect width in km for ground pre-action survey (km)
Grd_PreA_Lbr_n <- 2 # Number of personnel for ground pre-action survey
Rate_Grd_PreA_Survey_HrKm <- 10 / 60 # 10 minutes of activity time spent every km walked

Area_Grd_PreA_km2 <- Stnd_Mgnt_A * Grd_PreA_Survey_prop # Area to be surveyed in pre-action by ground (km²)
Dist_Grd_PreA_km <- Area_Grd_PreA_km2 / Grd_PreA_Transect_km # Distance to be walked in pre-action by ground (km)
Dura_Grd_PreA_Survey_hr <- Dist_Grd_PreA_km * Rate_Grd_PreA_Survey_HrKm # Duration of activity time spent in pre-action by ground (hour)

# Walking speed for ground pre-action survey (km/h) based on terrain ruggedness index (TRI)
Grd_walkspeed_kmh <- c(high = 4.46, moderate = 3.345, low = 2.23)

# Total duration of ground pre-action survey (hour) for every 100 km² management area
Dura_Grd_PreA_hr <- (Dura_Grd_PreA_Survey_hr + (Dist_Grd_PreA_km / Grd_walkspeed_kmh)) *
                    Grd_PreA_Lbr_n

# Total cost of ground pre-action survey ($) for every 100 km² management area per year
Cost_Grd_PreA_D <- Dura_Grd_PreA_hr * Rate_Wage1_Dhr
```

Final results for pre-action cost of fire management actions for every
100 km² management area per year are summarised below:

``` r
PreAction_Cost_df <- data.frame(
  "Action" = c("Office planning", 
                        "Aerial survey", 
                        "On-ground survey (High)",
                        "On-ground survey (Moderate)",
                        "On-ground survey (Low)"),
  "Duration_h" = c(Dura_PreA_Off_hr, Dura_Aer_PreA_hr, Dura_Grd_PreA_hr),
  "Cost" = c(Cost_PreA_Off_D, Cost_Aer_PreA_D, Cost_Grd_PreA_D)
)
PreAction_Cost_df %>% 
  gt::gt() %>% 
  gt::cols_label(Action = "Pre-action work", 
                 Duration_h = "Duration (h)", 
                 Cost = "Cost ($)") %>% 
  gt::fmt_number(columns = Duration_h, decimals = 1) %>% 
  gt::fmt_number(columns = Cost, decimals = 2, use_seps = TRUE) %>% 
  gt::cols_align(align = "left", columns = Action) %>% 
  gt::cols_align(align = "right", columns = c(Duration_h, Cost)) %>% 
  gt::tab_options(table.font.size = gt::px(14), data_row.padding = gt::px(4))
```

<div id="tbl-pre-action-cost">

Table 3: Pre-action cost of fire management actions for every 100 km²
management area per year

<div class="cell-output-display">

<div id="masrwyejbc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#masrwyejbc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#masrwyejbc thead, #masrwyejbc tbody, #masrwyejbc tfoot, #masrwyejbc tr, #masrwyejbc td, #masrwyejbc th {
  border-style: none;
}
&#10;#masrwyejbc p {
  margin: 0;
  padding: 0;
}
&#10;#masrwyejbc .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#masrwyejbc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#masrwyejbc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#masrwyejbc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#masrwyejbc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#masrwyejbc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#masrwyejbc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#masrwyejbc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#masrwyejbc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#masrwyejbc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#masrwyejbc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#masrwyejbc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#masrwyejbc .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#masrwyejbc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#masrwyejbc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#masrwyejbc .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#masrwyejbc .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#masrwyejbc .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#masrwyejbc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#masrwyejbc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#masrwyejbc .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#masrwyejbc .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#masrwyejbc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#masrwyejbc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#masrwyejbc .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#masrwyejbc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#masrwyejbc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#masrwyejbc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#masrwyejbc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#masrwyejbc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#masrwyejbc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#masrwyejbc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#masrwyejbc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#masrwyejbc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#masrwyejbc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#masrwyejbc .gt_left {
  text-align: left;
}
&#10;#masrwyejbc .gt_center {
  text-align: center;
}
&#10;#masrwyejbc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#masrwyejbc .gt_font_normal {
  font-weight: normal;
}
&#10;#masrwyejbc .gt_font_bold {
  font-weight: bold;
}
&#10;#masrwyejbc .gt_font_italic {
  font-style: italic;
}
&#10;#masrwyejbc .gt_super {
  font-size: 65%;
}
&#10;#masrwyejbc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#masrwyejbc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#masrwyejbc .gt_indent_1 {
  text-indent: 5px;
}
&#10;#masrwyejbc .gt_indent_2 {
  text-indent: 10px;
}
&#10;#masrwyejbc .gt_indent_3 {
  text-indent: 15px;
}
&#10;#masrwyejbc .gt_indent_4 {
  text-indent: 20px;
}
&#10;#masrwyejbc .gt_indent_5 {
  text-indent: 25px;
}
&#10;#masrwyejbc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#masrwyejbc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Pre-action work             | Duration (h) | Cost (\$) |
|-----------------------------|--------------|-----------|
| Office planning             | 225.0        | 10,125.00 |
| Aerial survey               | 1.5          | 1,353.85  |
| On-ground survey (High)     | 46.9         | 1,407.17  |
| On-ground survey (Moderate) | 55.9         | 1,676.23  |
| On-ground survey (Low)      | 73.8         | 2,214.35  |

</div>

</div>

</div>

# Management action 1 - Fuel-reduced buffer establishment

Fuel-reduced buffers are established before prescribed burning to
provide safer boundaries for fire-management operations. Here, buffer
establishment is represented as an indicative estimate of the effort
required to prepare an area for prescribed burning rather than as a
prescriptive firebreak design. Actual establishment methods may vary
among locations and management organisations.

We adopted the buffer-establishment assumptions of Yong et al. (2023)
for a standardised management area of 100 km². The total buffer length
was derived from an assumed distribution of burn-patch sizes: 35% of the
managed area in patches of approximately 1 km², 35% in patches of
approximately 6 km², and 30% in patches of approximately 35 km². Under
these assumptions, treating a 100 km² management area requires
approximately 217 km of fuel-reduced buffer.

Rather than explicitly modelling individual burn-patch geometry, we used
this derived rate as an indicative buffer requirement. Annual buffer
length was then scaled by the vegetation-specific annual treatment
proportion. Buffers were assumed to be established at an average rate of
1.754 km h⁻¹.

## Labour requirements

Following Yong et al. (2023), buffer establishment was assumed to
require seven personnel:

- 2 personnel operating a 4WD fitted with a vegetation drag/harrow and
  mounted flame thrower;
- 4 personnel undertaking back-burning using drip torches; and
- 1 person operating a water truck for fire suppression and ground
  support.

## Vehicle requirements

We assumed one 4WD for the two-person buffer-establishment crew and
additional 4WDs for the remaining personnel, at approximately two
personnel per vehicle. This resulted in a total requirement of four 4WDs
for the seven-person field crew.

## Consumables and ground support

Consumable and support costs included fuel for the mounted flame thrower
and drip torches, water-truck hire, water refills, accommodation, and
meals. The mounted flame thrower was assumed to consume 1 L of fuel per
kilometre of buffer. Each of the four drip torches was also assumed to
consume 1 L km⁻¹, with fuel priced at \$2.00 L⁻¹.

Ground support was represented by a heavy-duty water truck available
throughout buffer-establishment activities. Water-truck hire was assumed
to cost \$200 h⁻¹, with two water refills required per 100 km of buffer
at \$300 per refill. Accommodation and meals were assumed to cost \$210
per person per night. The number of accommodation nights was calculated
from the integer number of field days.

## Equipment

Equipment requirements included one 5m harrow (\$1,690), one mounted
flame thrower (\$400), and four drip torches (\$330 each). All equipment
was assumed to have a 10-year replacement interval. Equipment costs were
subsequently annualised over the 30-year implementation period (see Step
9).

``` r
# For establishing fuel-reduced buffers for complete treatment
# of a standardised 100 km² management area

Buff_rate_kmh <- 1.754 # Buffer establishment rate in km/h
Buff_length_km <- 217 # Total length of fuel-reduced buffers in km for 100 km² management area

Buff_Lbr_n <- 7 # Total number of personnel for buffer establishment
Buff_Veh_n <- ceiling(Buff_Lbr_n / 2) # Assuming 2 personnel per vehicle

Rate_CarHire1_Dday <- 285 # Vehicle hire rate in $/vehicle-day
Rate_TorchFuel_Lkm <- 1 # Fuel use rate in L/km for mounted flame thrower and drip torches
Rate_Fuel_DL <- 2.00 # Fuel cost in $/L

Rate_WaterTruck_Dhr <- 200 # Water truck hire rate in $/hour
Rate_WaterRefill_n100km <- 2 # Number of water refills per 100 km of buffer
Rate_WaterRefill_Dn <- 300 # Cost per water refill in $

Rate_Accom_Dnight <- 210 # Accommodation and meals cost in $/person-night

Cost_Harrow_D <- 1690 # Cost of harrow in $
Cost_Flamethrw_D <- 400 # Cost of mounted flame thrower in $
Cost_DripTorch_D <- 330 # Cost of drip torch in $
Buff_DripTorch_n <- 4 # Number of drip torches for establishing fuel-reduced buffers

EquipLife_yr <- 10 # Assumed lifespan of equipment in years
```

``` r
# Deriving effort for establishing 100 km² management area
# ---- Activity duration ----

Dura_Buff_hr <- Buff_length_km / Buff_rate_kmh # Duration of buffer establishment (hours)

Dura_Buff_day <- Dura_Buff_hr / Workday_hr # Duration of buffer establishment in days (days)

Dura_Buff_Lbr_paxday <- Dura_Buff_day * Buff_Lbr_n # Total labour days for buffer establishment (person days)

Dura_Buff_Veh_vehday <- Dura_Buff_day * Buff_Veh_n # Total vehicle days for buffer establishment (vehicle days)

# ---- Labour durations and costs ----

Dura_Buff_Lbr_hr <- Dura_Buff_Lbr_paxday * Workday_hr # Total labour hours for buffer establishment (person hours)

Cost_Buff_Lbr_D <- Dura_Buff_Lbr_hr * Rate_Wage1_Dhr # Total labour cost for buffer establishment ($)

# ---- Vehicle hire duration and costs ----

Dura_Buff_Veh_day <- Dura_Buff_day * Buff_Veh_n # Total vehicle days for buffer establishment (days)

Cost_Buff_Veh_D <- Dura_Buff_Veh_day * Rate_CarHire1_Dday # Total vehicle hire cost for buffer establishment ($)

# ---- Consumables and ground support costs ----

Cost_Buff_FlamethrwFuel_D <- Buff_length_km * Rate_TorchFuel_Lkm * Rate_Fuel_DL # Fuel cost for mounted flame thrower ($)

Cost_Buff_DripTorchFuel_D <- Buff_length_km * Rate_TorchFuel_Lkm * Rate_Fuel_DL * Buff_DripTorch_n # Fuel cost for drip torches ($)

Cost_Buff_WaterTruck_D <- Dura_Buff_hr * Rate_WaterTruck_Dhr # Water truck hire cost ($)

Cost_Buff_WaterRefill_D <- round(Buff_length_km / 100) * Rate_WaterRefill_n100km * Rate_WaterRefill_Dn # Water refill cost ($)

Buff_Accom_night <- floor(Dura_Buff_day) # Number of accommodation nights (integer number of days)

Cost_Buff_Accom_D <- Buff_Accom_night * Buff_Lbr_n * Rate_Accom_Dnight # Accommodation and meals cost ($)

Cost_Buff_Consumables_D <- Cost_Buff_FlamethrwFuel_D +  # Total consumables and ground support cost ($)
                           Cost_Buff_DripTorchFuel_D +
                           Cost_Buff_WaterTruck_D +
                           Cost_Buff_WaterRefill_D +
                           Cost_Buff_Accom_D


# ---- Equipment purchase costs ----
# Equipment purchase cost; assumed replacement interval = 10 years
Cost_Buff_Equip_D <- Cost_Harrow_D +   # Total equipment purchase cost ($)
                     Cost_Flamethrw_D +
                     Cost_DripTorch_D * Buff_DripTorch_n

# ---- Buffer recurrent treatment cost ----

Cost_Buff_Treatment_D <- Cost_Buff_Lbr_D +  # Total buffer recurrent treatment cost ($)
                         Cost_Buff_Veh_D +
                         Cost_Buff_Consumables_D
```

Terrain ruggedness was not used to modify fuel-reduced buffer
establishment costs. We assumed that buffer placement would
preferentially follow practicable terrain and avoid locations where
ground access or buffer establishment was infeasible.

``` r
Buffer_Cost_df <- tibble::tibble(
  component = c("Activity duration", "Labour effort", "Vehicle effort",
                "Labour",            "Vehicle hire",  "Flame-thrower fuel",
                "Drip-torch fuel",   "Water truck",   "Water refills",
                "Accommodation and meals",            "Equipment purchase"),
  value = c(Dura_Buff_day, Dura_Buff_Lbr_hr, Dura_Buff_Veh_vehday,
    Cost_Buff_Lbr_D, Cost_Buff_Veh_D, Cost_Buff_FlamethrwFuel_D, Cost_Buff_DripTorchFuel_D, Cost_Buff_WaterTruck_D, Cost_Buff_WaterRefill_D,
    Cost_Buff_Accom_D, Cost_Buff_Equip_D),
  unit = c("field days", "hours", "vehicle-days", rep("$", 8)))

Buffer_Cost_df %>%
  gt::gt() %>%
  gt::cols_label(component = "Component", value = "Value", unit = "Unit") %>%
  gt::fmt_number(columns = value, decimals = 2, use_seps = TRUE) %>%
  gt::cols_align(align = "left", columns = c(component, unit)) %>%
  gt::cols_align(align = "right", columns = value) %>%
  gt::tab_options(table.font.size = gt::px(14), data_row.padding = gt::px(4))
```

<div id="tbl-buffer-cost">

Table 4: Fuel-reduced buffer establishment cost for every 100 km²
management area per year

<div class="cell-output-display">

<div id="axgutnxnci" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#axgutnxnci table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#axgutnxnci thead, #axgutnxnci tbody, #axgutnxnci tfoot, #axgutnxnci tr, #axgutnxnci td, #axgutnxnci th {
  border-style: none;
}
&#10;#axgutnxnci p {
  margin: 0;
  padding: 0;
}
&#10;#axgutnxnci .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#axgutnxnci .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#axgutnxnci .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#axgutnxnci .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#axgutnxnci .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#axgutnxnci .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#axgutnxnci .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#axgutnxnci .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#axgutnxnci .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#axgutnxnci .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#axgutnxnci .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#axgutnxnci .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#axgutnxnci .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#axgutnxnci .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#axgutnxnci .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#axgutnxnci .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#axgutnxnci .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#axgutnxnci .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#axgutnxnci .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#axgutnxnci .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#axgutnxnci .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#axgutnxnci .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#axgutnxnci .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#axgutnxnci .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#axgutnxnci .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#axgutnxnci .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#axgutnxnci .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#axgutnxnci .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#axgutnxnci .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#axgutnxnci .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#axgutnxnci .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#axgutnxnci .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#axgutnxnci .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#axgutnxnci .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#axgutnxnci .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#axgutnxnci .gt_left {
  text-align: left;
}
&#10;#axgutnxnci .gt_center {
  text-align: center;
}
&#10;#axgutnxnci .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#axgutnxnci .gt_font_normal {
  font-weight: normal;
}
&#10;#axgutnxnci .gt_font_bold {
  font-weight: bold;
}
&#10;#axgutnxnci .gt_font_italic {
  font-style: italic;
}
&#10;#axgutnxnci .gt_super {
  font-size: 65%;
}
&#10;#axgutnxnci .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#axgutnxnci .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#axgutnxnci .gt_indent_1 {
  text-indent: 5px;
}
&#10;#axgutnxnci .gt_indent_2 {
  text-indent: 10px;
}
&#10;#axgutnxnci .gt_indent_3 {
  text-indent: 15px;
}
&#10;#axgutnxnci .gt_indent_4 {
  text-indent: 20px;
}
&#10;#axgutnxnci .gt_indent_5 {
  text-indent: 25px;
}
&#10;#axgutnxnci .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#axgutnxnci div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Component               | Value     | Unit         |
|-------------------------|-----------|--------------|
| Activity duration       | 16.50     | field days   |
| Labour effort           | 866.02    | hours        |
| Vehicle effort          | 65.98     | vehicle-days |
| Labour                  | 25,980.62 | \$           |
| Vehicle hire            | 18,805.02 | \$           |
| Flame-thrower fuel      | 434.00    | \$           |
| Drip-torch fuel         | 1,736.00  | \$           |
| Water truck             | 24,743.44 | \$           |
| Water refills           | 1,200.00  | \$           |
| Accommodation and meals | 23,520.00 | \$           |
| Equipment purchase      | 3,410.00  | \$           |

</div>

</div>

</div>

# Management action 2 - Aerial burning

This management action represents the indicative effort required to
conduct prescribed burning by air rather than a prescriptive
fire-management design. Actual methods and equipment may vary among
locations and management organisations.

Aerial burning was assumed to be conducted using a helicopter equipped
with an incendiary dispenser. The aircraft was assumed to operate at
130km h⁻¹ along flight transects with an effective width of 10km, with
aircraft and pilot hire costing \$1,500 h⁻¹. Incendiary capsules were
assumed to be deployed at a rate of one capsule per second, at a cost of
\$1 per capsule. One additional entry-level staff member was included to
assist with capsule loading and other operational tasks. The incendiary
dispenser was assumed to cost \$20,000 with a 10-year lifespan.

Ground support was represented by one water truck. Ground-support
duration was approximated as the time required to traverse 217km per
100km² management area at an average speed of 40km h⁻¹. Water-truck hire
was assumed to cost \$200h⁻¹, with two water refills per 100 km
travelled at \$300 per refill.

Costs were first estimated for complete treatment of a standardised 100
km² management area before scaling by the annual treatment proportion of
aerial-management areas.

``` r
# For aerial burning of a complete standardised 100 km² management area

# ---- Aerial burning assumptions ----

Aer_Burn_Transect_km <- 10 # Transect width in km for aerial burning (km)
Rate_Aircraft2_Dhr <- 1500 # Aircraft2 hire and pilot cost ($/h)

Rate_capsule_nhr <- 3600    # one capsule per second meaning 3600 capsules per hour
Rate_capsule_Dn <- 0.35        # $ per capsule

Aer_Burn_Lbr_n <- 1            # additional staff member for aerial burning

Cost_IncendiaryMac_D <- 20000 # Cost of incendiary dispenser ($)

# ---- Ground support assumptions ----
Aer_Burn_GrdsupDist_km <- 217 # Ground support distance in km for aerial burning (km)
Rate_Aer_Burn_GrdsupSpeed_kmh <- 40 # Ground support speed in km/h for aerial burning (km/h)
```

``` r
# Calculations for activities duration and cost
# ---- Activity duration ----
Dist_Aer_Burn_km <- Stnd_Mgnt_A / Aer_Burn_Transect_km # Distance to be flown in aerial burning (km)
Dura_Aer_Burn_hr <- Dist_Aer_Burn_km / Aer_workspeed1_kmh # Duration of aerial burning (hour)
Dura_Aer_Burn_Grdsup_hr <- Aer_Burn_GrdsupDist_km / Rate_Aer_Burn_GrdsupSpeed_kmh # Duration of ground support for aerial burning (hour)

# --- Labour durations and costs ----
Dura_Aer_Burn_Lbr_hr <- Dura_Aer_Burn_hr * Aer_Burn_Lbr_n # Duration of labour for aerial burning (hour)

Cost_Aer_Burn_Lbr_D <- Dura_Aer_Burn_Lbr_hr * Rate_Wage1_Dhr # Cost of labour for aerial burning ($)

# --- Aircraft hire costs ----
Cost_Aer_Burn_Aircraft_D <- Dura_Aer_Burn_hr * Rate_Aircraft2_Dhr # Cost of aircraft hire for aerial burning ($)


# ---- Consumables and ground support costs ----
Aer_Burn_Capsule_n <- Dura_Aer_Burn_hr * Rate_capsule_nhr # Number of incendiary capsules used in aerial burning

Cost_Aer_Burn_Capsule_D <- Aer_Burn_Capsule_n * Rate_capsule_Dn # Cost of incendiary capsules for aerial burning ($)

Cost_Aer_Burn_WaterTruck_D <- Dura_Aer_Burn_Grdsup_hr * Rate_WaterTruck_Dhr # Cost of water truck hire for aerial burning ($)

Cost_Aer_Burn_WaterRefill_D <- round(Aer_Burn_GrdsupDist_km/100) * Rate_WaterRefill_n100km * Rate_WaterRefill_Dn # Cost of water refills for aerial burning ($)

Cost_Aer_Burn_Consumables_D <- Cost_Aer_Burn_Capsule_D +  # Total consumables and ground support cost ($)
                           Cost_Aer_Burn_WaterTruck_D +
                           Cost_Aer_Burn_WaterRefill_D

Cost_Aer_Burn_Treatment_D <- Cost_Aer_Burn_Lbr_D +  # Total aerial burning recurrent treatment cost ($)
                         Cost_Aer_Burn_Aircraft_D +
                         Cost_Aer_Burn_Consumables_D

# ---- Equipment purchase costs ----
# Equipment purchase cost; assumed replacement interval = 10 years
Cost_Aer_Burn_Equip_D <- Cost_IncendiaryMac_D # Total equipment purchase cost ($)
```

``` r
AerBurn_Cost_df <- tibble::tibble(
  component = c("Aerial burning duration", "Ground-support duration", "Incendiary capsules", "Aircraft hire", "Additional labour", "Incendiary capsules", "Water truck", "Water refills", "Equipment purchase"),
  value = c( Dura_Aer_Burn_hr, Dura_Aer_Burn_Grdsup_hr, Aer_Burn_Capsule_n, Cost_Aer_Burn_Aircraft_D, Cost_Aer_Burn_Lbr_D, Cost_Aer_Burn_Capsule_D, Cost_Aer_Burn_WaterTruck_D, Cost_Aer_Burn_WaterRefill_D, Cost_Aer_Burn_Equip_D),
  unit = c( "hours", "hours", "capsules", rep("$", 6)))

AerBurn_Cost_df %>%
  gt::gt() %>%
  gt::cols_label(component = "Component", value = "Value", unit = "Unit") %>%
  gt::fmt_number(columns = value, decimals = 2, use_seps = TRUE ) %>%
  gt::cols_align(align = "left", columns = c(component, unit)) %>%
  gt::cols_align(align = "right", columns = value) %>%
  gt::tab_options(table.font.size = gt::px(14), data_row.padding = gt::px(4))
```

<div id="tbl-aerial-burning-cost">

Table 5: Aerial burning cost for complete treatment of a 100 km²
management area

<div class="cell-output-display">

<div id="envihvobue" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#envihvobue table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#envihvobue thead, #envihvobue tbody, #envihvobue tfoot, #envihvobue tr, #envihvobue td, #envihvobue th {
  border-style: none;
}
&#10;#envihvobue p {
  margin: 0;
  padding: 0;
}
&#10;#envihvobue .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#envihvobue .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#envihvobue .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#envihvobue .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#envihvobue .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#envihvobue .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#envihvobue .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#envihvobue .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#envihvobue .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#envihvobue .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#envihvobue .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#envihvobue .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#envihvobue .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#envihvobue .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#envihvobue .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#envihvobue .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#envihvobue .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#envihvobue .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#envihvobue .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#envihvobue .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#envihvobue .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#envihvobue .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#envihvobue .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#envihvobue .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#envihvobue .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#envihvobue .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#envihvobue .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#envihvobue .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#envihvobue .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#envihvobue .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#envihvobue .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#envihvobue .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#envihvobue .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#envihvobue .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#envihvobue .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#envihvobue .gt_left {
  text-align: left;
}
&#10;#envihvobue .gt_center {
  text-align: center;
}
&#10;#envihvobue .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#envihvobue .gt_font_normal {
  font-weight: normal;
}
&#10;#envihvobue .gt_font_bold {
  font-weight: bold;
}
&#10;#envihvobue .gt_font_italic {
  font-style: italic;
}
&#10;#envihvobue .gt_super {
  font-size: 65%;
}
&#10;#envihvobue .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#envihvobue .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#envihvobue .gt_indent_1 {
  text-indent: 5px;
}
&#10;#envihvobue .gt_indent_2 {
  text-indent: 10px;
}
&#10;#envihvobue .gt_indent_3 {
  text-indent: 15px;
}
&#10;#envihvobue .gt_indent_4 {
  text-indent: 20px;
}
&#10;#envihvobue .gt_indent_5 {
  text-indent: 25px;
}
&#10;#envihvobue .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#envihvobue div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Component               | Value     | Unit     |
|-------------------------|-----------|----------|
| Aerial burning duration | 0.08      | hours    |
| Ground-support duration | 5.42      | hours    |
| Incendiary capsules     | 276.92    | capsules |
| Aircraft hire           | 115.38    | \$       |
| Additional labour       | 2.31      | \$       |
| Incendiary capsules     | 96.92     | \$       |
| Water truck             | 1,085.00  | \$       |
| Water refills           | 1,200.00  | \$       |
| Equipment purchase      | 20,000.00 | \$       |

</div>

</div>

</div>

# Management action 3 - On-ground burning

This management action represents the indicative effort required to
conduct prescribed burning on the ground. Ground burning was applied
within the ground-management area identified in Step 2.

The burn was assumed to be conducted by a crew of four personnel walking
with drip torches along transects with an effective width of 500 m. A
slow walking speed of 2.23 km h⁻¹, corresponding to the high
terrain-resistance category, was used as a conservative assumption for
ground-burning operations. An additional 10 minutes of activity time was
added for every kilometre walked.

Each of the four drip torches was assumed to consume 1 L of fuel per
kilometre travelled, with fuel costing \$2.00 L⁻¹.

Ground support was represented by one water truck available throughout
the on-ground burning activity. Water-truck hire was assumed to cost
\$200 h⁻¹, with two water refills per 100 km travelled at \$300 per
refill.

``` r
# For on-ground burning of a complete standardised 100 km² management area

Grd_Burn_Transect_km <- 0.5 # Transect width in km for on-ground burning (km)
Rate_Grd_Burn_WalkSpeed_kmh <- Grd_walkspeed_kmh["low"] # Walking speed for on-ground burning (km/h)

Grd_Burn_Lbr_n <- 4 # Number of personnel for on-ground burning
Rate_Grd_Burn_HrKm <- 10 / 60 # 10 minutes of activity time spent every km walked

# ---- Activity duration ----
Dist_Grd_Burn_km <- Stnd_Mgnt_A / Grd_Burn_Transect_km # Distance to be walked in on-ground burning (km)

Dura_Grd_Burn_walk_hr <- Dist_Grd_Burn_km / Rate_Grd_Burn_WalkSpeed_kmh # Duration of walking for on-ground burning (hour)

Dura_Grd_Burn_activity_hr <- Dist_Grd_Burn_km * Rate_Grd_Burn_HrKm # Duration of activity for on-ground burning (hour)

Dura_Grd_Burn_total_hr <- Dura_Grd_Burn_walk_hr + Dura_Grd_Burn_activity_hr # Total duration of on-ground burning (hour)

# ---- Labour ----

Dura_Grd_Burn_Lbr_hr <- Dura_Grd_Burn_total_hr * Grd_Burn_Lbr_n # Total labour hours for on-ground burning (person hours)

Cost_Grd_Burn_Lbr_D <- Dura_Grd_Burn_Lbr_hr * Rate_Wage1_Dhr # Total labour cost for on-ground burning ($)

# ---- Consumables and ground support ----
Cost_Grd_Burn_Fuel_D <- Dist_Grd_Burn_km * Rate_TorchFuel_Lkm * Rate_Fuel_DL * Grd_Burn_Lbr_n # Total fuel cost for on-ground burning ($)

Cost_Grd_Burn_WaterTruck_D <- Dura_Grd_Burn_total_hr * Rate_WaterTruck_Dhr # Total water truck hire cost for on-ground burning ($)

Cost_Grd_Burn_WaterRefill_D <- round(Dist_Grd_Burn_km / 100) * Rate_WaterRefill_n100km * Rate_WaterRefill_Dn # Total water refill cost for on-ground burning ($)

Cost_Grd_Burn_Consumables_D <- Cost_Grd_Burn_Fuel_D +  # Total consumables and ground support cost ($)
                           Cost_Grd_Burn_WaterTruck_D +
                           Cost_Grd_Burn_WaterRefill_D

Cost_Grd_Burn_Treatment_D <- Cost_Grd_Burn_Lbr_D +  # Total on-ground burning recurrent treatment cost ($)
                             Cost_Grd_Burn_Consumables_D

# ---- Equipment purchase costs ----
# Equipment purchase cost; assumed replacement interval = 10 years
Cost_Grd_Burn_Equip_D <- Cost_DripTorch_D * Grd_Burn_Lbr_n # Total equipment purchase cost ($)
```

``` r
GrdBurn_Cost_df <- tibble::tibble(
  component = c("On-ground burning duration", "Walking duration", "Activity duration", "Labour", "Drip-torch fuel", "Water truck", "Water refills", "Equipment purchase"),
  value = c(Dura_Grd_Burn_total_hr, Dura_Grd_Burn_walk_hr, Dura_Grd_Burn_activity_hr, Cost_Grd_Burn_Lbr_D, Cost_Grd_Burn_Fuel_D, Cost_Grd_Burn_WaterTruck_D, Cost_Grd_Burn_WaterRefill_D, Cost_Grd_Burn_Equip_D),
  unit = c("hours", "hours", "hours", rep("$", 5)))

GrdBurn_Cost_df %>%
  gt::gt() %>%
  gt::cols_label(component = "Component", value = "Value", unit = "Unit") %>%
  gt::fmt_number(columns = value, decimals = 2, use_seps = TRUE) %>%
  gt::cols_align(align = "left", columns = c(component, unit)) %>%
  gt::cols_align(align = "right", columns = value) %>%
  gt::tab_options(table.font.size = gt::px(14), data_row.padding = gt::px(4))
```

<div id="tbl-ground-burning-cost">

Table 6: On-ground burning cost for complete treatment of a 100 km²
management area

<div class="cell-output-display">

<div id="wtfiduwnin" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#wtfiduwnin table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#wtfiduwnin thead, #wtfiduwnin tbody, #wtfiduwnin tfoot, #wtfiduwnin tr, #wtfiduwnin td, #wtfiduwnin th {
  border-style: none;
}
&#10;#wtfiduwnin p {
  margin: 0;
  padding: 0;
}
&#10;#wtfiduwnin .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#wtfiduwnin .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#wtfiduwnin .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#wtfiduwnin .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#wtfiduwnin .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wtfiduwnin .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wtfiduwnin .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wtfiduwnin .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#wtfiduwnin .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#wtfiduwnin .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#wtfiduwnin .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#wtfiduwnin .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#wtfiduwnin .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#wtfiduwnin .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#wtfiduwnin .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#wtfiduwnin .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#wtfiduwnin .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#wtfiduwnin .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#wtfiduwnin .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wtfiduwnin .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#wtfiduwnin .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#wtfiduwnin .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#wtfiduwnin .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wtfiduwnin .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#wtfiduwnin .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#wtfiduwnin .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wtfiduwnin .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wtfiduwnin .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#wtfiduwnin .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#wtfiduwnin .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#wtfiduwnin .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wtfiduwnin .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wtfiduwnin .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wtfiduwnin .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wtfiduwnin .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wtfiduwnin .gt_left {
  text-align: left;
}
&#10;#wtfiduwnin .gt_center {
  text-align: center;
}
&#10;#wtfiduwnin .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#wtfiduwnin .gt_font_normal {
  font-weight: normal;
}
&#10;#wtfiduwnin .gt_font_bold {
  font-weight: bold;
}
&#10;#wtfiduwnin .gt_font_italic {
  font-style: italic;
}
&#10;#wtfiduwnin .gt_super {
  font-size: 65%;
}
&#10;#wtfiduwnin .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#wtfiduwnin .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#wtfiduwnin .gt_indent_1 {
  text-indent: 5px;
}
&#10;#wtfiduwnin .gt_indent_2 {
  text-indent: 10px;
}
&#10;#wtfiduwnin .gt_indent_3 {
  text-indent: 15px;
}
&#10;#wtfiduwnin .gt_indent_4 {
  text-indent: 20px;
}
&#10;#wtfiduwnin .gt_indent_5 {
  text-indent: 25px;
}
&#10;#wtfiduwnin .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#wtfiduwnin div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Component                  | Value     | Unit  |
|----------------------------|-----------|-------|
| On-ground burning duration | 123.02    | hours |
| Walking duration           | 89.69     | hours |
| Activity duration          | 33.33     | hours |
| Labour                     | 14,762.33 | \$    |
| Drip-torch fuel            | 1,600.00  | \$    |
| Water truck                | 24,603.89 | \$    |
| Water refills              | 1,200.00  | \$    |
| Equipment purchase         | 1,320.00  | \$    |

</div>

</div>

</div>

# Post-action monitoring (Aerial and on-ground) and evaluation

Post-action work included aerial and ground-based monitoring of
management areas, followed by office-based evaluation and reporting.
Costs were estimated for each standardised 100 km² management area per
year. Rates were:

- Post-action office evaluation and reporting: 15 person-days of
  experienced staff time, assuming a 7.5h working day and a labour rate
  of \$45 h⁻¹.
- Post-action aerial survey: the full management area was surveyed using
  10km-wide flight transects, with aircraft and pilot hire costing \$850
  h⁻¹.
- Post-action ground survey: two personnel surveyed 30% of the
  management area on foot using 500m-wide transects, with an additional
  10 minutes of survey activity per kilometre walked.

``` r
Dura_PosA_Off_hr <- 15 * Workday_hr # Duration taken for post-action office evaluation and reporting (hour) for every 100 km² management area

Cost_PosA_Off_D <- Dura_PosA_Off_hr * Rate_Wage2_Dhr # 15 person-days of experienced staff time at $45 h⁻¹

Aer_PosA_Survey_prop <- 1 # Proportion of area to be surveyed in post-action by flight
Aer_PosA_Transect_km <- 10 # Transect width in km for aerial post-action survey (km)

Dura_Aer_PosA_hr <- (Stnd_Mgnt_A * Aer_PosA_Survey_prop) /  
                    Aer_PosA_Transect_km / 
                    Aer_workspeed1_kmh

# Cost of aerial post-action survey ($) for every 100 km² management area per year
Cost_Aer_PosA_Aircraft_D <- Dura_Aer_PosA_hr * Rate_Aircraft1_Dhr
Cost_Aer_PosA_Lbr_D  <- Dura_Aer_PosA_hr * 1 * Rate_Wage1_Dhr
Cost_Aer_PosA_D <- Cost_Aer_PosA_Aircraft_D + Cost_Aer_PosA_Lbr_D

Grd_PosA_Survey_prop <- 0.30 # Proportion of area to be surveyed in post-action by ground
Grd_PosA_Transect_km <- 0.5 # Transect width in km for ground post-action survey (km)
Grd_PosA_Lbr_n <- 2 # Number of personnel for ground post-action survey
Rate_Grd_PosA_Survey_HrKm <- 10 / 60 # 10 minutes of activity time spent every km walked

Area_Grd_PosA_km2 <- Stnd_Mgnt_A * Grd_PosA_Survey_prop # Area to be surveyed in post-action by ground (km²)
Dist_Grd_PosA_km <- Area_Grd_PosA_km2 / Grd_PosA_Transect_km # Distance to be walked in post-action by ground (km)
Dura_Grd_PosA_Survey_hr <- Dist_Grd_PosA_km * Rate_Grd_PosA_Survey_HrKm # Duration of activity time spent in post-action by ground (hour)

# Total duration of ground post-action survey (hour) for every 100 km² management area
Dura_Grd_PosA_hr <- (Dura_Grd_PosA_Survey_hr + (Dist_Grd_PosA_km / Grd_walkspeed_kmh)) *
                    Grd_PosA_Lbr_n

# Total cost of ground post-action survey ($) for every 100 km² management area per year
Cost_Grd_PosA_D <- Dura_Grd_PosA_hr * Rate_Wage1_Dhr
```

Post-action monitoring and evaluation costs for fire management actions
for every 100 km² management area per year are summarised below:

``` r
PostAction_Cost_df <- data.frame(
  "Action" = c("Office evaluation and reporting", 
                        "Aerial survey", 
                        "On-ground survey (High)",
                        "On-ground survey (Moderate)",
                        "On-ground survey (Low)"),
  "Duration_h" = c(Dura_PosA_Off_hr, Dura_Aer_PosA_hr, Dura_Grd_PosA_hr),
  "Cost" = c(Cost_PosA_Off_D, Cost_Aer_PosA_D, Cost_Grd_PosA_D)
)

PostAction_Cost_df %>% 
  gt::gt() %>% 
  gt::cols_label(Action = "Post-action work", 
                 Duration_h = "Duration (h)", 
                 Cost = "Cost ($)") %>% 
  gt::fmt_number(columns = Duration_h, decimals = 1) %>% 
  gt::fmt_number(columns = Cost, decimals = 2, use_seps = TRUE) %>% 
  gt::cols_align(align = "left", columns = Action) %>% 
  gt::cols_align(align = "right", columns = c(Duration_h, Cost)) %>% 
  gt::tab_options(table.font.size = gt::px(14), data_row.padding = gt::px(4))
```

<div id="tbl-post-action-cost">

Table 7: Post-action monitoring and evaluation cost of fire management
actions for every 100 km² management area per year

<div class="cell-output-display">

<div id="ioomcmoiht" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ioomcmoiht table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ioomcmoiht thead, #ioomcmoiht tbody, #ioomcmoiht tfoot, #ioomcmoiht tr, #ioomcmoiht td, #ioomcmoiht th {
  border-style: none;
}
&#10;#ioomcmoiht p {
  margin: 0;
  padding: 0;
}
&#10;#ioomcmoiht .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ioomcmoiht .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ioomcmoiht .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ioomcmoiht .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ioomcmoiht .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ioomcmoiht .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ioomcmoiht .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ioomcmoiht .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ioomcmoiht .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ioomcmoiht .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ioomcmoiht .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ioomcmoiht .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ioomcmoiht .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ioomcmoiht .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ioomcmoiht .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ioomcmoiht .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ioomcmoiht .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ioomcmoiht .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ioomcmoiht .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ioomcmoiht .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ioomcmoiht .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ioomcmoiht .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ioomcmoiht .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ioomcmoiht .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ioomcmoiht .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ioomcmoiht .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ioomcmoiht .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ioomcmoiht .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ioomcmoiht .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ioomcmoiht .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ioomcmoiht .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ioomcmoiht .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ioomcmoiht .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ioomcmoiht .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ioomcmoiht .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ioomcmoiht .gt_left {
  text-align: left;
}
&#10;#ioomcmoiht .gt_center {
  text-align: center;
}
&#10;#ioomcmoiht .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ioomcmoiht .gt_font_normal {
  font-weight: normal;
}
&#10;#ioomcmoiht .gt_font_bold {
  font-weight: bold;
}
&#10;#ioomcmoiht .gt_font_italic {
  font-style: italic;
}
&#10;#ioomcmoiht .gt_super {
  font-size: 65%;
}
&#10;#ioomcmoiht .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ioomcmoiht .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ioomcmoiht .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ioomcmoiht .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ioomcmoiht .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ioomcmoiht .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ioomcmoiht .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ioomcmoiht .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ioomcmoiht div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Post-action work                | Duration (h) | Cost (\$) |
|---------------------------------|--------------|-----------|
| Office evaluation and reporting | 112.5        | 5,062.50  |
| Aerial survey                   | 0.1          | 67.69     |
| On-ground survey (High)         | 46.9         | 1,407.17  |
| On-ground survey (Moderate)     | 55.9         | 1,676.23  |
| On-ground survey (Low)          | 73.8         | 2,214.35  |

</div>

</div>

</div>

# Travel to management sites

Travel costs were estimated separately from on-site management costs.
Consistent with the standardised management-area approach used
throughout the costing, the study area was divided into 100 km²
management units for estimating mobilisation costs, while the underlying
1 arc second spatial data were retained for determining burnable area
and management type.

For ground-based management, travel to each management unit was assumed
to occur along the existing [road
network](https://catalogue.data.wa.gov.au/dataset/mrwa-road-network)
from the nearest operational centre, identified based on [ABS Urban
Centres and
Localities](https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs/edition-3-july-2021-june-2026/access-and-downloads/digital-boundary-files).
Ground-management areas were restricted to locations within 5 km of
roads; therefore, additional off-road mobilisation was not explicitly
modelled. Road travel time was first estimated along the road network at
approximately 1-km resolution. Travel-time values were extended to
nearby non-road cells within 8 km using the value of the nearest
accessible road cell, and the minimum travel time within each 100-km²
management unit was then used to represent mobilisation time.

The cost of a return journey by road was calculated as

$$C_{\text{Ground trip, j}} = 2 \times T_j \times R_{\mathrm{ground}} \times N_{\mathrm{trips}}$$

where $T_j$ is one-way road travel time to management unit $j$,
$R_{\mathrm{ground}}$ is the hourly cost of road travel, and
$N_{\mathrm{trips}}$ is the number of return trips required.

For aerial management, travel distance was represented by the minimum
straight-line distance between each 100-km² management unit and the
nearest assumed aerial operational base (i.e. urban centre with 5000
inhabitants or more based on [ABS Urban Centres and
Localities](https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs/edition-3-july-2021-june-2026/access-and-downloads/digital-boundary-files)).
Aerial transit time was calculated from this distance using an assumed
aircraft transit speed, and return-trip cost was calculated as:

$$C_{\text{Aerial trip, j}} = 2 \times \frac{D_j}{V_{\text{aircraft}}} \times R_{\mathrm{aerial}} \times N_{\mathrm{trips}}$$

where $D_j$ is the straight-line distance to management unit $j$,
$V_{\text{aircraft}}$ is the aircraft transit speed,
$R_{\mathrm{aerial}}$ is the hourly cost of aerial travel, and
$N_{\mathrm{trips}}$ is the number of return trips required.

Field teams were assumed to remain at a management site for a maximum of
21 consecutive days. The number of trips required for each management
action was therefore:

$$N_{\mathrm{trips}} = \lceil \frac{D_{\mathrm{action}}}{21} \rceil$$

where $D_{\mathrm{action}}$ is the total number of days required to
complete the management action at a given site.

``` r
# ---- 100 km² management units ----
# StudyArea_GdaMga <- StudyArea_SF %>% st_transform(crs = st_crs("EPSG:7851"))

# BBOX_GdaMga_10km_rast <- aggregate(BBOX_GdaMga_rast, fun = "none", cores = 8, 
#                                   fact = 10000 / res(BBOX_GdaMga_rast)[1])
# aggregate from 1arcsec to approximately 1km resolution for travel time calculations
BBOX_GDA_1km_rast <- aggregate(BBOX_GDA_rast, fun = "none", cores = 8, fact = 30)
values(BBOX_GDA_1km_rast) <- 1

SA_1km_grid <- crop(BBOX_GDA_1km_rast, vect(StudyArea_SF), mask = TRUE, snap = "out")

# To balance spatial accuracy with the computational demands of the
# cost-distance calculation, travel time is estimated using
# approximately 1 × 1 km raster cells.


# ---- Process road layers ----
Road_speed_kmh <- c("State Road" = 100, "Local Road" = 40, "Miscellaneous Road" = 60)

# Read and process road network data
Roads_sf <- st_read(file.path(INPUT_SPA_DIR, "Road_Network", "Road_Network.shp")) %>% 
  st_transform(crs = st_crs(BBOX)) %>% st_crop(BBOX) %>% 
  filter(NETWORK_TY != "Proposed Road") %>% 
  mutate(GeoType = st_geometry_type(.)) %>%
  filter(GeoType == "LINESTRING")  %>% 
  mutate(speed_kmh = ifelse(NETWORK_TY %in% names(Road_speed_kmh), 
                            Road_speed_kmh[NETWORK_TY], 60))
  
# Rasterize road network with speed values
RoadSpeed_kmh_1km_rast <- rasterize(vect(Roads_sf), BBOX_GDA_1km_rast, field = "speed_kmh", 
                                    background = NA, touches = TRUE)

# Read in urban centre data and process for travel calculations
WK_urb_sel_pt <- st_read(file.path(
  INPUT_SPA_DIR, "UCL_2021_AUST_GDA94_SHP", "UCL_2021_AUST_GDA94.shp")) %>%
  st_transform(st_crs(STE)) %>%
  st_centroid(.) %>% st_intersection(BBOX) %>% 
  dplyr::select(UCL_NAME21, geometry, SSR_CODE21, SSR_NAME21, SOS_CODE21, SOS_NAME21) %>%
  filter(SOS_NAME21 %in% c("Other Urban", "Bounded Locality")) %>%
  distinct(UCL_NAME21, .keep_all = TRUE) %>% drop_na()

WK_Other_urb_pt <- WK_urb_sel_pt %>% # Urban centres with approx 5000 or more pops.
  filter(SOS_NAME21 == "Other Urban")
WK_local_pt <- WK_urb_sel_pt %>%     # Local centres with approx 500 or less pops.
  filter(SOS_NAME21 == "Bounded Locality")

WK_urb_1km_rast <- rasterize(vect(WK_urb_sel_pt), BBOX_GDA_1km_rast, field = 1, background = NA, touches = TRUE)

WK_Murb_1km_rast <- rasterize(vect(WK_Other_urb_pt), BBOX_GDA_1km_rast, field = 1, background = NA, touches = TRUE)

# ---- Calculate travel time to nearest urban centre along roads ----

# Hours required to travel one metre along each road cell
RoadFriction_hrm_1km_rast <- 1 / (RoadSpeed_kmh_1km_rast * 1000)
names(RoadFriction_hrm_1km_rast) <- "Friction_hrm"

WK_urb_cell <- cellFromXY(RoadFriction_hrm_1km_rast, crds(vect(WK_urb_sel_pt)))

# Set urban centres as targets
RoadFriction_hrm_1km_rast[WK_urb_cell] <- 0


# Delete any existing travel time raster for recalculations
# if(file.exists(file.path(OUTPUT_SPA_DIR, "Dura_Grd_Trav_hr.tif"))) {
#   file.remove(file.path(OUTPUT_SPA_DIR, "Dura_Grd_Trav_hr.tif"))}

# Calculate travel time to nearest urban centre along roads
# This step may take more than 30 minutes to run
Dura_Grd_Trav_hr_1km_rast <- if(file.exists(file.path(OUTPUT_SPA_DIR, "Dura_Grd_Trav_hr.tif"))) {
  rast(file.path(OUTPUT_SPA_DIR, "Dura_Grd_Trav_hr.tif"))
} else {
  costDist(RoadFriction_hrm_1km_rast, target = 0, maxiter = 250, 
           filename = file.path(OUTPUT_SPA_DIR, "Dura_Grd_Trav_hr.tif"), overwrite = TRUE)}

# Assign nearby non-road cells the travel-time value of the nearest accessible road cell, up to a maximum distance of 6 km
# This step can take more than 30 minutes to run
# For refunning purposes, delete any existing travel time raster for recalculations
# if(file.exists(file.path(OUTPUT_SPA_DIR, "Dura_Grd_Trav_hr_exp.tif"))){
#   file.remove(file.path(OUTPUT_SPA_DIR, "Dura_Grd_Trav_hr_exp.tif"))
# }

Dura_Grd_Trav_hr_1km_exp_rast <- if(file.exists(file.path(
  OUTPUT_SPA_DIR, "Dura_Grd_Trav_hr_exp.tif"))) {
    rast(file.path(OUTPUT_SPA_DIR, "Dura_Grd_Trav_hr_exp.tif"))
} else {
  distance(Dura_Grd_Trav_hr_1km_rast, values = TRUE, maxdist = 6000, 
    filename = file.path(OUTPUT_SPA_DIR, "Dura_Grd_Trav_hr_exp.tif"), overwrite = TRUE)}
toc(log = TRUE)

Dura_Grd_Trav_hr_1km_expInc_rast <- cover(Dura_Grd_Trav_hr_1km_exp_rast, Dura_Grd_Trav_hr_1km_rast)

# Summarise mobilisation time to approximately 100 km² management units
# Then resample to 30 m resolution for plotting and further analysis
Dura_Grd_Trav_hr_30m_rast <- aggregate(Dura_Grd_Trav_hr_1km_expInc_rast, cores = 8,
                                       fact = 10, fun = "min", na.rm = TRUE) %>%
              disagg(fact = 300, method = "near") %>%
              resample(DEM_SA, method = "near", threads = TRUE) %>%
              crop(FireExtent_Ground_rast, mask = TRUE)

# For plotting
Dura_Grd_Trav_hr_300m_rast <- aggregate(Dura_Grd_Trav_hr_30m_rast, 
                                        fact = 10, fun = "min", na.rm = TRUE)
names(Dura_Grd_Trav_hr_300m_rast) <- "Duration (hours)"

# ---- Calculate straight-line distance to nearest main urban centre ----
# Estimating aerial management travel distance between main urban centres and management units
WK_Murb_1km_rast <- crop(WK_Murb_1km_rast, SA_1km_grid, mask = TRUE)

Dist_Aer_Trav_1km_km <- if(file.exists(file.path(
    OUTPUT_SPA_DIR, "Dist_Aer_Trav_1km_km.tif"))) {
  rast(file.path(OUTPUT_SPA_DIR, "Dist_Aer_Trav_1km_km.tif"))
} else {
  distance(WK_Murb_1km_rast, unit="km")
}

Dist_Aer_Trav_30m_km <- aggregate(Dist_Aer_Trav_1km_km, cores = 8, 
    fact = 30, fun = "min", na.rm = TRUE) %>%
  disagg(fact = 300, method = "near") %>%
  resample(DEM_SA, method = "near", threads = TRUE) %>%
  crop(FireExtent_Ground_rast, mask = TRUE)

Aer_TransitSpeed_kmh <- 250 # Aircraft transit speed (km/h)

Dura_Aer_Trav_hr_30m_rast <- Dist_Aer_Trav_30m_km / Aer_TransitSpeed_kmh

Dura_Aer_Trav_hr_300m_rast <- aggregate(Dura_Aer_Trav_hr_30m_rast, 
                                        fact = 10, fun = "min", na.rm = TRUE)
```

``` r
Dura_Grd_Trav_hr_plot <- ggplot()+
  geom_sf(data = STE_WA, fill = "grey70", color = NA) +
  geom_spatraster(data = Dura_Grd_Trav_hr_300m_rast) +
  scale_fill_gradientn(colours = hcl.colors(n = 9, palette = "YlGnBu", rev = TRUE), 
                       name = "Duration (hours)", na.value = "transparent") +
  geom_sf(data = Roads_sf, aes(color = "grey50"), linewidth = 0.2) +
  geom_sf(data = StudyArea_SF, aes(color = "black"), fill = NA, linewidth = 0.5) +
  scale_color_manual(values = c("grey50" = "grey50", "black" = "black"),
                     labels = c("grey50" = "Roads", "black" = "Study area"), name = NULL) +
  geom_sf(data = WK_urb_sel_pt, aes(color = "red"), size = 2, shape = 17) +
  labs(title = "(A) Ground travel", fill = "Duration (hours)")+
  coord_sf(xlim = st_bbox(StudyArea_SF)[c(1, 3)], ylim = st_bbox(StudyArea_SF)[c(2, 4)]) +
  theme_bw()+
  theme(legend.position = "top")+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))

Dura_Aer_Trav_hr_plot <- ggplot()+
  geom_sf(data = STE_WA, fill = "grey70", color = NA) +
  geom_spatraster(data = Dura_Aer_Trav_hr_300m_rast) +
  scale_fill_gradientn(colours = hcl.colors(n = 9, palette = "YlGnBu", rev = TRUE), 
                       name = "Duration (hours)", na.value = "transparent") +
  geom_sf(data = StudyArea_SF, aes(color = "black"), fill = NA, linewidth = 0.5) +
  scale_color_manual(values = c("black" = "black"),
                     labels = c("black" = "Study area"), name = NULL) +
  geom_sf(data = WK_Other_urb_pt, aes(color = "red"), size = 2, shape = 17) +
  labs(title = "(B) Aerial travel", fill = "Duration (hours)") +
  coord_sf(xlim = st_bbox(StudyArea_SF)[c(1, 3)], ylim = st_bbox(StudyArea_SF)[c(2, 4)]) +
  theme_bw()+
  theme(legend.position = "top")+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))
library(patchwork)

Dura_Grd_Trav_hr_plot / Dura_Aer_Trav_hr_plot
```

<div id="fig-travel-duration">

![](Fire_regime_files/figure-commonmark/fig-travel-duration-1.png)

Figure 3: Estimated one-way travel duration to fire-management sites by
(A) ground and (B) aerial transport.

</div>

``` r
# ---- Travel-cost assumptions ----

Rate_Grd_Trav_Veh_Dhr <- 76 # Ground travel vehicle cost per hour ($/h)
Rate_Grd_Trav_Lbr_Dhr <- 60 # Ground travel labour cost per hour ($/h)
Rate_Aer_Trav_Aircraft_Dhr <- 850 # Aerial travel aircraft cost per hour ($/h)
Rate_Aer_Trav_Lbr_Dhr <- 30 # Aerial travel labour cost per hour ($/h)

Rate_LabourOverhead <- 0.30 # Overhead costs for labour
Rate_FieldContingency <- 0.10 # Contingency costs for consumables, labour, and travelling costs

Rate_Grd_Trav_adj_Dhr  <- Adjust_Field_Cost_fn(LabourCost = Rate_Grd_Trav_Lbr_Dhr,
                                               TransportCost = Rate_Grd_Trav_Veh_Dhr,
                                               ConsumablesCost = 0,
                                               Rate_LbrOverhead = Rate_LabourOverhead,
                                               Rate_FieldContin = Rate_FieldContingency)

Rate_Aer_Trav_adj_Dhr <- Adjust_Field_Cost_fn(LabourCost = Rate_Aer_Trav_Lbr_Dhr,
                                             TransportCost = Rate_Aer_Trav_Aircraft_Dhr,
                                             ConsumablesCost = 0,
                                             Rate_LbrOverhead = Rate_LabourOverhead,
                                             Rate_FieldContin = Rate_FieldContingency)

MaxSiteStay_day <- 21 # Maximum number of days spent at a management site before returning to base (days)

# ---- Travel-cost calculations ----
Cost_Grd_Trav_Dtrip <- 2 * Dura_Grd_Trav_hr_30m_rast * Rate_Grd_Trav_adj_Dhr # Ground travel cost per return trip ($)

Cost_Aer_Trav_Dtrip <- 2 * Dura_Aer_Trav_hr_30m_rast * Rate_Aer_Trav_adj_Dhr # Aerial travel cost per return trip ($)

Trip_Buff_n <- ceiling(Dura_Buff_day / MaxSiteStay_day) # Number of return trips required for buffer treatment

Trip_Grd_Burn_n <- ceiling((Dura_Grd_Burn_total_hr / Workday_hr) / MaxSiteStay_day) # Number of return trips required for on-ground burning

Trip_Aer_Burn_n <- max(1, ceiling((Dura_Aer_Burn_hr / Workday_hr) / MaxSiteStay_day)) # Number of return trips required for aerial burning

Trip_Grd_PreA_n <- ceiling((max(Dura_Grd_PreA_hr) / Workday_hr) / MaxSiteStay_day) # Number of return trips required for on-ground Pre-action survey

Trip_Aer_PreA_n <- max(1, ceiling((Dura_Aer_PreA_hr / Workday_hr) / MaxSiteStay_day)) # Number of return trips required for aerial Pre-action survey

Trip_Grd_PosA_n <- ceiling((max(Dura_Grd_PosA_hr) / Workday_hr) / MaxSiteStay_day) # Number of return trips required for on-ground Post-action survey

Trip_Aer_PosA_n <- max(1, ceiling((Dura_Aer_PosA_hr / Workday_hr) / MaxSiteStay_day)) # Number of return trips required for aerial Post-action survey
```

# Annualise and total combine costs

We assumed a 30% overhead on labour costs and a 10% contingency on field
costs, including labour, transport and consumables. Equipment costs were
annualised over a 30-year implementation period using a 4% discount rate
and their assumed replacement intervals. Travel-to-site costs were
adjusted separately to account for labour overhead and field
contingency. The total annualised fire-management cost was calculated as
the sum of pre-action, management-action, post-action and travel costs.

``` r
Rate_Discount <- 0.04 # Discount rate for annualising costs
TimeHorizon_yr <- 30  # Assumed time horizon for annualising costs (years)

Rate_LabourOverhead <- 0.30 # Overhead costs for labour
Rate_FieldContingency <- 0.10 # Contingency costs for consumables, labour, and travelling costs

CellArea_km2 <- cellSize(DEM_SA, unit = "km")

# Present value of annuity factor for annualising costs over the time horizon
PV_AnnuityFactor <- sum(1 / (1 + Rate_Discount)^(0:(TimeHorizon_yr - 1)))

Cost_Buff_Equip_Ann_D <- Annual_period_cost_fn(Cost = Cost_Buff_Equip_D,
                                                 Interval = 10, 
                                                 Disc_Rate = Rate_Discount,
                                                 TimeHorizon_yr = TimeHorizon_yr)

Cost_Aer_Burn_Equip_Ann_D <- Annual_period_cost_fn(Cost = Cost_Aer_Burn_Equip_D,
                                                 Interval = 10, 
                                                 Disc_Rate = Rate_Discount,
                                                 TimeHorizon_yr = TimeHorizon_yr)

Cost_Grd_Burn_Equip_Ann_D <- Annual_period_cost_fn(Cost = Cost_Grd_Burn_Equip_D,
                                                 Interval = 10, 
                                                 Disc_Rate = Rate_Discount,
                                                 TimeHorizon_yr = TimeHorizon_yr)


# ---- Adjust Treatment costs for overheads and contingencies ----

Cost_Buff_Treatment_adj_D <- Adjust_Field_Cost_fn(LabourCost = Cost_Buff_Lbr_D,
                                                  TransportCost = Cost_Buff_Veh_D,
                                                  ConsumablesCost = Cost_Buff_Consumables_D,
                                                  Rate_LbrOverhead = Rate_LabourOverhead,
                                                  Rate_FieldContin = Rate_FieldContingency)

Cost_Aer_Burn_Treatment_adj_D <- Adjust_Field_Cost_fn(LabourCost = Cost_Aer_Burn_Lbr_D,
                                                  TransportCost = Cost_Aer_Burn_Aircraft_D,
                                                  ConsumablesCost = Cost_Aer_Burn_Consumables_D,
                                                  Rate_LbrOverhead = Rate_LabourOverhead,
                                                  Rate_FieldContin = Rate_FieldContingency)

Cost_Grd_Burn_Treatment_adj_D <- Adjust_Field_Cost_fn(LabourCost = Cost_Grd_Burn_Lbr_D,
                                                  TransportCost = 0,
                                                  ConsumablesCost = Cost_Grd_Burn_Consumables_D,
                                                  Rate_LbrOverhead = Rate_LabourOverhead,
                                                  Rate_FieldContin = Rate_FieldContingency)

# Per-action and post-action costs adjustment for overheads and contingencies?
Cost_Aer_PreA_adj_D <-  Adjust_Field_Cost_fn(LabourCost = Cost_Aer_PreA_Lbr_D,
                                             TransportCost = Cost_Aer_PreA_Aircraft_D,
                                             ConsumablesCost = 0,
                                             Rate_LbrOverhead = Rate_LabourOverhead,
                                             Rate_FieldContin = Rate_FieldContingency)

Cost_Aer_PosA_adj_D <-  Adjust_Field_Cost_fn(LabourCost = Cost_Aer_PosA_Lbr_D,
                                             TransportCost = Cost_Aer_PosA_Aircraft_D,
                                             ConsumablesCost = 0,
                                             Rate_LbrOverhead = Rate_LabourOverhead,
                                             Rate_FieldContin = Rate_FieldContingency)

Cost_Grd_PreA_adj_D <- Adjust_Field_Cost_fn(LabourCost = Cost_Grd_PreA_D,
                                            TransportCost = 0,
                                            ConsumablesCost = 0,
                                            Rate_LbrOverhead = Rate_LabourOverhead,
                                            Rate_FieldContin = Rate_FieldContingency)

Cost_Grd_PosA_adj_D <- Adjust_Field_Cost_fn(LabourCost = Cost_Grd_PosA_D,
                                            TransportCost = 0,
                                            ConsumablesCost = 0,
                                            Rate_LbrOverhead = Rate_LabourOverhead,
                                            Rate_FieldContin = Rate_FieldContingency)

Cost_PreA_Off_adj_D <- Adjust_Office_Cost_fn(LabourCost = Cost_PreA_Off_D,
                                            Rate_LbrOverhead = Rate_LabourOverhead)

Cost_PosA_Off_adj_D <- Adjust_Office_Cost_fn(LabourCost = Cost_PosA_Off_D,
                                            Rate_LbrOverhead = Rate_LabourOverhead)
```

``` r
Cost_Buff_Treatment_adj_Ann_D_rast <- 
  (Cost_Buff_Treatment_adj_D / Stnd_Mgnt_A) * FireExtent_rast * CellArea_km2

Cost_Buff_Equip_Ann_D_rast <- 
  (Cost_Buff_Equip_Ann_D / Stnd_Mgnt_A) * FireExtent_rast * CellArea_km2

Cost_Aer_Burn_Treatment_adj_Ann_D_rast <- 
  (Cost_Aer_Burn_Treatment_adj_D / Stnd_Mgnt_A) * FireExtent_Aerial_rast * CellArea_km2

Cost_Aer_Burn_Equip_Ann_D_rast <- 
  (Cost_Aer_Burn_Equip_Ann_D / Stnd_Mgnt_A) * FireExtent_Aerial_rast * CellArea_km2

Cost_Grd_Burn_Treatment_adj_Ann_D_rast <- 
  (Cost_Grd_Burn_Treatment_adj_D / Stnd_Mgnt_A) * FireExtent_Ground_rast * CellArea_km2

Cost_Grd_Burn_Equip_Ann_D_rast <- 
  (Cost_Grd_Burn_Equip_Ann_D / Stnd_Mgnt_A) * FireExtent_Ground_rast * CellArea_km2

# ---- Travel costs ----

Cost_Aer_Trav_Ann_D_rast <- ((Cost_Aer_Trav_Dtrip * 
                             (Trip_Aer_Burn_n + Trip_Aer_PosA_n + Trip_Aer_PreA_n)) /
                             Stnd_Mgnt_A) *
                              FireExtent_Aerial_rast * CellArea_km2

Cost_Grd_Trav_Ann_D_rast <- ((Cost_Grd_Trav_Dtrip * 
                             (Trip_Grd_Burn_n + Trip_Grd_PosA_n + Trip_Grd_PreA_n)) /
                             Stnd_Mgnt_A) *
                              FireExtent_Ground_rast * CellArea_km2

# Pre-action and post-action costs
Cost_PrePos_Office_Ann_D_rast <- ((Cost_PreA_Off_adj_D + Cost_PosA_Off_adj_D) / Stnd_Mgnt_A) *
  FireExtent_rast * CellArea_km2

Cost_Aer_PrePos_Ann_D_rast <- ((Cost_Aer_PreA_adj_D + Cost_Aer_PosA_adj_D) / Stnd_Mgnt_A) *
  FireExtent_Aerial_rast * CellArea_km2

Cost_Grd_PreA_rast <- subst(
  TRI_cat, from = c(3, 2, 1),
  to = c(Cost_Grd_PreA_adj_D["high"], Cost_Grd_PreA_adj_D["moderate"], Cost_Grd_PreA_adj_D["low"]))


Cost_Grd_PosA_rast <- subst(
  TRI_cat, from = c(3, 2, 1),
  to = c(Cost_Grd_PosA_adj_D["high"], Cost_Grd_PosA_adj_D["moderate"], Cost_Grd_PosA_adj_D["low"]))

Cost_Grd_PrePos_Ann_D_rast <- ((Cost_Grd_PreA_rast + Cost_Grd_PosA_rast) / Stnd_Mgnt_A) *
  FireExtent_Ground_rast * CellArea_km2
```

The final annualised cost raster was calculated as the sum of all
annualised costs for pre-action, management action, post-action and
travel costs. The total cost over 30 years (assumed implementation
period) was calculated as annual cost multiplied by the present value of
an annuity factor for a 4% discount rate over 30 years, specified as

$$C_{\text{Total, 30yrs, present value}} = C_{\text{Total, annualised}} \times \sum_{t=0}^{29} \frac{1}{(1 + 0.04)^t}$$

``` r
# Final annualised cost raster

Cost_PrePos_Ann_D_rast <- sum(c(
  Cost_Aer_PrePos_Ann_D_rast, 
  Cost_Grd_PrePos_Ann_D_rast,
  Cost_PrePos_Office_Ann_D_rast
), na.rm = TRUE)

Cost_Treatment_Ann_D_rast <- sum(c(
  Cost_Buff_Treatment_adj_Ann_D_rast,
  Cost_Aer_Burn_Treatment_adj_Ann_D_rast,
  Cost_Grd_Burn_Treatment_adj_Ann_D_rast
), na.rm = TRUE)

Cost_Equip_Ann_D_rast <- sum(c(
  Cost_Buff_Equip_Ann_D_rast,
  Cost_Aer_Burn_Equip_Ann_D_rast,
  Cost_Grd_Burn_Equip_Ann_D_rast
), na.rm = TRUE)

Cost_Trav_Ann_D_rast <- sum(c(
  Cost_Aer_Trav_Ann_D_rast,
  Cost_Grd_Trav_Ann_D_rast
), na.rm = TRUE)

Cost_Ann_D_rast <- sum(c(
  Cost_PrePos_Ann_D_rast,
  Cost_Treatment_Ann_D_rast,
  Cost_Equip_Ann_D_rast,
  Cost_Trav_Ann_D_rast
), na.rm = TRUE)

names(Cost_Ann_D_rast) <- "CostAnn_AUD_pix"

Cost_PV30_D_rast <- Cost_Ann_D_rast * PV_AnnuityFactor
names(Cost_PV30_D_rast) <- "CostPv30_AUD_pix"

Cost_UD30yr_D_rast <- Cost_Ann_D_rast * TimeHorizon_yr
names(Cost_UD30yr_D_rast) <- "CostUd30_AUD_pix"

Total_Cost_Ann_D <- global(Cost_Ann_D_rast, "sum", na.rm = TRUE)[1,1]
Total_Cost_PV30_D <- global(Cost_PV30_D_rast, "sum", na.rm = TRUE)[1,1]
Total_Cost_UD30_D <- global(Cost_UD30yr_D_rast, "sum", na.rm = TRUE)[1,1]
```

The annualised total estimated costs of fire management actions is
\$39,067,518 per year, with a present value of \$702,579,093 over 30
years. The undiscounted total cost over 30 years is \$1,172,025,537.

``` r
AnnualCost_df <- tibble::tibble(
  Component = c("Pre- and post-action", "Treatment", "Equipment", "Travel"),
  AnnualCost = c(
    global(Cost_PrePos_Ann_D_rast, "sum", na.rm = TRUE)[1,1],
    global(Cost_Treatment_Ann_D_rast, "sum", na.rm = TRUE)[1,1],
    global(Cost_Equip_Ann_D_rast, "sum", na.rm = TRUE)[1,1],
    global(Cost_Trav_Ann_D_rast, "sum", na.rm = TRUE)[1,1]
  )
) %>%
  mutate(Proportion = AnnualCost / sum(AnnualCost),
         PV30yrCost = AnnualCost * PV_AnnuityFactor,
         UD30yrCost = AnnualCost * TimeHorizon_yr)

AnnualCost_df %>% gt::gt() %>% 
  gt::cols_label(Component = "Cost component", 
                 AnnualCost = "Annual cost ($)", 
                 Proportion = "Proportion of total cost",
                 PV30yrCost = "Present value of 30-year cost ($)",
                 UD30yrCost = "Undiscounted 30-year cost ($)") %>%
  gt::fmt_number(columns = c(AnnualCost, PV30yrCost, UD30yrCost), decimals = 2, use_seps = TRUE) %>%
  gt::fmt_percent(columns = Proportion, decimals = 1) %>%
  gt::cols_align(align = "left", columns = Component) %>%
  gt::cols_align(align = "right", columns = c(AnnualCost, Proportion, PV30yrCost, UD30yrCost)) %>%
  gt::tab_options(table.font.size = gt::px(14), data_row.padding = gt::px(4))
```

<div id="tbl-annual-cost">

Table 8: Total annualised cost of fire management actions for the study
area

<div class="cell-output-display">

<div id="ebgmwvbfgw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ebgmwvbfgw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ebgmwvbfgw thead, #ebgmwvbfgw tbody, #ebgmwvbfgw tfoot, #ebgmwvbfgw tr, #ebgmwvbfgw td, #ebgmwvbfgw th {
  border-style: none;
}
&#10;#ebgmwvbfgw p {
  margin: 0;
  padding: 0;
}
&#10;#ebgmwvbfgw .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ebgmwvbfgw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ebgmwvbfgw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ebgmwvbfgw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ebgmwvbfgw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ebgmwvbfgw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ebgmwvbfgw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ebgmwvbfgw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ebgmwvbfgw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ebgmwvbfgw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ebgmwvbfgw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ebgmwvbfgw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ebgmwvbfgw .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ebgmwvbfgw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ebgmwvbfgw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ebgmwvbfgw .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ebgmwvbfgw .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ebgmwvbfgw .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ebgmwvbfgw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ebgmwvbfgw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ebgmwvbfgw .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ebgmwvbfgw .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ebgmwvbfgw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ebgmwvbfgw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ebgmwvbfgw .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ebgmwvbfgw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ebgmwvbfgw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ebgmwvbfgw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ebgmwvbfgw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ebgmwvbfgw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ebgmwvbfgw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ebgmwvbfgw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ebgmwvbfgw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ebgmwvbfgw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ebgmwvbfgw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ebgmwvbfgw .gt_left {
  text-align: left;
}
&#10;#ebgmwvbfgw .gt_center {
  text-align: center;
}
&#10;#ebgmwvbfgw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ebgmwvbfgw .gt_font_normal {
  font-weight: normal;
}
&#10;#ebgmwvbfgw .gt_font_bold {
  font-weight: bold;
}
&#10;#ebgmwvbfgw .gt_font_italic {
  font-style: italic;
}
&#10;#ebgmwvbfgw .gt_super {
  font-size: 65%;
}
&#10;#ebgmwvbfgw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ebgmwvbfgw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ebgmwvbfgw .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ebgmwvbfgw .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ebgmwvbfgw .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ebgmwvbfgw .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ebgmwvbfgw .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ebgmwvbfgw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ebgmwvbfgw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Cost component | Annual cost (\$) | Proportion of total cost | Present value of 30-year cost (\$) | Undiscounted 30-year cost (\$) |
|----|----|----|----|----|
| Pre- and post-action | 5,546,802.91 | 14.2% | 99,752,120.73 | 166,404,087.41 |
| Treatment | 32,857,430.19 | 84.1% | 590,898,648.06 | 985,722,905.62 |
| Equipment | 554,021.73 | 1.4% | 9,963,368.61 | 16,620,651.76 |
| Travel | 109,263.07 | 0.3% | 1,964,955.86 | 3,277,892.08 |

</div>

</div>

</div>

``` r
Cost_Ann_D_plot <- ggplot() + 
  geom_sf(data = STE_WA, fill = "grey70", color = NA) +
  geom_spatraster(data = Cost_Ann_D_rast) +
  scale_fill_gradientn(colours = hcl.colors(n = 9, palette = "YlGnBu", rev = TRUE), 
                       name = "Annual cost ($)", na.value = "transparent") +
  geom_sf(data = StudyArea_SF, aes(color = "black"), fill = NA, linewidth = 0.5) +
  geom_sf(data = Roads_sf, aes(color = "grey70"), linewidth = 0.5) +
  geom_sf(data = WK_urb_sel_pt, aes(shape = SOS_NAME21), colour = "tomato", size = 2) +
  scale_color_manual(values = c("black" = "black", "grey70" = "grey70"),
                     labels = c("black" = "Study area", "grey70" = "Roads"),
                     name = NULL) +
  scale_shape_manual(values = c("Other Urban" = 17, "Bounded Locality" = 18),
                     labels = c("Other Urban" = "Urban", "Bounded Locality" = "Locality"),
                     name = NULL) +
  coord_sf(xlim = st_bbox(StudyArea_SF)[c(1, 3)], ylim = st_bbox(StudyArea_SF)[c(2, 4)]) +
  theme_bw()+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))

Cost_PV30_D_plot <- ggplot() +
  geom_sf(data = STE_WA, fill = "grey70", color = NA) +
  geom_spatraster(data = Cost_PV30_D_rast) +
  scale_fill_gradientn(colours = hcl.colors(n = 9, palette = "YlOrRd", rev = TRUE), 
                       name = "Present value\nof 30-year\ncost ($)", na.value = "transparent") +
  geom_sf(data = StudyArea_SF, aes(color = "black"), fill = NA, linewidth = 0.5) +
  geom_sf(data = Roads_sf, aes(color = "grey70"), linewidth = 0.5) +
  geom_sf(data = WK_urb_sel_pt, aes(shape = SOS_NAME21), colour = "tomato", size = 2) +
  scale_color_manual(values = c("black" = "black", "grey70" = "grey70"),
                     labels = c("black" = "Study area", "grey70" = "Roads"),
                     name = NULL) +
  scale_shape_manual(values = c("Other Urban" = 17, "Bounded Locality" = 18),
                     labels = c("Other Urban" = "Urban", "Bounded Locality" = "Locality"),
                     name = NULL) +
  coord_sf(xlim = st_bbox(StudyArea_SF)[c(1, 3)], ylim = st_bbox(StudyArea_SF)[c(2, 4)]) +
  theme_bw()+
  theme(legend.position = "right")+
  theme(panel.background = element_rect(fill = "#C7E6F5"),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key = element_rect(fill = "white", colour = NA))

Cost_Ann_D_plot / Cost_PV30_D_plot
```

<div id="fig-annual-cost">

![](Fire_regime_files/figure-commonmark/fig-annual-cost-1.png)

Figure 4: Spatial distribution of equivalent (A) annual and (B) present
value of 30-year fire-management cost across the study area. Values
represent annual cost allocated to each ~1 arc-second raster cell.

</div>

We also estimated the total workforce time required to implement fire
management actions across the study area.

``` r
Workday_hr <- 7.5
Workyear_day <- 220
Workyear_hr <- Workday_hr * Workyear_day

Dura_PrePos_Ann_hr_rast <- (Dura_PreA_Off_hr + Dura_PosA_Off_hr) /
  Stnd_Mgnt_A * FireExtent_rast * CellArea_km2

Dura_Buff_Lbr_hr_rast <- Dura_Buff_Lbr_hr / Stnd_Mgnt_A * FireExtent_rast * CellArea_km2

Dura_Aer_PreA_Lbr_hr <- Dura_Aer_PreA_hr * 2
Dura_Aer_Burn_Lbr_hr <- Dura_Aer_Burn_hr * 2
Dura_Aer_Burn_GrdSup_Lbr_hr  <- Dura_Aer_Burn_Grdsup_hr
Dura_Aer_PosA_Lbr_hr <- Dura_Aer_PosA_hr * 2

Dura_Aer_Trav_Lbr_hr_rast <- Dura_Aer_Trav_hr_30m_rast * 2 *  # 2-way travel time
  (Trip_Aer_PreA_n + Trip_Aer_Burn_n + Trip_Aer_PosA_n)

Dura_Aer_Ann_hr_rast <-
  (Dura_Aer_PreA_Lbr_hr + Dura_Aer_Burn_Lbr_hr + Dura_Aer_Burn_GrdSup_Lbr_hr + Dura_Aer_PosA_Lbr_hr) /
  Stnd_Mgnt_A * FireExtent_Aerial_rast * CellArea_km2 +
  (Dura_Aer_Trav_Lbr_hr_rast / Stnd_Mgnt_A * FireExtent_Aerial_rast * CellArea_km2)

Dura_Grd_PreA_Lbr_hr <- Dura_Grd_PreA_hr * Grd_PreA_Lbr_n
Dura_Grd_PosA_Lbr_hr <- Dura_Grd_PosA_hr * Grd_PosA_Lbr_n

Dura_Grd_Trav_Lbr_hr_rast <- Dura_Grd_Trav_hr_30m_rast * 2 * # 2 ways
  (Trip_Grd_PreA_n + Trip_Grd_Burn_n + Trip_Grd_PosA_n)

Dura_Grd_PreA_Lbr_hr_rast <- subst(
  TRI_cat, from = c(3, 2, 1),
  to = c(Dura_Grd_PreA_Lbr_hr["high"], Dura_Grd_PreA_Lbr_hr["moderate"], Dura_Grd_PreA_Lbr_hr["low"]))

Dura_Grd_PosA_Lbr_hr_rast <- subst(
  TRI_cat, from = c(3, 2, 1),
  to = c(Dura_Grd_PosA_Lbr_hr["high"], Dura_Grd_PosA_Lbr_hr["moderate"], Dura_Grd_PosA_Lbr_hr["low"]))

Dura_Grd_Ann_hr_rast <- (Dura_Grd_Burn_Lbr_hr + 
                         Dura_Grd_PreA_Lbr_hr_rast + 
                         Dura_Grd_PosA_Lbr_hr_rast) /
  Stnd_Mgnt_A * FireExtent_Ground_rast * CellArea_km2 +
  (Dura_Grd_Trav_Lbr_hr_rast / Stnd_Mgnt_A * FireExtent_Ground_rast * CellArea_km2)

Dura_Ann_hr_rast <- sum(c(
    Dura_PrePos_Ann_hr_rast, 
    Dura_Buff_Lbr_hr_rast,
    Dura_Aer_Ann_hr_rast, 
    Dura_Grd_Ann_hr_rast), na.rm = TRUE)

Dura_Ann_hr <- global(Dura_Ann_hr_rast, "sum", na.rm = TRUE)[1,1]

LabourHour <- tibble::tibble(
  Component = c("Pre- and post-action", "Buffer Establishment", "Aerial management", "Ground management", "Total"),
  AnnualLabourHour = c(
    global(Dura_PrePos_Ann_hr_rast, "sum", na.rm = TRUE)[1,1],
    global(Dura_Buff_Lbr_hr_rast, "sum", na.rm = TRUE)[1,1],
    global(Dura_Aer_Ann_hr_rast, "sum", na.rm = TRUE)[1,1],
    global(Dura_Grd_Ann_hr_rast, "sum", na.rm = TRUE)[1,1],
    Dura_Ann_hr
  )) %>%
  mutate(FTE_FireManagement = AnnualLabourHour / Workyear_hr)

LabourHour %>% gt::gt() %>% 
  gt::cols_label(Component = "Cost component", 
                 AnnualLabourHour = "Annual labour hours (h)", 
                 FTE_FireManagement = "Full-time equivalent (FTE) staff") %>%
  gt::fmt_number(columns = c(AnnualLabourHour), decimals = 0, use_seps = TRUE) %>%
  gt::fmt_number(columns = c(FTE_FireManagement), decimals = 1, use_seps = TRUE) %>%
  gt::cols_align(align = "left", columns = Component) %>%
  gt::cols_align(align = "right", columns = c(AnnualLabourHour, FTE_FireManagement)) %>%
  gt::tab_options(table.font.size = gt::px(14), data_row.padding = gt::px(4))
```

<div id="tbl-annual-workforce">

Table 9: Estimated total workforce time required to implement fire
management actions across the study area

<div class="cell-output-display">

<div id="crizcamcgp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#crizcamcgp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#crizcamcgp thead, #crizcamcgp tbody, #crizcamcgp tfoot, #crizcamcgp tr, #crizcamcgp td, #crizcamcgp th {
  border-style: none;
}
&#10;#crizcamcgp p {
  margin: 0;
  padding: 0;
}
&#10;#crizcamcgp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 14px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#crizcamcgp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#crizcamcgp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#crizcamcgp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#crizcamcgp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#crizcamcgp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#crizcamcgp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#crizcamcgp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#crizcamcgp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#crizcamcgp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#crizcamcgp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#crizcamcgp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#crizcamcgp .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#crizcamcgp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#crizcamcgp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#crizcamcgp .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#crizcamcgp .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#crizcamcgp .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#crizcamcgp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#crizcamcgp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#crizcamcgp .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#crizcamcgp .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#crizcamcgp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#crizcamcgp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#crizcamcgp .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#crizcamcgp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#crizcamcgp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#crizcamcgp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#crizcamcgp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#crizcamcgp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#crizcamcgp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#crizcamcgp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#crizcamcgp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#crizcamcgp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#crizcamcgp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#crizcamcgp .gt_left {
  text-align: left;
}
&#10;#crizcamcgp .gt_center {
  text-align: center;
}
&#10;#crizcamcgp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#crizcamcgp .gt_font_normal {
  font-weight: normal;
}
&#10;#crizcamcgp .gt_font_bold {
  font-weight: bold;
}
&#10;#crizcamcgp .gt_font_italic {
  font-style: italic;
}
&#10;#crizcamcgp .gt_super {
  font-size: 65%;
}
&#10;#crizcamcgp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#crizcamcgp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#crizcamcgp .gt_indent_1 {
  text-indent: 5px;
}
&#10;#crizcamcgp .gt_indent_2 {
  text-indent: 10px;
}
&#10;#crizcamcgp .gt_indent_3 {
  text-indent: 15px;
}
&#10;#crizcamcgp .gt_indent_4 {
  text-indent: 20px;
}
&#10;#crizcamcgp .gt_indent_5 {
  text-indent: 25px;
}
&#10;#crizcamcgp .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#crizcamcgp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

| Cost component | Annual labour hours (h) | Full-time equivalent (FTE) staff |
|----|----|----|
| Pre- and post-action | 85,226 | 51.7 |
| Buffer Establishment | 218,690 | 132.5 |
| Aerial management | NaN | NaN |
| Ground management | 45,700 | 27.7 |
| Total | 349,616 | 211.9 |

</div>

</div>

</div>
