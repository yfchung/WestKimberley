# This script is to process thrstened speceis data

library(terra)
library(sf)
library(tidyverse)
library(tidyterra)
library(ggspatial)
library(ggpubr)
library(shadowtext)
library(RColorBrewer)
library(ggrepel)
library(patchwork)
options(max.print = 99)

INPUT_DIR <- file.path("input")
OUTPUT_DIR <- file.path("output")

# Load state boundary
STE <- st_read(file.path(INPUT_DIR, "STE_2021_AUST_SHP_GDA2020", "STE_2021_AUST_GDA2020.shp"))
STE_WA <- STE %>% filter(STE_NAME21 == "Western Australia") %>% st_make_valid() %>% st_as_sf()

# Crop SNES to WA and make valid
SNES <- st_read(file.path(INPUT_DIR, "snes_public_gdb.gdb"), layer = "SPECIES_SDE_SNES_Public") %>%
        st_transform(st_crs(STE_WA)) %>%
        st_make_valid()

# Check which geometries are invalid
SNES[!st_is_valid(SNES),]
### One very stubburn geometry cannot be fixed.
### Black-browed Albatross (Thalassarche melanophris) is removed from the dataset.
SNES_WA <- SNES[st_is_valid(SNES),] %>%
    st_intersection(STE_WA) %>% st_make_valid() %>%
    select("LISTED_TAXON_ID", "SCIENTIFIC_NAME","VERNACULAR_NAME", "THREATENED_STATUS", "TAXON_GROUP", "TAXON_FAMILY", "TAXON_ORDER", "TAXON_CLASS", "TAXON_PHYLUM", "TAXON_KINGDOM")

st_write(SNES_WA, file.path(OUTPUT_DIR, "data", "SNES_WA.gpkg"), append = FALSE)

# Get SNES that intersect with IBRA ----
SNES_WA <- st_read(file.path(OUTPUT_DIR, "data", "SNES_WA.gpkg"))

## Load IBRA subregions, filter to West Kimberley and WA
IBRAsub <- st_read(file.path(INPUT_DIR, "Interim_Biogeographic_Regionalisation_for_Australia_(IBRA)_Version_7.1_(Subregions).gdb"), layer = "IBRA71_subregions") %>%
    st_transform(crs = st_crs(STE)) %>% st_make_valid()
IBRA_WK <- IBRAsub %>%
  filter(REG_NAME_7 %in% c("Central Kimberley", "Dampierland", "Northern Kimberley", "Ord Victoria Plain", "Victoria Bonaparte")) %>%
  mutate(Area_Ha = as.numeric(st_area(.))/10000) %>% select(REG_NAME_7, REG_CODE_7, SUB_NAME_7, SUB_CODE_7, Area_Ha)
IBRA_WK_WA <- st_intersection(IBRA_WK, STE_WA)  %>% st_make_valid()%>% select(REG_NAME_7, REG_CODE_7, SUB_NAME_7, SUB_CODE_7, Area_Ha)
IBRA_WK_WA_dsvl <- st_union(IBRA_WK_WA) %>% st_as_sf() %>% st_make_valid()

## Get SNES in each IBRA subregion in West Kimberley
SNES_IBRA_WK <-  st_read(file.path(OUTPUT_DIR, "data", "SNES_WA.gpkg"))%>%
        filter(THREATENED_STATUS != "NA") %>%
        st_intersection(IBRA_WK_WA) %>% st_make_valid()
st_write(SNES_IBRA_WK, file.path(OUTPUT_DIR, "data", "SNES_IBRA_WK.gpkg"), append = FALSE)

SNES_IBRA_WK <- st_read(file.path(OUTPUT_DIR, "data", "SNES_IBRA_WK.gpkg"))

SNES_IBRA_WK_DF <- SNES_IBRA_WK %>%
    st_drop_geometry() %>%
    distinct()

### Generate output for threat status and taxonomic group
names(SNES_IBRA_WK_DF)

unique(SNES_IBRA_WK_DF$LISTED_TAXON_ID)

SNES_IBRA_WK_Status_Taxon_DF <- SNES_IBRA_WK_DF %>%
    select(LISTED_TAXON_ID, TAXON_CLASS, THREATENED_STATUS) %>%
    distinct() %>%
    group_by(TAXON_CLASS, THREATENED_STATUS) %>%
    summarise(n = n(), .groups = 'drop_last') %>%
    arrange(TAXON_CLASS, THREATENED_STATUS) %>%
    left_join(SNES_IBRA_WK_DF %>% select(TAXON_CLASS, THREATENED_STATUS, TAXON_GROUP, TAXON_PHYLUM) %>% distinct(), by = c("TAXON_CLASS", "THREATENED_STATUS"))
SNES_IBRA_WK_Status_Taxon_DF

TAXON_CLASS_LVL <- c("Liliopsida", "Magnoliopsida", "Gastropoda", "Chondrichthyes", "Reptilia", "Aves", "Mammalia") %>% rev()

SNES_IBRA_WK_Status_Taxon_DF2 <- SNES_IBRA_WK_Status_Taxon_DF %>%
    mutate(TAXON_CLASS = factor(TAXON_CLASS, levels = TAXON_CLASS_LVL)) %>%
    arrange(TAXON_CLASS, THREATENED_STATUS)

SNES_IBRA_WK_Status_Taxon_plot <- ggplot(SNES_IBRA_WK_Status_Taxon_DF2, aes(y = THREATENED_STATUS, x = n, fill = TAXON_CLASS)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 5, color = "grey20", alpha = .75) +
    scale_y_discrete(limits = c("Conservation Dependent", "Critically Endangered", "Endangered", "Vulnerable"),
                     labels = c("Conservation\nDependent", "Critically\nendangered", "Endangered", "Vulnerable")) +
    scale_x_continuous(minor_breaks = seq(0, SNES_IBRA_WK_Status_Taxon_DF %>% group_by(THREATENED_STATUS) %>% summarise(N = sum(n)) %>% pull(N) %>% max(), by = 2)) +
    labs(x = "Threatened Status", y = "Number of Species", fill = "Taxon Class") +
    theme_pubr() +
    scale_fill_manual(values = palette.colors(9, palette = "Okabe-Ito")[2:9]) +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
          axis.title.y = element_blank(),
          legend.position = c(0.8, 0.2),
          legend.text = element_text(size = 14), legend.title = element_text(size = 14),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
          panel.grid.minor.x = element_line(color = "grey80", linetype = "dashed", linewidth = 0.2))
SNES_IBRA_WK_Status_Taxon_plot
ggsave(file.path(OUTPUT_DIR, "figures", "SNES_IBRA_WK_Status_Taxon_plot.png"),SNES_IBRA_WK_Status_Taxon_plot, width = 10, height = 6, dpi = 300)

SNES_IBRA_WK_Status_Taxon_TBL <- SNES_IBRA_WK_Status_Taxon_DF %>% ungroup() %>%
    select(TAXON_GROUP, THREATENED_STATUS, n) %>%
    pivot_wider(names_from = THREATENED_STATUS, values_from = n) %>%
    arrange(TAXON_GROUP)
SNES_IBRA_WK_Status_Taxon_TBL
write.csv(SNES_IBRA_WK_Status_Taxon_TBL, file.path(OUTPUT_DIR, "data", "SNES_IBRA_WK_Status_Taxon_TBL.csv"), row.names = FALSE)


# SNES threats and  Alaia2025 ----

# Read in Alaia2025 data
ALAIA2025 <- readxl::read_xlsx(file.path(INPUT_DIR, "Species-Threat-TAS-DS-ALAIA2025 .xlsx"), sheet = "Alaia2025STMUpdate(+TAS)")
ALAIA2025 <- ALAIA2025 %>%
  mutate(across(starts_with("TAS"), ~ as.integer(.x == 1))) %>%
  mutate(across(starts_with("TAS"), ~ replace_na(.x, 0L)))

# Identify which SNES_IBRA_WK_DF species are not in the ALAIA2025 list
anti_join(SNES_IBRA_WK_DF %>% select(SCIENTIFIC_NAME) %>% distinct(), 
          ALAIA2025 %>% select("Scientific Name (SPRAT)") %>% distinct(), by = c("SCIENTIFIC_NAME" = "Scientific Name (SPRAT)")) %>%
                pull(SCIENTIFIC_NAME)

SNES_IBRA_WK_ALAIA_DF <- full_join(SNES_IBRA_WK_DF %>% select(LISTED_TAXON_ID, SCIENTIFIC_NAME, VERNACULAR_NAME, THREATENED_STATUS, TAXON_GROUP, TAXON_CLASS) %>% distinct(),
                                   ALAIA2025, by = c("SCIENTIFIC_NAME" = "Scientific Name (SPRAT)")) %>% 
                         mutate(LISTED_TAXON_ID = na_if(LISTED_TAXON_ID, "<NA>")) %>%
                         drop_na(LISTED_TAXON_ID, "Broad level threat") %>% distinct()
write_csv(SNES_IBRA_WK_ALAIA_DF, file.path(OUTPUT_DIR, "data", "SNES_IBRA_WK_ALAIA_DF.csv"))                         
SNES_IBRA_WK_ALAIA_DF[SNES_IBRA_WK_ALAIA_DF$`Broad level threat`=="NA",]
sum(is.na(SNES_IBRA_WK_ALAIA_DF$LISTED_TAXON_ID))
unique(SNES_IBRA_WK_ALAIA_DF$LISTED_TAXON_ID) %>% sort()

nrow(SNES_IBRA_WK_ALAIA_DF)
SNES_IBRA_WK_ALAIA_DF %>% distinct() %>% nrow()
SNES_IBRA_WK_ALAIA_DF %>% select(SCIENTIFIC_NAME, 'Sub category threat') %>% distinct() %>% nrow()
View(SNES_IBRA_WK_ALAIA_DF)
SNES_IBRA_WK_ALAIA_DF$SCIENTIFIC_NAME %>% unique() %>% length()
SNES_IBRA_WK_ALAIA_DF$"Broad level threat" %>% unique()
SNES_IBRA_WK_ALAIA_DF$"Sub category threat" %>% unique()
SNES_IBRA_WK_ALAIA_DF[SNES_IBRA_WK_ALAIA_DF$`Broad level threat`=="NA",]

SNES_IBRA_WK_ALAIA_DF %>% filter(`Broad level threat` == "Invasive species and diseases") %>% 
    pivot_longer(cols = starts_with("TAS"), names_to = "TAS", values_to = "nSpecies") %>% 
    filter(nSpecies > 0) %>%
    pull("TAS") %>% unique()


SNES_IBRA_WK_B_THREAT <- SNES_IBRA_WK_ALAIA_DF  %>% 
    select(SCIENTIFIC_NAME, `Broad level threat`, THREATENED_STATUS) %>% distinct() %>%
    group_by(`Broad level threat`, THREATENED_STATUS) %>% summarise(nSpecies = n()) %>% arrange(desc(nSpecies)) %>% 
    mutate(`Broad level threat` = case_when(`Broad level threat` == "Overexploitation and other direct harm from human activities" ~ "Overexploitation and other direct harm", 
                                            # `Broad level threat` == "Habitat loss, fragmentation and degradation" ~ "Habitat loss & degradation",
                                            .default = `Broad level threat`),
           `Broad level threat` = str_replace(`Broad level threat`, pattern = " and ", replacement = " & "),
           `Broad level threat` = str_wrap(`Broad level threat`, width = 27))

BLT_lvl <- SNES_IBRA_WK_ALAIA_DF %>% select(SCIENTIFIC_NAME, `Broad level threat`) %>% distinct() %>% 
    group_by(`Broad level threat`) %>% summarise(nSpecies = n()) %>% arrange(desc(nSpecies)) %>% pull(`Broad level threat`)

SNES_IBRA_WK_subCat_THREAT <- SNES_IBRA_WK_ALAIA_DF  %>% 
    select(SCIENTIFIC_NAME, `Sub category threat`, THREATENED_STATUS) %>% distinct() %>%
    group_by(`Sub category threat`, THREATENED_STATUS) %>% summarise(nSpecies = n()) %>% arrange(desc(nSpecies))

SNES_IBRA_WK_TAS <- SNES_IBRA_WK_ALAIA_DF %>% 
    select(SCIENTIFIC_NAME, THREATENED_STATUS, starts_with("TAS")) %>% 
    group_by(SCIENTIFIC_NAME, THREATENED_STATUS) %>% 
    summarise(across(starts_with("TAS"), ~ max(.x))) %>% ungroup() %>% 
    group_by(THREATENED_STATUS) %>% 
    summarise(across(starts_with("TAS"), ~ sum(.x))) %>% 
    pivot_longer(cols = starts_with("TAS"), names_to = "TAS", values_to = "nSpecies") %>%
    filter(nSpecies > 0) %>%
    mutate(TAS = str_replace(TAS, pattern = "TAS ", replacement = ""),
           TAS = str_replace(TAS, pattern = ":", replacement = "-"))
print(SNES_IBRA_WK_TAS, n = Inf)

unique(SNES_IBRA_WK_ALAIA_DF$`Broad level threat`)
unique(SNES_IBRA_WK_ALAIA_DF$`Sub category threat`)
unique(SNES_IBRA_WK_TAS$TAS)

# Create 

####
# Endemic SNES in West Kimberley
SNES <- st_read(file.path(INPUT_DIR, "snes_public_gdb.gdb"), layer = "SPECIES_SDE_SNES_Public") %>%
        st_transform(st_crs(STE_WA)) %>%
        st_make_valid()
SNES <- SNES[st_is_valid(SNES),]
SNES_dsvl <- st_union(SNES) %>% st_as_sf() %>% st_make_valid()

SNES_IBRA_WK_dsvl <- st_union(SNES_IBRA_WK) %>% st_as_sf() %>% st_make_valid()

SNES_SName_DF <- SNES %>% st_drop_geometry() %>% select(SCIENTIFIC_NAME) %>% distinct()
SNES_IBRA_WK_SName_DF <- SNES_IBRA_WK %>% st_drop_geometry() %>% select(SCIENTIFIC_NAME) %>% distinct() 

SNES_IBRA_WK_Endemic_DF <- anti_join(SNES_SName_DF, SNES_IBRA_WK_SName_DF, by = "SCIENTIFIC_NAME")


# plotting ----

SNES_IBRA_WK_B_THREAT_plot <- ggplot(SNES_IBRA_WK_B_THREAT, 
                                    aes(y = fct_reorder(`Broad level threat`, nSpecies, .fun = sum, .desc = FALSE), 
                                    x = nSpecies, fill = THREATENED_STATUS)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = nSpecies), position = position_stack(vjust = 0.5), size = 5, color = "grey90", alpha = .75) +
    labs(x = "Number of Species", fill = "Threatened Status") +
    theme_pubr() +
    scale_fill_manual(values = palette.colors(3, palette = "Set 2"),
                      breaks = c("Conservation Dependent", "Critically Endangered", "Endangered", "Vulnerable")) +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
          axis.title.y = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 14), legend.title = element_text(size = 14),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
          panel.grid.minor.x = element_line(color = "grey80", linetype = "dashed", linewidth = 0.2))
SNES_IBRA_WK_B_THREAT_plot
ggsave(file.path(OUTPUT_DIR, "figures", "SNES_IBRA_WK_B_THREAT_plot.png"), SNES_IBRA_WK_B_THREAT_plot, width = 6, height = 5, dpi = 300)

SNES_IBRA_WK_subCat_THREAT_plot <- ggplot(SNES_IBRA_WK_subCat_THREAT, 
                                    aes(y = fct_reorder(`Sub category threat`, nSpecies, .fun = sum, .desc = FALSE), 
                                    x = nSpecies, fill = THREATENED_STATUS)) +
    geom_bar(stat = "identity", position = "stack") +
    # geom_text(aes(label = nSpecies), position = position_stack(vjust = 0.5), size = 3, color = "grey90", alpha = .75) +
    labs(x = "Number of Species", fill = "Threatened Status") +
    theme_pubr() +
    scale_fill_manual(values = palette.colors(3, palette = "Set 2"),
                      breaks = c("Conservation Dependent", "Critically Endangered", "Endangered", "Vulnerable")) +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
          axis.title.y = element_blank(),
          legend.position = c(0.6, 0.2),
          legend.text = element_text(size = 14), legend.title = element_text(size = 14),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
          panel.grid.minor.x = element_line(color = "grey80", linetype = "dashed", linewidth = 0.2))
SNES_IBRA_WK_subCat_THREAT_plot
ggsave(file.path(OUTPUT_DIR, "figures", "SNES_IBRA_WK_subCat_THREAT_plot.png"), SNES_IBRA_WK_subCat_THREAT_plot, width = 10, height = 10, dpi = 300)


SNES_IBRA_WK_TAS_plot <- ggplot(SNES_IBRA_WK_TAS, aes(y = fct_reorder(TAS, nSpecies, .fun = sum, .desc = FALSE), x = nSpecies, fill = THREATENED_STATUS)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = nSpecies), position = position_stack(vjust = 0.5), size = 5, color = "grey90", alpha = .75) +
    labs(x = "Number of Species", y = "Threat Abatement Strategies", fill = "Threatened Status") +
    theme_pubr() +
    scale_fill_manual(values = palette.colors(3, palette = "Set 2"),
                      breaks = c("Conservation Dependent", "Critically Endangered", "Endangered", "Vulnerable")) +
    theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14),
          legend.position = c(0.6, 0.2),
          legend.text = element_text(size = 14), legend.title = element_text(size = 14),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
          panel.grid.minor.x = element_line(color = "grey80", linetype = "dashed", linewidth = 0.2))
SNES_IBRA_WK_TAS_plot
ggsave(file.path(OUTPUT_DIR, "figures", "SNES_IBRA_WK_TAS_plot.png"), SNES_IBRA_WK_TAS_plot, width = 15, height = 7, dpi = 300)

###############################################################################################################################
summary(SNES_IBRA_WK_DF)
unique(SNES_IBRA_WK_DF$THREATENED_STATUS)
unique(SNES_IBRA_WK_DF$TAXON_GROUP)
SNES_IBRA_WK_DF %>% filter(THREATENED_STATUS == "Conservation Dependent")

SNES_SA_DF %>% group_by(TAXON_GROUP, THREATENED_STATUS) %>% summarise(n = n()) %>% arrange(TAXON_GROUP, THREATENED_STATUS)
SNES_SA_DF %>% group_by(TAXON_CLASS, THREATENED_STATUS) %>% summarise(n = n()) %>% arrange(TAXON_CLASS, THREATENED_STATUS)
SNES_SA_DF_SUM <- SNES_SA_DF %>% group_by(TAXON_GROUP, TAXON_CLASS, THREATENED_STATUS) %>% summarise(n = n()) %>%
    pivot_wider(names_from = THREATENED_STATUS, values_from = n) %>%
    arrange(TAXON_GROUP, TAXON_CLASS)
write.csv(SNES_SA_DF_SUM, file.path(OUTPUT_DIR, "data", "SNES_SA_DF_SUM.csv"), row.names = FALSE)

SNES_SA_DF_names <- SNES_SA_DF %>% filter(THREATENED_STATUS != "NA") %>%
    select(VERNACULAR_NAME, SCIENTIFIC_NAME, THREATENED_STATUS) %>%
    arrange(THREATENED_STATUS, SCIENTIFIC_NAME) %>%
    distinct()
SNES_SA_DF_names %>% group_by(THREATENED_STATUS) %>% summarise(n = n()) %>% arrange(THREATENED_STATUS)

SNES_SA_DF_names_list <- tibble(
    "Critically endangered" = c(SNES_SA_DF_names %>% filter(THREATENED_STATUS == "Critically Endangered") %>% pull(VERNACULAR_NAME), rep("NA", 25)),
    "Endangered" = c(SNES_SA_DF_names %>% filter(THREATENED_STATUS == "Endangered") %>% pull(VERNACULAR_NAME), rep("NA", 11)),
    "Vulnerable" = c(SNES_SA_DF_names %>% filter(THREATENED_STATUS == "Vulnerable") %>% pull(VERNACULAR_NAME)),
    "Conservation dependent" = c(SNES_SA_DF_names %>% filter(THREATENED_STATUS == "Conservation Dependent") %>% pull(VERNACULAR_NAME), rep("NA", 32))
)

write.csv(SNES_SA_DF_names_list, file.path(OUTPUT_DIR, "data", "SNES_SA_DF_names_list.csv"), row.names = FALSE)


SNES_SA_DF_na <- SNES_SA_DF %>% filter(is.na(THREATENED_STATUS))
SNES_SA_DF_na %>% pull(VERNACULAR_NAME)
SNES_SA_DF %>% filter(THREATENED_STATUS == "Critically Endangered") %>% pull(VERNACULAR_NAME)



# Combined with Species-Threat-TAS-DS-ALAIA2025 to get TAS for each treatedn species and actions