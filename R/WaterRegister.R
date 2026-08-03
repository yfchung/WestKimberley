library(tidyverse)
library(sf)
library(ggpubr)


INPUT_DIR <- file.path("input")
OUTPUT_DIR <- file.path("output")

WaterRegister_DIR <- file.path(INPUT_DIR, "WaterRegister")
WaterRegister_Flist <- list.files(WaterRegister_DIR, pattern = "\\.csv$", full.names = TRUE)

WaterRegister_Raw_DF <- map(WaterRegister_Flist, function(FPath){
    RawCSV <- read_csv(FPath, skip = 3)
    RedundantRowStart <- which(RawCSV[[1]] == "REGISTER EXTRACT INFORMATION")
    if(length(RedundantRowStart) > 0){
        RawCSV <- RawCSV[1:(RedundantRowStart - 1), ]
    }
    RawCSV
}) %>% bind_rows()  %>% 
    distinct(`Licence Number`, .keep_all = TRUE) %>% 
    rename_with(~ str_replace_all(., "\\s+", "_"))
View(WaterRegister_Raw_DF)

WaterRegister_Raw_DF %>% 
    mutate(Issue_Date = as.Date(Issue_Date, format = "%d/%m/%Y"),
           Expiry_Date = as.Date(Expiry_Date, format = "%d/%m/%Y"),
           Issue_YearMonth = format(Issue_Date, "%Y-%m"),
           Licence_Allocation = as.integer(str_replace(Licence_Allocation, " KL", "")) / 1000) %>% 
    filter(Expiry_Date > Sys.Date()) %>%
    count(Licence_Type)

WaterRegister_Raw_DF %>% 
    mutate(Issue_Date = as.Date(Issue_Date, format = "%d/%m/%Y"),
           Expiry_Date = as.Date(Expiry_Date, format = "%d/%m/%Y"),
           Issue_YearMonth = format(Issue_Date, "%Y-%m"),
           Licence_Allocation = as.integer(str_replace(Licence_Allocation, " KL", "")) / 1000) %>% 
    filter(Expiry_Date > Sys.Date()) %>%
    pull(Licence_Allocation) %>% sum()

WaterRegister_DF <- WaterRegister_Raw_DF %>% 
    mutate(Issue_Date = as.Date(Issue_Date, format = "%d/%m/%Y"),
           Expiry_Date = as.Date(Expiry_Date, format = "%d/%m/%Y"),
           Issue_YearMonth = format(Issue_Date, "%Y-%m"),
           Licence_Allocation = as.integer(str_replace(Licence_Allocation, " KL", "")) / 1000) %>%
    filter(Expiry_Date > as.Date("01/01/2026")) %>%
    select(Licence_Number, Issue_Date, Issue_YearMonth, Licence_Type, Licence_Allocation) %>%
    group_by(Issue_YearMonth, Licence_Type) %>%
    summarise(Licence_Allocation = sum(Licence_Allocation, na.rm = TRUE), .groups = "drop_last") %>%
    ungroup()

WaterRegister_Raw_DF %>% 
    mutate(Issue_Date = as.Date(Issue_Date, format = "%d/%m/%Y"),
           Expiry_Date = as.Date(Expiry_Date, format = "%d/%m/%Y"),
           Issue_YearMonth = format(Issue_Date, "%Y-%m"),
           Licence_Allocation = as.integer(str_replace(Licence_Allocation, " KL", "")) / 1000) %>%
    filter(Expiry_Date > as.Date("01/01/2026")) %>% View()

Licence_Allocation_Date_plot <- ggplot(WaterRegister_DF, aes(x = Issue_YearMonth, y = Licence_Allocation, fill = Licence_Type))+
    geom_bar(stat = "identity", position = "dodge")+
    theme_pubr() +
    scale_fill_manual(values = c("goldenrod3", "steelblue3"), name = "Licence Type")+
    scale_y_continuous(breaks = seq(0, 325, by = 50),
                        minor_breaks = seq(0, 325, by = 25)) +
    #label the bars that extended beyond the y-axis limit
    geom_text(data = WaterRegister_DF %>% filter(Licence_Allocation > 300), 
                aes(x = Issue_YearMonth,
                    y = 280, 
                label = paste0("Licence allocation\n> 300 mega litres:\n", round(Licence_Allocation, 1), "mega litres")), 
                hjust = 0, size = 4)+
    labs(x = "Licence date (Issued year-month)", y = "Licence allocation (Mega Litres)")+
    guides(y = guide_axis(minor.ticks = TRUE))+
    coord_cartesian(y = c(0, 300))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.text = element_text(size = 14), legend.title = element_text(size = 14))+
    theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14))

Licence_Allocation_Date_plot
ggsave(file.path("output", "figures", "Licence_Allocation_Date_plot.png"), Licence_Allocation_Date_plot, width = 10, height = 6, dpi = 300, bg = "transparent")

?scale_y_continuous
