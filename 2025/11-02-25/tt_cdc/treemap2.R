# load packages #

library(tidyverse)
library(treemapify)
library(ggplot2)
library(paletteer)
library(sysfonts)
library(showtext)

# load data #

tuesdata <- tidytuesdayR::tt_load(2025, week = 6)
cdc_datasets <- tuesdata$cdc_datasets
fpi_codes <- tuesdata$fpi_codes

# tidy data #

codes <- fpi_codes %>% # extracts the program codes to add to the main data frame
  select(agency_name, program_name, program_code_pod_format) %>%
  rename(program_code = program_code_pod_format)

programs <- full_join(codes, cdc_datasets) %>% # join with the main data frame
  select(-c(collection, language, analytical_methods_reference,
            access_level_comment, glossary_methodology,
            references, geospatial_resolution, geographic_unit_of_analysis,
            described_by)) %>% # removes columns not needed
  drop_na(dataset_url)

treemap_df <- programs %>% # manipulating the data frame for plotting by count data
  group_by(category) %>%
  mutate(category = case_when(
    category == "NNDSS" ~ "National Notifiable Disease Surveillance System (NNDSS)",
    category == "NCHS" ~ "National Center for Health Statistics",
    category == "National Institute for Occupational Safety and Health" ~ "National Institute for\nOccupational Safety and Health",
    category == "This dataset has not been categorized" ~ "Not Categorized",
    TRUE ~ category),
    category = factor(category)) %>%
  count(category, sort = TRUE)

# plot preparation #
font_add_google("Karla", "karla")
showtext_auto(enable = TRUE)
font <- "karla"


title_text = "CDC Datasets backed up during Trump Administration"
subtitle_text = "Visualises the number of datasets, by category, of approximately 1257 datasets 
uploaded to https://data.cdc.gov/browse before January 28th, 2025, backed up to archive.org. 

Among these, 293 were created by the NNDSS, which collects case surveillance data to help 
understand the spread of diseases and control outbreaks."
caption_text = "Created by: jessimoore@bsky.social  Source: archive.org/details/20250128-cdc-datasets"
legend_title = "Number of Datasets"

# making the plot #

treemap <- ggplot(treemap_df, aes(area = n, fill = n, subgroup = n)) +
  geom_treemap(start = "topleft", layout = "srow") + 
  geom_treemap_text(start = "topleft", layout = "srow", colour = "#f8f5ec", place = "centre",
                    family = font, size = 12, aes(label = category),
                    reflow = TRUE) + 
  geom_treemap_subgroup_border(start = "topleft", layout = "srow", 
                               colour = "#ded9cc", size = 1.5) +
  geom_treemap_subgroup_text(start = "topleft", layout = "srow",
                             place = "bottomright",
                             alpha = 0.6, colour = "#f8f5ec",
                             fontface = "italic", size = 12) +
  scale_fill_paletteer_c("ggthemes::Sunset-Sunrise Diverging") +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       fill = legend_title) +
  theme(plot.title = element_text(family = font, size = 20, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0.5),
        legend.title = element_text(family = font, size = 10),
        legend.text = element_text(family = font, size = 10),
        legend.position = "bottom",
        plot.caption = element_text(family = font, size = 8, hjust = 0.5),
        plot.background = element_rect(fill = "#f8f5ec"),
        legend.background = element_rect(fill = "#f8f5ec"),
        plot.margin = margin(1,1,1,1, "cm"))
treemap

