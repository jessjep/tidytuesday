# load packages #
library(tidyverse)
library(treemapify)
library(ggplot2)
library(paletteer)

# load data #
tuesdata <- tidytuesdayR::tt_load(2025, week = 6)
cdc_datasets <- tuesdata$cdc_datasets
fpi_codes <- tuesdata$fpi_codes

# tidy data #

codes <- fpi_codes %>%
  select(agency_name, program_name, program_code_pod_format) %>%
  rename(program_code = program_code_pod_format)

programs <- full_join(codes, cdc_datasets) %>%
  select(-c(collection, language, analytical_methods_reference,
            access_level_comment, glossary_methodology,
            references, geospatial_resolution, geographic_unit_of_analysis,
            described_by)) %>%
  drop_na(dataset_url)

treemap_df <- programs %>%
  group_by(program_name, category) %>%
  mutate(category = case_when(
    category == "NNDSS" ~ "National Notifiable Disease\nSurveillance System",
    category == "NCHS" ~ "National Center\nfor Health Statistics",
    category == "National Institute for Occupational Safety and Health" ~ "National Institute for\nOccupational Safety and Health",
    category == "This dataset has not been categorized" ~ "Not Categorized",
    TRUE ~ category),
    category = factor(category),
    program_name = factor(program_name),
    program_name = case_when(
      program_name == "HIV/AIDS, Viral Hepatitis, Sexually Transmitted Diseases (STD), and Tuberculosis (TB) Prevention" ~
        "HIV/AIDS, Viral Hepatitis, STD, and TB Prevention",
      program_name == "Birth Defects, Developmental Disabilities, Disabilities and Health" ~
        "Birth Defects, Developmental Disabilities",
      TRUE ~ program_name)
  ) %>%
  count(category, sort = TRUE) %>%
  drop_na(program_name)

font_add_google("Goudy Bookletter 1911", "goudy_bookletter")
showtext_auto(enable = TRUE)
font <- "Goudy Bookletter 1911"


title_text = "Archived CDC Datasets"
subtitle_text = "A visualisation of the categories of approximately 1257 datasets uploaded to https://data.cdc.gov/browse
       before January 28th, 2025, backed up to archive.org."
caption_text = "Created by: jessimoore@bsky.social  Source: archive.org/details/20250128-cdc-datasets"
legend_title = "Number of Datasets"

p <- ggplot(treemap_df, aes(area = n, fill = program_name)) +
  geom_treemap(start = "topleft", layout = "squarified") + 
  geom_treemap_text(start = "topleft", colour = "white", place = "centre",
                    size = 15, aes(label = category)) + 
  scale_fill_paletteer_d("MetBrewer::Signac") +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       fill = legend_title) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
p

