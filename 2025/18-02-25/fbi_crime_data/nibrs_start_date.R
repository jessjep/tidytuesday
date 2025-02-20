#----------load data----------#

tuesdata <- tidytuesdayR::tt_load(2025, week = 7)
agencies <- tuesdata$agencies

# load packages #

library(tidyverse)
library(stringr)
library(ggplot2)
library(ggnewscale)

#----------tidy data----------#

state <- map_data("state") %>%
  mutate(fill = "fill")

agencies <- agencies %>% 
  filter(between(longitude, -130, -60))

#----------font and text----------#

font_add_google("Vollkorn", "vollkorn")
font <- "vollkorn"
showtext_auto()

title <- "Agency Adoption of the National Incident-Based Reporting System (NIBRS)"
subtitle <- 
"The NIBRS is the USA's national standard for law enforcement crime data reporting.
It captures up to 57 data elements for individual crime incidents, allowing for a 
comprehensive understanding of crime in the country."
caption <- "Created by: jessimoore@bsky.social   Source: FBI Crime Data API"

#----------make the plot----------#

p1 <- ggplot() +
  geom_polygon(data = state, 
               aes(long, lat, 
                   group = group,
                   fill = fill)) +
  geom_point(data = agencies, 
             aes(longitude, latitude, 
                 color = nibrs_start_date,
                 alpha = 0.4),
             size = 2,
             shape = 18) +
  scale_color_viridis_c(labels = scales::date_format("%Y"),
                        option = "G",
                        breaks = as.Date(c("1993-01-01", "2003-01-01",
                                           "2013-01-01", "2023-01-01")),
                        limits = as.Date(c("1991-01-01", "2024-01-01")),
                        guide = guide_colorbar(order = 2)) +
  scale_fill_manual(values = "grey65") +
  labs(color = "Adoption Date",
       title = title,
       subtitle = subtitle,
       caption = caption) +
  new_scale_color() +
  geom_point(data = subset(agencies, is.na(nibrs_start_date)),
             aes(longitude, latitude, 
                 color = "palevioletred3",
                 alpha = 0.4),
             size = 2,
             shape = 18) +
  scale_color_manual(name=NULL, labels="Not Yet Adopted", values="palevioletred3") +
  theme_void() +
  guides(alpha = "none",
         fill = "none") +
  theme(plot.background = element_rect(fill = "slategray4"),
        plot.margin = margin(10,10,10,10),
        plot.title = element_text(family = font, size = 16),
        plot.subtitle = element_text(family = font, size = 11),
        legend.title = element_text(family = font, size = 11),
        legend.text = element_text(family = font, size = 10),
        plot.caption = element_text(family = font, size = 9),
        plot.caption.position = "plot")
p1
