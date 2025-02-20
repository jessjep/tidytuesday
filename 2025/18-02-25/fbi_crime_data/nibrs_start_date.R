# load data #

tuesdata <- tidytuesdayR::tt_load(2025, week = 7)
agencies <- tuesdata$agencies

# load packages #

library(ggplot2)
library(tidyverse)
library(stringr)
library(ggnewscale)

# tidy data #

state <- map_data("state") %>%
  mutate(fill = "fill")

agencies <- agencies %>% 
  filter(between(longitude, -130, -60))

# make the plot #

p1 <- ggplot() +
  geom_polygon(data = state, 
               aes(long, lat, 
                   group = group,
                   fill = fill)) +
  geom_point(data = agencies, 
             aes(longitude, latitude, 
                 color = nibrs_start_date),
             alpha = 0.4,
             size = 2,
             shape = 18) +
  scale_color_viridis_c(labels = scales::date_format("%Y"),
                        option = "G",
                        breaks = as.Date(c("1995-01-01", "2000-01-01", 
                                           "2005-01-01", "2010-01-01",
                                           "2015-01-01", "2020-01-01")),
                        limits = as.Date(c("1991-01-01", "2024-01-01")),
                        guide = guide_colorbar(order = 2)) +
  scale_fill_manual(values = "grey65") +
  labs(color = "Start Date") +
  new_scale_color() +
  geom_point(data = subset(agencies, is.na(nibrs_start_date)),
             aes(longitude, latitude, 
                 color = "palevioletred3"),
             alpha = 0.4,
             size = 2,
             shape = 18) +
  scale_color_manual(name=NULL, labels="Not Yet Participating", values="palevioletred3") +
  theme_void() +
  guides(alpha = "none",
         fill = "none") +
  theme(plot.background = element_rect(fill = "slategray4"))
p1
