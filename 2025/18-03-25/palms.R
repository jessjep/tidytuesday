# load data #
palmtrees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')

# load libraries #
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggtext)
library(sysfonts)
library(showtext)

# create a hierarchical data frame #
palms <- palmtrees %>%
  group_by(palm_subfamily) %>%
  arrange(palm_subfamily) %>%
  mutate(level1 = "palmcenter",
         level2 = palm_subfamily,
         level3 = palm_tribe,
         level4 = spec_name) %>%
  ungroup() %>%
  select(level1:level4)

# create the edges #
palms_edges_1_2 <- palms %>%
  select(level1, level2) %>%
  unique() %>%
  rename(from = level1, to = level2) %>%
  mutate(subfam = to) # adding this column for colour grouping in the plot

palms_edges_2_3 <- palms %>%
  select(level2, level3) %>%
  unique() %>%
  rename(from = level2, to = level3) %>%
  mutate(subfam = from) # adding this column for colour grouping in the plot

palms_edges_3_4 <- palms %>%
  select(level2, level3, level4) %>%
  unique() %>%
  rename(from = level3, to = level4) %>%
  mutate(subfam = level2) %>% # adding this column for colour grouping in the plot
  select(-level2) 

edge_list <- rbind(palms_edges_1_2, palms_edges_2_3, palms_edges_3_4)

palms_plot <- graph_from_data_frame(edge_list) %>%
  as_tbl_graph()

subfamilies <- c("Arecoideae", "Calamoideae",
                 "Ceroxyloideae", "Coryphoideae",
                 "Nypoideae")

# text and colors #
font_add_google("Milonga", "milonga")
font_add_google("Kurale", "kurale")
ft <- "milonga"
ft2 <- "kurale"
showtext_auto()

title <- "Palm Trees"
subtitle <- "Over 2,500 species of palms exist worldwide, categorized into 29 tribes across
5 subfamilies: <span style = 'color: #04724d;'>**Arecoideae**</span>, <span style = 'color: #74a57f;'>**Calamoideae**</span>, <span style = 'color: #06AD81;'>**Ceroxyloideae**</span>, <span style = 'color: #4c934c;'>**Coryphoideae**</span>, and <span style = 'color: #b7ce63;'>**Nypoideae**</span>.\n
In this diagram, the leaves are made up of thousands of lines, each representing a species of palm.
Each leaf represents a palm tribe, and each colour a subfamily."
caption <- "Created by: jessimoore.bsky.social   Source: {palmtrees}"

c <- c("#04724d", "#74a57f", "#06AD81",
       "#4c934c", "#b7ce63")


# create the plot #
palmtree <- ggraph(palms_plot, layout = "dendrogram", circular = TRUE) +
  geom_edge_arc(aes(width = after_stat(index), edge_colour = subfam)) +
  scale_edge_color_manual(values = c) +
  scale_edge_width(range = c(1,0.2)) +
  theme_void() +
  labs(title = title, subtitle = subtitle, caption = caption) +
  theme(panel.background = element_rect(fill = "#d7fff1", color = NA),
        plot.background = element_rect(fill = "#d7fff1", color = NA),
        plot.title = element_text(family = ft, size = 34, hjust = 0.5),
        plot.subtitle = element_textbox_simple(family = ft2, size = 10, 
                                               hjust = 0.5, halign = 0.5,
                                               margin = margin(15,0,0,0)),
        plot.caption = element_text(family = ft2, size = 8, hjust = 0.5),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = margin(20,20,20,20))
palmtree

