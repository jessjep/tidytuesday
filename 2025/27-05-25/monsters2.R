library(ggiraph)
library(tidyverse)
library(sysfonts)
library(showtext)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load(2025, week = 21)
monsters <- tuesdata$monsters

monsters2 <- monsters %>%
  mutate(size = case_match(size,
                           "Tiny" ~ 1,
                           "Small" ~ 3,
                           "Medium or Small" ~ 3,
                           "Medium" ~ 3,
                           "Large" ~ 4.5,
                           "Huge" ~ 7,
                           "Gargantuan" ~ 10)) %>%
  filter(alignment != "Unaligned")

palette <- c(
  "Aberration"           = "#7A4DA1",
  "Celestial"            = "#F1F1C0",
  "Construct"            = "#2C2A8C",
  "Dragon"               = "#FF6E27",
  "Elemental"            = "#53A6D8",
  "Fey"                  = "#C88BCE",
  "Fiend"                = "#D35445",
  "Giant"                = "#BC5A89",
  "Humanoid"             = "#45B1C6",
  "Monstrosity"          = "#E0B329",
  "Plant"                = "#4C8B7D",
  "Swarm of Tiny Undead" = "#30BEB1",
  "Undead"               = "#89D1A1"
)

background <- "#1E1E1E"
title <- "Monsters of Dungeons & Dragons"
caption <- "Created by @jessimoore.bsky.social, Source: DnD System Reference Document v5.2.1"


font_add_google("Tangerine", "Tangerine")
font_add_google("EB Garamond", "EB Garamond")
font <- "Tangerine"
font2 <- "EB Garamond"
showtext_auto()


p <- ggplot(monsters2, aes(str, int, tooltip = name)) +
  geom_point_interactive(aes(size = size, colour = type), alpha = 0.6) +
  scale_color_manual(values = palette) +
  scale_size_identity(guide = "legend",
                      breaks = c(1, 3, 4.5, 7, 10),
                      labels = c("Tiny", "Small or Medium", "Large", "Huge", "Gargantuan")) +
  guides(color = guide_legend(override.aes = list(size = 4)),
         size = guide_legend(override.aes = list(color = "#EDEDED", alpha = 1))) +
  facet_wrap_interactive(
    ~factor(alignment, levels = c(
      "Lawful Good", "Neutral Good", "Chaotic Good",
      "Lawful Neutral", "Neutral", "Chaotic Neutral",
      "Lawful Evil", "Neutral Evil", "Chaotic Evil")),
    nrow = 3, ncol = 3, scales = "fixed") +
  labs(title = title, colour = "Type", size = "Size", caption = caption,
       x = "Strength", y = "Intelligence") +
  theme(plot.background = element_rect(fill = background),
        panel.background = element_rect(fill = background),
        legend.background = element_rect(fill = background),
        text = element_text(family = font2, colour = "#EDEDED", size = 16),
        plot.title = element_text(size = 58, family = font, face = "bold",
                                  hjust = 0.5, margin = margin(0,0,25,0)),
        plot.title.position = "plot",
        plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(40,0,0,0)),
        plot.caption.position = "plot",
        axis.title = element_text(size = 36, family = font, face = "bold"),
        legend.title = element_text(size = 32, family = font, face = "bold"),
        legend.text = element_text(size = 18, family = font2),
        axis.text = element_text(size = 14, color = "#EDEDED"),
        panel.grid = element_line(colour = "#454e54", linetype = "dashed"),
        strip.background = element_rect(fill = "#444444"),
        strip.text = element_text(face = "bold", color = "#EDEDED", size = 16),
        plot.margin = margin(30,30,30,30))
p

# hover and tooltip aesthetics
hover_css <- "cursor:pointer; 
filter: brightness(1.2) drop-shadow(0px 0px 2px rgba(47, 35, 41, 0.2));
stroke-width:4;
r:4px;
transition: all 0.3s ease;"

tooltip_css <- "color:white;
padding:5px;
border-radius:3px;
font-size:16px;
font-family: 'EB Garamond';"

gdtools::register_gfont("Tangerine")
gdtools::register_gfont("EB Garamond")
interactive_plot <- girafe(ggobj = p, width_svg = 10, height_svg = 10)
int_plot <- girafe_options(interactive_plot, opts_hover(css = hover_css),
                           opts_tooltip(css = tooltip_css, use_fill = TRUE))
int_plot
