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

palette <- palette <- c(
  "Aberration"           = "#7A4DA1",
  "Celestial"            = "#D6D4A7",
  "Construct"            = "#2C2A8C",
  "Dragon"               = "#FF6E27",
  "Elemental"            = "#3671A3",
  "Fey"                  = "#935BA1",
  "Fiend"                = "#9E3A2A",
  "Giant"                = "#BC5A89",
  "Humanoid"             = "#45B1C6",
  "Monstrosity"          = "#D8A31E",
  "Plant"                = "#2E5E4E",
  "Swarm of Tiny Undead" = "#1E8A84",
  "Undead"               = "#57A773"
)

background <- "#262e33"
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
  guides(color = guide_legend(override.aes = list(size = 4))) +
  facet_wrap_interactive(~factor(alignment, c("Lawful Good", "Lawful Neutral", "Lawful Evil",
                                  "Neutral Good", "Neutral", "Neutral Evil",
                                  "Chaotic Good", "Chaotic Neutral", "Chaotic Evil")),
                     nrow = 3, ncol = 3, scales = "fixed") +
  labs(title = title, colour = "Type", size = "Size", caption = caption,
       x = "Strength", y = "Intelligence") +
  theme(plot.background = element_rect(fill = background),
        panel.background = element_rect(fill = background),
        legend.background = element_rect(fill = background),
        text = element_text(family = font2, colour = "#919aa1", size = 16),
        plot.title = element_text(size = 50, family = font, face = "bold",
                                  hjust = 0.5, margin = margin(0,0,25,0)),
        plot.title.position = "plot",
        plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(40,0,0,0)),
        plot.caption.position = "plot",
        axis.title = element_text(size = 30, family = font, face = "bold"),
        legend.title = element_text(size = 28, family = font, face = "bold"),
        panel.grid = element_line(colour = "#454e54", linetype = "dashed"),
        strip.background = element_rect(fill = "#454e54"),
        strip.text = element_text(face = "bold"),
        plot.margin = margin(30,30,10,30))
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
