tuesdata <- tidytuesdayR::tt_load(2025, week = 13)
pokemon_df <- tuesdata$pokemon_df

library(tidyverse)
library(waffle)
library(forcats)

pok <- pokemon_df %>%
  drop_na(generation_id) %>%
  mutate(generation_id = factor(generation_id)) %>%
  group_by(generation_id) %>%
  arrange(generation_id, color_1) %>%
  mutate(pokemon = fct_reorder(factor(pokemon), type_1)) %>%
  mutate(type_1 = factor(toupper(type_1))) %>%
  ungroup()


colors <- pok %>%
  distinct(type_1, color_1) %>%
  deframe()

t <- "POKEMON TYPES AND COLORS"
st <- "Each row represents a generation, from Gen 1 (top) to Gen 7 (bottom)."
cptn <- "Created by jessimoore.bsky.social      Source: {pokemon}"

library(sysfonts)
library(showtext)

font_add_google("Delius", "delius")
ft <- "delius"
showtext_auto()

plot <- ggplot(pok, aes(fill = type_1, values = 1)) +
  geom_waffle(n_rows = 1, color = "white",
              radius = grid::unit(2.5, "npc")) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~generation_id, ncol = 1,
             scales = "free_x") +
  guides(fill = guide_legend(title ="Type (1)", 
                             position = "bottom", nrow = 2)) +
  labs(title = t, subtitle = st, caption = cptn) +
  theme_void() +
  theme(plot.title = element_text(family = ft, size = 16, hjust = 0.5),
        plot.subtitle = element_text(family = ft, size = 12, hjust = 0.5, margin = margin(10,10,10,10)),
        plot.caption = element_text(family = ft, size = 9, hjust = 0.5, vjust = -5),
        legend.text = element_text(family = ft, size = 10),
        legend.title = element_text(family = ft, size = 12),
        plot.caption.position = "panel",
        strip.text = element_blank(),
        panel.spacing = unit(0,'lines'),
        plot.margin = margin(40,40,40,40),
        legend.box.margin = margin(20,0,0,0))
plot




# tried to order by different stats to see if there was a trend. There wasn't a trend #

pok2 <- pokemon_df %>%
  drop_na(generation_id) %>%
  mutate(generation_id = factor(generation_id)) %>%
  group_by(generation_id) %>%
  arrange(generation_id, desc(hp)) %>%
  mutate(pokemon = fct_reorder(factor(pokemon), type_1)) %>%
  mutate(type_1 = factor(toupper(type_1))) %>%
  ungroup()


ordered_by_desc_hp_plot <- ggplot(pok2, aes(fill = type_1, values = 1)) +
  geom_waffle(n_rows = 1, color = "white",
              radius = grid::unit(4, "npc")) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~generation_id, ncol = 1,
             scales = "free_x") +
  theme_void() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        panel.spacing = unit(0,'lines'),
        plot.margin = margin(40,40,40,40))

ordered_by_desc_hp_plot
