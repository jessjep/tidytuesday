# load the data #
tuesdata <- tidytuesdayR::tt_load(2025, week = 5)

simpsons_characters <- tuesdata$simpsons_characters
simpsons_episodes <- tuesdata$simpsons_episodes
simpsons_locations <- tuesdata$simpsons_locations
simpsons_script_lines <- tuesdata$simpsons_script_lines


# load packages #
library(tidyverse)
library(tidytext)
library(ggplot2)

# creating df to use #

simpsons_episodes <- simpsons_episodes %>%
  select(imdb_rating, number_in_series)

simpsons_script <- simpsons_script_lines %>%
  select(episode_id, raw_character_text, spoken_words) %>%
  drop_na()

simpsons <- full_join(simpsons_episodes, simpsons_script, join_by(number_in_series == episode_id)) %>%
  rename(rating = imdb_rating,
         episode = number_in_series,
         character = raw_character_text,
         line = spoken_words)

rm(simpsons_characters, simpsons_episodes, simpsons_locations, simpsons_script_lines,
   simpsons_script, tuesdata)

# tidying the df for text analysis #
simpsons_tidy <- simpsons %>%
  filter(str_detect(character, "Simpson")) %>%
  filter(!(character %in% c("Mona Simpson", "Virgil Simpson", "Grampa Simpson"))) %>%
  group_by(character) %>%
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 5, order_by = n)

# first plot #
p <- ggplot(simpsons_tidy, aes(reorder_within(word, n, character),
                          n, fill = word)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ character, scale = "free_y") +
  coord_flip() +
  scale_x_reordered(labels = function(x) gsub("__.+$", "", x))

# adding some color #
colors <- c("dad" = "#2f64d6",
            "homer" = "#2f64d6",
            "homie" = "#2f64d6",
            "lisa" = "#ff81c1",
            "bart" = "#9c5b01",
            "mom" = "#f8db27",
            "marge" = "#f8db27",
            "kids" = "#CE6E61")

p + scale_fill_manual(values = colors) +
  labs(x = NULL, y = "Frequency",
       title = "The Simpsons",
       subtitle = "Most spoken words by character") +
  theme(plot.background = element_rect(fill = "#CBD8F5"),
        panel.background = element_rect(fill = "#CBD8F5"),
        strip.background.x = element_rect(fill = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
