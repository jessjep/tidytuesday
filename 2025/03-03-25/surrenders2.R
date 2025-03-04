tuesdata <- tidytuesdayR::tt_load(2025, week = 9)
longbeach <- tuesdata$longbeach

library(tidyverse)
library(ggtext)
library(paletteer)
library(sysfonts)
library(showtext)

surrenders <- longbeach %>%
  filter(intake_type == "owner surrender",
         animal_type %in% c("cat", "dog")) %>%
  mutate(animal_type = str_to_title(animal_type),
         reason_for_intake = 
           factor(case_match(reason_for_intake,
                             c("no home", "landlord", "move", "forclosure", "fence") ~ "Housing",
                             c("no time", "abandon", "responsibl", "owner prob", 
                               "child prob", "new baby", "divorce", "attention", "travel") ~ "Owner",
                             "cost" ~ "Financial",
                             c("ill", "poor helth", "allergic", "injured") ~ "Health",
                             "owner died" ~ "Owner Death",
                             c("bites", "agg animal", "jumps up", "escapes",
                               "hyper", "agg people", "sep anxity", "disobedien",
                               "afraid", "destruc in", "vocal", "nofriendly", "kills anim") ~ "Behavioral",
                             c("too many", "other pet") ~ "Other Pet/s",
                             c(NA, "unknown") ~ "Unknown",
                             .default = "Other"))) %>%
  group_by(animal_type, reason_for_intake) %>%
  tally() %>%
  ungroup()

total_sums <- surrenders %>%
  group_by(animal_type) %>%
  summarise(total_n = sum(n))

font_add_google("Lato", "lato")
font <- "lato"
showtext_auto()

title <- "Reasons for Pet Surrender to Long Beach Animal Shelter"
caption <- caption <- "Created by: jessimoore@bsky.social   Source: City of Long Beach Animal Care Services"

p <- ggplot(surrenders, aes(x = animal_type, y = n, fill = reason_for_intake)) +
  geom_bar(stat = "identity") +
  geom_text(data = total_sums, aes(x = animal_type, y = total_n, label = total_n), 
            family = font, hjust = -0.3, inherit.aes = FALSE) +
  scale_fill_paletteer_d("peRReo::calle13") +
  scale_y_continuous(limits = c(0, 1400)) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = title, fill = NULL,
       caption = caption) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = font, size = 12, hjust = 2),
        plot.title = element_text(family = font, size = 18, hjust = 0.5),
        plot.subtitle = element_text(family = font, size = 11),
        legend.title = element_text(family = font, size = 11),
        legend.text = element_text(family = font, size = 10),
        plot.caption = element_text(family = font, size = 9, hjust = 0.5),
        plot.caption.position = "plot",
        plot.margin = margin(30,30,30,30),
        plot.background = element_rect(fill = "#F2F0EF"))
p
