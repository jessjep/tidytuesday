tuesdata <- tidytuesdayR::tt_load(2025, week = 9)
longbeach <- tuesdata$longbeach

library(tidyverse)
library(ggtext)
library(ggstream)
library(paletteer)

glimpse(longbeach)

surrenders <- longbeach %>%
  filter(animal_type == "dog", intake_type == "owner surrender") %>%
  mutate(reason_for_intake = factor(case_match(reason_for_intake,
    c("no home", "landlord", "move", "forclosure", "fence") ~ "Housing",
    c("no time", "abandon", "responsibl", "owner prob", 
      "child prob", "new baby", "divorce", "attention", "owner died") ~ "Other Owner-Related",
    "cost" ~ "Financial",
    c("ill", "poor helth", "allergic", "injured") ~ "Health",
    c("bites", "agg animal", "jumps up", "escapes",
      "hyper", "agg people", "sep anxity", "disobedien",
      "afraid", "destruc in", "vocal", "nofriendly", "kills anim") ~ "Behavioral",
    c(NA, "unknown") ~ "Unknown",
    .default = "Other"),
    levels = c("Behavioral", "Financial", "Health", "Housing", "Other Owner-Related", "Other", "Unknown"))) %>%
  group_by(intake_date, reason_for_intake) %>%
  tally()

# color palette #

# plot text #
title <- "Why Do Owners Surrender their Dogs?"
subtitle <- "Shows the number of dogs surrendered to Long Beach Animal Shelter between 2017 and 2025.
Reasons for surrender were related to the dog's behavior (e.g., aggressive, hyper, separation anxiety), financial,
housing-related (e.g. landlord, forclosure, fencing), health-related (e.g. owner illness, allergies), other owner-related reasons 
(e.g. divorce, lack of time, a new baby), other (e.g. too many other pets, too big, shedding), and unknown (reason not provided)."
caption <- "Source: Long Beach Animal Shelter   Created by: jessimoore@bsky.social"

#plot
p2 <- ggplot(surrenders, aes(intake_date, n, fill = reason_for_intake)) +
  geom_area() +
  labs(x = NULL, y = "Number of Surrenders",
       title = title, subtitle = subtitle) +
  scale_y_continuous(breaks = seq(5,25,5)) +
  scale_x_date(limits = as.Date(c("2017-01-04", "2024-12-26"))) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())
p2


