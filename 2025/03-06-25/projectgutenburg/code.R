library(tidyverse)
library(ggthemes)
library(paletteer)

tuesdata <- tidytuesdayR::tt_load(2025, week = 22)

gutenberg_authors <- tuesdata$gutenberg_authors %>%
  arrange(author, birthdate) %>%
  distinct(author, .keep_all = TRUE)
gutenberg_languages <- tuesdata$gutenberg_languages
gutenberg_metadata <- tuesdata$gutenberg_metadata
gutenberg_subjects <- tuesdata$gutenberg_subjects

# number of distinct languages per author #

translated <- gutenberg_metadata %>%
  distinct(author, language) %>%
  count(author) %>%
  filter(!(author %in% c(NA, "Anonymous", "Various", "Unknown"))) %>%
  slice_max(order_by = n, n = 20)

t <- translated %>%
  left_join(gutenberg_authors %>%
            select(author, birthdate, deathdate),
    by = "author") %>%
  separate(author, into = c("surname", "firstname"), 
           sep = ",", fill = "right") %>%
  mutate(firstname = str_trim(firstname),
         surname = str_trim(surname),
         author = if_else(is.na(firstname), surname, 
                          paste(firstname, surname))) %>%
  mutate(birthdate = if_else(author == "Daniel Defoe", 1660, birthdate),
    deathdate = if_else(author == "Daniel Defoe", 1731, deathdate)) %>%
  select(-c(firstname, surname)) %>%
  mutate(active = (deathdate + birthdate)/2,
    century = factor(case_when(
      is.na(active) ~ "Unknown",
      TRUE ~ paste0((birthdate %/% 100 + 1), "th century"))),
      author = fct_reorder(factor(author), n)) %>%
  mutate(century = if_else(author == "Homer", "8th century BCE", century))

# plot #

p <- ggplot(t, aes(author, n, fill = century)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Project Gutenberg's Most Translated Authors",
       x = NULL,
       y = "Number of languages",
       fill = "Period active",
       caption = "@jessimoore.bsky.social   Source: {gutenbergr}") +
  theme_wsj()+ 
  scale_fill_paletteer_d("Manu::Kereru", direction = -1) +
  theme(plot.margin = margin(30,40,30,30),
        plot.title.position = "plot",
        plot.title = element_text(size = 20, hjust = 0.5, vjust = 3),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 14, face = "bold"),
        plot.caption = element_text(size = 10, hjust = 0.5, vjust = -8),
        plot.caption.position = "plot")
p
