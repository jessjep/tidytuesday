# load the data #
tuesdata <- tidytuesdayR::tt_load(2025, week = 10)
pixar_films <- tuesdata$pixar_films
public_response <- tuesdata$public_response
rm(tuesdata)

# load libraries #
library(tidyverse)
library(sysfonts)
library(showtext)
library(ggrepel)
library(paletteer)
library(forcats)

# join the data frames #
pixar <- left_join(pixar_films, public_response) %>%
  mutate(film = factor(film))

# manually input some missing data found online #
pixar <- pixar %>%
  mutate(film = case_when(
    number == 27 ~ "Elemental", 
    TRUE ~ film),
    rotten_tomatoes = case_when(
      number == 24 ~ 91, # Luca
      number == 25 ~ 95, # Turning Red
      number == 26 ~ 74, # Lightyear
      number == 27 ~ 73, # Elemental
      TRUE ~ rotten_tomatoes),
    run_time = case_when(
      number == 25 ~ 100, # Turning Red
      number == 26 ~ 105, # Lightyear
      TRUE ~ run_time),
    metacritic = case_when(
      number == 24 ~ 71, # Luca
      number == 25 ~ 83, # Turning Red
      number == 26 ~ 60, # Lightyear
      number == 27 ~ 58, # Elemental
      TRUE ~ metacritic)) %>%
  select(-cinema_score) %>% # remove non-numeric ratings
  mutate(avg_rating = rowMeans(select(., rotten_tomatoes, metacritic, critics_choice), na.rm = TRUE)) %>%
  mutate(film = fct_reorder(film, avg_rating)) %>%
  mutate(decade = factor(case_when(
    release_date < "2000-01-01" ~ "90s",
    release_date < "2010-01-01" & release_date > "2000-01-01" ~ "00s",
    release_date < "2020-01-01" & release_date > "2010-01-01" ~ "10s",
    release_date > "2020-01-01" ~ "20s")),
    decade = fct_reorder(decade, release_date))

# prepare the data for plotting #
pixar_long <- pixar %>%
  pivot_longer(cols = 6:9, names_to = "site", values_to = "rating") %>%
  drop_na(rating)

segments <- pixar_long %>%
  group_by(release_date) %>%
  summarise(min_rating = min(rating),
            max_rating = max(rating))

labels <- pixar_long %>%
  group_by(film, release_date) %>%
  summarise(min_rating = min(rating))

# plot text and colors #
font_add_google("Viaoda Libre", "viaoda")
font_add_google("M PLUS 1p", "mplus")
font <- "viaoda"
font2 <- "mplus"
showtext_auto()

t <- "Pixar Film Rankings"
st <- "According to the average of Rotten Tomatoes, 
Metacritic, and Critics Choice ratings."
cpt <- "Created by: jessimoore@bsky.social   Source: {pixar}"

bg = "#80B8D9"
txt = "grey18"
pal <- "grDevices::Purple-Orange"

# plotting #
p2 <- ggplot(pixar) +
  geom_segment(aes(x = film, y = 50, yend = avg_rating),
               linewidth = 1, color = "white", alpha = 0.4) +
  geom_point(aes(x = film, y = avg_rating, 
                 color = release_date,
                 size = run_time), alpha = 0.9) +
  coord_flip() +
  scale_color_gradientn(colors = c("#2c363f", "#e75a7c", "#f2f5ea"), 
                        labels = function(x) year(as.Date(x, origin = "1970-01-01"))) +
  scale_size(range = c(2,9)) +
  scale_y_continuous(position = "right", limits = c(50, 100)) +
  labs(title = t, subtitle = st,
       x = NULL, y = "Average Rating",
       color = "Release Date", size = "Run Time (minutes)",
       caption = cpt) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(color = "white", linewidth = 0.3, linetype = "dotted"),
        panel.grid.minor.x = element_line(color = "white", linewidth = 0.3, linetype = "dotted"),
        panel.grid.minor.y = element_line(color = "white", linewidth = 0.3, linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = bg),
        panel.background = element_rect(fill = bg, color = NA),
        plot.title = element_text(family = font, size = 30, hjust = 0.5,
                                  color = txt),
        plot.subtitle = element_text(family = font2, size = 10, 
                                     color = txt, hjust = 0.5, vjust = 3),
        axis.title.x = element_text(family = font2, size = 10, color = txt, 
                                    hjust = 1, vjust = 0.5),
        axis.text = element_text(family = font2, size = 10, color = txt),
        legend.text = element_text(family = font2, size = 10, color = txt),
        legend.title = element_text(family = font2, size = 11, color = txt),
        plot.margin = margin(35,20,35,20),
        plot.caption = element_text(family = font2, size = 9, 
                                    hjust = 0.45, vjust = -6),
        plot.caption.position = "plot")
p2
  
