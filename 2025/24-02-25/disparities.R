#---------------load the data---------------#

tuesdata <- tidytuesdayR::tt_load(2025, week = 8)

article_dat <- tuesdata$article_dat
model_dat <- tuesdata$model_dat

#---------------load the packages---------------#
library(tidyverse)
library(forcats)
library(stringr)
library(ggplot2)
library(patchwork)
library(sysfonts)
library(showtext)

#---------------wrangle the data---------------#
model_dat <- left_join(model_dat, article_dat, by = "doi") %>%
  filter(access_to_care ==1 | treatment_received ==1 | health_outcome == 1) # filter to studies that compared race

models <- model_dat %>%
  filter(ref %in% c("White", "Caucasian", "white", "Caucasian or white"),
         point != -99, lower != -99, upper != -99,
         point < 73,
         str_detect(outcome, "death|mortality|suicide|homicide|survival|morbidity"),
         measure %in% c("HR", "OR", "RR")) %>%
  mutate(compare = str_to_sentence(compare),
         race = factor(case_when(
           str_detect(compare, regex("black|african american", ignore_case = TRUE)) ~ "Black",
           str_detect(compare, regex("asian", ignore_case = TRUE)) ~ "Asian American",
           str_detect(compare, regex("white|caucasian", ignore_case = TRUE)) ~ "White",
           str_detect(compare, regex("\\bhispanic\\b|spanish|mexican|
                                     dominican|puerto rican|cuban|
                                     south and central american|latin", ignore_case = TRUE)) ~ "Hispanic",
           str_detect(compare, regex("pacific islander|hawaiian", ignore_case = TRUE)) ~ "Pacific Islander",
           str_detect(compare, regex("native|indigenous|indian|alaskan", ignore_case = TRUE)) ~ "Pacific Islander",
           TRUE ~ "Other/Mixed")),
         Significance = factor(case_when(
           lower < 1 & upper > 1 ~ "Not Significant", # odds ratio of 1 indicates both groups odds are equal
           TRUE ~ "Significant"
         ))) %>%
  filter(race !="White") %>%
  mutate(id = factor(row_number()),
         id = fct_reorder(id, point)) %>%
  select(doi, outcome, measure, ref, point, lower, upper, race, study_type, Significance, id)

n_studies <- n_distinct(models$doi)
n_outcomes <- n_distinct(models$outcome)
n_results <- n_distinct(models$id)

#---------------create the summary table---------------#
summary <- models %>%
  group_by(race) %>%
  summarise(n = n_distinct(id))

#---------------font, text, and colors---------------#
font_add_google("Nunito", "nunito")
font <- "nunito"
showtext_auto()

title <- "Racial Disparities in Reproductive Medicine Research: White vs. Non-White Outcomes"
subtitle <- glue::glue("Shows {n_results} estimates and confidence intervals of odds ratios, risk ratios, and hazard ratios for {n_outcomes} reproductive health outcomes 
relating to mortality and morbidity, measured across {n_studies} studies published in reproductive health journals between 2010 and 2023. 
Estimates greater than 1 indicate a higher risk/odds for the comparison (non-white) groups.")
caption <- "Created by: jessimoore@bsky.social   Source: 10.1016/j.ajog.2024.07.024"

colors <- c("#8ecae6", "#023047")
summary_colors <- c("#219ebc","#219ebc","#219ebc","#219ebc","#219ebc")


#---------------caterpillar plot---------------#
main_plot <- ggplot(models, aes(x = id, y = point, 
                        color = Significance)) +
  geom_point(size = 1) +
  geom_segment(aes(y = lower, yend = upper)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  annotate(geom = "text", x = 320, y = 0.42, 
           label = "Results Published per Comparison Group",
           family = font) +
  scale_color_manual(values = colors) +
  scale_y_log10(breaks = c(0, 0.5, 1, 1.5, 2.5, 4, 6)) +
  labs(x = NULL, y = "Point Estimate\n(log scale)",
       title = title, subtitle = subtitle, caption = caption,
       color = "Statistical Significance") +
  theme_minimal() +
  theme(plot.title = element_text(family = font, face = "bold", size = 16),
        plot.subtitle = element_text(family = font, size = 11),
        legend.title = element_text(family = font, size = 11),
        legend.text = element_text(family = font, size = 10),
        plot.caption = element_text(family = font, size = 9, hjust = 0.5),
        plot.caption.position = "plot",
        legend.position = "inside",
        legend.position.inside = c(0.56, 0.2),
        axis.text.x = element_blank(),
        axis.title.y = element_text(),
        panel.grid.major.x = element_blank(),
        plot.margin = margin(30,30,30,30))

#---------------summary plot---------------#
summary_plot <- ggplot(summary, 
                       aes(x = fct_reorder(race, n),
                           y = n, fill = race)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), family = font, hjust = -0.3) +
  scale_fill_manual(values = summary_colors) +
  scale_y_continuous(limits = c(0, 350)) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(family = font),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())

#---------------combined plot---------------#
viz <- main_plot + inset_element(summary_plot,
                                 left = 0.62, right = 1,
                                 bottom = 0.06, top = 0.27)
viz
