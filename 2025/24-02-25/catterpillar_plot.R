tuesdata <- tidytuesdayR::tt_load(2025, week = 8)

article_dat <- tuesdata$article_dat
model_dat <- tuesdata$model_dat

library(tidyverse)
library(forcats)
library(stringr)
library(tidyplots)

model_dat <- left_join(model_dat, article_dat, by = "doi")

models <- model_dat %>%
  filter(ref %in% c("White", "Caucasian", "white", "Caucasian or white"),
         point != -99, lower != -99, upper != -99,
         str_detect(outcome, "death|mortality|survival")) %>%
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
         significance = factor(case_when(
           lower < 1 & upper > 1 ~ "Not Significant", # odds ratio of 1 indicates both groups odds are equal
           TRUE ~ "Significant"
         ))) %>%
  filter(!(race %in% c("Other/Mixed", "White"))) %>%
  mutate(id = factor(row_number()),
         id = fct_reorder(id, point)) %>%
  select(doi, outcome, measure, ref, point, lower, upper, race, study_type, significance, id)

# make the plot #
models %>%
  tidyplot(x = id, y = point, color = significance, 
           width = 300, height = 150) %>%
  add_data_points() %>%
  add(ggplot2::geom_segment(aes(y = lower, yend = upper))) %>%
  remove_x_axis_labels() %>%
  add_reference_lines(y = 1, linetype = "dashed") %>%
  adjust_colors(c("#219ebc", "#023047")) %>%
  adjust_title("Racial and Ethnic Disparities in Reproductive Medicine Research",
               fontsize = 20) %>%
  add_caption("Testing caption")

