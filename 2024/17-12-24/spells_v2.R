spells <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-17/spells.csv')

lapply(c("ggplot2", "dplyr", "tidyr",
         "forcats", "stringr", "purrr",
         "ggiraph", "patchwork", "grid",
         "gfonts", "gridExtra", "cowplot",
         "showtext"), library, character.only=TRUE)

font_add_google("Goudy Bookletter 1911", "goudy_bookletter")
showtext_auto(enable = TRUE)
font <- "Goudy Bookletter 1911"

spells_long <- spells |>
  pivot_longer(cols=4:11, names_to="species", values_to="binary") |>
  filter(binary==TRUE)

#nesting the data to edit by species
spells_by_species <- spells_long |>
  group_by(species) |>
  select(-binary) |>
  nest()

#rearranging each dataframe to get the ID numbers (this will be the x-axis)
spells_by_species <- spells_by_species |>
  mutate(data = map(data, ~ .x |>
                      arrange(name) |>                             
                      mutate(level = factor(level)) |>             
                      group_by(level) |>
                      mutate(id = row_number()) |>
                      arrange(id) |>
                      mutate(description = if_else(
                        !is.na(description),
                        str_trunc(description, 250, ellipsis = " ... (cont.)"),
                        NA_character_))
  ))

# Adding empty observations to add space between groups
empty_space <- 4
spells_by_species$data[1:8] <- map(spells_by_species$data[1:8], function(df) {
  max_id <- ifelse(nrow(df) > 0, max(df$id, na.rm = TRUE), 0)
  to_add <- data.frame(matrix(NA, nrow = empty_space, ncol = ncol(df)))
  colnames(to_add) <- colnames(df)
  to_add$id <- seq(max_id + 1, max_id + empty_space) # Assign unique `id` values
  rbind(df, to_add)
})


#adding the cumulative sum of the previous dataframe to each to get unique IDs 
#(and extend the x-axis, removing overlap between groups)
distinct_counts <- cumsum(map_int(spells_by_species$data, ~ n_distinct(.x$id)))
spells_by_species$data <- map2(
  spells_by_species$data,
  c(0, head(distinct_counts, -1)),
  ~ mutate(.x, id = id + .y))

#unnesting the dataframes so they can all be plotted on one donut plot
#also pivoting to create the component variable (verbal, material, somatic)
sbs_unnested <- spells_by_species |> 
  unnest(cols=c(species,data)) |>
  pivot_longer(cols = 14:16,                   
               names_to = "component", 
               values_to = "binary2") |> 
  filter(binary2 == TRUE)

#setting the aesthetics of the plot
hover_css <- "cursor:pointer; 
filter: brightness(1.2) drop-shadow(0px 0px 2px rgba(47, 35, 41, 0.2));
stroke-width:4;
r:4px;
transition: all 0.3s ease;"

tooltip_css <- "color:white;
padding:5px;
border-radius:3px;
font-size:12px;
font-family: 'Goudy Bookletter 1911', serif;
width:170px;"

school_colors1 <- c("abjuration"="#4169E1", "conjuration"="#F1C40F", "divination"="#BDC3C7",
                    "enchantment"="#E573B7", "evocation"="#C70039", "illusion"="#9B59B6",
                    "necromancy"="#32DF51", "transmutation"="#FF5733")

#creating the plots
base_data1 <- sbs_unnested %>%    #this step helps create the curved segments under the x-axis
  group_by(species) %>% 
  summarize(start=min(id), end=max(id) - empty_space) %>% 
  rowwise() %>% 
  mutate(title_position=mean(c(start, end)))

p2 <- ggplot(sbs_unnested, aes(id, level, tooltip = paste(paste0(name, ":"), description, sep = "\n\n"), data_id = name))+
  geom_point_interactive(aes(color=school, fill=school), size=2)+
  theme_void()+
  scale_y_discrete(limits=rev)+
  scale_fill_manual_interactive(values=school_colors1)+
  scale_color_manual_interactive(values=school_colors1,
                                 labels = str_to_title(names(school_colors1)))+
  geom_segment(data=base_data1, aes(x = start, y = -1, xend = end, yend = -1), 
               colour = "grey", alpha=0.8, linewidth=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data1, aes(x = title_position, y = -4, label=str_to_title(species)), 
            hjust=c(0.5,0.5,0.3,0.3,0.4,0.6,0.2,0.6),
            vjust=c(1,1,1,0,0,0,0,0),
            alpha=0.8, family="goudy_bookletter", inherit.aes = FALSE)+
  coord_radial(start=0*pi, end=2*pi, inner.radius=0.5,
               r.axis.inside = TRUE)+
  theme(axis.text.y=element_text(family="goudy_bookletter", size=10),
        legend.text = element_text(family = "goudy_bookletter", size = 14),
        legend.title = element_text(family = "goudy_bookletter", size = 14),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.position = "top",
        legend.box="horizontal",
        legend.direction = "horizontal",
        plot.margin = margin(0,0,0,0))+
  labs(color="School:",
       y="Level")+
  guides(fill="none")
p2

#creating a title
title <- 
  ggplot()+
  theme_map()+
  geom_text(aes(x = 0, y = 0), label = "Spells of Dungeons & Dragons", 
            size = 14, family = "goudy_bookletter", fontface = 'bold') +
  theme(plot.margin=margin(0,0,0,0),
        plot.background=element_blank())

#building the plot
layout <- c(area(1,1,1,7), area(2,1,4,7))
plot(layout)
plot <- wrap_plots(title, p2, design = layout, heights = c(2,5,5,5))

i_plot <- girafe(ggobj = plot, fonts = list(sans = font), width_svg = 10, height_svg = 10) 
i_plot <- girafe_options(i_plot, opts_hover(css = hover_css),
                         opts_tooltip(css = tooltip_css, 
                                      offx = 300, offy = 360, 
                                      use_cursor_pos = FALSE, 
                                      use_fill = TRUE), 
                         opts_hover_inv(css = "opacity:0.2"))
i_plot

htmlwidgets::saveWidget(i_plot, "spells_v2.html",
                        selfcontained = FALSE)



