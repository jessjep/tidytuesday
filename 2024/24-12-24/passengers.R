setwd("/Users/jess.moore/Documents/GitHub/Tidy Tuesday/24-12-24")

library(tidyverse)
library(readxl)
library(lubridate)
library(ggiraph)
library(viridis)

global_holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/global_holidays.csv')
monthly_passengers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-24/monthly_passengers.csv')
aus_erp <- read_xlsx("AU_ERP.xlsx")

global_holidays <- global_holidays |>
  mutate(Year=year(ymd(Date)),
         Month=month(ymd(Date)))

aus_erp <- aus_erp |>
  mutate(Year=year(ymd(Date)),
         ERP=ERP*1000) |>
  filter(Year >= 2010) |>
  select(ERP, Year) |>
  slice(seq(1, n(), 4))

travellers <- full_join(global_holidays, monthly_passengers) |>
  filter(ISO3 == "AUS") |>
  pivot_longer(cols = 8:10, names_to = "Travel_Type", values_to = "Passengers")

travellers <- full_join(travellers, aus_erp) |>
  filter(Travel_Type != "Total" & !is.na(Passengers)) |>
  mutate(Year=as.factor(Year),
         Month=as.factor(Month)) |>
  arrange(Month)

months <- c("1"="Jan", "2"="Feb", "3"="Mar", "4"="Apr",
            "5"="May", "6"="Jun", "7"="Jul", "8"="Aug",
            "9"="Sep", "10"="Oct", "11"="Nov", "12"="Dec")

tooltip <- paste("Air Passengers:", travellers$Passengers, 
                 "<br>Population:", paste(round(travellers$ERP/1000000,2), "million"))

heatmap <- ggplot(travellers,aes(Year, Month, fill=Passengers/ERP*100))+
  geom_tile_interactive(aes(tooltip=tooltip, data_id=Passengers), 
                        color= "white",size=0.1) + 
  scale_fill_viridis(name="Passengers as a % of Population", option ="D")+
  facet_wrap(vars(Travel_Type))+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.title=element_text(size=10),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))+
  scale_y_discrete(limits=rev, labels=months)+
  scale_x_discrete(breaks=seq(2010,2018,2))+
  labs(x=NULL, y=NULL, title = "Air Passengers, Australia",
       subtitle = "2010 to 2018",
       caption = "Sources: Tidy Tuesday, Global Holidays and Travel; ABS, Population Statistics")
heatmap

hover_css <- "filter:brightness(110%)"
tooltip_css <- "background-color:#e85a60; color:white;padding:5px;
font-family: Arial, open sans;font-size: 14px;"

heati <- girafe(ggobj=heatmap, fonts = list(sans = "Open Sans"))
heati <- girafe_options(heati, opts_hover(hover_css),
                        opts_tooltip(tooltip_css, opacity = 1))
heati

htmlwidgets::saveWidget(heati, "passengers.html",
                        selfcontained = FALSE)
