library(sf)
library(ggplot2)
library(rnaturalearth)
library(glue)
library(ggtext)
library(dplyr)
library(showtext)

UMI <- read.csv ("data/UMI.csv")

UMI_sf <- st_as_sf(UMI, coords = c("Long", "Lat"), crs = 4326)
UMI_sf$Public_Transit_Rank <- rank(-UMI_sf$Public_Transit, ties.method = "min")

world <- ne_countries(scale = "medium", returnclass = "sf")
world_transformed <- st_transform(world, crs = 4326)

highlight_cities <- c("Hong Kong", "Zurich", "Singapore")
highlight_colors <- c("red", "hotpink", "orange")

UMI_sf$highlight_color <- ifelse(UMI_sf$City %in% highlight_cities,
                                 highlight_colors[match(UMI_sf$City, highlight_cities)],
                                 "grey")

# Plotting
ggplot(data = world_transformed) +
  geom_sf(fill = "#d9e7f0", color = "white", size = 0.2) +  
  geom_sf(data = UMI_sf, 
          aes(color = Public_Transit_Rank, 
              size = cut(Public_Transit, 
                         breaks = c(0, 20, 30, 40, 50, 60, Inf),
                         labels = c("Below 20", "20-30", "30-40", "40-50", "50-60", "Above 60"), show.legend = "color")),
          alpha = 0.7) + 
  geom_sf(data = UMI_sf %>% filter(City %in% highlight_cities), 
          aes(geometry = geometry), 
          size = 5, shape = 21, stroke = 1.5, 
          fill = highlight_colors[UMI_sf$City[UMI_sf$City %in% highlight_cities]],
          show.legend = FALSE) +
  geom_segment(aes(x = 173.1, y = 22.3, xend = 115.1, yend = 22.3), color = "red", size = 0.5) + 
  geom_segment(aes(x = -40.5, y = 47.4, xend = 8.0, yend = 47.4), color = "hotpink", size = 0.5) +
  geom_segment(aes(x = 60.8, y = 1.4, xend = 103.0, yend = 1.4), color = "orange", size = 0.5) +
  scale_color_viridis_c(option = "D", name = "Public Transit Rank") +
  annotate("text", x = 150.1, y = 22.3, label = "Hong Kong\nIndex: 76.4", color = "red", size = 3, fontface = "bold", hjust = 0) +
  annotate("text", x = -40.5, y = 47.4, label = "Zurich\nIndex: 74.0", color = "hotpink", size = 3, fontface = "bold", hjust = 0) +
  annotate("text", x = 60.8, y = 1.4, label = "Singapore\nIndex: 73.9", color = "orange", size = 3, fontface = "bold", hjust = 0) +
  coord_sf(crs = st_crs(4326), xlim = c(-180, 180), ylim = c(-60, 90)) +  
  theme_minimal() +
  theme(text = element_text(family = "Book Antiqua"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = ggtext::element_textbox_simple(margin = margin(5, 0, 5, 0), size = 12, lineheight = 1.2),
        plot.caption = element_text(size = 7, hjust = 1, margin = margin(5, 0, 5, 0)),
        plot.caption.position = "plot",
        legend.position = c(0.1,0.2),
        legend.title = element_text(size = 10, face = "bold"),
        legend.background = element_rect(fill = "white", color = FALSE)) +
  labs(title = "Which Cities Have the Best Public Transit in 2023?",
       subtitle = "1st <span style='color:red;'>Hong Kong</span>, 
                   2nd <span style='color:hotpink;'>Zurich</span>, 
                   and 3rd <span style='color:orange;'>Singapore</span>",
       caption = "Source: Urban Mobility Index 2023\nTutut Indriaty\n#30DayMapChallenge",
       color = "Transit Readiness Rank", size = "Transit Category") +
  guides(size = "none")