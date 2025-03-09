library(sf) # Geodata handling
library(showtext) # Font handling
library(tidyverse) # Data wrangling

# Add and load fonts
font_add("Work Sans", "fonts/WorkSans-Regular.ttf")
font_add("Work Sans Black", "fonts/WorkSans-Black.ttf")
showtext_auto()

# Read election results
btw25_ergebnisse_wahlbezirke <- read_delim("data/Open-Data-03159016-Bundestagswahl-Wahlbezirk.csv", 
                                        delim = ";") |> 
  select(1:5, "D", "D6") |>
  rename(stimmen_gesamt = "D",
         stimmen_linke = "D6") |>
  mutate(prozent_linke = stimmen_linke / stimmen_gesamt * 100)

# Read geodata
wahlbezirke <- read_sf("goe_wahlbezirke_shape/Wahlbezirk_Kombi.shp")
stadtbezirke <- read_sf("goe_stadtbezirke_shape/Stadtbezirk_Polygone.shp")

# Join geodata and election results
wahlbezirke_ergebnisse <- wahlbezirke |>
  mutate(WBez = as.numeric(WBez)) |>
  left_join(btw25_ergebnisse_wahlbezirke |>
              select(4, 5:8),
            by = c("WBez" = "gebiet-nr"))

# Specify sizes
size_title = 30
size_subtitle = size_title * 2/3
size_legend = size_title * 2/3
size_caption = size_title * 2/5

# Visualize
wahlbezirke_ergebnisse_viz <- wahlbezirke_ergebnisse |>
  filter(!is.na(WBez)) |> 
  rename("bezirk_name" = "BEZIR_NAME") |>
  mutate(prozent_linke_cat = case_when(between(prozent_linke, 0, 4.9999) ~ "<5",
                                       between(prozent_linke, 5, 9.9999) ~ "5\u20139,9",
                                       between(prozent_linke, 10, 14.9999) ~ "10\u201314,9",
                                       between(prozent_linke, 15, 19.9999) ~ "15\u201319,9",
                                       prozent_linke >= 20 ~ "\u226520"),
         prozent_linke_cat = factor(prozent_linke_cat, levels = c("<5", "5\u20139,9", "10\u201314,9", "15\u201319,9", "\u226520"))) |> 
  ggplot() +
  geom_sf(data = stadtbezirke, # Add districts area: uniform color
          fill = "lightgrey",
          color = "white") +
  geom_sf(aes(fill = prozent_linke_cat), # Add districts borders
          color = "white", 
          linewidth = 0.15) +
  geom_sf(data = stadtbezirke, # Add districts area: election results
          fill = NA,
          color = "#6F003C",
          linewidth = 0.15) +
  theme_void() + # Remove plot elements
  scale_fill_brewer(name = "", palette = "Reds") + # Add reds scale
  labs(title = "Wie rot ist Göttingen?", # Add labels
       subtitle = "Zweitstimmenergebnisse bei der BTW 2025 (in %)",
       caption = "Ohne Berücksichtigung von Briefwahlergebnissen") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.text = element_text(size = size_legend),
        text = element_text(family = "Work Sans", colour = "white"),
        plot.background = element_rect(fill = "#6F003C",
                                       colour = NA),
        plot.caption = element_text(size = size_caption, 
                                    hjust = 1, 
                                    vjust = -2),
        plot.margin = unit(c(0.3, 0.2, 0.3, 0.2), "cm"),
        plot.subtitle = element_text(size = size_subtitle, 
                                     hjust = 0.5),
        plot.title = element_text(family = "Work Sans Black", 
                                  size = size_title, 
                                  hjust = 0.5),
        legend.key.size = unit(3, "mm"))

# Save output
ggsave(filename = "./output/wahlbezirke_ergebnisse_viz.png",
       wahlbezirke_ergebnisse_viz,
       width = 80,
       height = 75,
       units = "mm")
