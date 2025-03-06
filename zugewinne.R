library(sf) # Geodata handling
library(showtext) # Font handling
library(tidyverse) # Data wrangling

# Add and load fonts
font_add("Work Sans", "fonts/WorkSans-Regular.ttf")
font_add("Work Sans Black", "fonts/WorkSans-Black.ttf")
showtext_auto()

# Read election results: BTW 2021
btw21_ergebnisse_wahlbezirke <- read_delim("data/Open-Data-Bundestagswahl2496.csv", 
                                         delim = ";") |> 
  select(1:5, "D", "D6") |>
  rename(stimmen_gesamt = "D",
         stimmen_linke = "D6") |>
  mutate(prozent_linke = stimmen_linke / stimmen_gesamt * 100)

# Read election results: BTW 2025
btw25_ergebnisse_wahlbezirke <- read_delim("data/Open-Data-03159016-Bundestagswahl-Wahlbezirk.csv", 
                                        delim = ";") |> 
  select(1:5, "D", "D6") |>
  rename(stimmen_gesamt = "D",
         stimmen_linke = "D6") |>
  mutate(prozent_linke = stimmen_linke / stimmen_gesamt * 100)

# Calculate change
btw_vergleich <- btw21_ergebnisse_wahlbezirke |> 
  transmute(ags, `gebiet-nr`, `gebiet-name`, linke_prozent_2021 = prozent_linke) |> 
  left_join(btw25_ergebnisse_wahlbezirke |> 
              transmute(ags, `gebiet-nr`, `gebiet-name`, linke_prozent_2025 = prozent_linke)) |> 
  mutate(diff_2021_2025 = round(linke_prozent_2025 - linke_prozent_2021, digits = 1)) |> 
  select(`gebiet-nr`, `gebiet-name`, diff_2021_2025) |> 
  filter(!is.na(diff_2021_2025))

# Read geodata
wahlbezirke <- read_sf("goe_wahlbezirke_shape/Wahlbezirk_Kombi.shp")
stadtbezirke <- read_sf("goe_stadtbezirke_shape/Stadtbezirk_Polygone.shp")

# Join shapefile and results
wahlbezirke_zugewinne_vergleich <- wahlbezirke |>
  mutate(WBez = as.numeric(WBez)) |>
  left_join(btw_vergleich,
            by = c("WBez" = "gebiet-nr"))

# Visualize
wahlbezirke_zugewinne_viz <- wahlbezirke_zugewinne_vergleich |> 
  filter(!is.na(diff_2021_2025)) |> 
  mutate(diff_cat = case_when(between(diff_2021_2025, -Inf, 0) ~ "<0",
                              between(diff_2021_2025, 0, 4.999) ~ "0\u20134,9",
                              between(diff_2021_2025, 5, 9.999) ~ "5\u20139,9",
                              between(diff_2021_2025, 10, 14.999) ~ "10\u201314,9",
                              between(diff_2021_2025, 15, 19.999) ~ "15\u201319,9",
                              diff_2021_2025 >= 20 ~ "\u226520"),
         diff_cat = factor(diff_cat, levels = c("<0", "0\u20134,9", "5\u20139,9", "10\u201314,9", "15\u201319,9", "\u226520"))) |> 
  ggplot() +
  geom_sf(data = stadtbezirke, # Add districts area: uniform color
          fill = "lightgrey",
          color = "white") +
  geom_sf(aes(fill = diff_cat), # Add districts borders
          color = "white", 
          linewidth = 0.15) +
  geom_sf(data = stadtbezirke, # Add districts area: election results
          fill = NA,
          color = "#6F003C",
          linewidth = 0.15) +
  theme_void() + # Remove plot elements
  scale_fill_brewer(name = "", palette = "Reds") + # Add reds scale
  labs(title = "Wo wurde Göttingen roter?", # Add labels
       subtitle = "Zugewinne im Vergleich zur BTW 2021 (in %-Punkten)",
       caption = "Ohne Berücksichtigung von Briefwahlergebnissen") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        text = element_text(family = "Work Sans", colour = "white"),
        plot.background = element_rect(fill = "#6F003C",
                                       colour = NA),
        plot.caption = element_text(size = 12, 
                                    hjust = 1, 
                                    vjust = -2),
        plot.margin = unit(c(0.3, 0.2, 0.3, 0.2), "cm"),
        plot.subtitle = element_text(size = 16, 
                                     hjust = 0.5),
        plot.title = element_text(family = "Work Sans Black", 
                                  size = 22, 
                                  hjust = 0.5))

ggsave(filename = "./output/wahlbezirke_zugewinne_viz.png",
       wahlbezirke_zugewinne_viz,
       width = 80,
       height = 75,
       units = "mm")
