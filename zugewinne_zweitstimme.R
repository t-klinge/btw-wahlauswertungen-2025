library(janitor) # Data cleaning
library(sf) # Geodata handling
library(showtext) # Font handling
library(tidyverse) # Data wrangling

# Add and load fonts
font_add("Work Sans", "fonts/WorkSans-Regular.ttf")
font_add("Work Sans Black", "fonts/WorkSans-Black.ttf")
showtext_auto()

# Read election results: BTW 2017
btw17_ergebnisse_wahlbezirke <- read_csv2("data/Bundestagswahl16.csv") |> 
  select(1:2, 5:7, 28:29) |> 
  clean_names() |> 
  rename(stimmen_linke = z_die_linke,
         prozent_linke = z_die_linke_proz)

# Read election results: BTW 2021
btw21_ergebnisse_wahlbezirke <- read_delim("data/Open-Data-Bundestagswahl2496.csv", 
                                         delim = ";") |> 
  select(1:5, "F", "F6") |>
  rename(stimmen_gesamt = "F",
         stimmen_linke = "F6") |>
  mutate(prozent_linke = stimmen_linke / stimmen_gesamt * 100)

# Read election results: BTW 2025
btw25_ergebnisse_wahlbezirke <- read_delim("data/Open-Data-03159016-Bundestagswahl-Wahlbezirk.csv", 
                                        delim = ";") |> 
  select(1:5, "F", "F6") |>
  rename(stimmen_gesamt = "F",
         stimmen_linke = "F6") |>
  mutate(prozent_linke = stimmen_linke / stimmen_gesamt * 100)

# Calculate change: BTW 21 to BTW 25
btw2125_vergleich <- btw21_ergebnisse_wahlbezirke |> 
  transmute(ags, `gebiet-nr`, `gebiet-name`, linke_prozent_2021 = prozent_linke) |> 
  left_join(btw25_ergebnisse_wahlbezirke |> 
              transmute(ags, `gebiet-nr`, `gebiet-name`, linke_prozent_2025 = prozent_linke)) |> 
  mutate(diff_2021_2025 = round(linke_prozent_2025 - linke_prozent_2021, digits = 1)) |> 
  select(`gebiet-nr`, `gebiet-name`, diff_2021_2025) |> 
  filter(!is.na(diff_2021_2025))

# Calculate change: BTW 17 to BTW 25
btw1725_vergleich <- btw17_ergebnisse_wahlbezirke |> 
  transmute(`gebiet-nr` = nr, `gebiet-name` = name, linke_prozent_2017 = prozent_linke) |> 
  left_join(btw25_ergebnisse_wahlbezirke |> 
              transmute(`gebiet-nr`, `gebiet-name`, linke_prozent_2025 = prozent_linke)) |> 
  mutate(diff_2017_2025 = round(linke_prozent_2025 - linke_prozent_2017, digits = 1)) |> 
  select(`gebiet-nr`, `gebiet-name`, diff_2017_2025) |> 
  filter(!is.na(diff_2017_2025))

# Read geodata
wahlbezirke <- read_sf("goe_wahlbezirke_shape/Wahlbezirk_Kombi.shp")
stadtbezirke <- read_sf("goe_stadtbezirke_shape/Stadtbezirk_Polygone.shp")

# Join shapefile and results
wahlbezirke_zugewinne_btw1725_vergleich <- wahlbezirke |>
  mutate(WBez = as.numeric(WBez)) |>
  left_join(btw1725_vergleich,
            by = c("WBez" = "gebiet-nr"))

wahlbezirke_zugewinne_btw2125_vergleich <- wahlbezirke |>
  mutate(WBez = as.numeric(WBez)) |>
  left_join(btw2125_vergleich,
            by = c("WBez" = "gebiet-nr"))

# Specify sizes
size_title = 30
size_subtitle = size_title * 2/3
size_legend = size_title * 2/3
size_caption = size_title * 2/5

# Visualize
wahlbezirke_zugewinne_btw1725_viz <- wahlbezirke_zugewinne_btw1725_vergleich |> 
  filter(!is.na(diff_2017_2025)) |> 
  mutate(diff_cat = case_when(between(diff_2017_2025, -Inf, -0.1) ~ "\u22640",
                              between(diff_2017_2025, 0, 4.9) ~ "0\u20134,9",
                              between(diff_2017_2025, 5, 9.9) ~ "5\u20139,9",
                              between(diff_2017_2025, 10, Inf) ~ "\u226510"),
         diff_cat = factor(diff_cat, levels = c("\u22640", "0\u20134,9", "5\u20139,9", "\u226510"))) |> 
  ggplot() +
  geom_sf(data = stadtbezirke, # Add districts area: uniform color
          fill = "lightgrey",
          color = "white") +
  geom_sf(aes(fill = diff_cat), # Add districts borders
          color = "darkgrey", 
          linewidth = 0.15) +
  geom_sf(data = stadtbezirke, # Add districts area: election results
          fill = NA,
          color = "#6F003C",
          linewidth = 0.15) +
  theme_void() + # Remove plot elements
  scale_fill_manual(name = "",
                    values = c(brewer.pal(3, "Blues")[2], brewer.pal(3, "Reds")[1], brewer.pal(3, "Reds")[2], brewer.pal(3, "Reds")[3])) +
  labs(title = "Wo wurde Göttingen roter?", # Add labels
       subtitle = "Veränderungen im Vergleich zur BTW 2017 (in %-Punkten)",
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

ggsave(filename = "./output/wahlbezirke_zugewinne_btw1725_viz.png",
       wahlbezirke_zugewinne_btw1725_viz,
       width = 80,
       height = 75,
       units = "mm")

wahlbezirke_zugewinne_btw2125_viz <- wahlbezirke_zugewinne_btw2125_vergleich |> 
  filter(!is.na(diff_2021_2025)) |> 
  mutate(diff_cat = case_when(between(diff_2021_2025, -Inf, -0.1) ~ "\u22640",
                              between(diff_2021_2025, 0, 4.9) ~ "0\u20134,9",
                              between(diff_2021_2025, 5, 9.9) ~ "5\u20139,9",
                              between(diff_2021_2025, 10, 14.9) ~ "10\u201314,9",
                              between(diff_2021_2025, 15, Inf) ~ "\u226515"),
         diff_cat = factor(diff_cat, levels = c("\u22640", "0\u20134,9", "5\u20139,9", "10\u201314,9", "\u226515"))) |> 
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
  scale_fill_brewer(name = "",
                    palette = "Reds") +
  labs(title = "Wo wurde Göttingen roter?", # Add labels
       subtitle = "Veränderungen im Vergleich zur BTW 2021 (in %-Punkten)",
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

ggsave(filename = "./output/wahlbezirke_zugewinne_btw2125_viz.png",
       wahlbezirke_zugewinne_btw2125_viz,
       width = 80,
       height = 75,
       units = "mm")
