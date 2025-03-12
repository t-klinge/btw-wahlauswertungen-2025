library(ggpattern) # Pattern filling
library(janitor) # Data cleaning
library(RColorBrewer) # Colors
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

# Calculate change
btw2125_vergleich <- btw21_ergebnisse_wahlbezirke |> 
  transmute(ags, `gebiet-nr`, `gebiet-name`, linke_prozent_2021 = prozent_linke) |> 
  left_join(btw25_ergebnisse_wahlbezirke |> 
              transmute(ags, `gebiet-nr`, `gebiet-name`, linke_prozent_2025 = prozent_linke)) |> 
  mutate(diff_2021_2025 = round(linke_prozent_2025 - linke_prozent_2021, digits = 1)) |> 
  filter(!is.na(diff_2021_2025))

# Read geodata
wahlbezirke <- read_sf("goe_wahlbezirke_shape/Wahlbezirk_Kombi.shp")
stadtbezirke <- read_sf("goe_stadtbezirke_shape/Stadtbezirk_Polygone.shp")

# Join shapefile and results
wahlbezirke_zugewinne_vergleich <- wahlbezirke |>
  mutate(WBez = as.numeric(WBez)) |>
  left_join(btw2125_vergleich,
            by = c("WBez" = "gebiet-nr"))

# Import and summarize campaigning data
haustueren <- read_csv("data/haustueren.csv") |> 
  clean_names()
haustueren_bezirk <- haustueren |> 
  summarize(tueren = sum(turen, na.rm = TRUE),
            geoeffnet = sum(geoffnet, na.rm = TRUE),
            gespraeche = sum(gesprach, na.rm = TRUE),
            .by = vorwahlkampf) |> 
  rename(bezirk = vorwahlkampf)

# Join results and campaigning data
wahlbezirke_zugewinne_vergleich_tueren <- wahlbezirke_zugewinne_vergleich |> 
  left_join(haustueren_bezirk,
            by = c("WBez" = "bezirk"))

# Specify sizes
size_title = 30
size_subtitle = size_title * 2/3
size_legend = size_title * 2/3
size_caption = size_title * 2/5

# Visualize: circles
wahlbezirke_zugewinne_vergleich_tueren_kreise <- wahlbezirke_zugewinne_vergleich_tueren |> 
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
  geom_point(aes(size = tueren,
                 geometry = geometry),
             pch = 21,
             fill = alpha("lightgrey", 0.2),
             col = "black",
             stroke = 0.25,
             stat = "sf_coordinates") +
  theme_void() + # Remove plot elements
  scale_size_continuous(name = "Geklopfte Türen") +
  scale_fill_brewer(name = "Zuwächse (in %-Punkten)", 
                    palette = "Reds") + # Add reds scale
  labs(title = "You'll never knock alone?", # Add labels
       subtitle = "Geklopfte Türen und Zweistimmenzuwächse ggü. BTW 2021 im Vergleich",
       caption = "Ohne Berücksichtigung von Briefwahlergebnissen") +
  theme(legend.direction = "vertical",
        legend.position = "left",
        legend.title = element_text(size = size_legend),
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

ggsave(filename = "./output/wahlbezirke_zugewinne_tueren_viz_kreise.png",
       wahlbezirke_zugewinne_vergleich_tueren_kreise,
       width = 120,
       height = 75,
       units = "mm")

# Visualize: circles - detailed
wahlbezirke_zugewinne_vergleich_tueren_detailed_kreise <- wahlbezirke_zugewinne_vergleich_tueren |> 
  filter(!is.na(diff_2021_2025)) |> 
  mutate(diff_cat = cut(diff_2021_2025, 
                        breaks = seq(0, 22.5, 2.5))) |> 
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
  geom_point(aes(size = tueren,
                 geometry = geometry),
             pch = 21,
             fill = alpha("lightgrey", 0.2),
             col = "black",
             stroke = 0.25,
             stat = "sf_coordinates") +
  theme_void() + # Remove plot elements
  scale_size_continuous(name = "Geklopfte Türen") +
  scale_fill_brewer(name = "Zuwächse (in %-Punkten)", 
                    palette = "Reds") + # Add reds scale
  labs(title = "You'll never knock alone?", # Add labels
       subtitle = "Geklopfte Türen und Zweistimmenzuwächse ggü. BTW 2021 im Vergleich",
       caption = "Ohne Berücksichtigung von Briefwahlergebnissen") +
  theme(legend.direction = "vertical",
        legend.key.size = unit(3, "mm"),
        legend.key.spacing.y = unit(1.25, "mm"),
        legend.position = "left",
        legend.text = element_text(size = size_legend),
        legend.title = element_text(size = size_legend),
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
                                  hjust = 0.5))

ggsave(filename = "./output/wahlbezirke_zugewinne_tueren_detailed_viz_kreise.png",
       wahlbezirke_zugewinne_vergleich_tueren_detailed_kreise,
       width = 120,
       height = 75,
       units = "mm")

# Visualize: Fixed borders
wahlbezirke_zugewinne_vergleich_tueren_grenzen <- wahlbezirke_zugewinne_vergleich_tueren |> 
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
  geom_sf(data = wahlbezirke_zugewinne_vergleich_tueren |> 
            filter(!is.na(tueren)),
          fill = NA,
          color = "yellow",
          linewidth = 0.25) +
  theme_void() + # Remove plot elements
  scale_fill_brewer(name = "%-Punkte", palette = "Reds") + # Add reds scale
  labs(title = "You'll never knock alone?", # Add labels
       subtitle = "Haustürwahlkampf und Zweistimmenzuwächse ggü. BTW 2021 im Vergleich",
       caption = "Ohne Berücksichtigung von Briefwahlergebnissen") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.text = element_text(size = size_legend),
        legend.title = element_text(size = size_legend),
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

ggsave(filename = "./output/wahlbezirke_zugewinne_tueren_viz_grenzen.png",
       wahlbezirke_zugewinne_vergleich_tueren_grenzen,
       width = 90,
       height = 75,
       units = "mm")

# Visualize: Fixed borders - detailed
wahlbezirke_zugewinne_vergleich_tueren_detailed_grenzen <- wahlbezirke_zugewinne_vergleich_tueren |> 
  filter(!is.na(diff_2021_2025)) |> 
  mutate(diff_cat = cut(diff_2021_2025, 
                        breaks = seq(0, 22.5, 2.5))) |> 
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
  geom_sf(data = wahlbezirke_zugewinne_vergleich_tueren |> 
            filter(!is.na(tueren)),
          fill = NA,
          color = "yellow",
          linewidth = 0.25) +
  theme_void() + # Remove plot elements
  scale_fill_brewer(name = "%-Punkte", palette = "Reds") + # Add reds scale
  labs(title = "You'll never knock alone?", # Add labels
       subtitle = "Haustürwahlkampf und Zweistimmenzuwächse ggü. BTW 2021 im Vergleich",
       caption = "Ohne Berücksichtigung von Briefwahlergebnissen") +
  theme(legend.direction = "vertical",
        legend.key.size = unit(3, "mm"),
        legend.key.spacing.y = unit(1.5, "mm"),
        legend.position = "left",
        legend.text = element_text(size = size_legend),
        legend.title = element_text(size = size_legend),
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
                                  hjust = 0.5))

ggsave(filename = "./output/wahlbezirke_zugewinne_tueren_detailed_viz_grenzen.png",
       wahlbezirke_zugewinne_vergleich_tueren_detailed_grenzen,
       width = 105,
       height = 75,
       units = "mm")

# Visualize: stripe pattern fill
wahlbezirke_zugewinne_vergleich_tueren_streifen <- wahlbezirke_zugewinne_vergleich_tueren |> 
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
  geom_sf_pattern(data = wahlbezirke_zugewinne_vergleich_tueren |> 
                    filter(!is.na(tueren)),
                  fill = NA,
                  color = "black",
                  linewidth = NA,
                  pattern = "stripe",
                  pattern_angle = 45,
                  pattern_density = 0.5,
                  pattern_fill = NA,
                  pattern_size = 0.3) +
  theme_void() + # Remove plot elements
  scale_fill_brewer(name = "%-Punkte", palette = "Reds") + # Add reds scale
  labs(title = "You'll never knock alone?", # Add labels
       subtitle = "Haustürwahlkampf und Zweistimmenzuwächse ggü. BTW 2021 im Vergleich",
       caption = "Ohne Berücksichtigung von Briefwahlergebnissen") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.text = element_text(size = size_legend),
        legend.title = element_text(size = size_legend),
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

ggsave(filename = "./output/wahlbezirke_zugewinne_tueren_viz_streifen.png",
       wahlbezirke_zugewinne_vergleich_tueren_streifen,
       width = 90,
       height = 75,
       units = "mm")

# Visualize: stripe pattern fill - detailed
wahlbezirke_zugewinne_vergleich_tueren_detailed_streifen <- wahlbezirke_zugewinne_vergleich_tueren |> 
  filter(!is.na(diff_2021_2025)) |> 
  mutate(diff_cat = cut(diff_2021_2025, 
                        breaks = seq(0, 22.5, 2.5))) |> 
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
  geom_sf_pattern(data = wahlbezirke_zugewinne_vergleich_tueren |> 
                    filter(!is.na(tueren)),
                  fill = NA,
                  color = "black",
                  linewidth = NA,
                  pattern = "stripe",
                  pattern_angle = 45,
                  pattern_density = 0.5,
                  pattern_fill = NA,
                  pattern_size = 0.3) +
  theme_void() + # Remove plot elements
  scale_fill_brewer(name = "%-Punkte", palette = "Reds") + # Add reds scale
  labs(title = "You'll never knock alone?", # Add labels
       subtitle = "Haustürwahlkampf und Zweistimmenzuwächse ggü. BTW 2021 im Vergleich",
       caption = "Ohne Berücksichtigung von Briefwahlergebnissen") +
  theme(legend.direction = "vertical",
        legend.key.size = unit(3, "mm"),
        legend.key.spacing.y = unit(1.5, "mm"),
        legend.position = "left",
        legend.text = element_text(size = size_legend),
        legend.title = element_text(size = size_legend),
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
                                  hjust = 0.5))

ggsave(filename = "./output/wahlbezirke_zugewinne_tueren_detailed_viz_streifen.png",
       wahlbezirke_zugewinne_vergleich_tueren_detailed_streifen,
       width = 105,
       height = 75,
       units = "mm")

# Visualize: point pattern fill
wahlbezirke_zugewinne_vergleich_tueren_punkte <- wahlbezirke_zugewinne_vergleich_tueren |> 
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
  geom_sf_pattern(data = wahlbezirke_zugewinne_vergleich_tueren |> 
                    filter(!is.na(tueren)),
                  size = 0.03,
                  color = "black",
                  linewidth = NA,
                  fill = NA,
                  pattern = "pch",
                  pattern_angle = 0,
                  pattern_density = 0.2,
                  pattern_shape = 20,
                  pattern_spacing = 0.015) +
  theme_void() + # Remove plot elements
  scale_fill_brewer(name = "%-Punkte", palette = "Reds") + # Add reds scale
  labs(title = "You'll never knock alone?", # Add labels
       subtitle = "Haustürwahlkampf und Zweistimmenzuwächse ggü. BTW 2021 im Vergleich",
       caption = "Ohne Berücksichtigung von Briefwahlergebnissen") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        legend.text = element_text(size = size_legend),
        legend.title = element_text(size = size_legend),
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

ggsave(filename = "./output/wahlbezirke_zugewinne_tueren_viz_punkte.png",
       wahlbezirke_zugewinne_vergleich_tueren_punkte,
       width = 90,
       height = 75,
       units = "mm")

# Visualize: point pattern fill - detailed
wahlbezirke_zugewinne_vergleich_tueren_detailed_punkte <- wahlbezirke_zugewinne_vergleich_tueren |> 
  filter(!is.na(diff_2021_2025)) |> 
  mutate(diff_cat = cut(diff_2021_2025, 
                        breaks = seq(0, 22.5, 2.5))) |>   ggplot() +
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
  geom_sf_pattern(data = wahlbezirke_zugewinne_vergleich_tueren |> 
                    filter(!is.na(tueren)),
                  size = 0.03,
                  color = "black",
                  linewidth = NA,
                  fill = NA,
                  pattern = "pch",
                  pattern_angle = 0,
                  pattern_density = 0.2,
                  pattern_shape = 20,
                  pattern_spacing = 0.015) +
  theme_void() + # Remove plot elements
  scale_fill_brewer(name = "%-Punkte", palette = "Reds") + # Add reds scale
  labs(title = "You'll never knock alone?", # Add labels
       subtitle = "Haustürwahlkampf und Zweistimmenzuwächse ggü. BTW 2021 im Vergleich",
       caption = "Ohne Berücksichtigung von Briefwahlergebnissen") +
  theme(legend.direction = "vertical",
        legend.key.size = unit(3, "mm"),
        legend.key.spacing.y = unit(1.5, "mm"),
        legend.position = "left",
        legend.text = element_text(size = size_legend),
        legend.title = element_text(size = size_legend),
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
                                  hjust = 0.5))

ggsave(filename = "./output/wahlbezirke_zugewinne_tueren_detailed_viz_punkte.png",
       wahlbezirke_zugewinne_vergleich_tueren_detailed_punkte,
       width = 105,
       height = 75,
       units = "mm")

# Scatterplot: campaigning vs. electoral gains
wahlbezirke_zugewinne_vergleich_tueren |> 
  filter(!is.na(tueren)) |> 
  select(diff_2021_2025, tueren, geoeffnet, gespraeche) |> 
  pivot_longer(cols = 2:4, 
               names_to = "name",
               values_to = "value") |> 
  ggplot(aes(x = log(value),
             y = diff_2021_2025,
             col = name)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Anzahl (log. Skala)", y = "Stimmenzuwachs (%-Punkte)") +
  scale_color_viridis_d(name = "Art") 