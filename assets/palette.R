library(tidyverse)

# Palette based on http://www.colourlovers.com/palette/582235/October_Roads
# Color names from "Name that Color": http://chir.ag/projects/name-that-color/
# Font = Oswald: https://fonts.google.com/specimen/Oswald

palette <- tribble(
  ~my_name, ~hex, ~text_color, ~fancy_name,
  "light_blue", "#6CB9DC", "white", "Picton Blue",
  "maroon", "#821F29", "white", "Monarch",
  "orange", "#D46600", "white", "Bamboo",
  "yellow", "#F6E03F", "grey30", "Turbo",
  "green", "#ADBD06", "white", "Pistachio",
  "brown", "#7D4A04", "white", "Mocha"
) %>% 
  mutate(row = rep(2:1, each = 3),
         column = rep(1:3, times = 2),
         fancy_name_upper = str_to_upper(fancy_name))

ggplot(palette, aes(x = column, y = row)) +
  geom_tile(aes(fill = hex), width = 0.85, height = 0.85) +
  geom_text(aes(label = fancy_name_upper, color = text_color),
            family = "Oswald SemiBold", size = 8,
            nudge_y = 0.25) +
  geom_text(aes(label = hex, color = text_color),
            family = "Oswald Light", size = 5,
            nudge_y = -0.25) +
  scale_fill_identity() +
  scale_color_identity() + 
  theme_void()
