library(tidyverse)
library(plotly)

df <- read_delim(
  "/Users/zahra/Documents/zahramayo.github.io/projet_session/data/lancements_pays_2000_2024.csv",
  delim = ";",
  col_types = cols()
)

df_long <- df %>%
  pivot_longer(
    cols = c(Chine, `États-Unis`, Europe, Russie),
    names_to = "region",
    values_to = "launches"
  )

colors <- c(
  "Chine"       = "#bf281d",
  "États-Unis"  = "#395b9e",
  "Europe"      = "#348245",
  "Russie"      = "#76548f"
)


df_share <- df_long %>%
  group_by(année) %>%
  mutate(
    total_mondial = sum(launches),
    part = launches / total_mondial * 100
  ) %>%
  ungroup()


p2 <- ggplot(df_share, aes(x = année, y = part, color = region)) +
  geom_line(size = 1.5) +
  
  scale_color_manual(values = colors) +
  
  scale_x_continuous(
    breaks = seq(2000, 2024, by = 2),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 70, by = 10),
    limits = c(0, 70),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.y = element_line(color = "grey78", linewidth = 0.45),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "right",
    plot.margin = margin(10, 20, 10, 10)
  ) +
  
  labs(
    title = "Part des lancements orbitaux mondiaux par pays (2000–2024)",
    x = "Année",
    y = "Part des lancements mondiaux (%)",
    color = "Pays"
  )

p2

ggplotly(p2, tooltip = c("année", "region", "part"))
