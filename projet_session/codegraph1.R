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
    values_to = "lancements"
  )

colors <- c(
  "Chine"       = "#bf281d",
  "États-Unis"  = "#395b9e",
  "Europe"      = "#348245",
  "Russie"      = "#76548f"
)

p1 <- ggplot(df_long, aes(x = année, y = lancements, color = region)) +
  
  geom_line(size = 1.5) +  
  
  scale_color_manual(values = colors) +
  
  scale_x_continuous(
    breaks = seq(2000, 2024, by = 2),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 160, by = 20),
    limits = c(0, 160),
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
    title = "Lancements orbitaux par année et par pays (2000–2024)",
    x = "Année",
    y = "Nombre de lancements",
    color = "Pays"
  )

p1


ggplotly(p1, tooltip = c("année", "region", "lancements"))
