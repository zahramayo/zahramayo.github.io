library(tidyverse)
library(plotly)

european_countries <- tibble::tribble(
  ~region,        ~iso_alpha3,
  "Allemagne",    "DEU",
  "Autriche",     "AUT",
  "Belgique",     "BEL",
  "Danemark",     "DNK",
  "Espagne",      "ESP",
  "Estonie",      "EST",
  "Finlande",     "FIN",
  "France",       "FRA",
  "Grèce",        "GRC",  
  "Hongrie",      "HUN",
  "Irlande",      "IRL",
  "Italie",       "ITA",
  "Luxembourg",   "LUX",
  "Norvège",      "NOR",
  "Pays-Bas",     "NLD",
  "Pologne",      "POL",
  "Portugal",     "PRT",
  "République tchèque","CZE",
  "Roumanie",     "ROU",
  "Royaume-Uni",  "GBR",
  "Slovénie",     "SVN",
  "Suède",        "SWE",
  "Suisse",       "CHE"
)

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

df_europe_agg <- df_long %>% 
  filter(region == "Europe") %>%
  select(année, launches)

df_europe_expanded <- df_europe_agg %>%
  cross_join(european_countries)

df_world_countries <- df_long %>%
  filter(region %in% c("Chine", "États-Unis", "Russie")) %>%
  mutate(iso_alpha3 = case_when(
    region == "États-Unis" ~ "USA",
    region == "Chine"      ~ "CHN",
    region == "Russie"     ~ "RUS",
    TRUE                   ~ NA_character_
  ))

df_choropleth_data <- df_world_countries %>%
  bind_rows(df_europe_expanded) %>%
  filter(!is.na(launches))

max_launches_global <- max(df_choropleth_data$launches, na.rm = TRUE)

p_choropleth_europe <- plot_geo(df_choropleth_data) %>%
  add_trace(
    z = ~launches,          
    locations = ~iso_alpha3, 
    color = ~launches,
    colors = "Reds",
    frame = ~année,
    type = "choropleth",    
    marker = list(line = list(color = "grey", width = 0.5)),
    colorbar = list(title = "Nombre de lancements/année"),
    zmin = 0,
    zmax = max_launches_global,
    hoverinfo = "text",
    text = ~paste0(
      "Pays/Région : ", region, "<br>",
      "Année : ", année, "<br>",
      "Lancements : ", launches, 
      case_when(
        region %in% european_countries$region ~ " (Valeur agrégée de l'Europe)",
        TRUE ~ ""
      )
    )
  ) %>%
  layout(
    title = "Lancements orbitaux par pays et région",
    geo = list(
      scope = "world",
      projection = list(type = "natural earth"),
      showland = TRUE,
      landcolor = "rgb(240, 240, 240)",
      showcountries = TRUE,
      countrycolor = "grey",
      subunitcolor = "grey"
    )
  )

p_choropleth_europe