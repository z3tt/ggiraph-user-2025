library(readr)
library(dplyr)
library(sf)
library(rmapshaper)
library(rnaturalearth)
library(stringr)
library(here)
library(ggplot2)
library(geomtextpath)
library(ggiraph)
library(gdtools)
library(patchwork)
library(rcartocolor)

owid_old_to_match_regions <- readr::read_csv(here::here("data", "urban-gdp-pop.csv"))

countries_sf <- rnaturalearth::ne_countries(returnclass = "sf") |> 
  select(iso_a3, continent, region_un) |> 
  filter(iso_a3 != "ATA")

countries <- sf::st_drop_geometry(countries_sf)

owid_selected <- 
  readr::read_csv(here::here("data", "urbanization-vs-gdp.csv")) |> 
  filter(Year == 2022) |> 
  select(
    country = `Entity`,
    iso_a3 = `Code`,
    urban_pop = `Population share in urban areas`,
    gdp_per_capita = `GDP per capita`,
    pop_est = `Population (historical)`
  ) |> 
  filter(!is.na(iso_a3))

# owid <- owid_selected |> 
#   left_join(countries) |> 
#   mutate(region_un = if_else(iso_a3 == "SGP", "Asia", region_un)) |> 
#   filter(!is.na(region_un))

owid <- owid_selected |> 
  left_join(countries_sf) |> 
  mutate(
    region_un = if_else(iso_a3 == "SGP", "Asia", region_un),
    continent = if_else(iso_a3 == "SGP", "Asia", continent),
    country = stringr::str_replace_all(country, "'", "’")
  ) |> 
  filter(!is.na(continent)) |> 
  sf::st_as_sf()

owid_borders <- rmapshaper::ms_innerlines(countries_sf)

pal <- rcartocolor::carto_pal(
  name = "Bold", n = 9
)[c(1, 7, 5, 2, 3, 8)]

names(pal) <- unique(owid$continent)

owid_tooltip <- owid |> 
  mutate(
    color = recode(continent, !!! pal), 
    color_text = prismatic::clr_lighten(color, .6),
    tooltip = paste0(
      "<span style='font-family:spline sans:'>",
      "<span style='font-size:15px;color:", color_text, ";'>", continent, "</span><br>",
      "<b style='font-size:22px;'>", country, "</b><br><br>",
      "GDP per capita: <b>&ensp;$", round(gdp_per_capita / 1000, 3), " K</b><br>",
      "Overall population: <b>&ensp;", sprintf("%3.1f", pop_est / 10^6), " M</b><br>",
      "Share urban inhabitants: <b>&ensp;", round(urban_pop, 0), "%</b></span>"
    ),
    continent = factor(continent, levels = c("North America", "South America", "Europe", "Africa", "Asia", "Oceania"))
  ) |> 
  arrange(-pop_est) 

plot_owid <-
  ggplot(owid_tooltip, aes(x = gdp_per_capita, y = urban_pop, size = pop_est)) +
  stat_smooth(method = "lm", color = "grey67", fill = "grey90") +
  geom_point_interactive(
    aes(color = continent, tooltip = tooltip, data_id = country), 
    shape = 16, alpha = .67
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  scale_x_log10(labels = scales::label_dollar()) + 
  scale_y_continuous(labels = function(y) paste0(y, "%"), limits = c(0, 100)) + 
  scale_color_manual(values = pal) +
  scale_size_area(max_size = 18, guide = "none") +
  scale_alpha_manual(values = c(1, 0), guide = "none") +
  labs(x = "**GDP per capita** (international-$ in 2011 prices)", y = "**Share of urban population**", color = NULL,
       title = "The Geography of Growth: Urbanisation and GDP in 2022",
       subtitle = "Hover over the bubbles or map to explore GDP per capita, urban share, and population per country!",
       caption = "Note: GDP per capita (x-axis) is shown on a logarithmic scale.") +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1), nrow = 1)) +
  theme_minimal(base_family = "Spline Sans", base_size = 12) +
  theme(
    axis.title = element_markdown(color = "grey30"),
    axis.title.x = element_markdown(hjust = 0, margin = margin(12, 0, 0, 0)),
    axis.title.y = element_markdown(hjust = 0),
    axis.text = element_text(family = "Spline Sans Mono"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = rel(.9)),
    legend.position = "top", 
    legend.justification = "left",
    legend.location = "plot",
    legend.key.spacing = unit(.7, "lines"),
    legend.key.width = unit(.1, "lines"),
    legend.margin = margin(-56, 1, 12, 425),
    plot.title = element_text(face = "bold", margin = margin(b = 4), size = rel(1.3)),
    plot.subtitle = element_text(color ="grey30", margin = margin(b = 12), size = rel(.77)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, margin = margin(t = 12, b = 0), size = rel(.67), color ="grey30"),
    plot.caption.position = "plot"
  )

map_owid <- 
  ggplot(owid_tooltip) +
  geom_sf(data = countries_sf, color = "transparent", fill = "grey82") +
  geom_sf_interactive(
    aes(fill = continent, tooltip = tooltip, data_id = country), 
    color = "transparent", linewidth = .2
  ) +
  geom_sf(data = owid_borders, color = "white", linewidth = .2) +
  scale_fill_manual(values = pal, guide = "none") +
  coord_sf(crs = "+proj=eqearth", expand = FALSE, clip = "off") +
  labs(caption = "Source: HYDE (2023) – with minor processing by Our World in Data (ourworldindata.org/urbanization)") +
  theme_void(base_family = "Spline Sans", base_size = 12) +
  theme(
    plot.caption = element_text(margin = margin(t = 12, b = 0), size = rel(.67), color ="grey30"),
    plot.caption.position = "plot"
  )

combined_owid <- plot_owid + map_owid +
  plot_layout(ncol = 2, widths = c(.4, .6)) +
  plot_annotation(theme = theme(plot.margin = margin(12, 12, 12, 12)))

# girafe(
#   ggobj = combined_owid, width_svg = 12, height_svg = 5.3,
#   options = list(
#     opts_tooltip(use_fill = TRUE, css = "font-size:15px;font-weight:400;color:white;padding:7px;font-family:spline sans;lineheight:1.3;padding:10px;border-radius:5px;"), 
#     opts_hover(css = "stroke:white;stroke-width:0.5px;opacity:1;"),
#     opts_hover_inv(css = "opacity:0.2;"),
#     opts_toolbar(position = "topright"),
#     opts_zoom(min = 1, max = 4)
#   )
# )