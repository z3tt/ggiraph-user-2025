library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(geomtextpath)
library(ggiraph)
library(gdtools)
library(scico)

gdtools::register_gfont("Rethink Sans")
gdtools::register_gfont("Piazzolla")
gdtools::addGFontHtmlDependency(family = c("Rethink Sans", "Piazzolla"))

simpsons_imdb <- readr::read_csv("data/simpsons_imdb_ratings.csv") |> 
  rename("rating" = imdb_rating)

p_simpsons <-
  simpsons_imdb |> 
  mutate(
    title = stringr::str_replace_all(stringr::str_wrap(title, 22), "\\n", "<br>"),
    text_color = if_else(rating > 6.3 & rating < 8.5, "black", "white"),
    lab = paste0("<span style='font-family:rethink sans;color:", text_color, ";'>", "S", sprintf("%02d", season), " E", sprintf("%02d", episode), "<br><b style='font-size:150%;font-weight:600;font-family:piazzolla;'>", title, "</b><br><br>IMDb Rating: ", sprintf("%1.1f", rating))
  ) |> 
  ggplot(aes(x = episode, y = season, fill = rating)) +
  geom_tile_interactive(aes(tooltip = lab, data_id = id), color = "white", stroke = .2) +
  geomtextpath::geom_texthline(
    yintercept = 9.5, linewidth = 1, label = "Season 10 starts", 
    vjust = 1.4, hjust = .995, family = "Rethink Sans", lineheight = .6
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_continuous(breaks = c(1, 1:5*5), limits = c(NA, 27), position = "top") +
  scale_y_reverse(breaks = c(1, 1:5*5, 27)) +
  scico::scale_fill_scico(
    palette = "roma", name = "IMDb Rating",
    guide = guide_colorbar(title.position = "top"), midpoint = mean(simpsons_imdb$rating)
  ) +
  labs(title = 'From season 10 onwards, IMDb ratings for\n"The Simpsons" dropped considerably',
       x = "Episode →", y = "← Season") +
  theme_minimal(base_family = "Rethink Sans", base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title.x.top = element_text(margin = margin(t = 0, b = 6), hjust = 0),
        axis.title.y = element_text(margin = margin(l = 1, r = 6), hjust = .99),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", family = "Piazzolla", size = rel(2), margin = margin(b = 25)),
        legend.position = "top", legend.justification = "left",
        legend.key.width = unit(6, "lines"), legend.key.height = unit(.45, "lines"),
        plot.margin = margin(rep(12, 1)))

# girafe(
#   ggobj = p_simpsons,
#   width_svg = 10.5, height_svg = 8.5,
#   options = list(
#     opts_tooltip(
#       opacity = 1, use_fill = TRUE, offx = 10, offy = 10,
#       css = "background-color:white;padding:15px;border-radius:3px;lineheight:1.4; border:2px solid white;"
#     ),
#     opts_sizing(width = .7),
#     opts_hover_inv(css = "opacity:0.3;"),
#     opts_hover(css = "stroke-width:2;")
#   )
# )