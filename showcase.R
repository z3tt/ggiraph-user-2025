library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(zoo)
library(prismatic)
library(ggplot2)
library(ggtext)
library(ggforce)
library(ggiraph)
library(gdtools)
library(plotly)

colors <- c("#008075", "#660068")

bikes <- readr::read_csv(
  "https://cedricscherer.com/data/london-bikes.csv",
  col_types = "Dcfffilllddddc"
) |> 
  arrange(day_night, date) |> 
  mutate(
    color_text = if_else(day_night == "day", colors[1], colors[2]),
    before = lag(n, n = 1),
    tooltip = paste0(
      "<b style='font-size:13pt;font-weight:700;color:", 
      prismatic::clr_darken(color_text, .25), ";'>", 
      stringr::str_to_upper(day_night), " PERIOD</b><br>",
      "<b style='font-size:18pt;font-weight:700;font-family:asap;'>", 
      lubridate::stamp("Sun, Jan 1, 2000")(date), 
      "</b><br><br>",
      "Bikes rented: <b>&ensp;", format(n, big.mark = ","), "</b><br>",
      "Temperature: <b>&ensp;", sprintf("%2.1f", temp), "°C</b>"
    ),
    tooltip = paste0("<span style='font-family:asap condensed;'>", tooltip, "</span>")
  )

bikes_smoothed <- 
  bikes |> 
  group_by(day_night) |> 
  mutate(roll_n = zoo::rollmean(n, 30, align = 'center', fill = NA)) |> 
  ungroup()

gdtools::register_gfont("Asap")
gdtools::register_gfont("Asap Condensed")
gdtools::addGFontHtmlDependency(family = "Asap", "Asap Condensed")

theme_set(theme_minimal(base_size = 12, base_family = "Asap"))
theme_update(
  panel.grid = element_blank(),
  axis.text = element_text(color = "grey40"),
  axis.text.x = element_text(hjust = -.1, margin = margin(-10, 0, 0, 0)),
  axis.line = element_line(color = "grey80", linewidth = .4),
  axis.line.y = element_line(arrow = arrow(length = unit(2, "mm"))),
  axis.ticks = element_line(color = "grey80", linewidth = .4),
  axis.ticks.length.x = unit(1, "lines"),
  plot.title = ggtext::element_markdown(margin = margin(2, 0, 15, 0)),
  plot.title.position = "plot",
  plot.caption = ggtext::element_markdown(hjust = 0, color = "grey40", 
                                          margin = margin(24, 0, 0, 0), lineheight = 1.2),
  plot.caption.position = "plot",
  plot.background = element_rect(fill = "white", color = "white")
)

p_base <- 
  ggplot(bikes, aes(x = date, y = n)) +
  coord_cartesian(clip = "off") +
  #coord_fixed(clip = "off", ratio = 1/150) +
  scale_x_date(
    date_breaks = "3 months", date_labels = "%b '%y", expand = expansion(add = c(4, 14)),
    limits = lubridate::as_date(c("2015-01-01", "2016-12-31"))
  ) +
  scale_y_continuous(
    breaks = 0:5*10000, labels = c(0, paste0(1:5*10, "K")), 
    limits = c(0, NA), expand = expansion(add = c(0, 2000))
  ) +
  scale_color_manual(values = colors, guide = "none") +
  labs(
    x = NULL, y = NULL,
    title = "Registered TfL bike shares by *<b style='color:#008075;'>day</b>* and *<b style='color:#660068;'>night</b>*",
    caption = "Source: Transport for London, 2015-2016 (with modifications)  
               **Powered by TfL Open Data**"
  ) 

p_annotations <- list(
  annotate(
    geom = "text",
    family = "Asap Condensed",
    label = "Rolling average\n(30-day window)",
    x = lubridate::as_date("2016-11-05"),
    y = 34000,
    size = 3,
    color = "grey30",
    lineheight = .95,
    hjust = 0
  ),
  annotate(
    geom = "curve",
    x = lubridate::as_date("2016-12-08"),
    y = 31700,
    xend = c(lubridate::as_date("2016-10-28"),
             lubridate::as_date("2016-11-06")),
    yend = c(20500, 7400),
    linewidth = .5,
    arrow = arrow(length = unit(2, "mm"), type = "closed"),
    curvature = -.2,
    color = "grey30"
  ),
  ggforce::geom_mark_hull(
    aes(label = "Tube Network Strikes 2015", filter = n > 40000,
        color = stage(day_night, after_scale = prismatic::clr_lighten(color, .4))),
    description = "Commuters had to deal with severe disruptions in public transport on July 9 and August 6.",
    label.family = c("Asap", "Asap Condensed"), label.fontsize = c(12.5, 9.2),
    expand = unit(3, "mm"), linewidth = 1.2, con.cap = unit(0, "mm"), 
    label.fill = "transparent", con.colour = "#60B6AB"
  ),
  ggforce::geom_mark_hull(
    aes(label = "", filter = n > 18000 & day_night == "night",
        color = stage(day_night, after_scale = prismatic::clr_lighten(color, .4))),
    description = "The disruption also led to a considerable increase in bike rentals during the late hours.",
    label.family = "Asap Condensed", label.fontsize = 9.2,
    expand = unit(3, "mm"), linewidth = 1.2, con.cap = unit(0, "mm"), 
    label.fill = "transparent", con.colour = "#AA6AAC", label.minwidth = unit(58, "mm")
  )
)

p_ggplot <- 
  p_base + 
  p_annotations +
  geom_point(
    aes(color = stage(day_night, after_scale = prismatic::clr_lighten(color, .2)), 
        fill = after_scale(clr_lighten(color, .6))), 
    size = 1.8, alpha = .7, shape = 21
  ) +
  geom_line(
    data = bikes_smoothed,
    aes(y = roll_n, color = stage(day_night, after_scale = prismatic::clr_darken(color, .15))), 
    linewidth = 1
  )

p_plotly <- plotly::ggplotly(p_ggplot, height = 550, width = 900)

p_ggiraph <- 
  p_base +
  p_annotations +
  geom_point_interactive(
    aes(color = stage(day_night, after_scale = prismatic::clr_lighten(color, .2)), 
        fill = after_scale(prismatic::clr_lighten(color, .6)),
        tooltip = date, data_id = date), 
    size = 1.8, alpha = .7, shape = 21
  ) +
  geom_line(
    data = bikes_smoothed,
    aes(y = roll_n, color = stage(day_night, after_scale = prismatic::clr_darken(color, .15))), 
    linewidth = 1
  )

# girafe(ggobj = p_interactive, width_svg = 9, height_svg = 5.5)

p_ggiraph_css <- 
  p_base +
  p_annotations +
  geom_point_interactive(
    aes(color = stage(day_night, after_scale = prismatic::clr_lighten(color, .2)), 
        fill = after_scale(prismatic::clr_lighten(color, .6)),
        tooltip = tooltip, data_id = date), 
    size = 1.8, alpha = .7, shape = 21
  ) +
  geom_line(
    data = bikes_smoothed,
    aes(y = roll_n, color = stage(day_night, after_scale = prismatic::clr_darken(color, .15))), 
    linewidth = 1
  )

# girafe(
#   ggobj = p_ggiraph_css, width_svg = 9, height_svg = 5.5,
#   options = list(
#     opts_tooltip(
#       use_fill = TRUE, offx = 18, offy = -35,
#       css = "font-size:16pt;font-weight:500;padding:12px;font-family:asap condensed;"
#     ),
#     opts_hover(
#       css = "opacity:1;stroke-width:3px;r:5px;transition:all 0.2s ease;"
#     ),
#     opts_hover_inv(css = "opacity:0.3;")
#     )
#   )