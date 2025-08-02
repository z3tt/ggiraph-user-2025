library(tidyverse)
library(ggiraph)
library(janitor)
library(glue)

plot_co_pal<-list(
  "Germany" = "#129490", 
  "United States" = "#5F4BB6")


#data from our world in data
df_co_emissions<-read.csv("data/per-capita-co-emissions.csv")|>
  janitor::clean_names()|>
  dplyr::rename(co2_emissions = annual_co_emissions_per_capita)|>
  dplyr::filter(entity %in% c("United States", "Germany") & year>=1900 & year<=2025)


# interpolate data for smoother crosshair experience
df_co_interpolated <- df_co_emissions |>
  dplyr::group_by(entity) |>
  dplyr::summarise(
    interpolated = list({
      year_seq <- seq(min(year), max(year), by = 0.1)
      co2_interp <- approx(x = year, y = co2_emissions, 
                           xout = year_seq, method = "linear")
      tibble(year = co2_interp$x, y_val = co2_interp$y)
    })
  ) |>
  tidyr::unnest(interpolated)|>
  dplyr::mutate(nearest_year = round(year))|>
  dplyr::left_join(
    df_co_emissions,
    by = c("nearest_year"="year", "entity"="entity")
  )

df_co_interpolated_year<-df_co_interpolated|>
  tidyr::pivot_wider(
    id_cols = year,
    names_from = entity, 
    values_from = co2_emissions
  )|>
  janitor::clean_names()|>
  dplyr::mutate(
    nearest_year = round(year),
    tooltip = glue::glue("
      <div style='background:white;box-shadow: rgba(0, 0, 0, 0.1) 0px 4px 6px -1px, rgba(0, 0, 0, 0.06) 0px 2px 4px -1px;'>
        <div style='padding:8px; font-weight: bold; margin-bottom: 8px; font-size: 16px;background:black;color:white;'>{nearest_year}</div>
        <div style='padding:0px 8px 8px'>
        <table style='border-collapse: collapse; width: 100%;'>
          <tr>
            <td style='text-align: left; padding: 2px 8px 2px 0; border: none;'>
              <span style='display: inline-block; width: 10px; height: 10px; background-color: {plot_co_pal$`United States`}; margin-right: 6px; vertical-align: middle;'></span>United States
            </td>
            <td style='text-align: right; padding: 2px 4px; border: none; font-family: Roboto Mono'>
              {format(round(united_states, 2), nsmall = 2)}
            </td>
          </tr>
          <tr>
            <td style='text-align: left; padding: 2px 8px 2px 0; border: none;'>
              <span style='display: inline-block; width: 10px; height: 10px; background-color: {plot_co_pal$Germany}; margin-right: 6px; vertical-align: middle;'></span>Germany
            </td>
            <td style='text-align: right; padding: 2px 4px; border: none; font-family: Roboto Mono'>
              {format(round(germany, 2), nsmall = 2)}
            </td>
          </tr>
        </table>
        </div>
      </div>
    ")
  )

emissions_plot<-ggplot()+
  #line chart with co2 emissions
  geom_line(
    data = df_co_emissions,
    mapping = aes(
      x = year,
      y = co2_emissions,
      group = entity,
      color = entity
    ),
    linewidth = 1
  )+
  #interactive cross hair, hide with alpha
  ggiraph::geom_vline_interactive(
    data = df_co_interpolated_year,
    mapping = aes(
      data_id = year,
      xintercept = year,
      tooltip = tooltip
    ),
    alpha = 0.001,
    color = "black",
    linewidth = 0.4
  )+
  #interactive point, hide with alpha
  ggiraph::geom_point_interactive(
    data = df_co_interpolated,
    mapping = aes(
      data_id = year,
      x = year,
      y = y_val,
      color = entity
    ),
    alpha = 0.001,
    shape = 19,
    size = 4,
    show.legend = FALSE
  )+
  #recolor lines
  scale_color_manual(values = unlist(plot_co_pal))+
  #labels
  labs(
    title = "Per Capita CO² Emissions: United States vs. Germany",
    subtitle = "Carbon dioxide emissions from fossil fuels and industry. Measured in tons per person.",
    caption = "Source: Our World in Data",
    color = "Country"
  )+
  guides(
    color = guide_legend(override.aes = list(linewidth =1))
  )+
  #theme
  theme(
    legend.position  = c(0.15,0.96),
    legend.direction = "horizontal",
    text = element_text(family = "Roboto"),
    plot.title = element_text(face = "bold", size = 18),
    plot.caption = element_text(color = "#AAAAAA", margin = margin(t=10)),
    plot.subtitle = element_text(color = "#AAAAAA", size=14),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(family = "Roboto Mono", size=12),
    axis.text.x = element_text(size=12, margin = margin(t=10)),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25)
  )

# ggiraph::girafe(
#   ggobj = emissions_plot,
#   width_svg = 8,
#   height_svg = 6,
#   options = list(
#     ggiraph::opts_toolbar(saveaspng = FALSE),
#     opts_tooltip(css = "font-family:Roboto"),
#     opts_hover(
#       css = "stroke-opacity:100%;fill-opacity:100%",
#       nearest_distance = NULL)
#   )
# )