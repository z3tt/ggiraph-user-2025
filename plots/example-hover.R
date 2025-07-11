library(tidyverse)
library(ggimage)
library(glue)
library(ggtext)
library(geomtextpath)

#break out data for eps
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-11-23/episodes.csv')

#base url for images
base_url = "https://raw.githubusercontent.com/tashapiro/TidyTuesday/master/2021/W48/images/"

#data frame for doctors
df_doctor <- data.frame(
  doctor = c(
    "Christopher Eccleston",
    rep("David Tennant", 3),
    rep("Matt Smith", 3),
    rep("Peter Capaldi", 3),
    rep("Jodie Whitaker", 3)
  ),
  doctor_num = c(
    "Ninth Doctor",
    rep("Tenth Doctor", 3),
    rep("Eleventh Doctor", 3),
    rep("Twelfth Doctor", 3),
    rep("Thirteenth Doctor", 3)
  ),
  season = 1:13
)

df_doctor$image <- paste0(base_url, tolower(str_replace(df_doctor$doctor, " ", "_")), "_no_bg.png")


df_subset_episodes <- episodes |>
  dplyr::left_join(df_doctor, by = c("season_number" = "season")) |>
  dplyr::filter(!is.na(rating) & !is.na(season_number))

overall_avg <- mean(df_subset_episodes$rating)

#data
df_eps <- df_subset_episodes |>
  dplyr::group_by(doctor) |>
  dplyr::mutate(
    avg_rating = mean(rating),
    color = case_when(
      doctor == "David Tennant" ~ '#658C5A',
      doctor == "Matt Smith" ~ '#79549F',
      doctor == "Jodie Whitaker" ~ '#F2BE5C',
      doctor == "Christopher Eccleston" ~ "#DA4E41",
      doctor == "Peter Capaldi" ~ "#72A5B8"
    ),
    font_color = case_when(
      doctor == "Jodie Whitaker" ~ "black",
      TRUE ~ "white"
    ),
    tooltip = glue::glue(
      "<div style='border:2px {color} solid;border-radius:2px;'>
    <div style='padding:8px;background:{color};color:{font_color};border-top-left-radius:2px;border-top-right-radius:2px;'>
      <div style='font-weight:bold;'>S{season_number}E{episode_number}</div>
      <div style='font-style:italic;'>{episode_title}</div>
    </div>
    <div style='padding:8px;background:black;color:white;border-bottom-left-radius:2px;border-bottom-right-radius:2px;'>
      <table style='border-collapse: collapse; width: 100%; margin: 0;'>
        <tr>
          <td style='text-align: left; padding: 2px 8px 2px 0; border: none; color: white;'>
            Episode Rating
          </td>
          <td style='text-align: right; padding: 2px 0; border: none; font-weight: 500; color: white;font-family: Roboto Mono;'>
            {format(round(rating, 1), nsmall = 1)}
          </td>
        </tr>
        <tr>
          <td style='text-align: left; padding: 2px 8px 2px 0; border: none; color: white;'>
            Avg Doctor Rating
          </td>
          <td style='text-align: right; padding: 2px 0; border: none; font-weight: 500; color: white;font-family: Roboto Mono;'>
            {format(round(avg_rating, 1), nsmall = 1)}
          </td>
        </tr>
        <tr>
          <td style='text-align: left; padding: 2px 8px 2px 0; border: none; color: white;'>
            Avg Overall Rating
          </td>
          <td style='text-align: right; padding: 2px 0; border: none; font-weight: 500; color: white;font-family: Roboto Mono;'>
            {format(round(overall_avg, 1), nsmall = 1)}
          </td>
        </tr>
      </table>
    </div>
  </div>"
    ))|>
  dplyr::ungroup()



df_doc_avg <- df_eps|> 
  dplyr::distinct(doctor, doctor_num, image, avg_rating, color)|>
  dplyr::mutate(
    label = glue(
      "<span style='font-size:10pt;'>**{doctor}**</span><br><span style='font-size:8pt;color:#CECECE;'>{doctor_num}</span>"
    )
  )



 
 pal_line = "white"
 pal_text = "white"
 pal_bg = "black"
 
 
 doctor_who_basic_plot<-ggplot() +
   #interactive points per episode
   ggiraph::geom_jitter_interactive(
     data = df_eps,
     position = position_jitter(seed = 42, height = .2, width =3),
     mapping = aes(
       data_id = story_number,
       x = rating,
       y = reorder(doctor, avg_rating),
       fill = I(color),
       tooltip = tooltip
     ),
     shape = 21,
     color = "black",
     size = 3,
     alpha = 0.8
   ) +
   geomtextpath::geom_textvline(
     mapping = aes(
       xintercept = overall_avg,
       label = paste0("Overall Avg: ", round(overall_avg, 0))
     ),
     size = 3,
     color = pal_line,
     hjust = 0.86,
     vjust = -.2,
     family = "Roboto"
   ) +
   geom_segment(
     data = df_doc_avg,
     mapping = aes(
       x = avg_rating,
       xend = overall_avg,
       y = doctor,
       yend = doctor
     ),
     color = pal_line
   ) +
   geom_point(
     data = df_doc_avg,
     mapping = aes(x = avg_rating, y = doctor, fill = I(color)),
     shape = 21, 
     color = "white",
     size = 10
   ) +
   geom_image(
     data = df_doc_avg,
     mapping = aes(x = avg_rating, y = doctor, image = image),
     size = 0.06,
     asp = 1.61
   ) +
   geom_text(
     data = df_doc_avg,
     mapping = aes(
       x = avg_rating,
       y = doctor,
       label = round(avg_rating, 1)
     ),
     size = 2.5,
     fontface= "bold",
     color = "white",
     vjust = 3.75,
     family = "Roboto"
   ) +
   geom_textbox(
     data = df_doc_avg,
     mapping = aes(x = 59.1, y = doctor, label = label),
     family = "Roboto",
     fill = NA,
     box.size = NA,
     box.padding = unit(rep(0, 4), "pt"),
     color = pal_text,
     hjust = 0
   ) +
   #arrows
   annotate(
     geom = "text",
     label = "Avg Rating\nper Doctor",
     x = 76,
     y = 2.5,
     size = 2.5,
     color = "white",
     family = "Roboto"
   ) +
   geom_curve(
     mapping = aes(
       x = 77,
       xend = 81.4,
       y = 2.7,
       yend = 3
     ),
     color = "white",
     curvature = -0.2,
     linewidth = 0.3,
     arrow = arrow(length = unit(0.08, "in"))
   ) +
   geom_curve(
     mapping = aes(
       x = 77,
       xend = 80.8,
       y = 2.3,
       yend = 2
     ),
     color = "white",
     curvature = 0.2,
     linewidth = 0.3,
     arrow = arrow(length = unit(0.08, "in"))
   ) +
   scale_x_continuous(
     limits = c(59, 95),
     expand = c(0, 0),
     breaks = c(70, 75, 80, 85, 90, 95)
   ) +
   coord_equal(ratio = 50 / 12) +
   labs(
     title = "Doctor Who was The Best? (Basic Highlight)",
     subtitle = "Ratings by Episode and Doctor for the popular TV series, Doctor Who.",
     x = "Rating"
   )+
   theme(
     legend.position = "none",
     plot.background = element_rect(fill = pal_bg, color = pal_bg),
     panel.background = element_blank(),
     panel.grid = element_blank(),
     plot.margin = margin(
       l = 20,
       r = 40,
       b = 10,
       t = 20
     ),
     plot.caption = element_text(size = 7, color = "grey80"),
     plot.title = element_text(
       size = 14,
       face = "bold",
       margin = margin(b = 5)
     ),
     plot.subtitle  = element_text(size = 9, color = "#BABABA"),
     text = element_text(color = pal_text, family = "Roboto"),
     axis.text = element_text(color = pal_text, family = "Roboto Mono"),
     axis.text.y = element_blank(),
     axis.title.y = element_blank(),
     axis.title.x = element_textbox_simple(
       margin = margin(t = 10),
       halign = 0.675,
       hjust = 0.5
     ),
     axis.ticks = element_blank()
   )
 
 

 doctor_who_advanced_plot<-ggplot() +
   #interactive points per episode
   ggiraph::geom_jitter_interactive(
     data = df_eps,
     position = position_jitter(seed = 42, height = .2, width =3),
     mapping = aes(
       data_id = doctor,
       x = rating,
       y = reorder(doctor, avg_rating),
       fill = I(color),
       tooltip = tooltip
     ),
     shape = 21,
     color = "black",
     size = 3,
     alpha = 0.8
   ) +
   geomtextpath::geom_textvline(
     mapping = aes(
       xintercept = overall_avg,
       label = paste0("Overall Avg: ", round(overall_avg, 0))
     ),
     size = 3,
     color = pal_line,
     hjust = 0.86,
     vjust = -.2,
     family = "Roboto"
   ) +
   geom_segment(
     data = df_doc_avg,
     mapping = aes(
       x = avg_rating,
       xend = overall_avg,
       y = doctor,
       yend = doctor
     ),
     color = pal_line
   ) +
   #interactive points per doctor (background) -link intearctive geoms with the same data_id
   ggiraph::geom_point_interactive(
     data = df_doc_avg,
     mapping = aes(data_id = doctor, x = avg_rating, y = doctor, fill = I(color)),
     shape = 21, 
     color = "white",
     size = 10
   ) +
   geom_image(
     data = df_doc_avg,
     mapping = aes(x = avg_rating, y = doctor, image = image),
     size = 0.06,
     asp = 1.61
   ) +
   geom_text(
     data = df_doc_avg,
     mapping = aes(
       x = avg_rating,
       y = doctor,
       label = round(avg_rating, 1)
     ),
     size = 2.5,
     fontface= "bold",
     color = "white",
     vjust = 3.75,
     family = "Roboto"
   ) +
   ggiraph::geom_text_interactive(
     data = df_doc_avg,
     mapping = aes(data_id = doctor, x = 59.1, y = doctor, label = doctor, hover_css = glue("fill:{color} ")),
     family = "Roboto",
     color = pal_text,
     fontface= "bold",
     hjust = 0
   ) +
   geom_text(
     data = df_doc_avg,
     mapping = aes(x = 59.1, y = doctor, label = doctor_num),
     vjust = 3,
     size = 3,
     family = "Roboto",
     color = '#CECECE',
     hjust = 0
   ) +
   #arrows
   annotate(
     geom = "text",
     label = "Avg Rating\nper Doctor",
     x = 76,
     y = 2.5,
     size = 2.5,
     color = "white",
     family = "Roboto"
   ) +
   geom_curve(
     mapping = aes(
       x = 77,
       xend = 81.4,
       y = 2.7,
       yend = 3
     ),
     color = "white",
     curvature = -0.2,
     linewidth = 0.3,
     arrow = arrow(length = unit(0.08, "in"))
   ) +
   geom_curve(
     mapping = aes(
       x = 77,
       xend = 80.8,
       y = 2.3,
       yend = 2
     ),
     color = "white",
     curvature = 0.2,
     linewidth = 0.3,
     arrow = arrow(length = unit(0.08, "in"))
   ) +
   scale_x_continuous(
     limits = c(59, 95),
     expand = c(0, 0),
     breaks = c(70, 75, 80, 85, 90, 95)
   ) +
   coord_equal(ratio = 50 / 12) +
   labs(
     title = "Doctor Who was The Best? (Advanced Highlight)",
     subtitle = "Ratings by Episode and Doctor for the popular TV series, Doctor Who.",
     x = "Rating"
   )+
   theme(
     legend.position = "none",
     plot.background = element_rect(fill = pal_bg, color = pal_bg),
     panel.background = element_blank(),
     panel.grid = element_blank(),
     plot.margin = margin(
       l = 20,
       r = 40,
       b = 10,
       t = 20
     ),
     plot.caption = element_text(size = 7, color = "grey80"),
     plot.title = element_text(
       size = 14,
       face = "bold",
       margin = margin(b = 5)
     ),
     plot.subtitle  = element_text(size = 9, color = "#BABABA"),
     text = element_text(color = pal_text, family = "Roboto"),
     axis.text = element_text(color = pal_text, family = "Roboto Mono"),
     axis.text.y = element_blank(),
     axis.title.y = element_blank(),
     axis.title.x = element_textbox_simple(
       margin = margin(t = 10),
       halign = 0.675,
       hjust = 0.5
     ),
     axis.ticks = element_blank()
   )
 
 # ggiraph::girafe(
 #   ggobj = doctor_who_advanced_plot,
 #   options = list(
 #     #turnoff download png
 #    ggiraph::opts_toolbar(saveaspng = F),
 #    #default tooltip font
 #    ggiraph::opts_tooltip(
 #      css = "font-family:Roboto;"
 #    ),
 #    #remove default opts_hover settings
 #    ggiraph::opts_hover(
 #      girafe_css(
 #        css = ""
 #      )
 #    ),
 #    #inverted hover, color points grey
 #    ggiraph::opts_hover_inv(
 #      girafe_css(
 #        css = "", 
 #        point = "fill:#515151",
 #        text = NULL
 #      )
 #      )
 #   )
 # )
