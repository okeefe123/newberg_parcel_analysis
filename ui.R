# # ui.R
# 
# fluidPage(
#   titlePanel("Interactive Leaflet Map"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       shiny::uiOutput("data_color_dropdown_ui"),
#       shiny::uiOutput("data_size_dropdown_ui"),
#       sliderInput("radius", "Select radius (in miles):", min = 0.1, max = 1, value = 0.1)
#     ),
#     
#     mainPanel(
#       list(
#         leafletOutput("mymap"),
#         DT::dataTableOutput("map_vals_dt_render")
#       )
#     )
#   )
# )




library(bs4Dash)
library(fresh)

mytheme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "gray",
    navbar_dark_color = "pink",
    navbar_light_active_color = "#FFF",
    navbar_light_hover_color = "#FFF"
  ),
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_dark = "#FFF", 
    text_light = "#272c30"
  ),
  bs4dash_layout(
    main_bg = "#353c42"
  ),
  bs4dash_sidebar_light(
    bg = "#272c30", 
    color = "#bec5cb",
    hover_color = "#FFF",
    submenu_bg = "#272c30", 
    submenu_color = "#FFF", 
    submenu_hover_color = "#FFF"
  ),
  bs4dash_status(
    primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
  ),
  bs4dash_color(
    gray_900 = "#FFF"
  )
)

dashboardPage(
  dashboardHeader(title = "Newburg Parcel Analysis"),
  dashboardSidebar(
    shiny::uiOutput("data_color_dropdown_ui"),
    shiny::uiOutput("data_size_dropdown_ui"),
    sliderInput("radius", "Select radius (in miles):", min = 0.1, max = 4, value = 1)
  ),
  dashboardBody(
    use_theme(mytheme),
    # Boxes need to be put in a row (or column)
    fluidRow(
      leafletOutput("mymap",height="750px"),
    ),
    fluidRow(
      DT::dataTableOutput("map_vals_dt_render")
    )
  )
)