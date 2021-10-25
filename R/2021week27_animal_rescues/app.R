#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(showtext)
library(myGraphicsToolkit)
library(sf)
library(maptools)
library(ggtext)
library(wesanderson)
library(patchwork)
library(gghighlight)

# Define UI for application that draws a histogram
ui <- fluidPage(
    setBackgroundColor(color = "white"),
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #899DA4}")),

    # Application title
    titlePanel(""),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "YEAR",
                        min = 2009,
                        max = 2020,
                        value = 2020,
                        step = 1,
                        animate = TRUE,
                        sep = ""),
            selectInput("animal", "ANIMAL",
                        choices = list("Cat" = "Cat", "Dog" = "Dog", "Bird" = "Bird"),
                        selected = "Cat"),
            helpText("Explore London animal rescues! Notice how the number of cat and bird rescues reached an all-time high in 2020, while the dog rescues have been steadily decreasing."),
            width = 4
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Selected animal and year
    animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')
    animal_rescues$animal_group_parent <- fct_collapse(animal_rescues$animal_group_parent, Cat = c("Cat", "cat"))
    animal_rescues$special_service_type_category <- fct_recode(animal_rescues$special_service_type_category,
                                                               Height = "Animal rescue from height",
                                                               Below_ground = "Animal rescue from below ground",
                                                               Water = "Animal rescue from water",
                                                               Other = "Other animal assistance")

    selected_animal_year <- reactive({
        animal_rescues %>%
            filter(animal_group_parent == input$animal,
                   cal_year == input$year)
        })
    across_time <- reactive({
        animal_rescues %>%
            filter(animal_group_parent == input$animal,
                   cal_year < 2021)
    })

    # Map
    london <- readRDS(gzcon(url("https://raw.githubusercontent.com/emmaSkarstein/tidytuesday/master/R/2021week27_londonmap.rds")))

    # Font
    f1 <- "Oswald"
    f2 <- "Open Sans"
    font_add_google(name = f1, family = f1)
    font_add_google(name = f2, family = f2)
    showtext_auto()

    # Colors
    col_points <- wes_palette("Royal1")
    col_bg <- "white"
    col_panel <- "gray20"
    col_text <- "gray10"
    col_lines <- "gray30"

    output$distPlot <- renderPlot({
        plot_map <- ggplot(data = selected_animal_year()) +
        # Background map
        geom_polygon(data = london, aes(long, lat, group = group), fill = "grey30", color = "grey40") +
        # Points for animal rescues
        geom_point(aes(x = easting_rounded, y = northing_rounded, fill = special_service_type_category),
                   pch = 21, color = "black", size = 5) +
        scale_fill_manual(values = col_points) +
        labs(title = "LONDON ANIMAL RESCUES",
             subtitle = "<span style='color:#899DA4;'>below ground   </span>
                 <span style='color:#C93312;'>from a hight   </span>
                 <span style='color:#FAEFD1;'>from water   </span>
                 <span style='color:#DC863B;'>other</span>") +
        coord_equal() +
        my_basic_theme(base_family = f2) +
        theme(text = element_text(family = f2, size = 18, color = col_text),
              plot.title = element_markdown(family = f1,
                                            size = 40,
                                            face = "bold",
                                            margin = margin(b = 20)),
              plot.subtitle = element_markdown(size = 30,
                                               face = "bold",
                                               margin = margin(b = 10),
                                               halign = 0.5),
              plot.caption = element_text(size = 12,
                                          margin = margin(t = 10)),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.background = element_rect(fill = col_bg, color = col_bg),
              panel.background = element_rect(fill = col_panel, color = col_panel),
              legend.position = "none",
              panel.spacing = unit(.05, "lines"),
              panel.border = element_rect(color = col_lines, fill = NA, size = 1))

        plot_time <- ggplot(across_time(), aes(x = cal_year, fill = special_service_type_category)) +
            geom_bar(width = 0.5, color = "black") +
            gghighlight(cal_year == input$year) +
            scale_fill_manual(values = col_points) +
            scale_x_continuous(breaks = 2009:2020) +
            scale_y_continuous(limits = c(0, 340)) +
            labs(caption = "Source: London.gov  |  Visualization: Emma Skarstein") +
            my_basic_theme(base_family = f2) +
            theme(plot.caption = element_text(size = 12, margin = margin(t = 20)),
                  axis.title = element_blank(),
                  axis.text.x = element_text(family = f2),
                  plot.background = element_rect(fill = col_bg, color = col_bg),
                  panel.background = element_rect(fill = col_panel, color = col_panel),
                  legend.position = "none",
                  panel.spacing = unit(.05, "lines"),
                  panel.border = element_rect(color = col_lines, fill = NA, size = 1),
                  aspect.ratio = 0.3)
        plot_map / plot_time
    }, width = 600, height = 800)
}

# Run the application
shinyApp(ui = ui, server = server)
