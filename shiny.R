### ----- Load packages ----

library(shiny)
library(tidyverse)
library(scales)
library(shinyWidgets)
library(plotly)


### ---- Define UI ----

# create vector of variable names
# -> take out the categorical variables

df <- read_csv("Real_Estate_Sales_2001-2023_GL.csv")

ui <- fluidPage(
  titlePanel("Shiny Real Estate Explorer"),
  
  sidebarPanel(
    sliderInput(
      "yearSlider",
      "Years",
      min = min(df$`List Year`, na.rm = TRUE),
      max = max(df$`List Year`, na.rm = TRUE),
      value = range(df$`List Year`, na.rm = TRUE),
      sep = ""
    ),
    selectInput(
      "yvar",
      "Y-Axis Variable:",
      choices = c("Sale Amount", "Assessed Value"),
      selected = "Sale Amount"
    ),
    pickerInput(
      inputId = "towns",
      label = "Choose Town(s):",
      choices = sort(unique(df$Town)),
      selected = unique(df$Town)[1:5],
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3"
      )
    ),
    pickerInput(
      inputId = "types",
      label = "Choose Property Type(s):",
      choices = sort(unique(df$`Property Type`)),
      selected = unique(df$`Property Type`),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3"
      )
    )
  ),
  
  
  # create spot for plot
  mainPanel(
    plotlyOutput(outputId = "plot1"),
    plotlyOutput(outputId = "plot2"),
    plotlyOutput(outputId = "plot3")
  )
)


### ---- Define server ----

server <- function(input, output) {
  
  # Filtered by slider
  filtered <- reactive({
    req(input$yearSlider)  # ensure slider exists
    
    df[df$`List Year` >= as.numeric(input$yearSlider[1]) &
         df$`List Year` <= as.numeric(input$yearSlider[2]), ]
  })
  
  # Plot
  output$plot1 <- renderPlotly({
    data <- filtered() %>%
      filter(Town %in% input$towns)
    
    town_year <- data %>%
      group_by(Town, `List Year`) %>%
      summarize(
        value = mean(.data[[input$yvar]], na.rm = TRUE),
        .groups = "drop"
      )
    
    town_year$`List Year` <- as.numeric(town_year$`List Year`)
    
    p <- ggplot(town_year, aes(x = `List Year`, y = value, color = Town, group = Town, text = paste0(
      "<b>Town:</b> ", Town, "<br>",
      "<b>Year:</b> ", `List Year`, "<br>",
      "<b>", input$yvar, ":</b> ", scales::comma(value)
    ))) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "List Year",
        y = input$yvar,
        title = paste("Average", input$yvar, "Over Time by Town"),
        color = "Town"
      ) +
      theme_bw()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot2 <- renderPlotly({
    data <- filtered() %>%
      filter(!is.na(`Residential Type`))
    
    residential_year <- data %>%
      group_by(`Residential Type`, `List Year`) %>%
      summarize(
        value = mean(.data[[input$yvar]], na.rm = TRUE),
        .groups = "drop"
      )
    
    residential_year$`List Year` <- as.numeric(residential_year$`List Year`)
    
    p <- ggplot(residential_year, aes(x = `List Year`, y = value, color = `Residential Type`, group = `Residential Type`, text = paste0(
      "<b>Type:</b> ", `Residential Type`, "<br>",
      "<b>Year:</b> ", `List Year`, "<br>",
      "<b>", input$yvar, ":</b> ", scales::comma(value)
    ))) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "List Year",
        y = input$yvar,
        title = paste("Average", input$yvar, "Over Time by Residential Type"),
        color = "Residential Type"
      ) +
      theme_bw()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot3 <- renderPlotly({
    df_clean <- filtered() %>% 
      filter(!is.na(`Property Type`)) %>%
      filter(`Property Type` %in% input$types)
    
    p <- ggplot(df_clean, aes(x = `Property Type`, fill = `Property Type`)) +
      geom_bar() +
      labs(
        title = "Number of Sales by Property Type",
        x = "Property Type",
        y = "Number of Sales",
        fill = "Property Type"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}


### ---- Run app ----

shinyApp(ui, server)


